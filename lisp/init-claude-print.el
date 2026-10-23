;;; init-claude-print.el --- Claude session backend over pipes (no PTY) -*- lexical-binding: t; -*-

;;; Commentary:
;; Claude backend for platforms without a PTY (native Windows Emacs, where
;; `eat'/`vterm' can't spawn a child) and without node (no ACP adapter).
;; Runs one long-lived `claude -p' process per buffer, speaking the CLI's
;; stream-json protocol over pipes, so Emacs never blocks.
;;
;; Prompts go in as JSON on stdin; output streams back as JSONL.  With
;; `--permission-prompt-tool stdio' the CLI asks Emacs before any tool the
;; permission mode doesn't already allow: the request shows in the buffer,
;; the session is flagged as waiting, and y / a / n answer it.

;;; Code:

(require 'json)
(require 'ansi-color)

(declare-function kdb-claude-flag-buffer "init-claude-workflow")

;;; ============================================================
;;; Options
;;; ============================================================

(defgroup kdb-claude-print nil
  "Claude sessions over the CLI's stream-json protocol."
  :group 'tools
  :prefix "kdb-claude-print-")

(defcustom kdb-claude-print-executable nil
  "Path to the `claude' CLI, or nil to look it up on `exec-path'.
On Windows the npm shim is usually `claude.cmd'; `executable-find'
resolves the extension automatically."
  :type '(choice (const :tag "Auto-detect" nil) file)
  :group 'kdb-claude-print)

(defcustom kdb-claude-print-model nil
  "Model alias or full name (e.g. \"opus\", \"sonnet\"), or nil for the default."
  :type '(choice (const :tag "CLI default" nil) string)
  :group 'kdb-claude-print)

(defcustom kdb-claude-print-permission-mode "acceptEdits"
  "Permission mode for sessions.
Anything the mode doesn't allow is asked in the session buffer.
\"acceptEdits\" auto-approves file edits and asks for shell; \"default\"
asks for both; \"dontAsk\" silently denies anything not pre-approved;
\"bypassPermissions\" allows everything; \"plan\" is read-only."
  :type '(choice (const "acceptEdits")
                 (const "default")
                 (const "bypassPermissions")
                 (const "plan")
                 (const "dontAsk")
                 (const "auto"))
  :group 'kdb-claude-print)

(defcustom kdb-claude-print-bare nil
  "When non-nil, pass `--bare' for a faster cold start.
Skips hooks, LSP, plugin sync, CLAUDE.md auto-discovery, keychain
reads, and background prefetches.  Note: `--bare' forces API-key auth
\(ANTHROPIC_API_KEY); it will not use a subscription/OAuth login."
  :type 'boolean
  :group 'kdb-claude-print)

(defcustom kdb-claude-print-extra-args nil
  "Extra command-line arguments appended to every invocation."
  :type '(repeat string)
  :group 'kdb-claude-print)

(defcustom kdb-claude-print-buffer-name "*claude-print*"
  "Base name for Claude session buffers (one per session)."
  :type 'string
  :group 'kdb-claude-print)

;;; ============================================================
;;; State (buffer-local in the session buffer)
;;; ============================================================

(defvar-local kdb-claude-print--session-id nil
  "Session id of this buffer's conversation, for `--resume'.")

(defvar-local kdb-claude-print--process nil
  "The live `claude' process for this buffer, if any.")

(defvar-local kdb-claude-print--stdout-acc ""
  "Accumulator for partial JSONL lines from stdout.")

(defvar-local kdb-claude-print--busy nil
  "Non-nil while a turn is running.")

(defvar-local kdb-claude-print--pending nil
  "Permission requests awaiting an answer, oldest first.")

;;; ============================================================
;;; Display mode
;;; ============================================================

(defvar kdb-claude-print-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'kdb-claude-print-continue)
    (define-key map (kbd "G") #'kdb-claude-print-new)
    (define-key map (kbd "y") #'kdb-claude-print-allow)
    (define-key map (kbd "a") #'kdb-claude-print-allow-always)
    (define-key map (kbd "n") #'kdb-claude-print-deny)
    (define-key map (kbd "C-c C-k") #'kdb-claude-print-stop)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `kdb-claude-print-mode'.")

(define-derived-mode kdb-claude-print-mode special-mode "Claude-p"
  "Major mode for Claude session buffers.
\\{kdb-claude-print-mode-map}"
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (setq-local bookmark-make-record-function #'kdb-claude-print--bookmark-record)
  (add-hook 'kill-buffer-hook #'kdb-claude-print--kill nil t))

(defun kdb-claude-print--bookmark-record ()
  "Bookmark record that resumes this buffer's session."
  (unless kdb-claude-print--session-id
    (user-error "No session yet; send a prompt first"))
  `(,(buffer-name)
    (session . ,kdb-claude-print--session-id)
    (filename . ,default-directory)
    (handler . kdb-claude-print--bookmark-jump)))

;;;###autoload
(defun kdb-claude-print--bookmark-jump (bm)
  "Reopen the session in bookmark BM; it resumes on the next prompt (g)."
  (let* ((sid (bookmark-prop-get bm 'session))
         (buf (or (seq-find (lambda (b)
                              (equal (buffer-local-value 'kdb-claude-print--session-id b) sid))
                            (buffer-list))
                  (let ((default-directory (bookmark-get-filename bm)))
                    (with-current-buffer (kdb-claude-print--new-buffer)
                      (setq kdb-claude-print--session-id sid)
                      (kdb-claude-print--insert "── starred session %s · g to resume ──\n\n"
                                                (substring sid 0 (min 8 (length sid))))
                      (current-buffer))))))
    (set-buffer buf)))

(defun kdb-claude-print--new-buffer (&optional label)
  "Create a session buffer named after LABEL or `default-directory'."
  (with-current-buffer
      (generate-new-buffer
       (format "%s: %s*" (string-remove-suffix "*" kdb-claude-print-buffer-name)
               (or label (abbreviate-file-name default-directory))))
    (kdb-claude-print-mode)
    (current-buffer)))

(defun kdb-claude-print--buffer ()
  "Return the current session buffer, else the one rooted deepest above
`default-directory' (most recent on ties), else a new one."
  (let ((dir default-directory))
    (or (and (derived-mode-p 'kdb-claude-print-mode) (current-buffer))
        (car (seq-sort-by
              (lambda (b) (length (buffer-local-value 'default-directory b))) #'>
              (seq-filter
               (lambda (b)
                 (and (eq (buffer-local-value 'major-mode b) 'kdb-claude-print-mode)
                      (file-in-directory-p dir (buffer-local-value 'default-directory b))))
               (buffer-list))))
        (kdb-claude-print--new-buffer))))

(defun kdb-claude-print--insert (fmt &rest args)
  "Insert FMT/ARGS at the end of the output buffer, read-only aware."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (apply #'format fmt args))))

(defun kdb-claude-print--flag ()
  "Mark this session as waiting on you."
  (when (fboundp 'kdb-claude-flag-buffer)
    (kdb-claude-flag-buffer (current-buffer))))

;;; ============================================================
;;; Process
;;; ============================================================

(defun kdb-claude-print--program ()
  "Resolve the claude executable or signal a clear error."
  (or kdb-claude-print-executable
      (executable-find "claude")
      (user-error "Cannot find the `claude' CLI; set `kdb-claude-print-executable'")))

(defun kdb-claude-print--args (resume)
  "Build the argument list.  RESUME non-nil resumes the buffer's session id."
  (append
   (list "-p"
         "--input-format" "stream-json"
         "--output-format" "stream-json"
         "--verbose"
         "--permission-prompt-tool" "stdio"
         "--permission-mode" kdb-claude-print-permission-mode)
   (when kdb-claude-print-model (list "--model" kdb-claude-print-model))
   (when (and resume kdb-claude-print--session-id)
     (list "--resume" kdb-claude-print--session-id))
   (when kdb-claude-print-bare (list "--bare"))
   kdb-claude-print-extra-args))

(defun kdb-claude-print--ensure-process (resume)
  "Start this buffer's session process unless it is running.
RESUME non-nil continues the buffer's session id."
  (unless (process-live-p kdb-claude-print--process)
    (setq kdb-claude-print--stdout-acc ""
          kdb-claude-print--process
          (make-process
           :name "claude-print"
           :buffer (current-buffer)
           :command (cons (kdb-claude-print--program) (kdb-claude-print--args resume))
           :connection-type 'pipe
           :noquery t
           :coding 'utf-8
           :filter #'kdb-claude-print--filter
           :sentinel #'kdb-claude-print--sentinel))))

(defun kdb-claude-print--send (obj)
  "Send OBJ to this buffer's process as one JSON line."
  (process-send-string kdb-claude-print--process (concat (json-serialize obj) "\n")))

(defun kdb-claude-print--kill ()
  "Kill this buffer's process; on Windows its whole tree (claude.cmd spawns node)."
  (when (process-live-p kdb-claude-print--process)
    (if (eq system-type 'windows-nt)
        (call-process "taskkill" nil nil nil "/T" "/F" "/PID"
                      (number-to-string (process-id kdb-claude-print--process)))
      (delete-process kdb-claude-print--process))))

;;; ============================================================
;;; Streaming JSONL parser
;;; ============================================================

(defun kdb-claude-print--parse-line (line)
  "Parse one JSONL LINE from claude and render the meaningful bits."
  (let ((obj (condition-case nil
                 (json-parse-string line :object-type 'alist :null-object nil)
               (error nil))))
    (when obj
      (let ((type (alist-get 'type obj)))
        (cond
         ;; Session bootstrap — capture the id for `--resume'.
         ((equal type "system")
          (when (equal (alist-get 'subtype obj) "init")
            (let ((sid (alist-get 'session_id obj)))
              (unless (equal sid kdb-claude-print--session-id)
                (setq kdb-claude-print--session-id sid)
                (kdb-claude-print--insert
                 "── session %s ──\n\n"
                 (if sid (substring sid 0 (min 8 (length sid))) "?"))))))
         ;; Assistant turn — text blocks and tool calls.
         ((equal type "assistant")
          (let ((content (alist-get 'content (alist-get 'message obj))))
            (seq-doseq (block (or content []))
              (pcase (alist-get 'type block)
                ("text"
                 (let ((txt (alist-get 'text block)))
                   (when (and txt (> (length (string-trim txt)) 0))
                     (kdb-claude-print--insert "%s\n\n" (string-trim-right txt)))))
                ("tool_use"
                 (kdb-claude-print--insert
                  "  ⚙ %s%s\n"
                  (or (alist-get 'name block) "tool")
                  (kdb-claude-print--tool-summary (alist-get 'input block))))))))
         ;; Tool results come back as a user turn — show a quiet marker.
         ((equal type "user")
          (kdb-claude-print--insert "  · result\n"))
         ;; The CLI asks before running a tool the mode doesn't allow.
         ((equal type "control_request")
          (let ((req (alist-get 'request obj)))
            (if (equal (alist-get 'subtype req) "can_use_tool")
                (progn
                  (setq kdb-claude-print--pending
                        (append kdb-claude-print--pending (list obj)))
                  (kdb-claude-print--insert
                   "%s\n"
                   (propertize
                    (format "  ? Allow %s%s   [y]es [a]lways [n]o"
                            (alist-get 'tool_name req)
                            (kdb-claude-print--tool-summary (alist-get 'input req)))
                    'face 'warning))
                  (kdb-claude-print--flag)
                  (unless (get-buffer-window (current-buffer))
                    (message "Claude wants %s in %s (C-c ' to review)"
                             (alist-get 'tool_name req) (buffer-name))))
              (kdb-claude-print--send
               `((type . "control_response")
                 (response (subtype . "error")
                           (request_id . ,(alist-get 'request_id obj))
                           (error . "Not supported by Emacs host")))))))
         ;; Final result — footer with turns/timing, or an error.
         ((equal type "result")
          (setq kdb-claude-print--busy nil)
          (if (equal (alist-get 'subtype obj) "success")
              (kdb-claude-print--insert
               "\n── done · %s turns · %.1fs ──\n\n"
               (or (alist-get 'num_turns obj) "?")
               (/ (or (alist-get 'duration_ms obj) 0) 1000.0))
            (kdb-claude-print--insert
             "\n── error: %s ──\n\n"
             (or (alist-get 'subtype obj) "unknown")))
          (kdb-claude-print--flag)))))))

(defun kdb-claude-print--tool-summary (input)
  "Return a short one-line summary string for a tool-use INPUT alist."
  (if (not input)
      ""
    (let* ((cmd (alist-get 'command input))
           (path (or (alist-get 'file_path input) (alist-get 'path input)))
           (pat (alist-get 'pattern input))
           (s (or cmd path pat)))
      (if (stringp s)
          (format ": %s" (truncate-string-to-width (string-trim s) 70 nil nil "…"))
        ""))))

(defun kdb-claude-print--filter (proc chunk)
  "Process filter: buffer CHUNK, render complete JSONL lines from PROC."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq kdb-claude-print--stdout-acc
              (concat kdb-claude-print--stdout-acc chunk))
        (let ((lines (split-string kdb-claude-print--stdout-acc "\n")))
          ;; Last element is an incomplete line; keep it for next chunk.
          (setq kdb-claude-print--stdout-acc (car (last lines)))
          (dolist (line (butlast lines))
            (let ((line (string-trim line)))
              (unless (string-empty-p line)
                (kdb-claude-print--parse-line line)))))))))

(defun kdb-claude-print--sentinel (proc event)
  "Process sentinel for PROC; report abnormal EVENT and clear state."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq kdb-claude-print--process nil
              kdb-claude-print--busy nil
              kdb-claude-print--pending nil)
        (unless (or (string-prefix-p "finished" event)
                    (string-prefix-p "exited abnormally with code 0" event))
          (when (memq (process-status proc) '(exit signal))
            (kdb-claude-print--insert "\n── process %s ──\n\n"
                                      (string-trim event))))
        (kdb-claude-print--flag)))))

;;; ============================================================
;;; Runner
;;; ============================================================

(defun kdb-claude-print--run (prompt &optional resume dir buf)
  "Send PROMPT to the session in buffer BUF.
RESUME non-nil continues the buffer's session if its process has to be
started.  DIR, when non-nil, becomes the buffer's working directory for
file tools.  BUF defaults to `kdb-claude-print--buffer'."
  (let ((buf (or buf (kdb-claude-print--buffer))))
    (with-current-buffer buf
      (when kdb-claude-print--busy
        (user-error "Claude is still working (C-c C-k to stop)"))
      (when dir (setq default-directory (file-name-as-directory dir)))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert (format "▶ %s\n\n"
                        (truncate-string-to-width prompt 120 nil nil "…"))))
      (kdb-claude-print--ensure-process resume)
      (setq kdb-claude-print--busy t)
      (kdb-claude-print--send
       `((type . "user") (message (role . "user") (content . ,prompt)))))
    (display-buffer buf)
    buf))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun kdb-claude-print (prompt)
  "Start a new Claude session for PROMPT in its own buffer."
  (interactive "sClaude: ")
  (kdb-claude-print--run prompt nil nil (kdb-claude-print--new-buffer)))

;;;###autoload
(defun kdb-claude-print-continue (prompt)
  "Send a follow-up PROMPT to the current session."
  (interactive "sClaude (follow-up): ")
  (kdb-claude-print--run prompt t))

;;;###autoload
(defun kdb-claude-print-new (prompt)
  "Reset the current buffer's session and start over with PROMPT."
  (interactive "sClaude (new session): ")
  (with-current-buffer (kdb-claude-print--buffer)
    (kdb-claude-print--kill)
    (setq kdb-claude-print--session-id nil
          kdb-claude-print--busy nil
          kdb-claude-print--pending nil))
  (kdb-claude-print--run prompt nil))

;;;###autoload
(defun kdb-claude-print-region (start end)
  "Send the region START..END to Claude, prefixed with an instruction."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (instr (read-string "Claude (about region): "))
         (prompt (format "%s\n\n```\n%s\n```" instr code)))
    (kdb-claude-print--run prompt t)))

;;;###autoload
(defun kdb-claude-print-buffer ()
  "Send the whole current buffer to Claude with an instruction."
  (interactive)
  (kdb-claude-print-region (point-min) (point-max)))

(defun kdb-claude-print--answer (allow &optional always)
  "Answer the oldest pending permission request.
ALLOW non-nil allows it; ALWAYS also applies Claude's suggested rule
for the rest of the session."
  (let* ((obj (or (pop kdb-claude-print--pending)
                  (user-error "No permission request pending")))
         (rules (alist-get 'permission_suggestions (alist-get 'request obj))))
    (kdb-claude-print--send
     `((type . "control_response")
       (response (subtype . "success")
                 (request_id . ,(alist-get 'request_id obj))
                 (response . ,(cond ((not allow)
                                     '((behavior . "deny")
                                       (message . "Denied by the user")))
                                    ((and always rules)
                                     `((behavior . "allow") (updatedPermissions . ,rules)))
                                    (t '((behavior . "allow"))))))))
    (kdb-claude-print--insert "    → %s\n"
                              (cond ((not allow) "denied")
                                    (always "allowed (always)")
                                    (t "allowed")))))

;;;###autoload
(defun kdb-claude-print-allow ()
  "Allow the session's oldest pending tool request."
  (interactive)
  (with-current-buffer (kdb-claude-print--buffer)
    (kdb-claude-print--answer t)))

;;;###autoload
(defun kdb-claude-print-allow-always ()
  "Allow the oldest pending request and its suggested rule for this session."
  (interactive)
  (with-current-buffer (kdb-claude-print--buffer)
    (kdb-claude-print--answer t t)))

;;;###autoload
(defun kdb-claude-print-deny ()
  "Deny the session's oldest pending tool request."
  (interactive)
  (with-current-buffer (kdb-claude-print--buffer)
    (kdb-claude-print--answer nil)))

;;;###autoload
(defun kdb-claude-print-stop (&optional kill)
  "Interrupt the current turn, keeping the session.
With prefix arg KILL, kill the session process instead."
  (interactive "P")
  (with-current-buffer (kdb-claude-print--buffer)
    (cond ((not (process-live-p kdb-claude-print--process))
           (message "No Claude session running"))
          (kill (kdb-claude-print--kill)
                (message "Claude session killed"))
          (t (setq kdb-claude-print--pending nil)
             (kdb-claude-print--send
              `((type . "control_request")
                (request_id . ,(format "emacs-%d" (random 1000000)))
                (request (subtype . "interrupt"))))
             (message "Claude turn interrupted")))))

(provide 'init-claude-print)
;;; init-claude-print.el ends here
