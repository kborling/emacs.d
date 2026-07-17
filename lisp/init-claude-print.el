;;; init-claude-print.el --- Claude print-mode backend (native Windows) -*- lexical-binding: t; -*-

;;; Commentary:
;; Non-interactive Claude backend for platforms without a PTY (native
;; Windows Emacs, where `eat'/`vterm' can't spawn a child).  Talks to the
;; `claude' CLI in print mode (`-p') over pipes via async `make-process',
;; so Emacs never blocks and there is no PTY requirement.
;;
;; Output streams as JSONL (`--output-format stream-json') into a dedicated
;; buffer.  A session id is captured from the first run so follow-ups
;; continue the same conversation with `--resume'.
;;
;; What you get vs. the real TUI: streaming text + visible tool calls, but
;; no interactive per-tool approval (there is nowhere to prompt).  The
;; permission mode below governs what the agent may do unattended.

;;; Code:

(require 'json)
(require 'ansi-color)

;;; ============================================================
;;; Options
;;; ============================================================

(defgroup kdb-claude-print nil
  "Non-interactive Claude backend using the CLI print mode."
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
  "Permission mode for unattended runs.
There is no interactive approval in print mode, so this decides what
the agent may do on its own.  \"acceptEdits\" auto-approves file edits;
\"bypassPermissions\" allows everything (including shell); \"plan\" and
\"manual\" are read-only/planning."
  :type '(choice (const "acceptEdits")
                 (const "bypassPermissions")
                 (const "plan")
                 (const "manual")
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
  "Name of the buffer that displays Claude print-mode output."
  :type 'string
  :group 'kdb-claude-print)

;;; ============================================================
;;; State (buffer-local in the output buffer)
;;; ============================================================

(defvar-local kdb-claude-print--session-id nil
  "Session id captured from the running/last conversation, for `--resume'.")

(defvar-local kdb-claude-print--process nil
  "The live `claude' process for this output buffer, if any.")

(defvar-local kdb-claude-print--stdout-acc ""
  "Accumulator for partial JSONL lines from stdout.")

(defvar-local kdb-claude-print--in-text nil
  "Non-nil while inside a rendered assistant text block (spacing helper).")

;;; ============================================================
;;; Display mode
;;; ============================================================

(defvar kdb-claude-print-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'kdb-claude-print-continue)
    (define-key map (kbd "G") #'kdb-claude-print-new)
    (define-key map (kbd "C-c C-k") #'kdb-claude-print-stop)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `kdb-claude-print-mode'.")

(define-derived-mode kdb-claude-print-mode special-mode "Claude-p"
  "Major mode for Claude print-mode output.
\\{kdb-claude-print-mode-map}"
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun kdb-claude-print--buffer ()
  "Return the output buffer, creating and initializing it if needed."
  (let ((buf (get-buffer-create kdb-claude-print-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'kdb-claude-print-mode)
        (kdb-claude-print-mode)))
    buf))

(defun kdb-claude-print--insert (fmt &rest args)
  "Insert FMT/ARGS at the end of the output buffer, read-only aware."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (apply #'format fmt args))))

;;; ============================================================
;;; Command construction
;;; ============================================================

(defun kdb-claude-print--program ()
  "Resolve the claude executable or signal a clear error."
  (or kdb-claude-print-executable
      (executable-find "claude")
      (user-error "Cannot find the `claude' CLI; set `kdb-claude-print-executable'")))

(defun kdb-claude-print--args (prompt resume)
  "Build the argument list for PROMPT.
When RESUME is non-nil, resume the buffer's captured session id."
  (append
   (list "-p" prompt
         "--output-format" "stream-json"
         "--verbose"
         "--permission-mode" kdb-claude-print-permission-mode)
   (when kdb-claude-print-model (list "--model" kdb-claude-print-model))
   (when (and resume kdb-claude-print--session-id)
     (list "--resume" kdb-claude-print--session-id))
   (when kdb-claude-print-bare (list "--bare"))
   kdb-claude-print-extra-args))

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
              (when sid (setq kdb-claude-print--session-id sid))
              (kdb-claude-print--insert
               "── session %s ──\n\n"
               (if sid (substring sid 0 (min 8 (length sid))) "?")))))
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
         ;; Final result — footer with cost/turns/timing, or an error.
         ((equal type "result")
          (if (equal (alist-get 'subtype obj) "success")
              (kdb-claude-print--insert
               "\n── done · %s turns · %.1fs · $%.4f ──\n\n"
               (or (alist-get 'num_turns obj) "?")
               (/ (or (alist-get 'duration_ms obj) 0) 1000.0)
               (or (alist-get 'total_cost_usd obj) 0.0))
            (kdb-claude-print--insert
             "\n── error: %s ──\n\n"
             (or (alist-get 'subtype obj) "unknown")))))))))

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
        (setq kdb-claude-print--process nil)
        (unless (or (string-prefix-p "finished" event)
                    (string-prefix-p "exited abnormally with code 0" event))
          (when (memq (process-status proc) '(exit signal))
            (kdb-claude-print--insert "\n── process %s ──\n\n"
                                      (string-trim event))))))))

;;; ============================================================
;;; Runner
;;; ============================================================

(defun kdb-claude-print--run (prompt &optional resume dir)
  "Send PROMPT to claude in print mode.
RESUME non-nil continues the buffer's session.  DIR is the working
directory for file tools (defaults to `default-directory')."
  (let ((buf (kdb-claude-print--buffer))
        (workdir (or dir default-directory)))
    (with-current-buffer buf
      (when (and kdb-claude-print--process
                 (process-live-p kdb-claude-print--process))
        (user-error "A Claude request is already running (C-c C-k to stop)"))
      (setq kdb-claude-print--stdout-acc "")
      (let ((default-directory workdir)
            (inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert (format "▶ %s\n\n"
                        (truncate-string-to-width prompt 120 nil nil "…")))
        (setq kdb-claude-print--process
              (make-process
               :name "claude-print"
               :buffer buf
               :command (cons (kdb-claude-print--program)
                              (kdb-claude-print--args prompt resume))
               :connection-type 'pipe
               :noquery t
               :coding 'utf-8
               :filter #'kdb-claude-print--filter
               :sentinel #'kdb-claude-print--sentinel))))
    (display-buffer buf)
    buf))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun kdb-claude-print (prompt)
  "Prompt for PROMPT and start a fresh Claude print-mode conversation."
  (interactive "sClaude: ")
  (with-current-buffer (kdb-claude-print--buffer)
    (setq kdb-claude-print--session-id nil))
  (kdb-claude-print--run prompt nil))

;;;###autoload
(defun kdb-claude-print-continue (prompt)
  "Send a follow-up PROMPT, resuming the current print-mode session."
  (interactive "sClaude (follow-up): ")
  (kdb-claude-print--run prompt t))

;;;###autoload
(defun kdb-claude-print-new (prompt)
  "Reset the session and start a new print-mode conversation with PROMPT."
  (interactive "sClaude (new session): ")
  (with-current-buffer (kdb-claude-print--buffer)
    (setq kdb-claude-print--session-id nil))
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

;;;###autoload
(defun kdb-claude-print-stop ()
  "Interrupt the running Claude request, if any."
  (interactive)
  (with-current-buffer (kdb-claude-print--buffer)
    (if (and kdb-claude-print--process
             (process-live-p kdb-claude-print--process))
        (progn (interrupt-process kdb-claude-print--process)
               (message "Claude request interrupted"))
      (message "No Claude request running"))))

(provide 'init-claude-print)
;;; init-claude-print.el ends here
