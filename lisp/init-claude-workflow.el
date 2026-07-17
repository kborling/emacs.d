;;; init-claude-workflow.el --- Unified Claude workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified transient menu and workflow for Claude (gptel + claude-code).
;; Think, code, recall, and transfer between sessions.

;;; Code:

(require 'gptel)

;;; ============================================================
;;; Quick Actions
;;; ============================================================

(defun kdb-gptel-code ()
  "Start a gptel session with current buffer added as context."
  (interactive)
  (gptel-add-file (or buffer-file-name
                      (let ((tmp (make-temp-file "gptel-" nil
                                   (when buffer-file-name
                                     (concat "." (file-name-extension buffer-file-name))))))
                        (write-region (point-min) (point-max) tmp)
                        tmp)))
  (gptel))

(defun kdb-gptel-add-project ()
  "Add the current project's files to gptel context."
  (interactive)
  (let ((root (if (project-current)
                  (project-root (project-current))
                (read-directory-name "Project root: "))))
    (gptel-add-file root)
    (message "Project %s added to context" root)))

(defun kdb-gptel-send-capture ()
  "Send a :claude: tagged org entry to a gptel session.
If point is on a heading in an org buffer, use that entry.
Otherwise, search org files for :claude: tagged entries and prompt."
  (interactive)
  (let (heading entry-text)
    (if (and (derived-mode-p 'org-mode)
             (ignore-errors (org-back-to-heading t) t))
        (save-excursion
          (org-back-to-heading t)
          (setq heading (org-get-heading t t t t)
                entry-text (buffer-substring-no-properties
                            (point) (org-end-of-subtree t t))))
      (let* ((files (directory-files "~/.org" t "\\.org$"))
             (entries '()))
        (dolist (file files)
          (with-temp-buffer
            (insert-file-contents file)
            (org-mode)
            (goto-char (point-min))
            (while (re-search-forward "^\\*+ .+:claude:" nil t)
              (org-back-to-heading t)
              (let ((h (org-get-heading t t t t))
                    (text (buffer-substring-no-properties
                           (point) (org-end-of-subtree t t))))
                (push (cons h text) entries)))))
        (unless entries
          (user-error "No :claude: tagged entries found in ~/.org/"))
        (let ((choice (completing-read "Send to Claude: "
                                       (mapcar #'car entries) nil t)))
          (setq heading choice
                entry-text (cdr (assoc choice entries))))))
    (let* ((prompt (when (string-match
                          "^\\*+ Prompt\n\\(\\(?:.*\n?\\)*\\)" entry-text)
                     (string-trim (match-string 1 entry-text))))
           (buf-name (format "*Claude: %s*" heading))
           (buf (get-buffer-create buf-name)))
      (unless (and prompt (not (string-empty-p prompt)))
        (setq prompt (read-string "Prompt for Claude: ")))
      (with-current-buffer buf
        (org-mode)
        (erase-buffer)
        (gptel-mode 1)
        (insert "* Context\n\n")
        (insert entry-text)
        (insert "\n\n* Prompt\n\n")
        (insert prompt "\n"))
      (switch-to-buffer buf)
      (gptel-send))))

;;; ============================================================
;;; Code Actions (in-place)
;;; ============================================================

(defun kdb-gptel-explain ()
  "Explain the selected region or function at point."
  (interactive)
  (gptel-request
   (or (when (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
       (thing-at-point 'defun t)
       (user-error "Select a region or place point in a function"))
   :system "Explain this code clearly and concisely. Focus on what it does and why."
   :callback (lambda (response _info)
               (with-current-buffer (get-buffer-create "*Claude: Explain*")
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (org-mode)
                   (insert "* Explanation\n\n" response "\n")
                   (goto-char (point-min))
                   (setq buffer-read-only t))
                 (display-buffer (current-buffer))))))

(defun kdb-gptel-refactor ()
  "Refactor the selected region with Claude."
  (interactive)
  (unless (use-region-p) (user-error "Select a region to refactor"))
  (let ((instruction (read-string "Refactor instruction (or empty for general): ")))
    (gptel-rewrite (if (string-empty-p instruction)
                       "Refactor this code: improve clarity, reduce complexity, follow best practices. Return only the code."
                     instruction))))

(defun kdb-gptel-fix ()
  "Fix bugs in the selected region."
  (interactive)
  (unless (use-region-p) (user-error "Select a region to fix"))
  (gptel-rewrite "Fix any bugs in this code. Return only the corrected code."))

(defun kdb-gptel-tests ()
  "Generate tests for the selected region or function."
  (interactive)
  (gptel-request
   (or (when (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
       (thing-at-point 'defun t)
       (user-error "Select a region or place point in a function"))
   :system "Generate unit tests for this code. Use the testing framework appropriate for the language. Return only the test code."
   :callback (lambda (response _info)
               (with-current-buffer (get-buffer-create "*Claude: Tests*")
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert response "\n")
                   (goto-char (point-min)))
                 (display-buffer (current-buffer))))))

(defun kdb-gptel-doc ()
  "Generate documentation for the selected region or function."
  (interactive)
  (unless (use-region-p) (user-error "Select a region to document"))
  (gptel-rewrite "Add documentation/docstrings to this code. Keep the code unchanged, only add documentation. Return the complete code with docs."))

;;; ============================================================
;;; Target System
;;; ============================================================

(defvar kdb-claude-target nil
  "Current target Claude session buffer.")

(defun kdb-claude--active-sessions ()
  "Return alist of (label . buffer) for all active Claude sessions."
  (let ((sessions '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p gptel-mode)
          (push (cons (format "chat: %s" (buffer-name buf)) buf) sessions))))
    (when (fboundp 'claude-code--find-all-claude-buffers)
      (dolist (buf (claude-code--find-all-claude-buffers))
        (push (cons (format "agent: %s" (buffer-name buf)) buf) sessions)))
    (nreverse sessions)))

(defun kdb-claude-set-target ()
  "Set the target session for subsequent commands."
  (interactive)
  (let ((sessions (kdb-claude--active-sessions)))
    (if (null sessions)
        (message "No active sessions. Start one with C-c l.")
      (let* ((choice (completing-read "Target: " (mapcar #'car sessions) nil t))
             (buf (cdr (assoc choice sessions))))
        (setq kdb-claude-target buf)
        (message "Target: %s" (buffer-name buf))))))

(defun kdb-claude--get-target ()
  "Return the current target buffer, prompting if unset or dead."
  (when (and kdb-claude-target (not (buffer-live-p kdb-claude-target)))
    (setq kdb-claude-target nil))
  (unless kdb-claude-target
    (kdb-claude-set-target))
  kdb-claude-target)

(defun kdb-claude--target-is-agent-p ()
  "Return t if the current target is a claude-code agent."
  (when-let* ((buf (kdb-claude--get-target)))
    (string-match-p "^\\*claude:" (buffer-name buf))))

(defun kdb-claude-send-region-to-target ()
  "Send region or buffer to the target session."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (target (kdb-claude--get-target)))
    (if (kdb-claude--target-is-agent-p)
        (let ((instruction (read-string "Instruction (empty to just send): ")))
          (claude-code--do-send-command
           (if (string-empty-p instruction) text
             (format "%s\n\n%s" instruction text))))
      (with-current-buffer target
        (goto-char (point-max))
        (insert "\n* Context\n\n" text "\n"))
      (message "Added to %s" (buffer-name target)))))

(defun kdb-claude-send-file-to-target ()
  "Send a file to the target session."
  (interactive)
  (let ((file (read-file-name "File: "))
        (target (kdb-claude--get-target)))
    (if (kdb-claude--target-is-agent-p)
        (claude-code--do-send-command
         (format "Look at this file: %s" (expand-file-name file)))
      (with-current-buffer target
        (gptel-add-file file))
      (message "Added %s to %s" (file-name-nondirectory file) (buffer-name target)))))

(defun kdb-claude-prompt-target ()
  "Send a prompt to the target session."
  (interactive)
  (let ((prompt (read-string "Prompt: "))
        (target (kdb-claude--get-target)))
    (if (kdb-claude--target-is-agent-p)
        (claude-code--do-send-command prompt)
      (with-current-buffer target
        (goto-char (point-max))
        (insert "\n* " prompt "\n")
        (gptel-send)))))

(defun kdb-claude-accept-target ()
  "Accept/confirm in the target agent session."
  (interactive)
  (if (kdb-claude--target-is-agent-p)
      (claude-code-send-return)
    (message "Target is not an agent")))

(defun kdb-claude-reject-target ()
  "Reject/cancel in the target agent session."
  (interactive)
  (if (kdb-claude--target-is-agent-p)
      (claude-code-send-escape)
    (message "Target is not an agent")))

;;; ============================================================
;;; Project Launcher
;;; ============================================================

(defun kdb-claude--recent-projects ()
  "Return list of recent project directories, most recent first."
  (let ((projects '()))
    (when (fboundp 'project-known-project-roots)
      (setq projects (project-known-project-roots)))
    (dolist (f recentf-list)
      (when-let* ((dir (file-name-directory f))
                  (root (locate-dominating-file dir ".git")))
        (let ((root (file-name-as-directory (expand-file-name root))))
          (unless (member root projects)
            (push root projects)))))
    (nreverse projects)))

(defun kdb-claude--branches (dir)
  "Return list of git branches in DIR."
  (let ((default-directory dir))
    (split-string
     (shell-command-to-string "git branch --format=%(refname:short) 2>/dev/null")
     "\n" t)))

(defun kdb-claude-start ()
  "Start a Claude session in a project of your choice.
Pick a recent project, optionally switch branch, choose chat or agent."
  (interactive)
  (let* ((projects (kdb-claude--recent-projects))
         (project (completing-read "Project: " projects nil nil nil nil
                                   (when (project-current)
                                     (project-root (project-current)))))
         (branches (kdb-claude--branches project))
         (branch (when (and branches (y-or-n-p "Switch branch? "))
                   (completing-read "Branch: " branches nil t nil nil
                                    (string-trim
                                     (let ((default-directory project))
                                       (shell-command-to-string "git branch --show-current 2>/dev/null"))))))
         (mode (completing-read "Mode: " '("Agent (Claude Code)" "Chat (gptel)") nil t)))
    (when branch
      (let ((default-directory project))
        (shell-command (format "git checkout %s" (shell-quote-argument branch)))))
    (cond
     ((string-prefix-p "Agent" mode)
      (let ((default-directory project))
        (claude-code)))
     ((string-prefix-p "Chat" mode)
      (let ((default-directory project))
        (kdb-gptel-code))))))

;;; ============================================================
;;; Session Navigation
;;; ============================================================

(defun kdb-claude-sessions ()
  "Switch between active Claude sessions (chats + agents)."
  (interactive)
  (let ((sessions (kdb-claude--active-sessions)))
    (if (null sessions)
        (message "No active sessions. Start one with C-c l.")
      (let* ((choice (completing-read "Session: " (mapcar #'car sessions) nil t))
             (buf (cdr (assoc choice sessions))))
        (switch-to-buffer buf)))))

(defun kdb-claude-next-session ()
  "Cycle to the next active Claude session."
  (interactive)
  (let ((sessions (append
                   (cl-remove-if-not
                    (lambda (b) (with-current-buffer b (bound-and-true-p gptel-mode)))
                    (buffer-list))
                   (when (fboundp 'claude-code--find-all-claude-buffers)
                     (claude-code--find-all-claude-buffers)))))
    (if (null sessions)
        (message "No active Claude sessions")
      (let* ((current (current-buffer))
             (pos (cl-position current sessions))
             (next (if pos
                       (nth (mod (1+ pos) (length sessions)) sessions)
                     (car sessions))))
        (switch-to-buffer next)))))

;;; ============================================================
;;; Star / Bookmark
;;; ============================================================

(defun kdb-claude-star ()
  "Bookmark the current Claude session for quick access."
  (interactive)
  (if (or (bound-and-true-p gptel-mode)
          (and (fboundp 'claude-code--buffer-p)
               (claude-code--buffer-p (current-buffer))))
      (let ((name (read-string "Star as: "
                               (format "claude: %s" (buffer-name)))))
        (bookmark-set name)
        (message "Starred: %s" name))
    (user-error "Not in a Claude session")))

(defun kdb-claude-stars ()
  "Jump to a starred Claude session."
  (interactive)
  (let* ((all-bookmarks (bookmark-all-names))
         (claude-bookmarks (cl-remove-if-not
                            (lambda (name) (string-prefix-p "claude:" name))
                            all-bookmarks)))
    (if (null claude-bookmarks)
        (message "No starred sessions. Star one with *")
      (let ((choice (completing-read "Starred: " claude-bookmarks nil t)))
        (bookmark-jump choice)))))

;;; ============================================================
;;; Image Paste
;;; ============================================================

(defun kdb-claude-paste-image ()
  "Paste an image from clipboard or pick a file, add to gptel context."
  (interactive)
  (let* ((dir (expand-file-name "~/.org/images/"))
         (filename (format "claude_%s.png" (format-time-string "%Y%m%d_%H%M%S")))
         (filepath (expand-file-name filename dir)))
    (make-directory dir t)
    (cond
     ((and (eq system-type 'darwin)
           (executable-find "pngpaste")
           (zerop (call-process "pngpaste" nil nil nil filepath)))
      (gptel-context-add-file filepath)
      (message "Image pasted and added to context"))
     ((and (executable-find "xclip")
           (zerop (call-process-shell-command
                   (format "xclip -selection clipboard -t image/png -o > %s"
                           (shell-quote-argument filepath)))))
      (if (> (file-attribute-size (file-attributes filepath)) 0)
          (progn (gptel-context-add-file filepath)
                 (message "Image pasted and added to context"))
        (delete-file filepath)
        (let ((file (read-file-name "Image file: ")))
          (gptel-context-add-file file))))
     (t (let ((file (read-file-name "Image file: ")))
          (gptel-context-add-file file))))))

;;; ============================================================
;;; Recall & Transfer
;;; ============================================================

(defun kdb-claude--list-sessions ()
  "Return alist of (display-name . buffer-or-file) for all Claude sessions."
  (let ((sessions '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p gptel-mode)
          (push (cons (format "[chat] %s" (buffer-name buf)) buf) sessions))))
    (let ((archive (expand-file-name "~/.claude-archive")))
      (when (file-directory-p archive)
        (dolist (f (directory-files-recursively archive "\\.org$"))
          (push (cons (format "[saved] %s"
                              (file-relative-name f archive))
                      f)
                sessions))))
    (let ((log (expand-file-name "gptel-log.org" "~/.org/")))
      (when (file-exists-p log)
        (push (cons "[log] gptel-log.org" log) sessions)))
    (nreverse sessions)))

(defun kdb-claude-recall ()
  "Browse and open any Claude session — live, archived, or imported.
Unified view across all sources."
  (interactive)
  (let* ((sessions (kdb-claude--list-sessions))
         (desktop-sessions
          (when (and (boundp 'kdb-claude-export-directory)
                     kdb-claude-export-directory
                     (file-directory-p kdb-claude-export-directory))
            (condition-case nil
                (let ((convs (kdb-claude--parse-conversations
                              kdb-claude-export-directory)))
                  (mapcar (lambda (conv)
                            (cons (format "[desktop] %s — %s"
                                          (kdb-claude--conversation-title conv)
                                          (kdb-claude--format-date
                                           (alist-get 'updated_at conv)))
                                  conv))
                          convs))
              (error nil))))
         (all (append sessions desktop-sessions))
         (choice (completing-read "Session: " (mapcar #'car all) nil t))
         (target (cdr (assoc choice all))))
    (cond
     ((bufferp target) (switch-to-buffer target))
     ((stringp target) (find-file target))
     ((listp target) (kdb-claude--display-conversation target)))))

(defun kdb-claude-send-to-code ()
  "Send a Claude session or region to Claude Code CLI."
  (interactive)
  (unless (fboundp 'claude-code--do-send-command)
    (require 'claude-code))
  (let* ((sources (append
                   (when (use-region-p)
                     '(("[region] Current selection" . :region)))
                   (when (bound-and-true-p gptel-mode)
                     `(("[current] This chat buffer" . :current)))
                   (kdb-claude--list-sessions)))
         (choice (completing-read "Send to Claude Code: "
                                  (mapcar #'car sources) nil t))
         (target (cdr (assoc choice sources)))
         (text (cond
                ((eq target :region)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((eq target :current)
                 (buffer-substring-no-properties (point-min) (point-max)))
                ((bufferp target)
                 (with-current-buffer target
                   (buffer-substring-no-properties (point-min) (point-max))))
                ((stringp target)
                 (with-temp-buffer
                   (insert-file-contents target)
                   (buffer-string)))))
         (instruction (read-string "Instruction for Claude Code: ")))
    (let ((prompt (if (string-empty-p instruction)
                      (format "Here is context from a previous conversation:\n\n%s" text)
                    (format "%s\n\nContext:\n\n%s" instruction text))))
      (when (> (length prompt) 50000)
        (setq prompt (concat (substring prompt 0 49000)
                             "\n\n[... truncated ...]")))
      (claude-code--do-send-command prompt))))

(defun kdb-claude-send-to-chat ()
  "Send a Claude session or region to a gptel chat buffer."
  (interactive)
  (let* ((sources (append
                   (when (use-region-p)
                     '(("[region] Current selection" . :region)))
                   (kdb-claude--list-sessions)))
         (choice (completing-read "Send to gptel chat: "
                                  (mapcar #'car sources) nil t))
         (target (cdr (assoc choice sources)))
         (text (cond
                ((eq target :region)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((bufferp target)
                 (with-current-buffer target
                   (buffer-substring-no-properties (point-min) (point-max))))
                ((stringp target)
                 (with-temp-buffer
                   (insert-file-contents target)
                   (buffer-string)))))
         (prompt (read-string "What to ask about this session: "))
         (buf-name (format "*Claude: %s*"
                           (truncate-string-to-width
                            (replace-regexp-in-string "^\\[.*?\\] " "" choice) 40)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (org-mode)
      (erase-buffer)
      (gptel-mode 1)
      (insert "* Context\n\n")
      (insert text)
      (insert "\n\n* Prompt\n\n")
      (insert prompt "\n"))
    (switch-to-buffer buf)
    (gptel-send)))

;;; ============================================================
;;; Unified Transient
;;; ============================================================

(defun kdb-claude ()
  "Claude menu — think, code, recall."
  (interactive)
  (require 'transient)
  (require 'claude-code nil t)
  (transient-define-prefix kdb-claude-menu ()
    "Claude"
    [:description (lambda ()
                    (format "Target: %s"
                            (if (and kdb-claude-target (buffer-live-p kdb-claude-target))
                                (buffer-name kdb-claude-target)
                              "none (set with ;)")))
     ["Start"
      ("N" "New (project)" kdb-claude-start)
      ("x" "Agent here" claude-code
       :if (lambda () (not (eq system-type 'windows-nt))))
      ("l" "Chat here" gptel)
      ("p" "Print (no PTY)" kdb-claude-print
       :if (lambda () (eq system-type 'windows-nt)))
      ("c" "Print follow-up" kdb-claude-print-continue
       :if (lambda () (eq system-type 'windows-nt)))]
     ["Send to Target"
      ("s" "Prompt" kdb-claude-prompt-target)
      ("r" "Region" kdb-claude-send-region-to-target)
      ("F" "File" kdb-claude-send-file-to-target)
      ("y" "Accept" kdb-claude-accept-target)
      ("n" "Reject" kdb-claude-reject-target)]
     ["Quick Fix (in-place)"
      ("e" "Explain" kdb-gptel-explain)
      ("R" "Refactor" kdb-gptel-refactor)
      ("f" "Fix Bugs" kdb-gptel-fix)
      ("t" "Gen Tests" kdb-gptel-tests)
      ("d" "Add Docs" kdb-gptel-doc)
      ("w" "Rewrite" gptel-rewrite)
      ("I" "Paste Image" kdb-claude-paste-image)]]
    [["Sessions"
      (";" "Set Target" kdb-claude-set-target)
      ("." "Toggle Session" claude-code-toggle)
      ("K" "End Session" claude-code-kill)
      ("*" "Star" kdb-claude-star)
      ("8" "Starred" kdb-claude-stars)
      ("o" "Recall" kdb-claude-recall)
      ("/" "Search" kdb-claude-search)]
     ["Move"
      (">" "Recall → Agent" kdb-claude-send-to-code)
      ("<" "Recall → Chat" kdb-claude-send-to-chat)
      ("q" "From Notes" kdb-gptel-send-capture)
      ("i" "Import Desktop" kdb-claude-import-export)
      ("S" "Archive" kdb-claude-sync)
      ""
      ("m" "Settings" gptel-menu)]])
  (kdb-claude-menu))

(provide 'init-claude-workflow)
;;; init-claude-workflow.el ends here
