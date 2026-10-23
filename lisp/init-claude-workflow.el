;;; init-claude-workflow.el --- Unified Claude workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified transient menu and workflow for Claude sessions: claude-code
;; TUI agents, agent-shell (ACP) and pipe-session buffers.
;; Think, code, recall, transfer, and jump to whatever is waiting on you.

;;; Code:

(require 'init-claude)
(require 'transient)

(declare-function claude-code--do-send-command "claude-code")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function kdb-claude-print--run "init-claude-print")
(declare-function kdb-claude-print-allow "init-claude-print")
(declare-function kdb-claude-print-deny "init-claude-print")

;;; ============================================================
;;; Sessions
;;; ============================================================

(defun kdb-claude--session-kind (buf)
  "Return BUF's Claude session kind: `shell', `print', `agent' or nil."
  (with-current-buffer buf
    (cond ((derived-mode-p 'agent-shell-mode) 'shell)
          ((derived-mode-p 'kdb-claude-print-mode) 'print)
          ((string-prefix-p "*claude:" (buffer-name)) 'agent))))

(defun kdb-claude--active-sessions ()
  "Return alist of (label . buffer) for all live Claude sessions."
  (cl-loop for buf in (buffer-list)
           for kind = (kdb-claude--session-kind buf)
           when kind collect (cons (format "%s: %s" kind (buffer-name buf)) buf)))

(defun kdb-claude--send (buf text)
  "Send TEXT as a prompt to session BUF."
  (pcase (kdb-claude--session-kind buf)
    ('shell (agent-shell-insert :text text :submit t :no-focus t :shell-buffer buf))
    ('print (kdb-claude-print--run text t nil buf))
    ('agent (with-current-buffer buf (claude-code--do-send-command text)))
    (_ (user-error "%s is not a Claude session" (buffer-name buf)))))

;;; ============================================================
;;; Attention Queue
;;; ============================================================

(defvar kdb-claude-waiting nil
  "Session buffers waiting on you, oldest first.")

(defun kdb-claude--waiting-count ()
  "Return how many live sessions are waiting."
  (seq-count #'buffer-live-p kdb-claude-waiting))

(defvar kdb-claude--mode-line
  '(:eval (let ((n (kdb-claude--waiting-count)))
            (when (> n 0) (format " Claude[%d]" n)))))
(put 'kdb-claude--mode-line 'risky-local-variable t)
(add-to-list 'global-mode-string 'kdb-claude--mode-line t)

(defun kdb-claude-flag-buffer (buf)
  "Queue session BUF as waiting, unless you are already looking at it."
  (when (and (buffer-live-p buf)
             (not (eq buf (window-buffer (selected-window))))
             (not (memq buf kdb-claude-waiting)))
    (setq kdb-claude-waiting (append kdb-claude-waiting (list buf)))
    (force-mode-line-update t)))

;;;###autoload
(defun kdb-claude-flag (dir)
  "Queue the claude-code TUI sessions running in DIR.
Called via emacsclient from Claude Code's Stop/Notification hooks."
  (dolist (s (kdb-claude--active-sessions))
    (let ((buf (cdr s)))
      (when (and (eq (kdb-claude--session-kind buf) 'agent)
                 (file-equal-p (buffer-local-value 'default-directory buf) dir))
        (kdb-claude-flag-buffer buf)))))

;;;###autoload
(defun kdb-claude-watch-agent-shell ()
  "Queue this agent-shell buffer when its turn ends or it asks permission."
  (let ((buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buf
     :on-event (lambda (event)
                 (when (memq (map-elt event :event) '(turn-complete permission-request))
                   (kdb-claude-flag-buffer buf))))))

(defun kdb-claude--unflag-selected (&rest _)
  "Drop the selected window's buffer from the waiting queue."
  (when (memq (window-buffer (selected-window)) kdb-claude-waiting)
    (setq kdb-claude-waiting (delq (window-buffer (selected-window)) kdb-claude-waiting))
    (force-mode-line-update t)))

(add-hook 'window-buffer-change-functions #'kdb-claude--unflag-selected)
(add-hook 'window-selection-change-functions #'kdb-claude--unflag-selected)

;;;###autoload
(defun kdb-claude-next-waiting ()
  "Jump to the oldest session waiting on you, else cycle sessions."
  (interactive)
  (setq kdb-claude-waiting (seq-filter #'buffer-live-p kdb-claude-waiting))
  (if kdb-claude-waiting
      (switch-to-buffer (pop kdb-claude-waiting))
    (kdb-claude-next-session)))

;;; ============================================================
;;; Target System
;;; ============================================================

(defvar kdb-claude-target nil
  "Current target Claude session buffer.")

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

(defun kdb-claude-send-region-to-target ()
  "Send region or buffer to the target session."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (target (kdb-claude--get-target))
         (instruction (read-string "Instruction (empty to just send): ")))
    (kdb-claude--send target (if (string-empty-p instruction) text
                               (format "%s\n\n%s" instruction text)))))

(defun kdb-claude-send-file-to-target ()
  "Send a file to the target session."
  (interactive)
  (let ((file (read-file-name "File: ")))
    (kdb-claude--send (kdb-claude--get-target)
                      (format "Look at this file: %s" (expand-file-name file)))))

(defun kdb-claude-prompt-target ()
  "Send a prompt to the target session."
  (interactive)
  (let ((target (kdb-claude--get-target)))
    (kdb-claude--send target (read-string "Prompt: "))))

(defun kdb-claude-accept-target ()
  "Accept the target session's pending permission prompt."
  (interactive)
  (let ((target (kdb-claude--get-target)))
    (pcase (kdb-claude--session-kind target)
      ('agent (with-current-buffer target (claude-code-send-return)))
      ('print (with-current-buffer target (kdb-claude-print-allow)))
      (_ (message "Answer agent-shell prompts in its buffer")))))

(defun kdb-claude-reject-target ()
  "Reject the target session's pending permission prompt."
  (interactive)
  (let ((target (kdb-claude--get-target)))
    (pcase (kdb-claude--session-kind target)
      ('agent (with-current-buffer target (claude-code-send-escape)))
      ('print (with-current-buffer target (kdb-claude-print-deny)))
      (_ (message "Answer agent-shell prompts in its buffer")))))

;;; ============================================================
;;; Project Launcher
;;; ============================================================

(defun kdb-claude--recent-projects ()
  "Return list of recent project directories, most recent first."
  (let ((projects '()))
    (when (fboundp 'project-known-project-roots)
      (setq projects (project-known-project-roots)))
    (dolist (f (bound-and-true-p recentf-list))
      (when-let* ((dir (file-name-directory f))
                  (root (locate-dominating-file dir ".git")))
        (let ((root (file-name-as-directory (expand-file-name root))))
          (unless (member root projects)
            (push root projects)))))
    (nreverse projects)))

(defun kdb-claude--git-lines (dir &rest args)
  "Return output lines of git ARGS in DIR, or nil on failure."
  (let ((default-directory dir))
    (ignore-errors (apply #'process-lines "git" args))))

(defun kdb-claude--branch-worktree (project branch)
  "Return the directory where BRANCH of PROJECT is checked out, or nil."
  (let (dir found)
    (dolist (line (kdb-claude--git-lines project "worktree" "list" "--porcelain"))
      (cond ((string-prefix-p "worktree " line) (setq dir (substring line 9)))
            ((equal line (concat "branch refs/heads/" branch)) (setq found dir))))
    (and found (file-name-as-directory found))))

(defun kdb-claude--worktree (project branch)
  "Return a worktree of PROJECT on BRANCH, creating it beside PROJECT.
The main checkout is never switched, so parallel sessions stay put."
  (or (kdb-claude--branch-worktree project branch)
      (let ((dir (expand-file-name
                  (format "%s-%s"
                          (file-name-nondirectory (directory-file-name project))
                          (replace-regexp-in-string "/" "-" branch))
                  (file-name-directory (directory-file-name project)))))
        (when (file-exists-p dir)
          (user-error "%s exists but is not a worktree of %s" dir branch))
        (let ((default-directory project))
          (unless (zerop (call-process "git" nil nil nil "worktree" "add" dir branch))
            (user-error "git worktree add failed for %s" branch)))
        (file-name-as-directory dir))))

(defun kdb-claude-start ()
  "Start a Claude session in a project of your choice.
Pick a recent project, optionally a branch (opened as a worktree),
then the session type."
  (interactive)
  (let* ((projects (kdb-claude--recent-projects))
         (project (file-name-as-directory
                   (expand-file-name
                    (completing-read "Project: " projects nil nil nil nil
                                     (when (project-current)
                                       (project-root (project-current)))))))
         (branches (kdb-claude--git-lines project "branch" "--format=%(refname:short)"))
         (branch (when (and branches (y-or-n-p "Other branch (worktree)? "))
                   (completing-read "Branch: " branches nil t nil nil
                                    (car (kdb-claude--git-lines project "branch" "--show-current")))))
         (modes (append
                 (when (kdb-claude--acp-p) '("Agent shell (ACP)"))
                 (unless (eq system-type 'windows-nt) '("Agent (Claude Code TUI)"))
                 '("Pipe session (no PTY/node)")))
         (mode (completing-read "Mode: " modes nil t))
         (default-directory (if branch (kdb-claude--worktree project branch) project)))
    (pcase mode
      ("Agent shell (ACP)" (agent-shell-anthropic-start-claude-code))
      ("Agent (Claude Code TUI)" (claude-code))
      ("Pipe session (no PTY/node)" (call-interactively #'kdb-claude-print)))))

;;; ============================================================
;;; Session Navigation
;;; ============================================================

(defun kdb-claude-sessions ()
  "Switch between active Claude sessions."
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
  (let ((sessions (mapcar #'cdr (kdb-claude--active-sessions))))
    (if (null sessions)
        (message "No active Claude sessions")
      (let ((pos (cl-position (current-buffer) sessions)))
        (switch-to-buffer (if pos
                              (nth (mod (1+ pos) (length sessions)) sessions)
                            (car sessions)))))))

;;; ============================================================
;;; Star / Bookmark
;;; ============================================================

(defun kdb-claude-star ()
  "Bookmark the current pipe session so it can be resumed later."
  (interactive)
  (unless (eq (kdb-claude--session-kind (current-buffer)) 'print)
    (user-error "Only pipe sessions can be starred; resume others from the archive (u)"))
  (let ((name (read-string "Star as: "
                           (format "claude: %s" (buffer-name)))))
    (bookmark-set name)
    (message "Starred: %s" name)))

(defun kdb-claude-stars ()
  "Jump to a starred Claude session."
  (interactive)
  (require 'bookmark)
  (let* ((all-bookmarks (bookmark-all-names))
         (claude-bookmarks (cl-remove-if-not
                            (lambda (name) (string-prefix-p "claude:" name))
                            all-bookmarks)))
    (if (null claude-bookmarks)
        (message "No starred sessions. Star one with *")
      (let ((choice (completing-read "Starred: " claude-bookmarks nil t)))
        (bookmark-jump choice)))))

;;; ============================================================
;;; Recall & Transfer
;;; ============================================================

(defun kdb-claude--list-sessions ()
  "Return alist of (display-name . buffer-or-file) for all Claude sessions."
  (let ((archive (expand-file-name kdb-claude-archive-dir)))
    (append
     (mapcar (lambda (s) (cons (format "[live] %s" (car s)) (cdr s)))
             (kdb-claude--active-sessions))
     (when (file-directory-p archive)
       ;; ponytail: reads every header per call; cache by mtime if the archive gets large
       (mapcar (lambda (f) (cons (format "[saved] %s" (kdb-claude--org-title f)) f))
               (directory-files-recursively archive "\\.org$"))))))

(defun kdb-claude-recall ()
  "Browse and open any Claude session — live, archived, or imported."
  (interactive)
  (let* ((sessions (kdb-claude--list-sessions))
         (desktop-sessions
          (when (and kdb-claude-export-directory
                     (file-directory-p kdb-claude-export-directory))
            (condition-case nil
                (mapcar (lambda (conv)
                          (cons (format "[desktop] %s — %s"
                                        (kdb-claude--conversation-title conv)
                                        (kdb-claude--format-date
                                         (alist-get 'updated_at conv)))
                                conv))
                        (kdb-claude--parse-conversations kdb-claude-export-directory))
              (error nil))))
         (all (append sessions desktop-sessions))
         (choice (completing-read "Session: " (mapcar #'car all) nil t))
         (target (cdr (assoc choice all))))
    (cond
     ((bufferp target) (switch-to-buffer target))
     ((stringp target) (find-file target))
     ((listp target) (kdb-claude--display-conversation target)))))

(defun kdb-claude--source-text (prompt)
  "Pick a region or session with PROMPT and return its text."
  (let* ((sources (append
                   (when (use-region-p)
                     '(("[region] Current selection" . :region)))
                   (kdb-claude--list-sessions)))
         (choice (completing-read prompt (mapcar #'car sources) nil t))
         (target (cdr (assoc choice sources))))
    (cons choice
          (cond
           ((eq target :region)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           ((bufferp target)
            (with-current-buffer target
              (buffer-substring-no-properties (point-min) (point-max))))
           ((stringp target)
            (with-temp-buffer
              (insert-file-contents target)
              (buffer-string)))))))

(defun kdb-claude-send-to-target ()
  "Send a past session or region to the target session."
  (interactive)
  (let* ((text (cdr (kdb-claude--source-text "Send to target: ")))
         (instruction (read-string "Instruction: "))
         (prompt (if (string-empty-p instruction)
                     (format "Here is context from a previous conversation:\n\n%s" text)
                   (format "%s\n\nContext:\n\n%s" instruction text))))
    (when (> (length prompt) 50000)
      (setq prompt (concat (substring prompt 0 49000)
                           "\n\n[... truncated ...]")))
    (kdb-claude--send (kdb-claude--get-target) prompt)))

(defun kdb-claude-send-capture ()
  "Send a :claude: tagged org entry to the target session.
If point is on a heading in an org buffer, use that entry.
Otherwise, search org files for :claude: tagged entries and prompt."
  (interactive)
  (let (entry-text)
    (if (and (derived-mode-p 'org-mode)
             (ignore-errors (org-back-to-heading t) t))
        (save-excursion
          (org-back-to-heading t)
          (setq entry-text (buffer-substring-no-properties
                            (point) (org-end-of-subtree t t))))
      (let ((entries '()))
        (dolist (file (directory-files "~/.org" t "\\.org$"))
          (with-temp-buffer
            (insert-file-contents file)
            (org-mode)
            (goto-char (point-min))
            (while (re-search-forward "^\\*+ .+:claude:" nil t)
              (org-back-to-heading t)
              (push (cons (org-get-heading t t t t)
                          (buffer-substring-no-properties
                           (point) (org-end-of-subtree t t)))
                    entries))))
        (unless entries
          (user-error "No :claude: tagged entries found in ~/.org/"))
        (setq entry-text (cdr (assoc (completing-read "Send to Claude: "
                                                      (mapcar #'car entries) nil t)
                                     entries)))))
    (let ((prompt (if (string-match "^\\*+ Prompt\n\\(\\(?:.*\n?\\)*\\)" entry-text)
                      (string-trim (match-string 1 entry-text))
                    (read-string "Prompt for Claude: "))))
      (kdb-claude--send (kdb-claude--get-target)
                        (format "%s\n\nContext:\n\n%s" prompt entry-text)))))

;;; ============================================================
;;; Unified Transient
;;; ============================================================

(defun kdb-claude--acp-p ()
  "Return non-nil when agent-shell and its ACP adapter are installed."
  (bound-and-true-p kdb-agent-shell-enabled))

(defun kdb-claude--tui-p ()
  "Return non-nil when the claude-code TUI can run (needs a PTY)."
  (not (eq system-type 'windows-nt)))

(transient-define-prefix kdb-claude-menu ()
  "Claude"
  [:description (lambda ()
                  (format "Target: %s   Waiting: %d"
                          (if (and kdb-claude-target (buffer-live-p kdb-claude-target))
                              (buffer-name kdb-claude-target)
                            "none (set with ;)")
                          (kdb-claude--waiting-count)))
   ["Start"
    ("N" "New (project)" kdb-claude-start)
    ("a" "Agent shell here" agent-shell-anthropic-start-claude-code :if kdb-claude--acp-p)
    ("x" "Agent TUI here" claude-code :if kdb-claude--tui-p)
    ("p" "Pipe session here" kdb-claude-print)
    ("c" "Pipe follow-up" kdb-claude-print-continue)]
   ["Send to Target"
    ("s" "Prompt" kdb-claude-prompt-target)
    ("r" "Region" kdb-claude-send-region-to-target)
    ("F" "File" kdb-claude-send-file-to-target)
    ("y" "Accept" kdb-claude-accept-target)
    ("n" "Reject" kdb-claude-reject-target)]]
  [["Sessions"
    ("'" "Next Waiting" kdb-claude-next-waiting)
    (";" "Set Target" kdb-claude-set-target)
    ("." "Toggle Session" claude-code-toggle :if kdb-claude--tui-p)
    ("K" "End Session" claude-code-kill :if kdb-claude--tui-p)
    ("*" "Star" kdb-claude-star)
    ("8" "Starred" kdb-claude-stars)]
   ["Archive"
    ("o" "Recall" kdb-claude-recall)
    ("b" "Browse" kdb-claude-browse)
    ("/" "Search" kdb-claude-search)
    ("A" "Ask Archive" kdb-claude-ask-archive)
    ("u" "Resume Archived" kdb-claude-resume-archived)
    ("S" "Sync & Commit" kdb-claude-sync)]
   ["Move"
    (">" "Recall → Target" kdb-claude-send-to-target)
    ("q" "From Notes" kdb-claude-send-capture)
    ("i" "Import Desktop" kdb-claude-import-export)]])

(defun kdb-claude ()
  "Claude menu — think, code, recall."
  (interactive)
  (when (kdb-claude--tui-p) (require 'claude-code nil t))
  (kdb-claude-menu))

(provide 'init-claude-workflow)
;;; init-claude-workflow.el ends here
