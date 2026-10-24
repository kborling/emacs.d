;;; init-claude.el --- Claude session management -*- lexical-binding: t; -*-

;;; Commentary:

;; Centralized Claude session log management.
;; Syncs Claude Code sessions and Desktop exports
;; into a private git repository for backup and search.
;;
;; Set `kdb-claude-archive-dir' in personal.el to the repo path.

;;; Code:

(require 'json)
(require 'cl-lib)

(defcustom kdb-claude-archive-dir "~/.claude-archive"
  "Path to the private git repo for Claude session archives."
  :type 'directory
  :group 'claude)

(defcustom kdb-claude-export-directory nil
  "Path to extracted Claude Desktop data export directory."
  :type '(choice (const nil) directory)
  :group 'claude)

(defun kdb-claude--ensure-archive ()
  "Ensure the archive directory exists and is a git repo."
  (unless kdb-claude-archive-dir
    (user-error "Set kdb-claude-archive-dir in personal.el"))
  (let ((dir (expand-file-name kdb-claude-archive-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (let ((default-directory dir))
        (call-process "git" nil nil nil "init")
        (with-temp-file (expand-file-name ".gitignore" dir)
          (insert "# Keep everything\n"))
        (call-process "git" nil nil nil "add" ".")
        (call-process "git" nil nil nil "commit" "-m" "Init claude archive")))
    dir))

;;; ============================================================
;;; Claude Code Session Sync
;;; ============================================================

(defun kdb-claude--projects-dir ()
  "Return Claude Code's session directory.
Windows Emacs often sets HOME to AppData, but Claude uses USERPROFILE."
  (expand-file-name "projects"
                    (or (getenv "CLAUDE_CONFIG_DIR")
                        (expand-file-name ".claude" (or (getenv "USERPROFILE") "~")))))

(defun kdb-claude-sync-code-sessions (&optional force)
  "Sync Claude Code sessions to the archive.
Converts JSONL sessions to readable org files.  With prefix arg FORCE,
reconvert every session."
  (interactive "P")
  (let* ((archive (kdb-claude--ensure-archive))
         (dest (expand-file-name "code-sessions/" archive))
         (source (kdb-claude--projects-dir))
         (count 0))
    (make-directory dest t)
    (when (file-directory-p source)
      (dolist (jsonl (seq-remove (lambda (f) (string-match-p "/subagents/" f))
                                 (directory-files-recursively source "\\.jsonl$")))
        (let* ((session-id (file-name-base jsonl))
               (project-dir (file-name-directory jsonl))
               (project-name (file-name-nondirectory
                              (directory-file-name project-dir)))
               (org-file (expand-file-name
                          (format "%s/%s.org" project-name session-id) dest))
               (org-dir (file-name-directory org-file)))
          ;; Only convert if org file doesn't exist or jsonl is newer
          (when (or force
                    (not (file-exists-p org-file))
                    (time-less-p (file-attribute-modification-time
                                  (file-attributes org-file))
                                 (file-attribute-modification-time
                                  (file-attributes jsonl))))
            (make-directory org-dir t)
            (kdb-claude--jsonl-to-org jsonl org-file)
            (cl-incf count)))))
    (message "Synced %d Claude Code sessions" count)))

(defun kdb-claude--session-title (messages)
  "Return the first line of the first real user prompt in MESSAGES."
  (let ((first (seq-find (lambda (m)
                           (and (equal (car m) "user")
                                (not (string-prefix-p "<" (cdr m)))))
                         messages)))
    (if first
        (truncate-string-to-width (car (split-string (cdr first) "\n" t))
                                  80 nil nil "…")
      "Claude Code Session")))

(defun kdb-claude--jsonl-to-org (jsonl-file org-file)
  "Convert JSONL-FILE to a readable ORG-FILE."
  (let (messages cwd)
    (with-temp-buffer
      (insert-file-contents jsonl-file)
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (let* ((obj (json-parse-string
                         (buffer-substring (line-beginning-position) (line-end-position))
                         :object-type 'alist :array-type 'list :null-object nil))
                   (msg (alist-get 'message obj))
                   (role (alist-get 'role msg))
                   (text (when (member role '("user" "assistant"))
                           (string-trim (kdb-claude--extract-text
                                         (alist-get 'content msg))))))
              (setq cwd (or cwd (alist-get 'cwd obj)))
              (unless (or (null text) (string-empty-p text))
                (push (cons role text) messages)))
          (error nil))
        (forward-line 1)))
    (setq messages (nreverse messages))
    (with-temp-file org-file
      (insert (format "#+TITLE: %s\n" (kdb-claude--session-title messages)))
      (insert (format "#+DATE: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M"
                                          (file-attribute-modification-time
                                           (file-attributes jsonl-file)))))
      (insert (format "#+SOURCE: %s\n" jsonl-file))
      (when cwd (insert (format "#+CWD: %s\n" cwd)))
      (insert "\n")
      (dolist (msg messages)
        (insert (format "* %s\n\n%s\n\n"
                        (if (equal (car msg) "user") "You" "Claude")
                        (cdr msg)))))))

(defun kdb-claude--org-keyword (file keyword)
  "Return the value of #+KEYWORD in the header of org FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 1000)
    (when (re-search-forward (format "^#\\+%s: \\(.*\\)" keyword) nil t)
      (match-string 1))))

(defun kdb-claude--org-title (file)
  "Return the title of archived org FILE, or its base name."
  (or (kdb-claude--org-keyword file "TITLE") (file-name-base file)))

(defun kdb-claude--org-header (file)
  "Return an alist of the #+KEYWORD values in the header of org FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (let (kw)
      (while (re-search-forward "^#\\+\\([A-Z]+\\): \\(.*\\)" nil t)
        (push (cons (match-string 1) (match-string 2)) kw))
      kw)))

;;; ============================================================
;;; Directory picker
;;; ============================================================

(defun kdb-claude--jsonl-cwd (file)
  "Return the working directory recorded in Claude Code transcript FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 65536)
    (when (re-search-forward "\"cwd\":\\(\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\)" nil t)
      (json-parse-string (match-string 1)))))

(defun kdb-claude--session-dirs ()
  "Return (DIR . TIME) for directories with Claude Code sessions, newest first."
  (let (dirs)
    (dolist (proj (ignore-errors (directory-files (kdb-claude--projects-dir) t "\\`[^.]")))
      (when-let* ((newest (car (sort (directory-files proj t "\\.jsonl\\'")
                                     #'file-newer-than-file-p)))
                  (cwd (ignore-errors (kdb-claude--jsonl-cwd newest))))
        (push (cons (file-name-as-directory (expand-file-name cwd))
                    (file-attribute-modification-time (file-attributes newest)))
              dirs)))
    (sort dirs (lambda (a b) (time-less-p (cdr b) (cdr a))))))

(defun kdb-claude-read-directory (prompt)
  "Read a directory with PROMPT, defaulting to the current project.
Directories with Claude sessions come first, most recent first, then
known projects and the repos of recent files.  Any other input is read
as a directory name."
  (let* ((tmp (file-truename temporary-file-directory))
         (claude (seq-remove (lambda (d) (or (not (file-directory-p (car d)))
                                             (string-prefix-p tmp (file-truename (car d)))))
                             (kdb-claude--session-dirs)))
         (others (append (and (fboundp 'project-known-project-roots)
                              (project-known-project-roots))
                         (delq nil (mapcar (lambda (f)
                                             (locate-dominating-file (file-name-directory f) ".git"))
                                           (bound-and-true-p recentf-list)))))
         (dirs (seq-uniq (append (mapcar #'car claude)
                                 (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                                         (seq-filter #'file-directory-p others)))))
         (cands (mapcar #'abbreviate-file-name dirs))
         (here (and (project-current) (abbreviate-file-name (project-root (project-current)))))
         (choice (completing-read
                  (format-prompt (string-remove-suffix ": " prompt) here)
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          (display-sort-function . identity)
                          (annotation-function
                           . ,(lambda (c)
                                (when-let* ((time (cdr (assoc (expand-file-name c) claude))))
                                  (format-time-string "  claude %Y-%m-%d %H:%M" time)))))
                      (complete-with-action action cands str pred)))
                  nil nil nil nil here)))
    (file-name-as-directory
     (expand-file-name (if (file-directory-p choice) choice (read-directory-name "Directory: " choice))))))

;;; ============================================================
;;; History
;;; ============================================================

(defvar-local kdb-claude--history-filter nil
  "Regexp the history list is narrowed to, matched against project and title.")

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-session-id "agent-shell")
(declare-function kdb-claude--start-with "init-claude-workflow")

(defun kdb-claude--live-sessions ()
  "Return (SESSION-ID . BUFFER) for open agent-shell sessions."
  (when (fboundp 'agent-shell-buffers)
    (cl-loop for b in (agent-shell-buffers)
             for id = (agent-shell-session-id :shell-buffer b)
             when id collect (cons id b))))

(defun kdb-claude--history-entries ()
  "Return `tabulated-list-entries' for every archived session."
  (let ((live (mapcar #'car (kdb-claude--live-sessions))))
    (seq-filter
     (lambda (e)
       (or (null kdb-claude--history-filter)
           (string-match-p kdb-claude--history-filter
                           (concat (aref (cadr e) 2) " " (aref (cadr e) 3)))))
     (mapcar
      (lambda (f)
        (let* ((h (kdb-claude--org-header f))
               (cwd (cdr (assoc "CWD" h))))
          (list f (vector (if (member (file-name-base f) live) "●" "")
                          (or (cdr (assoc "UPDATED" h)) (cdr (assoc "DATE" h)) "")
                          (cond (cwd (file-name-nondirectory (directory-file-name cwd)))
                                ((string-match-p "/desktop/" f) "desktop")
                                (t ""))
                          (or (cdr (assoc "TITLE" h)) (file-name-base f))))))
      (directory-files-recursively (kdb-claude--ensure-archive) "\\.org$")))))

(defvar kdb-claude-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'kdb-claude-history-open)
    (define-key map (kbd "u") #'kdb-claude-history-resume)
    (define-key map (kbd "/") #'kdb-claude-history-filter)
    (define-key map (kbd "s") #'kdb-claude-search)
    (define-key map (kbd "A") #'kdb-claude-ask-archive)
    map)
  "Keymap for `kdb-claude-history-mode'.")

(define-derived-mode kdb-claude-history-mode tabulated-list-mode "Claude-History"
  "Past Claude sessions, newest first.  ● marks sessions open in Emacs.
\\{kdb-claude-history-mode-map}"
  (setq tabulated-list-format [("" 1 t) ("Last active" 16 t) ("Project" 20 t) ("Title" 0 t)]
        tabulated-list-sort-key '("Last active" . t)
        tabulated-list-entries #'kdb-claude--history-entries)
  (add-hook 'tabulated-list-revert-hook #'kdb-claude-sync-code-sessions nil t)
  (setq-local header-line-format
              (substitute-command-keys
               "\\<kdb-claude-history-mode-map>\\[kdb-claude-history-open] open  \\[kdb-claude-history-resume] resume  \\[kdb-claude-history-filter] filter  \\[kdb-claude-search] search text  \\[kdb-claude-ask-archive] ask  \\[revert-buffer] refresh")))

;;;###autoload
(defun kdb-claude-history ()
  "List every past Claude session — Claude Code and imported Desktop chats."
  (interactive)
  (kdb-claude-sync-code-sessions)
  (with-current-buffer (get-buffer-create "*Claude History*")
    (kdb-claude-history-mode)
    (tabulated-list-print)
    (pop-to-buffer-same-window (current-buffer))))

(defun kdb-claude-history-filter (regexp)
  "Narrow the history to sessions whose project or title match REGEXP.
Empty input shows everything."
  (interactive (list (read-string "Filter (project/title, empty = all): ")))
  (setq kdb-claude--history-filter (unless (string-empty-p regexp) regexp))
  (tabulated-list-print))

(defun kdb-claude-history-open ()
  "View the session at point."
  (interactive)
  (view-file (or (tabulated-list-get-id) (user-error "No session here"))))

(defun kdb-claude-history-resume ()
  "Continue the session at point, switching to it if it is already open."
  (interactive)
  (let* ((file (or (tabulated-list-get-id) (user-error "No session here")))
         (buf (cdr (assoc (file-name-base file) (kdb-claude--live-sessions)))))
    (if buf (switch-to-buffer buf) (kdb-claude-resume-archived file))))

;;; ============================================================
;;; Claude Desktop Export
;;; ============================================================

(defun kdb-claude--find-export-dir ()
  "Find the Claude export directory, prompting if not set."
  (or kdb-claude-export-directory
      (let ((dir (read-directory-name "Claude export directory: ")))
        (setq kdb-claude-export-directory dir)
        dir)))

(defun kdb-claude--read-json (file)
  "Read JSON FILE and return parsed data."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun kdb-claude--find-conversations-file (dir)
  "Find the conversations JSON file in DIR."
  (let ((direct (expand-file-name "conversations.json" dir)))
    (if (file-exists-p direct)
        direct
      (car (directory-files-recursively dir "^conversations\\.json$")))))

(defun kdb-claude--parse-conversations (dir)
  "Parse conversations from export DIR."
  (let ((file (kdb-claude--find-conversations-file dir)))
    (unless file
      (error "No conversations.json found in %s" dir))
    (kdb-claude--read-json file)))

(defun kdb-claude--format-date (date-str)
  "Format DATE-STR for display."
  (if (and date-str (not (equal date-str :null)))
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M"
                              (encode-time (parse-time-string
                                            (replace-regexp-in-string "T" " "
                                              (replace-regexp-in-string "\\.[0-9]+Z$" "" date-str)))))
        (error (substring date-str 0 (min 16 (length date-str)))))
    "unknown"))

(defun kdb-claude--conversation-title (conv)
  "Get display title for conversation CONV."
  (let ((name (alist-get 'name conv)))
    (if (or (null name) (equal name :null) (string-empty-p name))
        "(untitled)"
      name)))

(defun kdb-claude--extract-text (content)
  "Extract text from message CONTENT."
  (cond
   ((stringp content) content)
   ((and (listp content) (listp (car content)))
    (mapconcat
     (lambda (block)
       (let ((type (alist-get 'type block)))
         (cond
          ((equal type "text") (or (alist-get 'text block) ""))
          ((equal type "tool_use")
           (format "[Tool: %s]" (or (alist-get 'name block) "unknown")))
          (t ""))))
     content "\n"))
   (t (format "%s" content))))

(defun kdb-claude-sync-desktop ()
  "Sync Claude Desktop export to the archive as org files."
  (interactive)
  (let* ((archive (kdb-claude--ensure-archive))
         (dest (expand-file-name "desktop/" archive))
         (dir (kdb-claude--find-export-dir))
         (conversations (kdb-claude--parse-conversations dir))
         (count 0))
    (make-directory dest t)
    (dolist (conv conversations)
      (let* ((uuid (alist-get 'uuid conv))
             (title (kdb-claude--conversation-title conv))
             (org-file (expand-file-name (format "%s.org" uuid) dest))
             (chat-messages (alist-get 'chat_messages conv)))
        (unless (file-exists-p org-file)
          (with-temp-file org-file
            (insert (format "#+TITLE: %s\n" title))
            (insert (format "#+DATE: %s\n" (kdb-claude--format-date (alist-get 'created_at conv))))
            (insert (format "#+UPDATED: %s\n\n" (kdb-claude--format-date (alist-get 'updated_at conv))))
            (when chat-messages
              (dolist (msg chat-messages)
                (let ((sender (or (alist-get 'sender msg) "unknown"))
                      (text (kdb-claude--extract-text
                             (or (alist-get 'content msg) (alist-get 'text msg) ""))))
                  (insert (format "* %s\n\n%s\n\n"
                                  (if (equal sender "human") "You" "Claude")
                                  text))))))
          (cl-incf count))))
    (message "Synced %d Claude Desktop conversations" count)))

;;; ============================================================
;;; Unified Sync & Search
;;; ============================================================

;;;###autoload
(defun kdb-claude-sync ()
  "Sync all Claude sessions to the archive and commit."
  (interactive)
  (kdb-claude-sync-code-sessions)
  (let ((default-directory (kdb-claude--ensure-archive)))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m"
                  (format "Sync %s" (format-time-string "%Y-%m-%d %H:%M")))
    (when (zerop (call-process "git" nil nil nil "remote" "get-url" "origin"))
      (call-process "git" nil nil nil "push"))
    (message "Claude archive synced and committed")))

;;;###autoload
(defun kdb-claude-search (query)
  "Search all Claude archives for QUERY."
  (interactive "sSearch Claude sessions: ")
  (kdb-claude-sync-code-sessions)
  (let ((dir (kdb-claude--ensure-archive)))
    (if (fboundp 'deadgrep)
        (deadgrep query dir)
      (grep (format "rg --no-heading --line-number %s %s"
                    (shell-quote-argument query)
                    (shell-quote-argument dir))))))

;;;###autoload
(defun kdb-claude-import-export (zip-or-dir)
  "Import a Claude Desktop data export from ZIP-OR-DIR."
  (interactive "fClaude export (zip or directory): ")
  (let ((path (expand-file-name zip-or-dir)))
    (if (file-directory-p path)
        (setq kdb-claude-export-directory path)
      (let ((dest (expand-file-name "claude-export" (file-name-directory path))))
        (make-directory dest t)
        (call-process "unzip" nil nil nil "-o" path "-d" dest)
        (setq kdb-claude-export-directory dest)))
    (message "Claude export set to: %s" kdb-claude-export-directory)
    (when (yes-or-no-p "Sync to archive now? ")
      (kdb-claude-sync-desktop))))

;;; ============================================================
;;; Ask & Resume
;;; ============================================================

;;;###autoload
(defun kdb-claude-ask-archive (question)
  "Ask Claude QUESTION about past sessions; it searches the archive to answer."
  (interactive "sAsk past sessions: ")
  (require 'init-claude-workflow)
  (kdb-claude-sync-code-sessions)
  (kdb-claude--start-with
   (kdb-claude--ensure-archive)
   (concat "This directory archives past Claude sessions as org files "
           "(code-sessions/, desktop/).  Search them with Grep to answer, "
           "citing the files you used.  Don't modify anything.\n\n" question)))

;;;###autoload
(defun kdb-claude-resume-archived (file)
  "Continue the Claude Code session archived in org FILE, in its directory."
  (interactive
   (list (if (and buffer-file-name (kdb-claude--org-keyword buffer-file-name "SOURCE"))
             buffer-file-name
           (read-file-name "Archived session: "
                           (expand-file-name "code-sessions/" (kdb-claude--ensure-archive))))))
  (require 'init-claude-workflow)
  (let ((source (or (kdb-claude--org-keyword file "SOURCE")
                    (user-error "Not an archived Claude Code session")))
        (cwd (kdb-claude--org-keyword file "CWD")))
    (unless (and cwd (file-directory-p cwd))
      (user-error "Session directory %s no longer exists" cwd))
    (kdb-claude--start-with (file-name-as-directory cwd) nil (file-name-base source))))

(provide 'init-claude)
;;; init-claude.el ends here
