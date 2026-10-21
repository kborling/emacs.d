;;; init-claude.el --- Claude session management -*- lexical-binding: t; -*-

;;; Commentary:

;; Centralized Claude session log management.
;; Syncs Claude Code sessions, gptel logs, and Desktop exports
;; into a private git repository for backup and search.
;;
;; Set `kdb-claude-archive-dir' in personal.el to the repo path.

;;; Code:

(require 'json)

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
                      (format-time-string "%Y-%m-%d"
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

;;; ============================================================
;;; gptel Log Sync
;;; ============================================================

(defun kdb-claude-sync-gptel ()
  "Copy gptel log to the archive."
  (interactive)
  (let* ((archive (kdb-claude--ensure-archive))
         (dest (expand-file-name "gptel/" archive))
         (source (expand-file-name "gptel-log.org" "~/.org/")))
    (make-directory dest t)
    (when (file-exists-p source)
      (copy-file source
                 (expand-file-name
                  (format "gptel-%s.org" (system-name)) dest)
                 t)
      (message "gptel log synced"))))

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
  (kdb-claude-sync-gptel)
  (let ((default-directory (kdb-claude--ensure-archive)))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m"
                  (format "Sync %s" (format-time-string "%Y-%m-%d %H:%M")))
    (when (zerop (call-process "git" nil nil nil "remote" "get-url" "origin"))
      (call-process "git" nil nil nil "push"))
    (message "Claude archive synced and committed")))

;;;###autoload
(defun kdb-claude-browse ()
  "Browse Claude sessions from the archive or Desktop export."
  (interactive)
  (let* ((source (completing-read "Browse: "
                                  '("Code Sessions" "Desktop Export") nil t))
         (dir (pcase source
                ("Code Sessions"
                 (expand-file-name "code-sessions/" (kdb-claude--ensure-archive)))
                ("Desktop Export"
                 (kdb-claude--find-export-dir)))))
    (if (and (equal source "Code Sessions") (file-directory-p dir))
        (let* ((files (directory-files-recursively dir "\\.org$"))
               (candidates (mapcar
                            (lambda (f)
                              (cons (format "%-12s  %-24s  %s"
                                            (format-time-string "%Y-%m-%d"
                                              (file-attribute-modification-time
                                               (file-attributes f)))
                                            (truncate-string-to-width
                                             (file-name-nondirectory
                                              (directory-file-name (file-name-directory f)))
                                             24 nil nil "…")
                                            (kdb-claude--org-title f))
                                    f))
                            (sort files (lambda (a b)
                                          (time-less-p
                                           (file-attribute-modification-time (file-attributes b))
                                           (file-attribute-modification-time (file-attributes a)))))))
               (choice (completing-read "Session: " candidates nil t))
               (file (cdr (assoc choice candidates))))
          (when file (find-file file)))
      ;; Desktop export browse
      (let* ((conversations (kdb-claude--parse-conversations dir))
             (sorted (sort (copy-sequence conversations)
                           (lambda (a b)
                             (string> (or (alist-get 'updated_at a) "")
                                      (or (alist-get 'updated_at b) "")))))
             (candidates (mapcar
                          (lambda (conv)
                            (cons (format "%-20s  %s"
                                          (kdb-claude--format-date (alist-get 'updated_at conv))
                                          (kdb-claude--conversation-title conv))
                                  conv))
                          sorted))
             (choice (completing-read "Conversation: " candidates nil t))
             (conv (cdr (assoc choice candidates))))
        (when conv
          (kdb-claude--display-conversation conv))))))

(defun kdb-claude--display-conversation (conv)
  "Display CONV in an org-mode buffer."
  (let* ((title (kdb-claude--conversation-title conv))
         (buf-name (format "*Claude: %s*" title))
         (chat-messages (alist-get 'chat_messages conv)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n" title))
        (insert (format "#+DATE: %s\n" (kdb-claude--format-date (alist-get 'created_at conv))))
        (insert (format "#+UPDATED: %s\n\n" (kdb-claude--format-date (alist-get 'updated_at conv))))
        (if chat-messages
            (dolist (msg chat-messages)
              (let ((sender (or (alist-get 'sender msg) "unknown"))
                    (text (kdb-claude--extract-text
                           (or (alist-get 'content msg) (alist-get 'text msg) ""))))
                (insert (format "* %s\n\n%s\n\n"
                                (if (equal sender "human") "You" "Claude")
                                text))))
          (insert "No messages found.\n")))
      (goto-char (point-min))
      (org-mode)
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun kdb-claude-search (query)
  "Search all Claude archives for QUERY."
  (interactive "sSearch Claude sessions: ")
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

(defvar kdb-claude-print-permission-mode)
(defvar kdb-claude-print--session-id)
(declare-function kdb-claude-print--run "init-claude-print")
(declare-function kdb-claude-print--new-buffer "init-claude-print")

;;;###autoload
(defun kdb-claude-ask-archive (question)
  "Ask Claude QUESTION about past sessions, searching the archive read-only."
  (interactive "sAsk the archive: ")
  (require 'init-claude-print)
  (kdb-claude-sync-code-sessions)
  (let* ((default-directory (kdb-claude--ensure-archive))
         (buf (kdb-claude-print--new-buffer "archive")))
    (with-current-buffer buf
      (setq-local kdb-claude-print-permission-mode "dontAsk"))
    (kdb-claude-print--run
     (concat "This directory archives past Claude sessions as org files "
             "(code-sessions/, desktop/, gptel/).  Search them to answer, "
             "citing the files you used:\n\n" question)
     nil nil buf)))

;;;###autoload
(defun kdb-claude-resume-archived (file)
  "Continue the Claude Code session archived in org FILE.
Opens a print-mode buffer that resumes the session on its next prompt."
  (interactive
   (list (if (and buffer-file-name (kdb-claude--org-keyword buffer-file-name "SOURCE"))
             buffer-file-name
           (read-file-name "Archived session: "
                           (expand-file-name "code-sessions/" (kdb-claude--ensure-archive))))))
  (require 'init-claude-print)
  (let* ((source (or (kdb-claude--org-keyword file "SOURCE")
                     (user-error "Not an archived Claude Code session")))
         (default-directory (file-name-as-directory
                             (or (kdb-claude--org-keyword file "CWD") default-directory)))
         (buf (kdb-claude-print--new-buffer)))
    (with-current-buffer buf
      (setq kdb-claude-print--session-id (file-name-base source)))
    (kdb-claude-print--run (read-string (format "Resume \"%s\": " (kdb-claude--org-title file)))
                           t nil buf)))

(provide 'init-claude)
;;; init-claude.el ends here
