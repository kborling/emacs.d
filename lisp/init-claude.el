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
(declare-function org-escape-code-in-string "org-src")

(defcustom kdb-claude-archive-dir "~/.claude-archive"
  "Path to the private git repo for Claude session archives."
  :type 'directory
  :group 'claude)

(defvar kdb-claude-prompt-history nil
  "Minibuffer history for what you type to Claude; never saved to disk.")
(defvar savehist-ignored-variables)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-ignored-variables 'kdb-claude-prompt-history))

(defun kdb-claude--read (prompt)
  "Read a string with PROMPT, keeping it out of saved minibuffer history."
  (read-string prompt nil 'kdb-claude-prompt-history))

(defcustom kdb-claude-export-directory nil
  "Path to extracted Claude Desktop data export directory."
  :type '(choice (const nil) directory)
  :group 'claude)

(defun kdb-claude--archive-repo-p ()
  "Return non-nil if the archive directory is already a git repo."
  (file-directory-p (expand-file-name ".git" kdb-claude-archive-dir)))

(defun kdb-claude--ensure-archive ()
  "Ensure the archive directory exists and is a git repo."
  (unless kdb-claude-archive-dir
    (user-error "Set kdb-claude-archive-dir in personal.el"))
  (let ((dir (expand-file-name kdb-claude-archive-dir)))
    (unless (kdb-claude--archive-repo-p)
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

(defun kdb-claude--config-dir ()
  "Return Claude Code's config directory.
Windows Emacs often sets HOME to AppData, but Claude uses USERPROFILE."
  (or (getenv "CLAUDE_CONFIG_DIR")
      (expand-file-name ".claude" (or (getenv "USERPROFILE") "~"))))

(defun kdb-claude--projects-dir ()
  "Return Claude Code's session directory."
  (expand-file-name "projects" (kdb-claude--config-dir)))

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

(defvar kdb-claude--sync-process nil
  "The background Emacs converting sessions, while one runs.")
(defvar kdb-claude--sync-next nil
  "Callbacks for the sync to run after the current one; non-nil = rerun.")
(defvar kdb-claude--sync-next-commit nil
  "Non-nil if the next sync should also commit the archive.")

(defun kdb-claude-sync-code-sessions-async (&optional callback commit)
  "Sync Claude Code sessions in a background Emacs, then call CALLBACK.
With COMMIT, that Emacs also commits the archive.  Conversion reads every
changed JSONL, which can be GBs, and git is slow on Windows, so neither
runs in this Emacs.  One sync at a time: a request made while one runs
is served by a fresh run right after it."
  (if (process-live-p kdb-claude--sync-process)
      (progn (push callback kdb-claude--sync-next)
             (when commit (setq kdb-claude--sync-next-commit t)))
    (let ((callbacks (cons callback kdb-claude--sync-next))
          (commit (or commit kdb-claude--sync-next-commit)))
      (setq kdb-claude--sync-next nil
            kdb-claude--sync-next-commit nil)
      (kdb-claude--ensure-archive)
      (with-current-buffer (get-buffer-create " *claude-archive-sync*")
        (erase-buffer))
      (setq kdb-claude--sync-process
            (make-process
             :name "claude-archive-sync" :buffer " *claude-archive-sync*" :noquery t
             :command
             (list (let ((emacs (expand-file-name invocation-name invocation-directory)))
                     (or (executable-find emacs) emacs)) ; adds .exe on Windows
                   "--batch" "-Q" "-L" (file-name-directory (locate-library "init-claude"))
                   "-l" "init-claude"
                   "--eval" (format "(progn (set-language-environment \"UTF-8\") \
(setq kdb-claude-archive-dir %S) (kdb-claude-sync-code-sessions)%s)"
                                    (expand-file-name kdb-claude-archive-dir)
                                    (if commit " (kdb-claude--commit-archive)" "")))
             :sentinel
             (lambda (p _)
               (unless (process-live-p p)
                 (setq kdb-claude--sync-process nil)
                 (unless (zerop (process-exit-status p))
                   (message "Claude archive sync failed; see buffer \" *claude-archive-sync*\""))
                 (dolist (cb (delq nil callbacks))
                   (with-demoted-errors "Claude archive sync callback: %S" (funcall cb)))
                 (when kdb-claude--sync-next
                   (kdb-claude-sync-code-sessions-async)))))))))

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
  "Convert JSONL-FILE to a readable ORG-FILE.
ORG-FILE gets the JSONL's mtime from before it was read, so turns
appended while converting make the JSONL newer and are picked up next sync."
  (let ((mtime (file-attribute-modification-time (file-attributes jsonl-file)))
        messages cwd)
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
                   ;; By entry type: api-request-blob entries repeat messages
                   (text (when (member (alist-get 'type obj) '("user" "assistant"))
                           (string-trim (kdb-claude--extract-text
                                         (alist-get 'content msg))))))
              (setq cwd (or cwd (alist-get 'cwd obj)))
              (unless (or (null text) (string-empty-p text))
                (push (cons role text) messages)))
          (error nil))
        (forward-line 1)))
    (setq messages (nreverse messages))
    (let ((coding-system-for-write 'utf-8-unix))  ; no CRLF churn from Windows
      (with-temp-file org-file
        (insert (format "#+TITLE: %s\n" (kdb-claude--session-title messages)))
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M" mtime)))
        (insert (format "#+SOURCE: %s\n" jsonl-file))
        (when cwd (insert (format "#+CWD: %s\n" cwd)))
        (insert "\n")
        (dolist (msg messages)
          (insert (format "* %s\n\n%s\n\n"
                          (if (equal (car msg) "user") "You" "Claude")
                          (cdr msg))))))
    (set-file-times org-file mtime)))

(defun kdb-claude--org-keyword (file keyword)
  "Return the value of #+KEYWORD in the header of org FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 1000)
    (when (re-search-forward (format "^#\\+%s: \\(.*\\)" keyword) nil t)
      (match-string 1))))

(defun kdb-claude--org-title (file)
  "Return the title of archived org FILE, or its base name."
  (or (kdb-claude--org-keyword file "TITLE") (file-name-base file)))

(defvar kdb-claude--header-cache (make-hash-table :test #'equal)
  "FILE -> (MTIME . HEADER), so redraws only reread changed files.")

(defun kdb-claude--org-header (file)
  "Return an alist of the #+KEYWORD values in the header of org FILE."
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        (hit (gethash file kdb-claude--header-cache)))
    (if (equal (car hit) mtime)
        (cdr hit)
      (cdr (puthash file (cons mtime (kdb-claude--read-org-header file))
                    kdb-claude--header-cache)))))

(defun kdb-claude--read-org-header (file)
  "Read the #+KEYWORD values in the header of org FILE."
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
      (when-let* ((newest (car (sort (directory-files-and-attributes proj t "\\.jsonl\\'" t)
                                     (lambda (a b) (time-less-p (file-attribute-modification-time (cdr b))
                                                                (file-attribute-modification-time (cdr a)))))))
                  (cwd (ignore-errors (kdb-claude--jsonl-cwd (car newest)))))
        (push (cons (file-name-as-directory (expand-file-name cwd))
                    (file-attribute-modification-time (cdr newest)))
              dirs)))
    (sort dirs (lambda (a b) (time-less-p (cdr b) (cdr a))))))

(defun kdb-claude-read-directory (prompt)
  "Read a directory with PROMPT, defaulting to the current project.
Directories with Claude sessions come first, most recent first, then
known projects and the repos of recent files.  Any other input is read
as a directory name."
  (let* ((tmps (list (expand-file-name temporary-file-directory)
                     (file-truename temporary-file-directory)))
         (claude (seq-remove (lambda (d) (or (seq-some (lambda (tmp) (string-prefix-p tmp (car d))) tmps)
                                             (not (file-directory-p (car d)))))
                             (kdb-claude--session-dirs)))
         (others (seq-remove
                  #'file-remote-p
                  (append (and (fboundp 'project-known-project-roots)
                               (project-known-project-roots))
                          ;; One lookup per directory, not per recent file.
                          (delq nil (mapcar (lambda (dir)
                                              (unless (file-remote-p dir)
                                                (locate-dominating-file dir ".git")))
                                            (seq-uniq (mapcar #'file-name-directory
                                                              (bound-and-true-p recentf-list))))))))
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
  (add-hook 'tabulated-list-revert-hook #'kdb-claude--history-refresh nil t)
  (setq-local header-line-format
              (substitute-command-keys
               "\\<kdb-claude-history-mode-map>\\[kdb-claude-history-open] open  \\[kdb-claude-history-resume] resume  \\[kdb-claude-history-filter] filter  \\[kdb-claude-search] search text  \\[kdb-claude-ask-archive] ask  \\[revert-buffer] refresh")))

;;;###autoload
(defun kdb-claude-history ()
  "List every past Claude session — Claude Code and imported Desktop chats.
Shows the archive as it is, then redraws once a background sync is done."
  (interactive)
  (with-current-buffer (get-buffer-create "*Claude History*")
    (kdb-claude-history-mode)
    (tabulated-list-print)
    (pop-to-buffer-same-window (current-buffer))
    (kdb-claude--history-refresh)))

(defun kdb-claude--history-refresh ()
  "Sync in the background, then redraw this history buffer."
  (let ((buf (current-buffer)))
    (kdb-claude-sync-code-sessions-async
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf (tabulated-list-print t)))))))

(defun kdb-claude-history-filter (regexp)
  "Narrow the history to sessions whose project or title match REGEXP.
Empty input shows everything."
  (interactive (list (kdb-claude--read "Filter (project/title, empty = all): ")))
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
    (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil)))

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

(defun kdb-claude--strings (x &optional key)
  "Return the string leaves of parsed JSON X as \"KEY: value\" lines.
Id-like keys are skipped."
  (cond ((and key (string-match-p "\\(?:\\`\\|_\\)\\(?:id\\|uuid\\)\\'" (symbol-name key))) nil)
        ((stringp x) (unless (string-empty-p x)
                       (list (if key (format "%s: %s" key x) x))))
        ((and (consp x) (consp (car x)) (symbolp (caar x)))
         (mapcan (lambda (kv) (kdb-claude--strings (cdr kv) (car kv))) x))
        ((consp x) (mapcan (lambda (e) (kdb-claude--strings e key)) x))))

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
           (let ((detail (string-join (kdb-claude--strings (alist-get 'input block)) "\n")))
             (concat (format "[Tool: %s]" (or (alist-get 'name block) "unknown"))
                     (unless (string-empty-p detail)
                       (require 'org-src)
                       (format "\n#+begin_example\n%s\n#+end_example"
                               (org-escape-code-in-string detail))))))
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
        (let ((coding-system-for-write 'utf-8-unix))
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
                                  text)))))))
        (cl-incf count)))
    (message "Synced %d Claude Desktop conversations" count)))

;;; ============================================================
;;; Unified Sync & Search
;;; ============================================================

;;;###autoload
(defun kdb-claude-sync ()
  "Sync and commit the archive in the background, then push it."
  (interactive)
  (message "Claude archive syncing...")
  (kdb-claude-sync-code-sessions-async #'kdb-claude--push-archive t))

(defun kdb-claude--commit-archive ()
  "Stage and commit everything in the archive (run in the batch Emacs)."
  (let ((default-directory (kdb-claude--ensure-archive)))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-q" "-m"
                  (format "Sync %s" (format-time-string "%Y-%m-%d %H:%M")))))

(defun kdb-claude--push-archive ()
  "Push the archive in the background if it has a remote."
  (let ((default-directory (kdb-claude--ensure-archive)))
    (if (not (kdb-claude--git-remote default-directory))
        (message "Claude archive committed (no remote; add one with C-c l !)")
      (message "Claude archive committed; pushing...")
      (make-process
       :name "claude-archive-push" :buffer "*claude-archive-push*"
       :command '("git" "push" "-u" "origin" "HEAD")
       :sentinel (lambda (p _)
                   (unless (process-live-p p)
                     (message "Claude archive push %s"
                              (if (zerop (process-exit-status p)) "done"
                                "FAILED, see *claude-archive-push*"))))))))

;;;###autoload
(defun kdb-claude-search (query)
  "Search all Claude archives for QUERY."
  (interactive (list (kdb-claude--read "Search Claude sessions: ")))
  (kdb-claude-sync-code-sessions-async)
  (let ((dir (kdb-claude--ensure-archive)))
    (if (fboundp 'deadgrep)
        (deadgrep query dir)
      (grep (format "rg --no-heading --line-number %s %s"
                    (shell-quote-argument query)
                    (shell-quote-argument dir))))))

;;;###autoload
(defun kdb-claude-import-export (path)
  "Import a Claude data export from PATH and sync it to the archive.
PATH is an export directory, a zip, a conversations.json, or the
manifest-*.json that claude.ai now exports.  The manifest only links to
the data zips, which need your browser login: the first run opens them
in your browser, and running again on the same manifest imports them."
  (interactive (list (read-file-name "Claude export (manifest, zip or directory): "
                                     (expand-file-name "~/Downloads/"))))
  (let* ((path (expand-file-name path))
         (dest (expand-file-name "claude-export/" (file-name-directory
                                                   (directory-file-name path)))))
    (cond
     ((file-directory-p path) (setq dest path))
     ((string-match-p "\\.zip\\'" path) (kdb-claude--unzip path dest))
     ((alist-get 'data_files (kdb-claude--read-json path))
      (let* ((dir (file-name-directory path))
             (files (mapcar (lambda (f) (cons (expand-file-name (alist-get 'filename f) dir)
                                              (alist-get 'export_url f)))
                            (alist-get 'data_files (kdb-claude--read-json path))))
             (missing (seq-remove (lambda (f) (file-exists-p (car f))) files)))
        (when missing
          (dolist (f missing) (browse-url (cdr f)))
          (user-error "Opened %d export download(s) in your browser; save them to %s, then run this again on %s"
                      (length missing) (abbreviate-file-name dir) (file-name-nondirectory path)))
        (dolist (f files) (kdb-claude--unzip (car f) dest))))
     (t (setq dest (file-name-directory path))))
    (unless (kdb-claude--find-conversations-file dest)
      (user-error "No conversations.json in %s (found: %s)" dest
                  (mapconcat #'file-name-nondirectory
                             (seq-take (directory-files-recursively dest "") 10) ", ")))
    (setq kdb-claude-export-directory dest)
    (kdb-claude-sync-desktop)))

(defun kdb-claude--unzip (zip dest)
  "Extract ZIP into DEST, with unzip or (on Windows) the bundled tar.
Windows' own tar.exe is preferred: Git's GNU tar misreads C:/ paths."
  (make-directory dest t)
  (unless (zerop (if (executable-find "unzip")
                     (call-process "unzip" nil nil nil "-o" "-q" zip "-d" dest)
                   (call-process (let ((sys (expand-file-name "System32/tar.exe"
                                                              (or (getenv "SystemRoot") ""))))
                                   (if (file-exists-p sys) sys "tar"))
                                 nil nil nil "-xf" zip "-C" dest)))
    (user-error "Couldn't extract %s" zip)))

(defun kdb-claude-agent-shell-subdir (subdir)
  "Return agent-shell's SUBDIR (screenshots, history) inside the archive."
  (file-name-as-directory
   (expand-file-name (file-name-concat "agent-shell" subdir)
                     (expand-file-name kdb-claude-archive-dir))))

;;; ============================================================
;;; Machine setup
;;; ============================================================

(defconst kdb-claude--acp-package "@agentclientprotocol/claude-agent-acp")

(defconst kdb-claude--archive-note
  "Past Claude sessions are archived as org files under %s (one file per \
session, with #+TITLE and #+CWD headers).  When the user refers to earlier \
work you have no context for, search there with Grep before asking.\n")

(defun kdb-claude--git-remote (dir)
  "Return the origin URL of the git repo in DIR, or nil."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "remote" "get-url" "origin"))
        (string-trim (buffer-string))))))

(defun kdb-claude--clone-archive (url)
  "Clone URL into the archive directory, which may already hold files."
  (let* ((dir (expand-file-name kdb-claude-archive-dir))
         (tmp (make-temp-file (expand-file-name ".claude-archive-clone"
                                                (file-name-directory (directory-file-name dir)))
                              t)))
    (unwind-protect
        (progn
          (unless (zerop (call-process "git" nil nil nil "clone" "--no-checkout" url tmp))
            (user-error "git clone %s failed" url))
          (make-directory dir t)
          (rename-file (expand-file-name ".git" tmp) (expand-file-name ".git" dir)))
      (delete-directory tmp t))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "checkout" "HEAD" "--" "."))))

(defun kdb-claude--merge-settings (file archive)
  "Make Claude settings FILE keep transcripts and allow reading ARCHIVE.
Other settings are left alone.  Return non-nil if FILE changed."
  (let* ((old (when (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (json-parse-buffer :object-type 'alist :null-object :null
                                     :false-object :false))))
         (new (copy-tree old))
         (perms (alist-get 'permissions new))
         (dirs (append (alist-get 'additionalDirectories perms) nil)))
    (unless (and (numberp (alist-get 'cleanupPeriodDays new))
                 (>= (alist-get 'cleanupPeriodDays new) 36500))
      (setf (alist-get 'cleanupPeriodDays new) 36500))
    (unless (member archive dirs)
      (setf (alist-get 'additionalDirectories perms) (vconcat dirs (list archive)))
      (setf (alist-get 'permissions new) perms))
    (unless (equal old new)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (json-serialize new :null-object :null :false-object :false))
        (json-pretty-print-buffer)
        (insert "\n"))
      t)))

;;;###autoload
(defun kdb-claude-setup ()
  "Set up this machine for the Claude workflow; safe to re-run.
Creates the archive, makes Claude keep transcripts and let every
session read the archive, and offers to install the ACP adapter (npm)
and the emacs-skills plugin.  Installs run in a compilation buffer."
  (interactive)
  (unless (executable-find "claude")
    (user-error "Install the Claude Code CLI and log in first: https://claude.com/claude-code"))
  (unless (kdb-claude--archive-repo-p)
    (let ((url (read-string "Clone an existing private archive repo (URL, RET = new): ")))
      (unless (string-empty-p url)
        (kdb-claude--clone-archive url))))
  (let* ((archive (kdb-claude--ensure-archive))
         (config (kdb-claude--config-dir))
         (settings (expand-file-name "settings.json" config))
         (memory (expand-file-name "CLAUDE.md" config))
         (note (format kdb-claude--archive-note archive))
         (done (list (format "Archive: %s" archive)))
         cmds)
    (if (kdb-claude--git-remote archive)
        (push (format "Archive remote: %s" (kdb-claude--git-remote archive)) done)
      (let ((url (read-string "Private git remote to back up the archive (URL, RET = skip): ")))
        (if (string-empty-p url)
            (push "Archive remote: none (local only)" done)
          (let ((default-directory archive))
            (call-process "git" nil nil nil "remote" "add" "origin" url))
          (push (format "Archive remote: %s (push with C-c l S)" url) done))))
    (push (if (kdb-claude--merge-settings settings archive)
              "settings.json: transcripts kept, archive readable (updated)"
            "settings.json: already set")
          done)
    (if (and (file-exists-p memory)
             (with-temp-buffer (insert-file-contents memory)
                               (or (search-forward archive nil t)
                                   (search-forward (abbreviate-file-name archive) nil t))))
        (push "CLAUDE.md: already mentions the archive" done)
      (make-directory config t)
      (write-region (concat "\n" note) nil memory t 'silent)
      (push "CLAUDE.md: archive note added" done))
    (cond ((executable-find "claude-agent-acp")
           (push "ACP adapter: installed" done))
          ((not (executable-find "npm"))
           (push (format "ACP adapter: MISSING, install node, then npm install -g %s"
                         kdb-claude--acp-package)
                 done))
          ((y-or-n-p "Install the ACP adapter with npm? ")
           (push (format "npm install -g %s" kdb-claude--acp-package) cmds)))
    (if (with-temp-buffer (insert-file-contents settings)
                          (search-forward "\"emacs-skills@" nil t))
        (push "emacs-skills: installed" done)
      (when (y-or-n-p "Install the emacs-skills plugin (lets Claude open files in Emacs)? ")
      (setq cmds (append cmds
                         '("claude plugin marketplace add xenodium/emacs-skills"
                           "claude plugin install emacs-skills@xenodium-emacs-skills")))))
    (when cmds
      ;; A pipe, not a pty: on a pty the claude CLI draws a TUI whose
      ;; cursor codes show up raw in the compilation buffer
      (let ((process-connection-type nil))
        (compile (mapconcat #'identity cmds " && "))))
    (message "Claude setup: %s" (mapconcat #'identity (nreverse done) "; "))))

;;; ============================================================
;;; Ask & Resume
;;; ============================================================

;;;###autoload
(defun kdb-claude-ask-archive (question)
  "Ask Claude QUESTION about past sessions; it searches the archive to answer."
  (interactive (list (kdb-claude--read "Ask past sessions: ")))
  (require 'init-claude-workflow)
  (kdb-claude-sync-code-sessions-async)
  (kdb-claude--start-with
   (kdb-claude--ensure-archive)
   (concat "This directory archives past Claude sessions as org files "
           "(code-sessions/, desktop/).  Search them with Grep to answer, "
           "citing the files you used.  Don't modify anything.\n\n" question)))

;;;###autoload
(defun kdb-claude-resume-archived (file)
  "Continue the session archived in org FILE.
A Claude Code session resumes in its own directory.  Anything else (an
imported claude.ai chat, or a session whose directory is gone) starts a
new session in a project you pick that reads FILE first and carries on."
  (interactive
   (list (if (and buffer-file-name
                  (file-in-directory-p buffer-file-name (kdb-claude--ensure-archive)))
             buffer-file-name
           (read-file-name "Archived session: " (kdb-claude--ensure-archive)))))
  (require 'init-claude-workflow)
  (let ((source (kdb-claude--org-keyword file "SOURCE"))
        (cwd (kdb-claude--org-keyword file "CWD")))
    (if (and source cwd (file-directory-p cwd))
        (kdb-claude--start-with (file-name-as-directory cwd) nil (file-name-base source))
      (let* ((dir (kdb-claude-read-directory "Continue in project: "))
             (ask (kdb-claude--read (format "Continue \"%s\" with (RET = recap): "
                                       (kdb-claude--org-title file)))))
        (kdb-claude--start-with
         dir
         (format "Earlier conversation, archived at %s.  Read it first, then %s"
                 (abbreviate-file-name file)
                 (if (string-empty-p ask)
                     "briefly recap where we left off and what's still open."
                   (concat "continue from it: " ask))))))))

(provide 'init-claude)
;;; init-claude.el ends here
