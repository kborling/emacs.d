;;; init-claude.el --- Browse Claude Desktop conversation exports -*- lexical-binding: t; -*-

;; Author: Kevin Borling
;; Keywords: claude ai conversations

;;; Commentary:

;; Browse exported Claude Desktop conversations in Emacs.
;; Export your data from Claude Desktop: Settings > Privacy > Export Data
;; Set `kdb-claude-export-directory' to the extracted export folder.

;;; Code:

(require 'json)

(defcustom kdb-claude-export-directory nil
  "Path to extracted Claude data export directory.
Export from Claude Desktop via Settings > Privacy > Export Data.
Set this in personal.el."
  :type '(choice (const nil) directory)
  :group 'claude)

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
      ;; Search one level deep for the file
      (car (directory-files-recursively dir "^conversations\\.json$" nil
                                        (lambda (d) (< (length (split-string (file-relative-name d dir) "/")) 3)))))))

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
  "Extract text from message CONTENT which may be a string or structured data."
  (cond
   ((stringp content) content)
   ((and (listp content) (listp (car content)))
    ;; List of content blocks
    (mapconcat
     (lambda (block)
       (let ((type (alist-get 'type block)))
         (cond
          ((equal type "text") (or (alist-get 'text block) ""))
          ((equal type "tool_use")
           (format "[Tool: %s]\n%s"
                   (or (alist-get 'name block) "unknown")
                   (or (alist-get 'input block) "")))
          ((equal type "tool_result")
           (format "[Tool Result]\n%s" (or (alist-get 'content block) "")))
          (t (format "[%s]" (or type "unknown"))))))
     content "\n"))
   (t (format "%s" content))))

;;;###autoload
(defun kdb-claude-browse ()
  "Browse Claude Desktop conversation exports."
  (interactive)
  (let* ((dir (kdb-claude--find-export-dir))
         (conversations (kdb-claude--parse-conversations dir))
         ;; Sort by updated_at descending
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
      (kdb-claude--display-conversation conv))))

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

        (if chat_messages
            (dolist (msg chat-messages)
              (let* ((sender (or (alist-get 'sender msg) "unknown"))
                     (text (kdb-claude--extract-text
                            (or (alist-get 'content msg)
                                (alist-get 'text msg) "")))
                     (heading (if (equal sender "human") "You" "Claude")))
                (insert (format "* %s\n\n%s\n\n" heading text))))
          (insert "No messages found in this conversation.\n")))

      (goto-char (point-min))
      (org-mode)
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun kdb-claude-search (query)
  "Search Claude conversations for QUERY."
  (interactive "sSearch conversations: ")
  (let* ((dir (kdb-claude--find-export-dir))
         (conversations (kdb-claude--parse-conversations dir))
         (matches '()))
    (dolist (conv conversations)
      (let ((title (kdb-claude--conversation-title conv))
            (found nil))
        ;; Search title
        (when (string-match-p (regexp-quote query) title)
          (setq found t))
        ;; Search messages
        (unless found
          (dolist (msg (alist-get 'chat_messages conv))
            (let ((text (kdb-claude--extract-text
                         (or (alist-get 'content msg)
                             (alist-get 'text msg) ""))))
              (when (string-match-p (regexp-quote query) text)
                (setq found t)))))
        (when found
          (push conv matches))))
    (if matches
        (let* ((candidates (mapcar
                            (lambda (conv)
                              (cons (format "%-20s  %s"
                                            (kdb-claude--format-date (alist-get 'updated_at conv))
                                            (kdb-claude--conversation-title conv))
                                    conv))
                            (nreverse matches)))
               (choice (completing-read
                        (format "Matches for \"%s\" (%d): " query (length matches))
                        candidates nil t))
               (conv (cdr (assoc choice candidates))))
          (when conv
            (kdb-claude--display-conversation conv)))
      (message "No conversations matching \"%s\"" query))))

;;;###autoload
(defun kdb-claude-import-export (zip-or-dir)
  "Import a Claude data export from ZIP-OR-DIR.
If a zip file, extract it first. Sets `kdb-claude-export-directory'."
  (interactive "fClaude export (zip or directory): ")
  (let ((path (expand-file-name zip-or-dir)))
    (if (file-directory-p path)
        (setq kdb-claude-export-directory path)
      ;; Extract zip
      (let ((dest (expand-file-name "claude-export"
                                     (file-name-directory path))))
        (make-directory dest t)
        (call-process "unzip" nil nil nil "-o" path "-d" dest)
        (setq kdb-claude-export-directory dest)))
    (message "Claude export directory set to: %s" kdb-claude-export-directory)))

(provide 'init-claude)
;;; init-claude.el ends here
