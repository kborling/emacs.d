;;; org-contacts-simple.el --- Contact & people management -*- lexical-binding: t -*-

;;; Commentary:
;; People management for managers/directors.
;; Contacts in ~/.org/contacts.org (or .gpg), performance in ~/.org/people/
;; Performance files are per fiscal year: FY2026.org, FY2025.org, etc.

;;; Code:

(require 'org)

;; File locations
(defvar kdb/contacts-file
  (if (file-exists-p "~/.org/contacts.org.gpg")
      "~/.org/contacts.org.gpg"
    "~/.org/contacts.org")
  "File for storing contacts.")

(defvar kdb/notes-file "~/.org/notes.org"
  "File for storing general notes.")

(defvar kdb/people-dir "~/.org/people/"
  "Directory for people/performance tracking files.")

(defvar kdb/fiscal-year-start-month 1
  "Month the fiscal year starts (1=Jan, 7=Jul, 10=Oct). Set in personal.el.")

(defun kdb/current-fiscal-year ()
  "Return the current fiscal year string like FY2026."
  (let* ((month (string-to-number (format-time-string "%m")))
         (year (string-to-number (format-time-string "%Y")))
         (fy (if (>= month kdb/fiscal-year-start-month) year (1- year))))
    (format "FY%d" fy)))

(defun kdb/people-file (&optional fy)
  "Return the performance file for fiscal year FY."
  (let ((year (or fy (kdb/current-fiscal-year))))
    (expand-file-name (concat year ".org") kdb/people-dir)))

(defun kdb/ensure-people-file ()
  "Ensure the current FY people file exists."
  (let ((file (kdb/people-file)))
    (unless (file-exists-p kdb/people-dir)
      (make-directory kdb/people-dir t))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (format "#+TITLE: People - %s\n" (kdb/current-fiscal-year)))
        (insert "#+COLUMNS: %25ITEM %TAGS\n")
        (insert "#+STARTUP: overview\n\n")))
    file))

;; === CONTACT HELPERS === ;;

(defun kdb/get-all-contacts ()
  "Get all contact names from the contacts file."
  (let ((contacts '()))
    (condition-case nil
        (with-current-buffer (find-file-noselect kdb/contacts-file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^\\* \\(.+\\)" nil t)
              (push (match-string-no-properties 1) contacts))))
      (error nil))
    (nreverse contacts)))

(defun kdb/read-contact-name ()
  "Read a contact name with completion from existing contacts."
  (let ((contacts (kdb/get-all-contacts)))
    (completing-read "Person: " contacts nil nil)))

;; === CONTACT MANAGEMENT === ;;

(defun kdb/add-contact ()
  "Add a new contact."
  (interactive)
  (let ((name (read-string "Name: "))
        (role (read-string "Role/Title: "))
        (email (read-string "Email: "))
        (phone (read-string "Phone: ")))
    (condition-case err
        (with-current-buffer (find-file-noselect kdb/contacts-file)
          (goto-char (point-max))
          (insert (format "\n* %s\n" name))
          (insert (format ":PROPERTIES:\n:ADDED: %s\n" (format-time-string "%Y-%m-%d")))
          (when (not (string-empty-p role))
            (insert (format ":ROLE: %s\n" role)))
          (when (not (string-empty-p email))
            (insert (format ":EMAIL: %s\n" email)))
          (when (not (string-empty-p phone))
            (insert (format ":PHONE: %s\n" phone)))
          (insert ":END:\n")
          (save-buffer)
          (message "Contact '%s' added" name))
      (error (message "Error: %s" (error-message-string err))))))

;; === PERFORMANCE TRACKING === ;;

(defun kdb/log-win ()
  "Log a win for someone. Stored in the current FY file."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (win (read-string "Win: "))
         (impact (read-string "Impact (optional): "))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "Wins")
      (insert (format "- [%s] %s" date win))
      (when (not (string-empty-p impact))
        (insert (format " — %s" impact)))
      (insert "\n")
      (save-buffer)
      (message "Win logged for %s" name))))

(defun kdb/log-issue ()
  "Log an issue/concern for someone. Stored in the current FY file."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (issue (read-string "Issue: "))
         (severity (completing-read "Severity: " '("low" "medium" "high") nil t))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "Issues")
      (insert (format "- [%s] (%s) %s\n" date severity issue))
      (save-buffer)
      (message "Issue logged for %s" name))))

(defun kdb/log-feedback ()
  "Log feedback given to someone."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (type (completing-read "Type: " '("praise" "coaching" "corrective") nil t))
         (feedback (read-string "Feedback: "))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "Feedback Given")
      (insert (format "- [%s] (%s) %s\n" date type feedback))
      (save-buffer)
      (message "Feedback logged for %s" name))))

(defun kdb/view-person ()
  "View all performance data for a person in current FY."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (file (kdb/ensure-people-file)))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s" (regexp-quote name)) nil t)
        (progn (org-show-subtree) (recenter-top-bottom))
      (message "No performance data for %s in %s" name (kdb/current-fiscal-year)))))

(defun kdb/view-person-all-years ()
  "View performance data for a person across all fiscal years."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (files (directory-files kdb/people-dir t "^FY.*\\.org$"))
         (buf (get-buffer-create (format "*%s - All Years*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s - Performance History\n\n" name))
        (dolist (file (reverse files))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward (format "^\\* %s" (regexp-quote name)) nil t)
              (let ((start (line-beginning-position))
                    (end (or (save-excursion
                               (when (re-search-forward "^\\* " nil t)
                                 (line-beginning-position)))
                             (point-max))))
                (with-current-buffer buf
                  (insert (format "* %s\n" (file-name-base file)))
                  (insert (substring
                           (with-temp-buffer
                             (insert-file-contents file nil (1- start) (1- end))
                             (buffer-string))
                           0))
                  (insert "\n"))))))
        (goto-char (point-min))
        (org-mode)
        (setq buffer-read-only t)))
    (switch-to-buffer buf)))

;; === HELPER FUNCTIONS === ;;

(defun kdb/ensure-person-section (name)
  "Ensure NAME has a top-level heading in current buffer."
  (goto-char (point-min))
  (unless (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote name)) nil t)
    (goto-char (point-max))
    (insert (format "\n* %s\n" name))))

(defun kdb/goto-person-subsection (name section)
  "Navigate to SECTION under person NAME, creating if needed."
  (goto-char (point-min))
  (re-search-forward (format "^\\* %s" (regexp-quote name)) nil t)
  (let ((person-end (save-excursion
                      (if (re-search-forward "^\\* " nil t)
                          (line-beginning-position)
                        (point-max)))))
    (if (re-search-forward (format "^\\*\\* %s" (regexp-quote section)) person-end t)
        (progn (end-of-line) (insert "\n"))
      ;; Create subsection at end of person
      (goto-char person-end)
      (insert (format "** %s\n" section)))))

;; === MEETING & DAILY NOTES === ;;

(defun kdb/add-meeting-note ()
  "Add a meeting note for a contact."
  (interactive)
  (let* ((contact-name (kdb/read-contact-name))
         (meeting-type (completing-read "Type: "
                                        '("Meeting" "Call" "Email" "Coffee" "Interview" "Follow-up")))
         (notes (read-string "Notes: "))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s" (regexp-quote contact-name)) nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\* " nil t)
                           (line-beginning-position) (point-max)))))
            (goto-char end)
            (insert (format "** %s - %s\n   %s\n" meeting-type date notes)))
        (goto-char (point-max))
        (insert (format "\n* %s\n** %s - %s\n   %s\n" contact-name meeting-type date notes)))
      (save-buffer)
      (message "Meeting note added for %s" contact-name))))

(defun kdb/daily-notes ()
  "Add quick daily notes to notes.org."
  (interactive)
  (let* ((note-type (completing-read "Type: "
                                     '("Standup" "Team Meeting" "1-on-1" "Project Update" "Note")))
         (notes (read-string "Notes: "))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect kdb/notes-file)
      (goto-char (point-max))
      (insert (format "\n* %s - %s\n  %s\n" note-type date notes))
      (save-buffer)
      (message "Note added: %s" note-type))))

;; === SEARCH === ;;

(defun kdb/search-contacts (search-term)
  "Search for SEARCH-TERM across contacts and notes."
  (interactive "sSearch: ")
  (let ((files (list kdb/contacts-file kdb/notes-file)))
    (when (file-directory-p kdb/people-dir)
      (setq files (append files (directory-files kdb/people-dir t "\\.org$"))))
    (multi-occur (mapcar #'find-file-noselect files) search-term)))

(defun kdb/find-contact ()
  "Jump to a contact using completion."
  (interactive)
  (let* ((contacts (kdb/get-all-contacts))
         (contact (completing-read "Contact: " contacts nil t)))
    (find-file kdb/contacts-file)
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s" (regexp-quote contact)) nil t)
    (org-show-subtree)
    (recenter-top-bottom)))

(defun kdb/search-all ()
  "Search contacts, notes, and people files."
  (interactive)
  (if (fboundp 'deadgrep)
      (deadgrep (read-string "Search all: ") "~/.org")
    (call-interactively 'kdb/search-contacts)))

;; === FILE ACCESS === ;;

(defun kdb/open-contacts ()
  "Open contacts file."
  (interactive)
  (find-file kdb/contacts-file))

(defun kdb/open-notes ()
  "Open notes file."
  (interactive)
  (find-file kdb/notes-file))

(defun kdb/open-people ()
  "Open current FY people file."
  (interactive)
  (find-file (kdb/ensure-people-file)))

;; === WEEKLY REVIEW === ;;

(defun kdb/weekly-review ()
  "Create weekly review entry."
  (interactive)
  (with-current-buffer (find-file-noselect kdb/notes-file)
    (goto-char (point-max))
    (insert (format "\n* Weekly Review - %s\n" (format-time-string "%Y-W%U")))
    (insert "** Accomplishments\n- \n\n")
    (insert "** Challenges\n- \n\n")
    (insert "** Next Week Focus\n- \n\n")
    (insert "** Team Updates\n- \n\n")
    (save-buffer)
    (switch-to-buffer (current-buffer))
    (forward-line -7)
    (end-of-line)))

;; === QUICK WIN (kept for backward compat) === ;;
(defalias 'kdb/quick-win 'kdb/log-win)
(defalias 'kdb/add-accomplishment 'kdb/log-win)
(defalias 'kdb/view-accomplishments 'kdb/view-person)

(provide 'org-contacts-simple)
;;; org-contacts-simple.el ends here
