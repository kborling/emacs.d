;;; org-contacts-simple.el --- Simple org-based contact management -*- lexical-binding: t -*-

;;; Commentary:
;; Complete contact management using only Org-mode features
;; Replaces Hyperbole/HyRolo with pure Org functionality

;;; Code:

;; Ensure org is loaded
(require 'org)

;; File locations
(defvar kdb/contacts-file 
  (if (file-exists-p "~/.org/contacts.org.gpg")
      "~/.org/contacts.org.gpg"
    "~/.org/contacts.org")
  "File for storing contacts, uses encrypted version if available.")

(defvar kdb/notes-file "~/.org/notes.org"
  "File for storing general notes.")

;; === CONTACT MANAGEMENT === ;;

(defun kdb/add-contact ()
  "Add a new contact."
  (interactive)
  (let ((name (read-string "Name: "))
        (email (read-string "Email: "))
        (phone (read-string "Phone: "))
        (notes (read-string "Initial note: ")))
    (condition-case err
        (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-max))
      (insert (format "\n* %s\n" name))
      (when (not (string-empty-p email))
        (insert (format "  Email: %s\n" email)))
      (when (not (string-empty-p phone))
        (insert (format "  Phone: %s\n" phone)))
      (when (not (string-empty-p notes))
        (insert (format "  Notes:\n  - %s\n" notes)))
      (insert "  Added: " (format-time-string "%Y-%m-%d") "\n")
      (save-buffer)
      (message "Contact '%s' added" name))
      (epg-error
       (if (yes-or-no-p "Failed to decrypt contacts file. Retry? ")
           (progn
             (password-cache-remove kdb/contacts-file)
             (kdb/add-contact))
         (message "Contact addition cancelled")))
      (file-error
       (message "Error accessing contacts file: %s" (error-message-string err))))))

(defun kdb/add-meeting-note ()
  "Add a meeting note for a contact."
  (interactive)
  (let* ((contact-name (read-string "Contact name: "))
         (meeting-type (completing-read "Meeting type: "
                                        '("Meeting" "Call" "Email" "Coffee" "Interview" "Follow-up")))
         (meeting-notes (read-string "Quick notes: "))
         (action-items (read-string "Action items (optional): "))
         (follow-up (read-string "Follow-up needed (optional): "))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (date-only (format-time-string "%Y-%m-%d")))
    
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact-name)) nil t)
          (progn
            (end-of-line)
            (insert (format "\n** %s - %s" meeting-type date-only))
            (insert (format "\n   Time: %s" timestamp))
            (when (not (string-empty-p meeting-notes))
              (insert (format "\n   Notes: %s" meeting-notes)))
            (when (not (string-empty-p action-items))
              (insert (format "\n   Action Items:\n   - %s" action-items)))
            (when (not (string-empty-p follow-up))
              (insert (format "\n   Follow-up: %s" follow-up)))
            (insert "\n")
            (message "Meeting note added for %s" contact-name))
        ;; Contact not found - create new
        (goto-char (point-max))
        (insert (format "\n* %s\n" contact-name))
        (insert (format "  Added: %s\n" date-only))
        (insert (format "** %s - %s\n" meeting-type date-only))
        (insert (format "   Time: %s\n" timestamp))
        (when (not (string-empty-p meeting-notes))
          (insert (format "   Notes: %s\n" meeting-notes)))
        (when (not (string-empty-p action-items))
          (insert (format "   Action Items:\n   - %s\n" action-items)))
        (when (not (string-empty-p follow-up))
          (insert (format "   Follow-up: %s\n" follow-up)))
        (message "New contact '%s' created with meeting note" contact-name))
      (save-buffer))))

;; === ACCOMPLISHMENTS & ISSUES === ;;

(defun kdb/add-accomplishment ()
  "Add an accomplishment for a contact."
  (interactive)
  (let* ((contact-name (read-string "Contact name: "))
         (accomplishment-type (completing-read "Type: " 
                                              '("Win" "Promotion" "Award" "Milestone" "Achievement" "Event" "Success")))
         (accomplishment (read-string (format "%s: " accomplishment-type)))
         (impact (read-string "Impact/Significance (optional): "))
         (date-only (format-time-string "%Y-%m-%d")))
    
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact-name)) nil t)
          (let ((contact-end (save-excursion
                              (if (re-search-forward "^\\* " nil t)
                                  (line-beginning-position)
                                (point-max)))))
            (if (re-search-forward "^\\*\\* Accomplishments\\s-*$" contact-end t)
                (progn
                  (end-of-line)
                  (insert (format "\n*** %s - %s (%s)" accomplishment-type date-only accomplishment))
                  (when (not (string-empty-p impact))
                    (insert (format "\n    Impact: %s" impact))))
              (goto-char contact-end)
              (when (not (eq contact-end (point-max)))
                (forward-line -1))
              (end-of-line)
              (insert (format "\n** Accomplishments"))
              (insert (format "\n*** %s - %s (%s)" accomplishment-type date-only accomplishment))
              (when (not (string-empty-p impact))
                (insert (format "\n    Impact: %s" impact))))
            (message "Accomplishment added for %s" contact-name))
        ;; Create new contact
        (goto-char (point-max))
        (insert (format "\n* %s\n" contact-name))
        (insert (format "  Added: %s\n" date-only))
        (insert (format "** Accomplishments\n"))
        (insert (format "*** %s - %s (%s)\n" accomplishment-type date-only accomplishment))
        (when (not (string-empty-p impact))
          (insert (format "    Impact: %s\n" impact)))
        (message "New contact '%s' created with accomplishment" contact-name))
      (save-buffer))))

(defun kdb/quick-win ()
  "Quickly log a win for a contact."
  (interactive)
  (let* ((contact-name (read-string "Contact: "))
         (win (read-string "Quick win: "))
         (date-only (format-time-string "%Y-%m-%d")))
    
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact-name)) nil t)
          (let ((contact-end (save-excursion
                              (if (re-search-forward "^\\* " nil t)
                                  (line-beginning-position)
                                (point-max)))))
            (if (re-search-forward "^\\*\\* Accomplishments\\s-*$" contact-end t)
                (progn
                  (end-of-line)
                  (insert (format "\n*** Win - %s (%s)" date-only win)))
              (goto-char contact-end)
              (when (not (eq contact-end (point-max)))
                (forward-line -1))
              (end-of-line)
              (insert (format "\n** Accomplishments"))
              (insert (format "\n*** Win - %s (%s)" date-only win)))
            (message "✓ Win logged for %s" contact-name))
        (goto-char (point-max))
        (insert (format "\n* %s\n" contact-name))
        (insert (format "  Added: %s\n" date-only))
        (insert (format "** Accomplishments\n"))
        (insert (format "*** Win - %s (%s)\n" date-only win))
        (message "✓ New contact '%s' created with win" contact-name))
      (save-buffer))))

(defun kdb/view-accomplishments ()
  "View accomplishments for a contact."
  (interactive)
  (let* ((contact-name (read-string "View accomplishments for: "))
         (found nil))
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward (format "* %s" contact-name) nil t)
          (setq found t)
          (if (search-forward "** Accomplishments" nil t)
              (progn
                (beginning-of-line)
                (let* ((acc-start (point))
                       (acc-end (or (save-excursion
                                    (when (re-search-forward "^\\*\\* " nil t)
                                      (beginning-of-line)
                                      (point)))
                                  (save-excursion
                                    (when (re-search-forward "^\\* " nil t)
                                      (beginning-of-line)
                                      (point)))
                                  (point-max)))
                       (content (buffer-substring-no-properties acc-start acc-end)))
                  (with-current-buffer (get-buffer-create "*Accomplishments*")
                    (read-only-mode -1)
                    (erase-buffer)
                    (insert (format "Accomplishments for %s:\n\n" contact-name))
                    (insert content)
                    (org-mode)
                    (goto-char (point-min))
                    (read-only-mode 1)
                    (pop-to-buffer (current-buffer)))))
            (message "No accomplishments found for %s" contact-name)))))
    (unless found
      (message "Contact '%s' not found" contact-name))))

;; === DAILY NOTES === ;;

(defun kdb/daily-notes ()
  "Add quick daily notes to notes.org."
  (interactive)
  (let* ((note-type (completing-read "Note type: "
                                     '("Daily Standup" "Team Meeting" "1-on-1" "Project Update" "Quick Note")))
         (notes (read-string "Notes: "))
         (date-only (format-time-string "%Y-%m-%d"))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))
    
    (with-current-buffer (find-file-noselect kdb/notes-file)
      (goto-char (point-max))
      (insert (format "\n* %s - %s\n" note-type date-only))
      (insert (format "  Time: %s\n" timestamp))
      (insert (format "  Notes: %s\n" notes))
      (save-buffer)
      (message "Daily note added: %s" note-type))))

;; === SEARCH & NAVIGATION === ;;

(defun kdb/search-contacts (search-term)
  "Search for SEARCH-TERM in contacts and notes."
  (interactive "sSearch: ")
  (condition-case err
      (let ((files (list kdb/contacts-file kdb/notes-file)))
        (if (fboundp 'multi-occur)
            (multi-occur
             (mapcar #'find-file-noselect files)
             search-term)
          ;; Fallback to basic occur in contacts file
          (with-current-buffer (find-file-noselect kdb/contacts-file)
            (occur search-term)
            (switch-to-buffer (current-buffer)))))
    (error
     (message "Search error: %s" (error-message-string err)))))

(defun kdb/get-all-contacts ()
  "Get all contact names from the contacts file."
  (let ((contacts '()))
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\(.+\\)" nil t)
          (push (match-string 1) contacts))))
    (nreverse contacts)))

(defun kdb/find-contact ()
  "Jump to a contact using completion."
  (interactive)
  (let ((contacts 
         (condition-case nil
             ;; Try org-map-entries if available
             (with-current-buffer (find-file-noselect kdb/contacts-file)
               (if (fboundp 'org-map-entries)
                   (org-map-entries
                    (lambda () (nth 4 (org-heading-components)))
                    "LEVEL=1"
                    (list kdb/contacts-file))
                 (kdb/get-all-contacts)))
           ;; Fallback on any error
           (error (kdb/get-all-contacts)))))
    (when contacts
      (let ((contact (completing-read "Jump to contact: " contacts nil t)))
        (with-current-buffer (find-file-noselect kdb/contacts-file)
          (goto-char (point-min))
          (search-forward (format "* %s" contact))
          (switch-to-buffer (current-buffer))
          (when (fboundp 'org-show-context)
            (org-show-context))
          (recenter-top-bottom))))))

;; Better search with deadgrep if available
(defun kdb/search-all ()
  "Search contacts and notes with deadgrep or occur."
  (interactive)
  (if (fboundp 'deadgrep)
      (let ((search-term (read-string "Search all: ")))
        (deadgrep search-term (file-name-directory kdb/contacts-file)))
    (call-interactively 'kdb/search-contacts)))

;; === FILE ACCESS === ;;

(defun kdb/open-contacts ()
  "Open contacts file with decryption retry support."
  (interactive)
  (condition-case err
      (find-file kdb/contacts-file)
    (epg-error
     (if (yes-or-no-p "Failed to decrypt contacts file. Retry? ")
         (progn
           (password-cache-remove kdb/contacts-file)
           (find-file kdb/contacts-file))
       (message "Cancelled opening contacts file")))
    (file-error
     (message "Error opening contacts file: %s" (error-message-string err)))))

(defun kdb/open-notes ()
  "Open notes file."
  (interactive)
  (find-file kdb/notes-file))

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
    (insert "** Team/People Updates\n- \n\n")
    (save-buffer)
    (switch-to-buffer (current-buffer))
    (forward-line -7)
    (end-of-line)))

(provide 'org-contacts-simple)
;;; org-contacts-simple.el ends here
