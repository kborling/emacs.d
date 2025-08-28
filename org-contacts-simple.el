;;; org-contacts-simple.el --- Simple org-based contact management -*- lexical-binding: t -*-

;;; Commentary:
;; Complete contact management using only Org-mode features
;; Replaces Hyperbole/HyRolo with pure Org functionality

;;; Code:

;; File locations
(defvar my/contacts-file "~/.org/contacts.org"
  "File for storing contacts.")

(defvar my/notes-file "~/.org/notes.org"
  "File for storing general notes.")

;; === CONTACT MANAGEMENT === ;;

(defun my/add-contact ()
  "Add a new contact."
  (interactive)
  (let ((name (read-string "Name: "))
        (email (read-string "Email: "))
        (phone (read-string "Phone: "))
        (notes (read-string "Initial note: ")))
    (with-current-buffer (find-file-noselect my/contacts-file)
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
      (message "Contact '%s' added" name))))

(defun my/add-meeting-note ()
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
    
    (with-current-buffer (find-file-noselect my/contacts-file)
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

(defun my/add-accomplishment ()
  "Add an accomplishment for a contact."
  (interactive)
  (let* ((contact-name (read-string "Contact name: "))
         (accomplishment-type (completing-read "Type: " 
                                              '("Win" "Promotion" "Award" "Milestone" "Achievement" "Event" "Success")))
         (accomplishment (read-string (format "%s: " accomplishment-type)))
         (impact (read-string "Impact/Significance (optional): "))
         (date-only (format-time-string "%Y-%m-%d")))
    
    (with-current-buffer (find-file-noselect my/contacts-file)
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

(defun my/quick-win ()
  "Quickly log a win for a contact."
  (interactive)
  (let* ((contact-name (read-string "Contact: "))
         (win (read-string "Quick win: "))
         (date-only (format-time-string "%Y-%m-%d")))
    
    (with-current-buffer (find-file-noselect my/contacts-file)
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

(defun my/view-accomplishments ()
  "View accomplishments for a contact."
  (interactive)
  (let* ((contact-name (read-string "View accomplishments for: "))
         (found nil))
    (with-current-buffer (find-file-noselect my/contacts-file)
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

(defun my/daily-notes ()
  "Add quick daily notes to notes.org."
  (interactive)
  (let* ((note-type (completing-read "Note type: "
                                     '("Daily Standup" "Team Meeting" "1-on-1" "Project Update" "Quick Note")))
         (notes (read-string "Notes: "))
         (date-only (format-time-string "%Y-%m-%d"))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))
    
    (with-current-buffer (find-file-noselect my/notes-file)
      (goto-char (point-max))
      (insert (format "\n* %s - %s\n" note-type date-only))
      (insert (format "  Time: %s\n" timestamp))
      (insert (format "  Notes: %s\n" notes))
      (save-buffer)
      (message "Daily note added: %s" note-type))))

;; === SEARCH & NAVIGATION === ;;

(defun my/search-contacts (search-term)
  "Search for SEARCH-TERM in contacts and notes."
  (interactive "sSearch: ")
  (let ((files (list my/contacts-file my/notes-file)))
    (multi-occur
     (mapcar #'find-file-noselect files)
     search-term)))

(defun my/find-contact ()
  "Jump to a contact using completion."
  (interactive)
  (let ((contacts (org-map-entries
                   (lambda () (nth 4 (org-heading-components)))
                   "LEVEL=1"
                   (list my/contacts-file))))
    (when contacts
      (let ((contact (completing-read "Jump to contact: " contacts nil t)))
        (with-current-buffer (find-file-noselect my/contacts-file)
          (goto-char (point-min))
          (search-forward (format "* %s" contact))
          (switch-to-buffer (current-buffer))
          (org-show-context)
          (recenter-top-bottom))))))

;; Better search with deadgrep if available
(defun my/search-all ()
  "Search contacts and notes with deadgrep or occur."
  (interactive)
  (if (fboundp 'deadgrep)
      (let ((search-term (read-string "Search all: ")))
        (deadgrep search-term (file-name-directory my/contacts-file)))
    (call-interactively 'my/search-contacts)))

;; === FILE ACCESS === ;;

(defun my/open-contacts ()
  "Open contacts file."
  (interactive)
  (find-file my/contacts-file))

(defun my/open-notes ()
  "Open notes file."
  (interactive)
  (find-file my/notes-file))

;; === WEEKLY REVIEW === ;;

(defun my/weekly-review ()
  "Create weekly review entry."
  (interactive)
  (with-current-buffer (find-file-noselect my/notes-file)
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

;; === KEYBINDINGS === ;;
;; Organized keybindings that replace HyRolo

(with-eval-after-load 'org
  ;; Contact management
  (global-set-key (kbd "C-c r a") 'my/add-contact)
  (global-set-key (kbd "C-c r m") 'my/add-meeting-note)
  (global-set-key (kbd "C-c r A") 'my/add-accomplishment)
  (global-set-key (kbd "C-c r w") 'my/quick-win)
  (global-set-key (kbd "C-c r v") 'my/view-accomplishments)
  
  ;; Daily workflow
  (global-set-key (kbd "C-c r d") 'my/daily-notes)
  (global-set-key (kbd "C-c r W") 'my/weekly-review)
  
  ;; Search and navigation
  (global-set-key (kbd "C-c r f") 'my/search-all)
  (global-set-key (kbd "C-c r j") 'my/find-contact)
  (global-set-key (kbd "C-c r s") 'my/search-contacts)
  
  ;; Quick file access
  (global-set-key (kbd "C-c r c") 'my/open-contacts)
  (global-set-key (kbd "C-c r o") 'my/open-notes)
  
  ;; Already defined in org config:
  ;; C-c c - org-capture
  ;; C-c a - org-agenda
  )

(provide 'org-contacts-simple)
;;; org-contacts-simple.el ends here
