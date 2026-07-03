;;; org-contacts-simple.el --- Contact & people management -*- lexical-binding: t -*-

;;; Commentary:
;; People and performance tracking.
;; Contacts in ~/.org/contacts.org (or .gpg), performance in ~/.org/people/
;; Performance files are per fiscal year: FY2026.org, FY2025.org, etc.
;;
;; Each person's FY file section looks like:
;; * Person Name
;; ** Goals
;; ** Wins
;; ** Issues
;; ** Feedback Given
;; ** 1:1 Notes
;; ** Follow-ups

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
        (insert "#+STARTUP: overview\n\n")))
    file))

;;; ============================================================
;;; Contact Helpers
;;; ============================================================

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

;;; ============================================================
;;; Contact Management
;;; ============================================================

(defun kdb/add-contact ()
  "Add a new contact."
  (interactive)
  (let ((name (read-string "Name: "))
        (role (read-string "Role/Title: "))
        (email (read-string "Email: "))
        (team (read-string "Team (optional): ")))
    (with-current-buffer (find-file-noselect kdb/contacts-file)
      (goto-char (point-max))
      (insert (format "\n* %s\n" name))
      (insert (format ":PROPERTIES:\n:ADDED: %s\n" (format-time-string "%Y-%m-%d")))
      (when (not (string-empty-p role))
        (insert (format ":ROLE: %s\n" role)))
      (when (not (string-empty-p email))
        (insert (format ":EMAIL: %s\n" email)))
      (when (not (string-empty-p team))
        (insert (format ":TEAM: %s\n" team)))
      (insert ":END:\n")
      (save-buffer)
      (message "Contact '%s' added" name))))

;;; ============================================================
;;; Performance Tracking
;;; ============================================================

(defun kdb/log-win ()
  "Log a win for someone."
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
  "Log an issue/concern for someone."
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

(defun kdb/log-1on1 ()
  "Log 1:1 meeting notes."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "1:1 Notes")
      (insert (format "*** %s\n" date))
      (insert "- Updates :: \n")
      (insert "- Discussion :: \n")
      (insert "- Action Items :: \n")
      (save-buffer)
      (switch-to-buffer (current-buffer))
      ;; Position cursor at Updates
      (re-search-backward "- Updates :: " nil t)
      (end-of-line))))

(defun kdb/log-followup ()
  "Log a follow-up item for someone with a deadline."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (item (read-string "Follow-up: "))
         (days (read-number "Follow up in how many days? " 14))
         (deadline (format-time-string "%Y-%m-%d"
                     (time-add (current-time) (days-to-time days))))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "Follow-ups")
      (insert (format "*** TODO %s\n" item))
      (insert (format "DEADLINE: <%s>\n" deadline))
      (insert (format "[%s] — %s\n" date name))
      (save-buffer)
      (message "Follow-up for %s due %s" name deadline))))

(defun kdb/set-goals ()
  "Set or update goals for a person."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (file (kdb/ensure-people-file)))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (goto-char (point-min))
      (re-search-forward (format "^\\* %s" (regexp-quote name)) nil t)
      (let ((person-end (save-excursion
                          (if (re-search-forward "^\\* " nil t)
                              (line-beginning-position)
                            (point-max)))))
        (if (re-search-forward "^\\*\\* Goals" person-end t)
            (progn (end-of-line) (insert "\n"))
          ;; Insert Goals as first subsection
          (end-of-line)
          (insert "\n** Goals\n")))
      (insert (format "- [ ] %s\n" (read-string "Goal: ")))
      (save-buffer)
      (switch-to-buffer (current-buffer)))))

;;; ============================================================
;;; View & Review
;;; ============================================================

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
         (files (when (file-directory-p kdb/people-dir)
                  (directory-files kdb/people-dir t "^FY.*\\.org$")))
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
              (let* ((start (line-beginning-position))
                     (end (or (save-excursion
                                (forward-line 1)
                                (when (re-search-forward "^\\* " nil t)
                                  (line-beginning-position)))
                              (point-max)))
                     (content (buffer-substring-no-properties start end)))
                (with-current-buffer buf
                  (insert (format "* %s\n" (file-name-base file)))
                  ;; Demote headings by one level
                  (let ((section-start (point)))
                    (insert content)
                    (save-excursion
                      (goto-char section-start)
                      (while (re-search-forward "^\\(\\*+\\) " nil t)
                        (replace-match (concat "*" (match-string 1) " ")))))
                  (insert "\n"))))))
        (goto-char (point-min))
        (org-mode)
        (setq buffer-read-only t)))
    (switch-to-buffer buf)))

(defun kdb/review-prep ()
  "Prepare a review summary for a person.
Gathers wins, issues, feedback, goals, and follow-ups from the current FY."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (file (kdb/ensure-people-file))
         (buf (get-buffer-create (format "*Review Prep: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Review Prep — %s (%s)\n" name (kdb/current-fiscal-year)))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        ;; Extract each section
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward (format "^\\* %s" (regexp-quote name)) nil t)
            (let* ((start (line-beginning-position))
                   (end (or (save-excursion
                              (forward-line 1)
                              (when (re-search-forward "^\\* " nil t)
                                (line-beginning-position)))
                            (point-max)))
                   (content (buffer-substring-no-properties start end)))
              (with-current-buffer buf
                (insert content "\n")))))
        ;; Add review template at the end
        (insert "\n* Review Notes\n")
        (insert "** Overall Assessment\n\n\n")
        (insert "** Strengths\n- \n\n")
        (insert "** Areas for Growth\n- \n\n")
        (insert "** Next Steps\n- \n\n")
        (goto-char (point-min))
        (org-mode)))
    (switch-to-buffer buf)))

(defun kdb/attention ()
  "Show what needs your attention — open follow-ups and recent issues."
  (interactive)
  (let* ((file (kdb/ensure-people-file))
         (buf (get-buffer-create "*Attention Needed*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Attention Needed — %s\n" (format-time-string "%Y-%m-%d")))
        (insert (format "#+SOURCE: %s\n\n" file))
        ;; Open follow-ups (TODOs)
        (insert "* Open Follow-ups\n\n")
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((person nil))
            (while (not (eobp))
              (cond
               ((looking-at "^\\* \\(.+\\)")
                (setq person (match-string-no-properties 1)))
               ((looking-at "^\\*\\*\\* TODO \\(.+\\)")
                (let ((todo (match-string-no-properties 1))
                      (deadline nil))
                  (save-excursion
                    (forward-line 1)
                    (when (looking-at "DEADLINE: <\\([^>]+\\)>")
                      (setq deadline (match-string-no-properties 1))))
                  (with-current-buffer buf
                    (insert (format "- *%s*: %s" person todo))
                    (when deadline (insert (format " (due %s)" deadline)))
                    (insert "\n")))))
              (forward-line 1))))
        ;; Recent high-severity issues (last 30 days)
        (insert "\n* Recent Issues (high/medium)\n\n")
        (let ((cutoff (format-time-string "%Y-%m-%d"
                        (time-subtract (current-time) (days-to-time 30)))))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((person nil))
              (while (not (eobp))
                (cond
                 ((looking-at "^\\* \\(.+\\)")
                  (setq person (match-string-no-properties 1)))
                 ((looking-at "^- \\[\\([0-9-]+\\)\\] (\\(high\\|medium\\)) \\(.+\\)")
                  (let ((date (match-string-no-properties 1))
                        (sev (match-string-no-properties 2))
                        (issue (match-string-no-properties 3)))
                    (when (string> date cutoff)
                      (with-current-buffer buf
                        (insert (format "- *%s* [%s] (%s) %s\n"
                                        person date sev issue)))))))
                (forward-line 1)))))
        (goto-char (point-min))
        (org-mode)
        (setq buffer-read-only t)))
    (switch-to-buffer buf)))

;;; ============================================================
;;; Meeting & Daily Notes
;;; ============================================================

(defun kdb/add-meeting-note ()
  "Add a meeting note for a contact to the FY people file."
  (interactive)
  (let* ((name (kdb/read-contact-name))
         (topic (read-string "Topic: "))
         (file (kdb/ensure-people-file))
         (date (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect file)
      (kdb/ensure-person-section name)
      (kdb/goto-person-subsection name "1:1 Notes")
      (insert (format "*** %s — %s\n" date topic))
      (insert "- \n")
      (save-buffer)
      (switch-to-buffer (current-buffer))
      (re-search-backward "^- " nil t)
      (end-of-line))))

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

;;; ============================================================
;;; Search
;;; ============================================================

(defun kdb/search-contacts (search-term)
  "Search for SEARCH-TERM across contacts, notes, and people files."
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

;;; ============================================================
;;; File Access
;;; ============================================================

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

;;; ============================================================
;;; Weekly Review
;;; ============================================================

(defun kdb/weekly-review ()
  "Create weekly review entry with per-person prompts."
  (interactive)
  (let ((contacts (kdb/get-all-contacts)))
    (with-current-buffer (find-file-noselect kdb/notes-file)
      (goto-char (point-max))
      (insert (format "\n* Weekly Review - %s\n" (format-time-string "%Y-W%U")))
      (insert "** Accomplishments\n- \n\n")
      (insert "** Challenges\n- \n\n")
      (insert "** Team Updates\n")
      (dolist (name contacts)
        (insert (format "- %s :: \n" name)))
      (insert "\n** Next Week Focus\n- \n\n")
      (save-buffer)
      (switch-to-buffer (current-buffer))
      (re-search-backward "^\\*\\* Accomplishments" nil t)
      (forward-line 1)
      (end-of-line))))

(provide 'org-contacts-simple)
;;; org-contacts-simple.el ends here
