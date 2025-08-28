;;; org-templates.el --- Org capture templates and helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode capture templates and related functions for contact management,
;; task tracking, and note-taking.

;;; Code:

;; Simple, focused org-capture templates
(setq org-capture-templates
      '(;; === TASKS & TODOS ===
        ("t" "Task" entry (file+headline "~/.org/notes.org" "Tasks")
         "** TODO %?\n   Added: %U")
        
        ;; === MEETINGS & PEOPLE ===
        ("m" "Meeting" entry (file+function "~/.org/contacts.org" my/org-capture-meeting)
         "** Meeting - %u\n   Notes: %?\n   Action Items: ")
        
        ("f" "Follow-up" entry (file+headline "~/.org/notes.org" "Follow-ups")  
         "** FOLLOWUP %? with %^{Person}\n   SCHEDULED: %^{When}t")
        
        ;; === QUICK CAPTURE ===
        ("n" "Note" entry (file+headline "~/.org/notes.org" "Quick Notes")
         "** %u - %?")
        
        ("w" "Win" entry (file+function "~/.org/contacts.org" my/org-capture-accomplishment)
         "*** Win - %u (%?)")
        
        ("i" "Issue" entry (file+function "~/.org/contacts.org" my/org-capture-issue)
         "*** Issue - %u (%?)\n    Action Needed: ")
        
        ;; === PROJECT WORK ===  
        ("p" "Project Update" entry (file+headline "~/.org/notes.org" "Projects")
         "** %^{Project} - %u\n   %?")))

;; Helper functions for org-capture templates

(defun my/org-capture-contact-todo ()
  "Move to or create contact heading for TODO capture."
  (let ((contact (read-string "Contact name: ")))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact)) nil t)
        (end-of-line)
      ;; Create new contact
      (goto-char (point-max))
      (insert (format "\n* %s\n  Added: %s\n" contact (format-time-string "%Y-%m-%d"))))
    (point)))

(defun my/org-capture-meeting ()
  "Move to or create contact heading for meeting capture."
  (let ((contact (read-string "Contact name (or empty for general): ")))
    (if (string-empty-p contact)
        (goto-char (point-max))
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact)) nil t)
          (end-of-line)
        (goto-char (point-max))
        (insert (format "\n* %s\n  Added: %s\n" contact (format-time-string "%Y-%m-%d")))))
    (point)))

(defun my/org-capture-accomplishment ()
  "Move to contact's accomplishments section."
  (let ((contact (read-string "Contact name: ")))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact)) nil t)
        (progn
          (if (re-search-forward "^\\*\\* Accomplishments" nil t)
              (end-of-line)
            (end-of-line)
            (insert "\n** Accomplishments")))
      ;; Create new contact with accomplishments
      (goto-char (point-max))
      (insert (format "\n* %s\n  Added: %s\n** Accomplishments\n" 
                     contact (format-time-string "%Y-%m-%d"))))
    (point)))

(defun my/org-capture-issue ()
  "Move to employee's issues section."
  (let ((contact (read-string "Employee name: ")))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact)) nil t)
        (progn
          (if (re-search-forward "^\\*\\* Issues" nil t)
              (end-of-line)
            (end-of-line)
            (insert "\n** Issues")))
      ;; Create new contact with issues section
      (goto-char (point-max))
      (insert (format "\n* %s\n  Added: %s\n** Issues\n" 
                     contact (format-time-string "%Y-%m-%d"))))
    (point)))

(defun my/org-capture-review ()
  "Move to or create contact for review capture."
  (let ((contact (read-string "Employee name: ")))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s\\s-*$" (regexp-quote contact)) nil t)
        (end-of-line)
      (goto-char (point-max))
      (insert (format "\n* %s\n  Added: %s\n" contact (format-time-string "%Y-%m-%d"))))
    (point)))

(provide 'org-templates)
;;; org-templates.el ends here