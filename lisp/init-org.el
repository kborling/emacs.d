;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete org-mode setup including templates and contacts

;;; Code:

(use-package org
  ;; :ensure nil
  :config
  (setq org-ellipsis "…"
        org-use-sub-superscripts "{}"
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-directory "~/.org/"
        org-startup-indented t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t)

  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-agenda-files '("~/.org/contacts.org" "~/.org/notes.org")
        org-log-done 'time
        org-agenda-include-diary nil
        org-agenda-start-on-weekday nil
        org-agenda-span 7
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  (setq org-agenda-custom-commands
        '(("w" "Work Overview"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)))
            (tags-todo "@work"
                       ((org-agenda-overriding-header "Work TODOs")))
            (tags "PROJECT={.+}"
                  ((org-agenda-overriding-header "Active Projects")))))
          ("p" "People Focus"
           ((tags-todo "@.*:"
                       ((org-agenda-overriding-header "People-related TODOs")))
            (agenda "" ((org-agenda-span 3)
                        (org-agenda-entry-types '(:scheduled))
                        (org-agenda-overriding-header "Upcoming Meetings/Reviews")))))
          ("r" "Weekly Review"
           ((tags "LEVEL=2+Weekly Review"
                  ((org-agenda-overriding-header "Recent Reviews")))
            (todo "DONE"
                  ((org-agenda-overriding-header "This Week's Accomplishments")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\[2025-.*\\]"))))))))

  (setq org-structure-template-alist
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (restclient . t)
     (python . t)))

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (when (eq system-type 'windows-nt)
    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-modern-list '((?+ . "•") (?- . "–") (?* . "•"))
          org-modern-block-name '("▼" . "▶")
          org-modern-keyword nil
          org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐"))
          org-modern-horizontal-rule "─"
          )))

;; Load org-templates and org-contacts
(let ((org-templates-file (expand-file-name "org-templates.el" user-emacs-directory))
      (org-contacts-file (expand-file-name "org-contacts-simple.el" user-emacs-directory)))
  (when (file-exists-p org-templates-file)
    (message "Loading org-templates from: %s" org-templates-file)
    (load org-templates-file nil t))
  (when (file-exists-p org-contacts-file)
    (message "Loading org-contacts from: %s" org-contacts-file) 
    (load org-contacts-file nil t)
    
    (global-set-key (kbd "C-c o n") 'kdb/add-contact)
    (global-set-key (kbd "C-c o m") 'kdb/add-meeting-note)
    (global-set-key (kbd "C-c o A") 'kdb/add-accomplishment)
    (global-set-key (kbd "C-c o w") 'kdb/quick-win)
    (global-set-key (kbd "C-c o v") 'kdb/view-accomplishments)
    (global-set-key (kbd "C-c o d") 'kdb/daily-notes)
    (global-set-key (kbd "C-c o W") 'kdb/weekly-review)
    (global-set-key (kbd "C-c o f") 'kdb/search-all)
    (global-set-key (kbd "C-c o j") 'kdb/find-contact)
    (global-set-key (kbd "C-c o s") 'kdb/search-contacts)
    (global-set-key (kbd "C-c o C") 'kdb/open-contacts)
    (global-set-key (kbd "C-c o N") 'kdb/open-notes)))

(provide 'init-org)
;;; init-org.el ends here