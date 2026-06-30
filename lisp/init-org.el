;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete org-mode setup including templates and contacts

;;; Code:

(use-package org
  ;; :ensure nil
  :config
  (setq org-ellipsis "…"
        org-use-sub-superscripts "{}"
        org-pretty-entities (not (eq system-type 'windows-nt)) ; Disable on Windows
        org-hide-emphasis-markers (not (eq system-type 'windows-nt)) ; Disable on Windows
        org-hide-leading-stars t
        org-directory "~/.org/"
        org-startup-indented (not (eq system-type 'windows-nt)) ; Disable on Windows for speed
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-cycle-separator-lines 2)

  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil
        org-export-with-toc nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Check for encrypted version first, fall back to regular org file
  (setq org-agenda-files 
        (list (if (file-exists-p "~/.org/contacts.org.gpg")
                  "~/.org/contacts.org.gpg"
                "~/.org/contacts.org")
              "~/.org/notes.org")
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

  ;; Capture templates — single inbox, then refile
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/.org/inbox.org")
           "* TODO %?\n%U\n" :empty-lines 1)
          ("n" "Note" entry (file "~/.org/inbox.org")
           "* %?\n%U\n" :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "~/.org/journal.org")
           "* %?\n%U\n" :empty-lines 1)
          ("w" "Work note" entry (file+olp+datetree "~/.org/work.org")
           "* %?\n%U\n" :empty-lines 1)
          ("l" "Link" entry (file "~/.org/inbox.org")
           "* %?\n%U\n%a\n" :empty-lines 1)
          ("c" "Claude artifact" entry (file "~/.org/inbox.org")
           "* %?\n%U\n#+begin_src markdown\n%x\n#+end_src\n" :empty-lines 1)))

  (setq org-refile-targets '(("~/.org/notes.org" :maxlevel . 3)
                              ("~/.org/todo.org" :maxlevel . 2)
                              ("~/.org/work.org" :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)))

;; Define function outside of use-package
(defun kdb/org-wrap-source-block (lang)
  "Wrap region in org source code block with LANG."
  (interactive
   (list (completing-read "Language: "
                         '("emacs-lisp" "python" "javascript" "bash" "shell" 
                           "sql" "json" "yaml" "xml" "html" "css" "c" "cpp"
                           "java" "go" "rust" "typescript" "ruby" "php")
                         nil nil nil nil "emacs-lisp")))
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "\n#+end_src")
        (goto-char beg)
        (insert (format "#+begin_src %s\n" lang)))
    (insert (format "#+begin_src %s\n\n#+end_src" lang))
    (forward-line -1)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  ;; Set up Unicode font fallback for Windows
  (when (eq system-type 'windows-nt)
    ;; Add Segoe UI Symbol and Symbola as fallback fonts for symbols
    (set-fontset-font "fontset-default" 'unicode "Segoe UI Symbol" nil 'append)
    (set-fontset-font "fontset-default" 'unicode "Segoe UI Emoji" nil 'append)
    (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'append)

    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-modern-list '((?+ . "•") (?- . "–") (?* . "•"))
          org-modern-block-name '("▼" . "▶")
          org-modern-keyword nil
          org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐"))
          org-modern-horizontal-rule "─"))

  ;; General org-modern settings for better appearance
  (setq org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((?+ . "•") (?- . "–") (?* . "•"))
        org-modern-block-fringe 4
        org-modern-keyword t
        org-modern-timestamp t
        org-modern-todo t
        org-modern-tag t
        org-modern-priority t))


(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))

;; Deft — fast note search and creation
(use-package deft
  :ensure t
  :commands deft
  :config
  (setq deft-directory "~/.org"
        deft-recursive t
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0
        deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^---$" ;; yaml frontmatter
                "\\)"))
  :bind ("C-c o d" . deft))

;; Smart word wrapping that preserves tables
(defun kdb/org-setup-wrapping ()
  "Set up smart word wrapping for org-mode."
  (visual-line-mode 1)
  (setq-local word-wrap t
              truncate-lines nil)

  ;; Add advice to disable wrapping in tables
  (advice-add 'org-table-align :before
              (lambda (&rest _)
                (when (org-at-table-p)
                  (setq-local truncate-lines t))))

  ;; Re-enable wrapping when leaving tables
  (advice-add 'org-table-next-field :after
              (lambda (&rest _)
                (unless (org-at-table-p)
                  (setq-local truncate-lines nil)))))

(add-hook 'org-mode-hook #'kdb/org-setup-wrapping)

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
    (global-set-key (kbd "C-c o D") 'kdb/daily-notes)
    (global-set-key (kbd "C-c o W") 'kdb/weekly-review)
    (global-set-key (kbd "C-c o f") 'kdb/search-all)
    (global-set-key (kbd "C-c o j") 'kdb/find-contact)
    (global-set-key (kbd "C-c o s") 'kdb/search-contacts)
    (global-set-key (kbd "C-c o C") 'kdb/open-contacts)
    (global-set-key (kbd "C-c o N") 'kdb/open-notes)))

(defun kdb/markdown-to-org ()
  "Convert markdown buffer or region to org-mode format."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))

        ;; First handle code blocks to avoid interfering with other replacements
        (while (re-search-forward "^```\\(.*\\)$" nil t)
          (let ((lang (match-string 1)))
            (if (string-empty-p (string-trim lang))
                (replace-match "#+BEGIN_SRC")
              (replace-match (concat "#+BEGIN_SRC " (string-trim lang))))))
        (goto-char (point-min))
        (while (re-search-forward "^```$" nil t)
          (replace-match "#+END_SRC"))

        ;; Headers # -> *
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) \\(.+\\)$" nil t)
          (let ((level (length (match-string 1)))
                (title (match-string 2)))
            (replace-match (concat (make-string level ?*) " " title))))

        ;; Bold **text** -> *text* (do this before italic to avoid conflicts)
        (goto-char (point-min))
        (while (re-search-forward "\\*\\*\\([^*\n]+?\\)\\*\\*" nil t)
          (replace-match "*\\1*"))

        ;; Italic _text_ -> /text/ (use underscore version to avoid conflicts)
        (goto-char (point-min))
        (while (re-search-forward "_\\([^_\n]+?\\)_" nil t)
          (replace-match "/\\1/"))

        ;; Code `text` -> =text=
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`\n]+?\\)`" nil t)
          (replace-match "=\\1="))

        ;; Links [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
          (let ((text (match-string 1))
                (url (match-string 2)))
            (replace-match (concat "[[" url "][" text "]]"))))

        ;; Lists: convert - or + to - (org uses - or +)
        (goto-char (point-min))
        (while (re-search-forward "^\\( *\\)[+*] " nil t)
          (replace-match "\\1- "))))))


;; Org-mode specific keybinding for wrapping in source blocks
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o b") 'kdb/org-wrap-source-block))

;; Global keybindings
(global-set-key (kbd "C-c o M") 'kdb/markdown-to-org)

;; Enable markdown export and other useful exporters for office work
(with-eval-after-load 'ox
  ;; Enable markdown export
  (require 'ox-md)
  ;; Enable HTML export
  (require 'ox-html)

  ;; Add custom export options for office compatibility
  (setq org-export-with-broken-links 'mark
        org-export-with-smart-quotes t
        org-export-preserve-breaks nil
        org-export-with-section-numbers nil  ; Disable numbered headings
        org-html-validation-link nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil))

;; Markdown preview — render markdown buffers as HTML in EWW
(defun kdb/markdown-preview ()
  "Preview current markdown buffer rendered as HTML in EWW."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (html-file (make-temp-file "md-preview-" nil ".html"))
         (css "body{font-family:sans-serif;max-width:50em;margin:2em auto;padding:0 1em;line-height:1.6}
               pre{background:#f4f4f4;padding:1em;overflow-x:auto;border-radius:4px}
               code{background:#f4f4f4;padding:0.2em 0.4em;border-radius:3px}
               blockquote{border-left:3px solid #ccc;margin:1em 0;padding-left:1em;color:#555}
               h1,h2,h3{margin-top:1.5em}
               table{border-collapse:collapse}td,th{border:1px solid #ddd;padding:0.5em}"))
    (with-temp-file html-file
      (insert "<!DOCTYPE html><html><head><meta charset='utf-8'>")
      (insert (format "<style>%s</style></head><body>" css))
      ;; Use markdown-mode's export if available, otherwise basic conversion
      (if (fboundp 'markdown-export-body-to-html)
          (let ((md-buf (generate-new-buffer " *md-tmp*")))
            (with-current-buffer md-buf
              (insert content)
              (markdown-mode)
              (insert (markdown-export-body-to-html)))
            (kill-buffer md-buf))
        ;; Basic markdown to HTML conversion
        (let ((text content))
          ;; Code blocks
          (setq text (replace-regexp-in-string
                      "```\\([a-z]*\\)\n\\(\\(?:.*\n\\)*?\\)```"
                      "<pre><code class=\"\\1\">\\2</code></pre>" text))
          ;; Headers
          (setq text (replace-regexp-in-string "^### \\(.*\\)$" "<h3>\\1</h3>" text))
          (setq text (replace-regexp-in-string "^## \\(.*\\)$" "<h2>\\1</h2>" text))
          (setq text (replace-regexp-in-string "^# \\(.*\\)$" "<h1>\\1</h1>" text))
          ;; Bold/italic
          (setq text (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "<strong>\\1</strong>" text))
          (setq text (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "<em>\\1</em>" text))
          ;; Inline code
          (setq text (replace-regexp-in-string "`\\([^`]+\\)`" "<code>\\1</code>" text))
          ;; Links
          (setq text (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\([^)]+\\))" "<a href=\"\\2\">\\1</a>" text))
          ;; List items
          (setq text (replace-regexp-in-string "^- \\(.*\\)$" "<li>\\1</li>" text))
          ;; Paragraphs
          (setq text (replace-regexp-in-string "\n\n" "</p><p>" text))
          (insert "<p>" text "</p>")))
      (insert "</body></html>"))
    (eww-open-file html-file)))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-p") #'kdb/markdown-preview))

(provide 'init-org)
;;; init-org.el ends here
