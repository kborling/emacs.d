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
        (list "~/.org/inbox.org"
              "~/.org/todo.org"
              "~/.org/notes.org"
              "~/.org/projects.org"
              "~/.org/work.org")
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
                    `(org-agenda-skip-entry-if 'notregexp ,(format "\\[%s-.*\\]" (format-time-string "%Y"))))))))))

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
     (python . t)
     (sql . t)))

  ;; Capture templates
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
           "* %?\n%U\n#+begin_src markdown\n%x\n#+end_src\n" :empty-lines 1)
          ;; Management
          ("m" "Meeting" entry (file+olp+datetree "~/.org/work.org")
           "* %? :meeting:\n%U\n** Attendees\n- \n** Notes\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("1" "1:1" entry (file+olp+datetree "~/.org/work.org")
           "* 1:1 with %? :1on1:\n%U\n** Updates\n\n** Discussion\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("p" "Project" entry (file+olp "~/.org/projects.org" "Active")
           "* %?\n:PROPERTIES:\n:STATUS: planning\n:OWNER: \n:END:\n%U\n** Overview\n\n** Requirements\n\n** Tasks\n- [ ] \n" :empty-lines 1)
          ("d" "Decision" entry (file "~/.org/decisions.org")
           "* %?\n%U\n** Context\n\n** Options Considered\n1. \n2. \n\n** Decision\n\n** Rationale\n" :empty-lines 1)
          ("s" "Spec/Doc" entry (file "~/.org/inbox.org")
           "* %?\n%U\n** Purpose\n\n** Requirements\n\n** Design\n\n** Implementation\n\n** Open Questions\n- \n" :empty-lines 1)
          ("q" "Queue for Claude" entry (file "~/.org/inbox.org")
           "* %? :claude:\n%U\n** Source\n\n** Thoughts\n\n** Next Actions\n- [ ] \n** Prompt\n" :empty-lines 1)))

  (setq org-refile-targets '(("~/.org/notes.org" :maxlevel . 3)
                              ("~/.org/todo.org" :maxlevel . 2)
                              ("~/.org/work.org" :maxlevel . 2)
                              ("~/.org/projects.org" :maxlevel . 2))
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
  (when (eq system-type 'windows-nt)
    (set-fontset-font "fontset-default" 'unicode "Segoe UI Symbol" nil 'append)
    (set-fontset-font "fontset-default" 'unicode "Segoe UI Emoji" nil 'append)
    (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji" nil 'append)
    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-modern-block-name '("▼" . "▶")
          org-modern-keyword nil
          org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐"))
          org-modern-horizontal-rule "─"))

  (setq org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((?+ . "•") (?- . "–") (?* . "•"))
        org-modern-block-fringe 4))


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
              truncate-lines nil))

(add-hook 'org-mode-hook #'kdb/org-setup-wrapping)

;; Disable wrapping in tables (advice added once, not per-buffer)
(advice-add 'org-table-align :before
            (lambda (&rest _)
              (when (org-at-table-p)
                (setq-local truncate-lines t))))
(advice-add 'org-table-next-field :after
            (lambda (&rest _)
              (unless (org-at-table-p)
                (setq-local truncate-lines nil))))

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
    (global-set-key (kbd "C-c o w") 'kdb/log-win)
    (global-set-key (kbd "C-c o i") 'kdb/log-issue)
    (global-set-key (kbd "C-c o F") 'kdb/log-feedback)
    (global-set-key (kbd "C-c o v") 'kdb/view-person)
    (global-set-key (kbd "C-c o V") 'kdb/view-person-all-years)
    (global-set-key (kbd "C-c o D") 'kdb/daily-notes)
    (global-set-key (kbd "C-c o W") 'kdb/weekly-review)
    (global-set-key (kbd "C-c o f") 'kdb/search-all)
    (global-set-key (kbd "C-c o j") 'kdb/find-contact)
    (global-set-key (kbd "C-c o s") 'kdb/search-contacts)
    (global-set-key (kbd "C-c o C") 'kdb/open-contacts)
    (global-set-key (kbd "C-c o N") 'kdb/open-notes)
    (global-set-key (kbd "C-c o p") 'kdb/open-people)))

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

;; Screenshots and image paste
(setq org-yank-image-save-method "~/.org/images/"
      org-yank-image-file-name-function 'org-yank-image-autogen-filename)

(defun kdb/org-screenshot ()
  "Take a screenshot and insert into current org buffer.
macOS: interactive region select. Linux: scrot/maim. Windows: snippingtool."
  (interactive)
  (let* ((dir (expand-file-name "~/.org/images/"))
         (filename (format "screenshot_%s.png" (format-time-string "%Y%m%d_%H%M%S")))
         (filepath (expand-file-name filename dir)))
    (make-directory dir t)
    (cond
     ((eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filepath))
     ((executable-find "maim")
      (call-process "maim" nil nil nil "-s" filepath))
     ((executable-find "scrot")
      (call-process "scrot" nil nil nil "-s" filepath))
     ((eq system-type 'windows-nt)
      (shell-command (format "snippingtool /clip"))
      ;; On Windows, fall back to yank-media after clipboard capture
      (message "Use yank-media (C-c o y) to paste from clipboard")
      (setq filepath nil))
     (t (user-error "No screenshot tool found")))
    (when (and filepath (file-exists-p filepath))
      (insert (format "[[file:%s]]\n" filepath))
      (org-display-inline-images nil t)
      (message "Screenshot inserted"))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o S") #'kdb/org-screenshot)
  (define-key org-mode-map (kbd "C-c o y") #'yank-media))

;; Show images inline by default
(setq org-startup-with-inline-images t)

;; Global keybindings
(global-set-key (kbd "C-c o M") 'kdb/markdown-to-org)
(global-set-key (kbd "C-c o x") 'kdb/export-deliverable)
(global-set-key (kbd "C-c o P") 'kdb/pandoc-convert)

;; Export backends
(with-eval-after-load 'ox
  (require 'ox-md)
  (require 'ox-html)
  (require 'ox-odt)

  (setq org-export-with-broken-links 'mark
        org-export-with-smart-quotes t
        org-export-preserve-breaks nil
        org-export-with-section-numbers nil
        org-html-validation-link nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil))

;; CSV support
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "\t")))

;; Excel (.xlsx) support — view and export
(defun kdb/xlsx-to-csv (file)
  "Convert FILE (.xlsx) to CSV and open in csv-mode."
  (interactive "fOpen Excel file: ")
  (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
         (csv-file (make-temp-file base nil ".csv"))
         (converted nil))
    ;; Try available converters in order
    (cond
     ((executable-find "ssconvert")
      (call-process "ssconvert" nil nil nil file csv-file)
      (setq converted t))
     ((executable-find "xlsx2csv")
      (call-process "xlsx2csv" nil nil nil file csv-file)
      (setq converted t))
     ((executable-find "libreoffice")
      (let ((tmp-dir (make-temp-file "lo-" t)))
        (call-process "libreoffice" nil nil nil
                      "--headless" "--convert-to" "csv"
                      "--outdir" tmp-dir file)
        (let ((lo-csv (expand-file-name (concat base ".csv") tmp-dir)))
          (when (file-exists-p lo-csv)
            (rename-file lo-csv csv-file t)
            (setq converted t)))))
     ((executable-find "python3")
      (call-process "python3" nil nil nil "-c"
                    (format "import csv,openpyxl;wb=openpyxl.load_workbook('%s');ws=wb.active;w=csv.writer(open('%s','w'));[w.writerow([c.value for c in r]) for r in ws.iter_rows()]"
                            (expand-file-name file) csv-file))
      (setq converted t)))
    (if converted
        (progn
          (find-file csv-file)
          (csv-mode)
          (csv-align-fields nil (point-min) (point-max))
          (setq-local buffer-read-only t)
          (message "Opened %s (converted to CSV)" (file-name-nondirectory file)))
      (message "No xlsx converter found. Install ssconvert, xlsx2csv, libreoffice, or python3+openpyxl"))))

(defun kdb/org-table-to-xlsx ()
  "Export current org table to Excel (.xlsx) via CSV."
  (interactive)
  (unless (org-at-table-p) (user-error "Not in an org table"))
  (let* ((base (file-name-base (or buffer-file-name "table")))
         (csv-file (make-temp-file base nil ".csv"))
         (xlsx-file (read-file-name "Export xlsx to: " nil nil nil (concat base ".xlsx"))))
    (org-table-export csv-file "orgtbl-to-csv")
    (cond
     ((executable-find "ssconvert")
      (call-process "ssconvert" nil nil nil csv-file xlsx-file))
     ((executable-find "libreoffice")
      (let ((tmp-dir (file-name-directory xlsx-file)))
        (call-process "libreoffice" nil nil nil
                      "--headless" "--convert-to" "xlsx"
                      "--outdir" tmp-dir csv-file)
        (let ((lo-file (expand-file-name (concat (file-name-base csv-file) ".xlsx") tmp-dir)))
          (unless (string= lo-file xlsx-file)
            (rename-file lo-file xlsx-file t)))))
     ((executable-find "python3")
      (call-process "python3" nil nil nil "-c"
                    (format "import csv,openpyxl;wb=openpyxl.Workbook();ws=wb.active;[ws.append(r) for r in csv.reader(open('%s'))];wb.save('%s')"
                            csv-file (expand-file-name xlsx-file))))
     (t (message "No xlsx converter. CSV saved to %s" csv-file) (setq xlsx-file csv-file)))
    (delete-file csv-file)
    (message "Exported to %s" xlsx-file)))

;; Auto-convert xlsx files when opened
(add-to-list 'auto-mode-alist '("\\.xlsx\\'" . kdb/xlsx-auto-open))
(defun kdb/xlsx-auto-open ()
  "Auto-convert xlsx to CSV on open."
  (kdb/xlsx-to-csv buffer-file-name))

;; Export org table to CSV
(defun kdb/org-table-to-csv ()
  "Export current org table to CSV file."
  (interactive)
  (when (org-at-table-p)
    (let ((file (read-file-name "Export CSV to: " nil nil nil
                                (concat (file-name-base (or buffer-file-name "table")) ".csv"))))
      (org-table-export file "orgtbl-to-csv")
      (message "Table exported to %s" file))))

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

(defun kdb/org-to-markdown ()
  "Export current org buffer or region to markdown and open in a buffer."
  (interactive)
  (let ((buf (generate-new-buffer
              (format "*%s.md*" (file-name-base (or buffer-file-name "export"))))))
    (if (use-region-p)
        (let ((text (buffer-substring (region-beginning) (region-end))))
          (with-current-buffer buf
            (insert (org-export-string-as text 'md t))))
      (org-export-to-buffer 'md (buffer-name buf)))
    (with-current-buffer buf
      (markdown-mode)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun kdb/export-deliverable ()
  "Export current buffer to a deliverable format.
Uses built-in org export where possible, pandoc for DOCX/PDF."
  (interactive)
  (let* ((has-pandoc (executable-find "pandoc"))
         (formats (append '("Markdown (.md)" "HTML (.html)"
                            "ODT (.odt)" "Plain text (.txt)"
                            "CSV (table only)" "Excel (table only)")
                          (when has-pandoc
                            '("DOCX (.docx)" "PDF (.pdf)"))))
         (format (completing-read "Export as: " formats nil t)))
    (pcase format
      ("Markdown (.md)" (org-md-export-to-markdown))
      ("HTML (.html)" (org-html-export-to-html))
      ("ODT (.odt)" (org-odt-export-to-odt))
      ("Plain text (.txt)" (org-ascii-export-to-ascii))
      ("CSV (table only)" (kdb/org-table-to-csv))
      ("Excel (table only)" (kdb/org-table-to-xlsx))
      ((guard has-pandoc)
       (let* ((ext (pcase format
                     ("DOCX (.docx)" "docx")
                     ("PDF (.pdf)" "pdf")))
              (md-file (org-md-export-to-markdown))
              (out-file (concat (file-name-sans-extension md-file) "." ext)))
         (if (zerop (call-process "pandoc" nil nil nil md-file "-o" out-file))
             (progn (delete-file md-file)
                    (message "Exported to %s" out-file))
           (message "Pandoc export failed")))))))

(defun kdb/pandoc-convert ()
  "Convert current file to another format using pandoc."
  (interactive)
  (unless (executable-find "pandoc")
    (user-error "Pandoc not installed"))
  (let* ((in-file (or buffer-file-name
                      (let ((tmp (make-temp-file "pandoc-" nil
                                                 (pcase major-mode
                                                   ('markdown-mode ".md")
                                                   ('org-mode ".org")
                                                   (_ ".txt")))))
                        (write-region (point-min) (point-max) tmp)
                        tmp)))
         (out-fmt (completing-read "Convert to: "
                                   '("docx" "pdf" "html" "md" "org" "odt" "rst" "txt")
                                   nil t))
         (out-file (read-file-name "Save as: " nil nil nil
                                   (concat (file-name-base in-file) "." out-fmt))))
    (if (zerop (call-process "pandoc" nil nil nil in-file "-o" out-file))
        (message "Converted to %s" out-file)
      (message "Pandoc conversion failed"))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o x") #'kdb/export-deliverable))
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-p") #'kdb/markdown-preview))

(provide 'init-org)
;;; init-org.el ends here
