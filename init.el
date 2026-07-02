;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.6.0
;; Keywords: configuration
;; URL: https://github.com/kborling/emacs.d
;; Homepage: https://github.com/kborling/emacs.d
;; Package-Requires: ((emacs "31"))

;;; Commentary:

;; Copyright (C) 2025 Kevin Borling
;; My personal Emacs config.

;;; Code:

;;; ============================================================
;;;                    PERSONAL SETTINGS
;;; ============================================================

;; Load personal settings if they exist (not tracked in git)
(let ((personal-file (locate-user-emacs-file "personal.el")))
  (when (file-exists-p personal-file)
    (load personal-file)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; ============================================================
;;;                  PERFORMANCE SETTINGS
;;; ============================================================

(fset #'jsonrpc--log-event #'ignore)

;; Native compilation settings
(setq native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 4)

;; Rendering optimizations
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      jit-lock-defer-time 0
      jit-lock-stealth-time 0.2)

;;; ============================================================
;;;                   PACKAGE MANAGEMENT
;;; ============================================================

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Package Management ========================================= ;;

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

(setq package-install-upgrade-built-in t
      package-vc-register-as-project nil)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; ============================================================
;;;                DIRECTORIES AND FILE MANAGEMENT
;;; ============================================================

;; Create var directory for cleaner organization
(defvar user-emacs-var-directory
  (expand-file-name "var/" user-emacs-directory)
  "Directory for storing variable data files.")

(unless (file-directory-p user-emacs-var-directory)
  (make-directory user-emacs-var-directory t))

;; Backup stored in /tmp
(setq
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms
 `((".*" , temporary-file-directory t))

 backup-by-copying t
 version-control t
 delete-old-versions t
 kept-new-versions 6
 create-lockfiles nil

 ;; Redirect files to var directory
 auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-var-directory)
 tramp-persistency-file-name (expand-file-name "tramp" user-emacs-var-directory)
 url-cache-directory (expand-file-name "url/cache/" user-emacs-var-directory)
 url-configuration-directory (expand-file-name "url/configuration/" user-emacs-var-directory)

 bookmark-default-file (expand-file-name "bookmarks" user-emacs-var-directory)
 abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-var-directory)
 nsm-settings-file (expand-file-name "network-security.data" user-emacs-var-directory))

;;; ============================================================
;;;                  EDITOR BEHAVIOR
;;; ============================================================

(setq
 sentence-end-double-space nil
 fill-column 80
 column-number-mode t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 set-mark-command-repeat-pop t
 global-mark-ring-max 50000
 ;; Built-in defaults
 view-read-only t
 completion-cycle-threshold 5)

;; Emacs 31+ features
(when (boundp 'kill-region-dwim)
  (setq kill-region-dwim 'kill-word
        split-window-preferred-direction 'longest
        mode-line-collapse-minor-modes t))

;;; ============================================================
;;;                 UI AND APPEARANCE
;;; ============================================================

;; Cursor configuration - thick bar
(setq-default cursor-type '(bar . 3))  ; Bar cursor with width of 3 pixels
(setq cursor-in-non-selected-windows 'hollow)  ; Hollow cursor in inactive windows

;; Contextual menu with right mouse button
(when (display-graphic-p)
  (context-menu-mode))

;; Auto-scroll Messages buffer when displayed
(defun kdb-messages-buffer-advice (buffer &optional _action _frame)
  "Scroll *Messages* to bottom when displayed."
  (when (and buffer (string= (buffer-name buffer) "*Messages*"))
    (with-current-buffer buffer
      (goto-char (point-max)))
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-point window (point-max)))))

(advice-add 'display-buffer :after #'kdb-messages-buffer-advice)

;; UI chrome — keep macOS menu bar (integrates with system), disable elsewhere
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (unless (eq system-type 'darwin)
    (menu-bar-mode -1)))

;; Frame sizing — large but not fullscreen (EXWM handles its own)
(unless (memq window-system '(x))  ; Skip on EXWM
  (when (display-graphic-p)
    (add-to-list 'default-frame-alist '(width . 120))
    (add-to-list 'default-frame-alist '(height . 45))))

;;; ============================================================
;;;                   SESSION MANAGEMENT
;;; ============================================================

;; Recent Files ============================================ ;;

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-var-directory))
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/opt/" "/.rustup/" "/ssh:" "/sudo:" "/node_modules/" "/nix/"))
  :config
  ;; Save silently every 5 minutes
  (run-at-time nil (* 5 60) (lambda ()
                              (let ((inhibit-message t))
                                (recentf-save-list)))))

;; Save Place ============================================== ;;

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-var-directory)))

;; Save History ============================================ ;;

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-var-directory))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 60)
  (history-length 10000)
  (history-delete-duplicates t))

;; Undo/redo
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil
              tab-stop-list ()
              tab-width 4
              c-basic-offset 4
              sgml-basic-offset 4
              typescript-ts-mode-indent-offset 4
              js-switch-indent-offset 4)

;; Subword Mode ============================================ ;;

(use-package subword-mode
  :ensure nil
  :hook (after-init . global-subword-mode))

;; Delete Selection ======================================== ;;

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Time and battery in mode line
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)

;; Smooth scrolling (GUI only)
(when (display-graphic-p)
  (add-hook 'after-init-hook #'pixel-scroll-precision-mode))

;; Repeat mode — after C-x o, press o to keep switching windows, etc.
(add-hook 'after-init-hook #'repeat-mode)

;; Inline completion preview (ghost text)
(add-hook 'prog-mode-hook #'completion-preview-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;;; ============================================================
;;;                    VISUAL APPEARANCE
;;; ============================================================

;; Fonts ================================================ ;;

(setopt line-spacing 0.20)

(let* ((settings (cond
                  ((eq system-type 'windows-nt)    '(:size 110 :families ("Rec Mono Semicasual" "Cascadia Code" "Consolas" "Courier New")))
                  ((eq system-type 'darwin)        '(:size 140 :families ("Overpass Mono" "Monaco" "Menlo" "monospace")))
                  (t '(:size 110 :families ("JetBrains Mono" "DejaVu Sans Mono" "Liberation Mono" "monospace")))))  ; Linux, BSD, etc.
       (default-font-size (plist-get settings :size))
       (font-families (plist-get settings :families))
       (default-font-family (cl-find-if (lambda (font) (find-font (font-spec :family font))) font-families)))

  ;; Set the font if we found one available
  (when default-font-family
    (set-face-attribute 'default nil
                        :family default-font-family :weight 'regular :height default-font-size)
    (set-face-attribute 'fixed-pitch nil
                        :family default-font-family :height 1.0))

  (let ((vp-family (cl-find-if (lambda (f) (find-font (font-spec :family f)))
                                 '("FreeSans" "Helvetica Neue" "Segoe UI" "sans-serif"))))
    (when vp-family
      (set-face-attribute 'variable-pitch nil
                          :family vp-family :height 1.0 :weight 'regular))))

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; Themes ================================================= ;;

(use-package uwu-theme
  :vc (:url "https://github.com/kborling/uwu-theme" :rev :newest)
  :config
  (setq
   uwu-distinct-line-numbers 'nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t
   uwu-cursor-color 'red)
  ;; (load-theme 'uwu t)
  )

(use-package fleury-theme
  :vc (:url "https://github.com/kborling/fleury-theme.el" :rev :newest)
  :config
  (load-theme 'fleury t)
  (add-hook 'prog-mode-hook 'hl-line-mode))

(use-package acme-theme)


;;; ============================================================
;;;                  CUSTOM FUNCTIONS
;;; ============================================================

;; Doctor ================================================= ;;

(defun kdb-doctor ()
  "Check for external programs used by this Emacs config."
  (interactive)
  (let* ((tools
          '(;; Core (required)
            ("ripgrep (rg)" "rg" t "Grep, xref, deadgrep")
            ("Git" "git" t "Version control")
            ("GCC" "gcc" nil "Compile treesit grammars")
            ("GnuPG (gpg)" "gpg" t "File encryption (.gpg)")
            ;; Development
            ("Node.js (npm)" "npm" nil "Angular LSP, JS tooling")
            ("eglot-booster" "emacs-lsp-booster" nil "Faster LSP")
            ("sqlcmd" "sqlcmd" nil "Azure SQL (or go-sqlcmd)")
            ("go-sqlcmd" "go-sqlcmd" nil "Azure SQL (alternative)")
            ;; Document conversion
            ("Pandoc" "pandoc" nil "DOCX/PDF export")
            ("ssconvert" "ssconvert" nil "Excel conversion (gnumeric)")
            ("LibreOffice" "libreoffice" nil "Excel/ODT fallback")
            ("Python 3" "python3" nil "Excel via openpyxl")
            ("multimarkdown" "multimarkdown" nil "Markdown export")
            ("unzip" "unzip" nil "Claude export import")
            ;; Terminal
            ("fd" "fd" nil "Fast file finder")
            ("fzf" "fzf" nil "Fuzzy finder")
            ("bat" "bat" nil "Cat with syntax highlighting")
            ;; EXWM / Desktop
            ("pactl" "pactl" nil "Volume control (EXWM)")
            ("brightnessctl" "brightnessctl" nil "Brightness (EXWM)")
            ("picom" "picom" nil "Compositor (EXWM)")
            ("feh" "feh" nil "Wallpaper (EXWM)")
            ("dunst" "dunst" nil "Notifications (EXWM)")
            ("scrot" "scrot" nil "Screenshots (EXWM)")
            ("i3lock" "i3lock" nil "Screen lock (EXWM)")
            ("redshift" "redshift" nil "Night light (EXWM)")))
         (found 0) (missing 0) (optional-missing 0))
    (with-current-buffer (get-buffer-create "*Emacs Doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Emacs Config Doctor\n")
        (insert (make-string 40 ?═) "\n\n")
        (insert (format "Emacs %s on %s\n\n" emacs-version system-type))
        (dolist (tool tools)
          (let* ((name (nth 0 tool))
                 (cmd (nth 1 tool))
                 (required (nth 2 tool))
                 (purpose (nth 3 tool))
                 (available (executable-find cmd)))
            (insert (format "  %s  %-22s %s\n"
                            (if available "OK" (if required "XX" "--"))
                            name
                            (if available
                                (progn (cl-incf found) "")
                              (if required
                                  (progn (cl-incf missing) (format "(REQUIRED) %s" purpose))
                                (progn (cl-incf optional-missing) purpose)))))
            ))
        (insert (format "\n%s\n" (make-string 40 ?─)))
        (insert (format "  Found: %d  Missing (required): %d  Missing (optional): %d\n"
                        found missing optional-missing))
        ;; Check personal.el
        (insert (format "\n  personal.el: %s\n"
                        (if (file-exists-p (locate-user-emacs-file "personal.el"))
                            "OK" "-- (create for API keys, SQL connections)")))
        ;; Check treesit grammars
        (when (fboundp 'treesit-language-available-p)
          (let ((grammar-missing
                 (cl-remove-if
                  (lambda (l) (treesit-language-available-p l))
                  '(typescript tsx javascript python json css html yaml
                    toml bash c cpp rust go zig markdown markdown-inline
                    dockerfile java))))
            (insert (format "  Treesit grammars: %s\n"
                            (if grammar-missing
                                (format "MISSING %s" grammar-missing)
                              "OK (all installed)")))))
        (insert "\nLegend: OK = found, XX = required missing, -- = optional missing\n")
        (goto-char (point-min))
        (special-mode))
      (switch-to-buffer (current-buffer)))))

;; Custom Functions ======================================= ;;

(defun config-visit ()
  "Load ~/.emacs.d/init.el for editing."
  (interactive)
  (find-file (expand-file-name (locate-user-emacs-file "init.el"))))

(defun config-reload ()
  "Reload ~/.emacs.d/init.el at runtime."
  (interactive)
  (load-file (expand-file-name (locate-user-emacs-file "init.el"))))

(defun kill-buffer-other-window ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))

(defun format-current-buffer ()
  "Format the current buffer while maintaining cursor position."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defun copy-whole-buffer ()
  "Copy the entire buffer to the kill ring."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer copied"))

;; Terminal management functions (defined early for keybindings)
(defun kdb-eshell-new ()
  "Create a new eshell buffer named after the directory."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name
                             (format "*eshell: %s*" (abbreviate-file-name default-directory)))))
    (eshell)))

(defun kdb-eshell-toggle ()
  "Toggle eshell window at bottom of frame and focus it."
  (interactive)
  (let ((eshell-window (get-buffer-window "*eshell*")))
    (if eshell-window
        (if (eq (selected-window) eshell-window)
            ;; If already in eshell, close it
            (delete-window eshell-window)
          ;; If eshell visible but not focused, focus it
          (select-window eshell-window))
      ;; If not visible, create/show and focus it
      (let ((buf (or (get-buffer "*eshell*")
                     (save-window-excursion (eshell)))))
        (select-window
         (display-buffer buf
                         '((display-buffer-at-bottom)
                           (window-height . 0.3))))))))

(defun kdb-eat-new ()
  "Create a new terminal using eat (GUI) or term (terminal).
Names buffer after the directory for easier identification."
  (interactive)
  (let ((shell (or (getenv "SHELL") (getenv "COMSPEC") "/bin/bash"))
        (name (format "*eat: %s*" (abbreviate-file-name default-directory))))
    (if (and (display-graphic-p) (fboundp 'eat))
        (let ((eat-buffer-name (generate-new-buffer-name name)))
          (eat))
      (term shell))))


(defun toggle-theme ()
  "Toggle between available themes."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (available-themes (mapcar 'symbol-name (custom-available-themes)))
         (chosen-theme (completing-read "Select a theme: " available-themes nil t nil nil current-theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern chosen-theme) t)))

(defun kdb-move-to-beginning-of-line ()
  "Move point to the first non-whitespace character of the line.
If point is already there, move to the true beginning of the line."
  (interactive)
  (let ((current-pos (point)))
    (back-to-indentation)
    (when (eq current-pos (point))
      (beginning-of-line))))

(defun kdb-kill-line ()
  "Kill from point to the end of the line.
If point is at the end of the line, kill the whole line including the newline."
  (interactive)
  (if (eolp)
      (kill-whole-line)
    (kill-line)))


(defun kdb-setup-keybindings ()
  "Set up all custom keybindings."
  (let ((map global-map))
    ;; Remove suspend keys
    (dolist (key '("C-z" "C-x C-z"))
      (define-key map (kbd key) nil))

    ;; General keybindings
    (dolist (binding '(("C-h C-r" . restart-emacs)
                       ("C-;" . comment-line)
                       ("C-a" . kdb-move-to-beginning-of-line)
                       ("C-k" . kdb-kill-line)
                       ("C-<return>" . electric-newline-and-maybe-indent)
                       ("M-<return>" . open-line)
                       ("C-j" . delete-indentation)
                       ("C-o" . occur)
                       ("C-z" . undo)
                       ("C-c b" . copy-whole-buffer)
                       ("C-c C-d" . duplicate-dwim)
                       ("C-x C-r" . recentf)
                       ("C-x f" . project-find-file)
                       ("C-c C-r" . replace-regexp)
                       ("C-\\" . hippie-expand)
                       ("M-z" . zap-up-to-char)
                       ("C-M-s" . isearch-forward-symbol-at-point)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; TODO navigation (hl-todo)
    (dolist (binding '(("M-[" . hl-todo-previous)
                       ("M-]" . hl-todo-next)
                       ("C-c o o" . hl-todo-occur)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Buffer management
    (dolist (binding '(("C-x b" . ibuffer)
                       ("C-c C-p" . previous-buffer)
                       ("C-c C-n" . next-buffer)
                       ("C-c C-o" . other-window)
                       ("C-x C-b" . switch-to-buffer)
                       ("C-x k" . kill-current-buffer)
                       ("C-x M-k" . kill-buffer-other-window)
                       ("<backtab>" . format-current-buffer)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Window management
    (dolist (binding '(("C-x w s" . window-swap-states)
                       ("C-x 4 4" . other-window-prefix)
                       ("C-x 4 1" . same-window-prefix)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Terminal management
    (dolist (binding '(("C-`" . kdb-eshell-toggle)
                       ("C-~" . kdb-eat-new)
                       ("C-c t e" . kdb-eshell-new)
                       ("C-c t t" . kdb-eat-new)
                       ("C-c t s" . shell)
                       ("C-c t d" . dired-jump-other-window)
                       ("C-c t =" . fido-vertical-mode)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Help extensions
    (dolist (binding '(("C-h M-k" . find-function-on-key)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Configuration shortcuts
    (dolist (binding '(("C-c e v" . config-visit)
                       ("C-c e r" . config-reload)
                       ("C-c e d" . kdb-doctor)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Toggling features
    (dolist (binding '(("C-c t h" . toggle-theme)
                       ("C-c t f" . toggle-frame-fullscreen)))
      (define-key map (kbd (car binding)) (cdr binding)))))

(add-hook 'after-init-hook #'kdb-setup-keybindings)

(defun kdb-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p) (keyboard-quit))
   ((> (minibuffer-depth) 0) (abort-recursive-edit))
   (t (keyboard-quit))))

(define-key global-map (kbd "C-g") #'kdb-keyboard-quit-dwim)


;; Auto-kill term buffer on exit
(add-hook 'term-exec-hook
          (lambda ()
            (let ((buf (current-buffer)))
              (set-process-sentinel
               (get-buffer-process buf)
               (lambda (_proc event)
                 (when (string= event "finished\n")
                   (kill-buffer buf)))))))

;;; ============================================================
;;;                   ESSENTIAL PACKAGES
;;; ============================================================

;; Auto Revert ============================================= ;;

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))


;; Grep ==================================================== ;;

(use-package grep
  :ensure nil
  :defer t
  :config
  (setq grep-command "rg --color=never --no-heading --line-number --smart-case "
        grep-use-null-device nil))


;; Deadgrep ================================================ ;;

(use-package deadgrep
  :ensure t
  :commands deadgrep
  :bind ("C-x g" . deadgrep)
  :config
  (setq deadgrep-extra-arguments '("--no-config" "--multiline")))


;; Xref ==================================================== ;;

(use-package xref
  :ensure nil
  :defer t
  :config
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-search-program 'ripgrep))



;; SQL Mode ================================================ ;;

(use-package sql
  :ensure nil
  :defer t
  :config
  (setq sql-ms-program (or (executable-find "sqlcmd")
                           (executable-find "go-sqlcmd")
                           "sqlcmd"))

  ;; Azure SQL connection helper
  (defun kdb-sql-azure (server database)
    "Connect to Azure SQL SERVER/DATABASE using managed identity."
    (interactive
     (list (read-string "Server (e.g. myserver.database.windows.net): ")
           (read-string "Database: ")))
    (let ((sql-ms-options
           (cond
            ;; go-sqlcmd supports --authentication-method
            ((executable-find "go-sqlcmd")
             (list "-S" server "-d" database "--authentication-method" "ActiveDirectoryDefault"))
            ;; sqlcmd with -G for Azure AD
            (t (list "-S" server "-d" database "-G")))))
      (sql-ms)))

  (defvar kdb-sql-connections nil
    "Alist of named Azure SQL connections. Set in personal.el:
  (setq kdb-sql-connections
        \\='((\"prod\" :server \"prod.database.windows.net\" :database \"mydb\")
          (\"dev\"  :server \"dev.database.windows.net\"  :database \"mydb-dev\")))")

  (defun kdb-sql-connect ()
    "Connect to a saved Azure SQL database."
    (interactive)
    (if (null kdb-sql-connections)
        (call-interactively #'kdb-sql-azure)
      (let* ((name (completing-read "Connection: "
                                    (mapcar #'car kdb-sql-connections) nil t))
             (conn (cdr (assoc name kdb-sql-connections)))
             (server (plist-get conn :server))
             (database (plist-get conn :database)))
        (kdb-sql-connect-internal server database name))))

  (defun kdb-sql-connect-internal (server database &optional name)
    "Connect to Azure SQL SERVER/DATABASE with optional buffer NAME."
    (let* ((buf-name (format "*SQL: %s*" (or name database)))
           (sql-ms-options
            (cond
             ((executable-find "go-sqlcmd")
              (list "-S" server "-d" database "--authentication-method" "ActiveDirectoryDefault"))
             (t (list "-S" server "-d" database "-G")))))
      (sql-ms)
      (when (get-buffer "*SQL*")
        (with-current-buffer "*SQL*"
          (rename-buffer buf-name t)))))

  ;; Send region/paragraph to SQL and show results
  (defun kdb-sql-send-dwim ()
    "Send region, paragraph, or buffer to SQL process."
    (interactive)
    (cond
     ((use-region-p) (sql-send-region (region-beginning) (region-end)))
     (t (sql-send-paragraph))))

  :bind (:map sql-mode-map
              ("C-c C-c" . kdb-sql-send-dwim)
              ("C-c C-a" . kdb-sql-connect)))


;; Which Key =============================================== ;;

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))


;; EditorConfig ============================================ ;;

(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))


;; Isearch ================================================= ;;

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-g" . isearch-cancel)
              ("M-/" . isearch-complete)
              ("M-e" . isearch-edit-string)
              ("M-s M-<" . isearch-beginning-of-buffer)
              ("M-s M->" . isearch-end-of-buffer))
  :config
  (setq
   search-whitespace-regexp ".*?"
   isearch-lazy-count t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " (%s/%s)"
   isearch-wrap-pause 'no
   isearch-yank-on-move 'shift
   isearch-allow-scroll 'unlimited
   isearch-allow-motion t
   isearch-motion-changes-direction t
   isearch-repeat-on-direction-change t
   isearch-regexp-lax-whitespace t
   lazy-highlight-initial-delay 0.1)

  ;; Occur integration - show all matches in a buffer
  (define-key isearch-mode-map (kbd "M-s o") 'isearch-occur)

  (defun kdb-isearch-remove-failed-part ()
    "Remove the failing part of the search string."
    (interactive)
    (while (isearch-fail-pos)
      (isearch-pop-state)))
  (define-key isearch-mode-map (kbd "C-<backspace>") 'kdb-isearch-remove-failed-part)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit))


;; Dabbrev ================================================= ;;

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq
   dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
   dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
   dabbrev-backward-only nil
   dabbrev-case-distinction 'case-replace
   dabbrev-case-fold-search nil
   dabbrev-case-replace 'case-replace
   dabbrev-check-other-buffers t
   dabbrev-eliminate-newlines t
   dabbrev-upcase-means-case-search t
   dabbrev-ignored-buffer-modes
   '(archive-mode image-mode docview-mode pdf-view-mode)))


;; Diff Mode =============================================== ;;

(use-package diff-mode
  :ensure nil
  :defer t
  :hook ((diff-mode . (lambda ()
                        (setq-local truncate-lines t)
                        (diff-refine-hunk)
                        (outline-minor-mode 1))))
  :config
  (setq
   diff-refine nil  ; Disable refinement
   diff-font-lock-prettify nil  ; Disable prettification
   diff-font-lock-syntax nil  ; Disable syntax highlighting in diffs
   diff-font-lock-leading-indicator nil  ; Disable special indicator formatting
   diff-update-on-the-fly t
   diff-advance-after-apply-hunk t
   diff-default-read-only t)  ; Make diff buffers read-only by default

  (defun kdb-diff-toggle-refine ()
    "Toggle diff refinement between 'navigation and nil."
    (interactive)
    (setq diff-refine (if diff-refine nil 'navigation))
    (if diff-refine
        (diff-refine-hunk)
      (diff-unrefine-hunk))
    (message "Diff refinement: %s" (if diff-refine "enabled" "disabled")))

  (defun kdb-diff-navigate-and-refine (orig-fun &rest args)
    "Automatically refine hunk after navigation."
    (apply orig-fun args)
    (when diff-refine
      (diff-refine-hunk)))

  (advice-add 'diff-hunk-next :around #'kdb-diff-navigate-and-refine)
  (advice-add 'diff-hunk-prev :around #'kdb-diff-navigate-and-refine)

  :bind (:map diff-mode-map
              ("C-c C-r" . kdb-diff-toggle-refine)
              ("n" . diff-hunk-next)
              ("p" . diff-hunk-prev)
              ("N" . diff-file-next)
              ("P" . diff-file-prev)
              ("k" . diff-hunk-kill)
              ("K" . diff-file-kill)
              ("RET" . diff-goto-source)
              ("o" . diff-goto-source)
              ("SPC" . scroll-up-command)
              ("S-SPC" . scroll-down-command)
              ("TAB" . diff-hunk-next)
              ("<backtab>" . diff-hunk-prev)))

;; Ediff =================================================== ;;

(use-package ediff
  :ensure nil
  :config
  (setq
   ediff-keep-variants nil
   ediff-make-buffers-readonly-at-startup nil
   ediff-merge-revisions-with-ancestor t
   ediff-show-clashes-only t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain))


;; Dired =================================================== ;;

(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-free-space nil
   dired-isearch-filenames 'dwim
   dired-create-destination-dirs 'ask
   dired-vc-rename-file t
   dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))
   dired-kill-when-opening-new-dired-buffer t
   delete-by-moving-to-trash t
   dired-dwim-target t
   dired-ls-F-marks-symlinks t
   dired-clean-confirm-on-delete t
   dired-deletion-confirmer 'y-or-n-p
   dired-mouse-drag-files t)

  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil)); macOS ls doesn't support --dired

  :bind (:map dired-mode-map
              ("E" . dired-toggle-read-only)
              ("?" . dired-summary)
              ("C-c C-e" . wdired-change-to-wdired-mode)))


;; Ibuffer ================================================= ;;

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq
   ibuffer-expert t
   ibuffer-display-summary nil
   ibuffer-use-other-window nil
   ibuffer-movement-cycle nil
   ibuffer-default-sorting-mode 'filename/process
   ibuffer-use-header-line t
   ibuffer-default-shrink-to-minimum-size nil

   ibuffer-show-empty-filter-groups nil  ; Hide empty groups
   ibuffer-saved-filter-groups
   '(("Default"
      ("EXWM: Browser" (or (and (mode . exwm-mode)
                                (name . "Firefox"))
                           (and (mode . exwm-mode)
                                (name . "Librewolf"))
                           (and (mode . exwm-mode)
                                (name . "Icecat"))))
      ("EXWM: Terminal" (and (mode . exwm-mode)
                             (name . "kitty")))
      ("EXWM: Other" (mode . exwm-mode))
      ("Emacs Config" (or (filename . "\\.emacs\\.d")
                          (name . "^\\*scratch\\*")
                          (name . "^\\*Messages\\*")))
      ("VC/Git" (or (name . "^\\*vc-")
                    (name . "^\\*git-")
                    (mode . diff-mode)
                    (mode . log-view-mode)))
      ("AI/Claude" (or (name . "^\\*Claude")
                      (name . "^\\*gptel")
                      (mode . gptel-mode)))
      ("Terminal" (or (mode . eshell-mode)
                      (mode . shell-mode)
                      (mode . term-mode)
                      (name . "^\\*eat\\*")))
      ("Org" (or (mode . org-mode)
                 (mode . org-agenda-mode)
                 (filename . "\\.org$")))
      ("Programming" (derived-mode . prog-mode))
      ("Dired" (mode . dired-mode))
      ("Completion" (or (name . "^\\*Completions\\*")
                        (name . "^\\*Flymake")
                        (name . "^\\*corfu")))
      ("Help/Info" (or (name . "^\\*Help\\*")
                       (name . "^\\*Apropos\\*")
                       (name . "^\\*info\\*")
                       (name . "^\\*Man ")
))
      ("Special" (name . "^\\*")))))

  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))

  :bind (:map ibuffer-mode-map
              ("M-o" . other-window)
              ("/" . ibuffer-filter-by-name)))


;; Project ================================================= ;;

(use-package project
  :ensure nil
  :config
  (setq
   vc-directory-exclusion-list (nconc vc-directory-exclusion-list '("node_modules" "elpa" ".sl"))
   project-vc-extra-root-markers '(".envrc" "package.json" ".project" ".sl")
   project-prune-zombie-projects '((prompt . project-prune-zombies-default))))


;; Eshell ================================================== ;;

(use-package eshell
  :ensure nil
  :config
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t)

  (defun kdb-eshell-prompt ()
    "Custom Eshell prompt."
    (concat
     (propertize (abbreviate-file-name (eshell/pwd)) 'face 'font-lock-keyword-face)
     (if (zerop (user-uid)) " # " " $ ")))

  (setq eshell-prompt-function 'kdb-eshell-prompt
        eshell-prompt-regexp "^[^#$\n]* [#$] "))


;; Orderless =============================================== ;;

(use-package orderless
  :ensure t
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil))
  :config
  (setq orderless-matching-styles '(orderless-initialism orderless-flex orderless-regexp)
        completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  ;; fido-mode forces completion-styles to (flex) on every minibuffer entry.
  ;; Override it after fido's setup hook runs.
  (defun kdb-orderless-override-fido ()
    "Restore orderless as the completion style in fido-mode."
    (when (and (bound-and-true-p icomplete-mode)
               (not (eq (car-safe completion-styles) 'orderless)))
      (setq-local completion-styles '(orderless basic))))
  (add-hook 'minibuffer-setup-hook #'kdb-orderless-override-fido 10))


;; Icomplete =============================================== ;;

(use-package icomplete
  :ensure nil
  :hook (after-init . fido-mode)
  :config
  (setq icomplete-tidy-shadowed-file-names t
        icomplete-show-matches-on-no-input t
        icomplete-compute-delay 0
        icomplete-max-delay-chars 0
        icomplete-delay-completions-threshold 0
        icomplete-hide-common-prefix nil
        icomplete-prospects-height 2
        icomplete-with-completion-tables t
        icomplete-scroll t))


;; Minibuffer ============================================== ;;

(use-package minibuffer
  :ensure nil
  :config
  (setq
   completions-format 'one-column
   completion-show-help nil
   completion-show-inline-help nil
   ;; completion-auto-help 'always
   completion-auto-help nil
   completion-auto-select nil
   completions-detailed t
   completion-ignore-case t
   read-buffer-completion-ignore-case t
   completions-max-height 14
   completions-header-format nil
   minibuffer-visible-completions nil
   enable-recursive-minibuffers t
   completions-sort 'historical
   read-answer-short t))

;;; ============================================================
;;;                   DEVELOPMENT TOOLS
;;; ============================================================

;; Eglot (LSP) ============================================= ;;

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c c r" . eglot-rename)
              ("C-c c f" . eglot-format-buffer)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c a" . eglot-code-actions)
              ("C-." . eglot-code-actions)
              ("C-c c q" . eglot-code-action-quickfix)
              ("C-c c e" . eglot-code-action-extract)
              ("C-c c j" . eglot-code-action-inline)
              ("C-c c k" . eglot-code-action-rewrite)
              ("C-c c i" . eglot-find-implementation)
              ("C-c c d" . eglot-find-declaration)
              ("C-c c t" . eglot-find-typeDefinition)
              ("C-c c h" . eldoc))
  :config
  (setq
   eglot-sync-connect 0
   eglot-send-changes-idle-time 0
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentHighlightProvider)
   ;; Windows-specific optimization
   eglot-events-buffer-size (if (eq system-type 'windows-nt) 0 2000000))

  ;; Language Servers
  (add-to-list 'eglot-server-programs '(csharp-mode . ("csharp-ls")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(html-ts-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-ts-mode . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")))
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode)
                                        . ("clangd"
                                           "-j=8"
                                           "--log=error"
                                           "--malloc-trim"
                                           "--background-index"
                                           "--clang-tidy"
                                           "--cross-file-rename"
                                           "--completion-style=detailed"
                                           "--pch-storage=memory"
                                           "--header-insertion=never"
                                           "--header-insertion-decorators=0")))

  ;; Angular Language Server (only when npm is available)
  (when (executable-find "npm")
    (let* ((global-prefix (string-trim (shell-command-to-string "npm config get --global prefix")))
           (modules-path (if (eq system-type 'windows-nt) "node_modules" "lib/node_modules"))
           (node-modules-path (expand-file-name modules-path global-prefix)))
      (add-to-list 'eglot-server-programs
                   `(angular-template-mode . ("ngserver"
                                              "--stdio"
                                              "--tsProbeLocations"
                                              ,(concat node-modules-path "/typescript/lib")
                                              "--ngProbeLocations"
                                              ,(concat node-modules-path "/@angular/language-server/bin"))))))

  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Make sure Eldoc will show us all of the feedback at point."
              (setq-local eldoc-documentation-strategy
                          #'eldoc-documentation-compose)))

  ;; Remove inlay hints
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  (dolist (mode '(html-ts-mode
                  angular-template-mode
                  typescript-ts-mode
                  css-ts-mode
                  js-ts-mode
                  c-ts-mode
                  c++-ts-mode
                  rust-ts-mode
                  zig-ts-mode
                  csharp-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'eglot-ensure)))

;; NOTE: Be sure to grab the latest release 'https://github.com/blahgeek/emacs-lsp-booster/releases'
;; and place in PATH

;; Eglot Booster =========================================== ;;

(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :config
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode)))

;; Flymake ================================================= ;;

(use-package flymake
  :ensure nil
  :config
  (setq
   flymake-suppress-zero-counters t
   flymake-no-changes-timeout 0.5 ; Small delay helps performance
   flymake-wrap-around nil
   flymake-start-on-save-buffer t
   flymake-start-on-flymake-mode t
   flymake-start-on-newline nil ; Don't check on every newline
   flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters)
   flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ""))


  (defun kdb-flymake-quickfix ()
    "Apply quickfix at point if available."
    (interactive)
    (when-let* ((diag (get-char-property (point) 'flymake-diagnostic)))
      (when-let* ((data (flymake-diagnostic-data diag)))
        (when (plist-get data :quickfix)
          (funcall (plist-get data :quickfix))))))

  (defun kdb-flymake-goto-next-error-only ()
    "Go to next error, skipping warnings and notes."
    (interactive)
    (flymake-goto-next-error nil '(:error)))

  (defun kdb-flymake-goto-prev-error-only ()
    "Go to previous error, skipping warnings and notes."
    (interactive)
    (flymake-goto-prev-error nil '(:error)))

  (define-key ctl-x-x-map "m" #'flymake-mode)

  :bind (:map flymake-mode-map
              ("C-c f s" . flymake-start)
              ("C-c f d" . flymake-show-buffer-diagnostics)
              ("C-c f D" . flymake-show-project-diagnostics)
              ("C-c f n" . kdb-flymake-goto-next-error-only)
              ("C-c f p" . kdb-flymake-goto-prev-error-only)
              ("C-c f q" . kdb-flymake-quickfix)
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :init
  (add-hook 'flymake-mode-hook
            (lambda ()
              (setq eldoc-documentation-functions
                    (cons 'flymake-eldoc-function
                          (delq 'flymake-eldoc-function eldoc-documentation-functions))))))


;; Eldoc =================================================== ;;

(use-package eldoc
  :ensure nil
  :hook (after-init . global-eldoc-mode)
  :config
  (setq
   eldoc-echo-area-use-multiline-p t))

;; Exec Path From Shell ==================================== ;;

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :init
  (setq exec-path-from-shell-variables '("PATH" "SSH_AUTH_SOCK")
        exec-path-from-shell-debug nil
        exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))


;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load VC configuration when needed
(defun kdb-load-init-vc ()
  "Load init-vc configuration once."
  (unless (featurep 'init-vc)
    (require 'init-vc)))

;; Autoload the main VC transient command and bind it
(autoload 'kdb-vc-transient "init-vc" "VC transient menu" t)
(global-set-key (kbd "C-c g") 'kdb-vc-transient)

;; Hook into various VC entry points for other VC commands
(add-hook 'vc-dir-mode-hook #'kdb-load-init-vc)
(with-eval-after-load 'vc-dir (kdb-load-init-vc))
(with-eval-after-load 'log-view (kdb-load-init-vc))

;; Load Org configuration when org-mode is loaded
(with-eval-after-load 'org
  (require 'init-org))

;; EPA — built-in GPG encryption (symmetric, password-based)
(setq epa-file-encrypt-to nil              ; Always prompt (symmetric by default)
      epa-file-select-keys nil             ; Skip public key selection → symmetric
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-armor t)                         ; ASCII armor for version control

(defun kdb-encrypt-file ()
  "Encrypt current file. Saves as .gpg and deletes the original."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer has no file")))
         (gpg-file (concat file ".gpg")))
    (when (string-suffix-p ".gpg" file)
      (user-error "File is already encrypted"))
    (when (yes-or-no-p (format "Encrypt %s and delete unencrypted copy? "
                               (file-name-nondirectory file)))
      (let ((content (buffer-string)))
        (with-temp-file gpg-file
          (insert content))
        (delete-file file)
        (kill-buffer)
        (find-file gpg-file)
        (message "Encrypted to %s" (file-name-nondirectory gpg-file))))))

(defun kdb-decrypt-file ()
  "Decrypt current .gpg file. Saves without .gpg and deletes the encrypted copy."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer has no file"))))
    (unless (string-suffix-p ".gpg" file)
      (user-error "File is not encrypted (.gpg)"))
    (let ((plain-file (string-remove-suffix ".gpg" file)))
      (when (yes-or-no-p (format "Decrypt to %s and delete encrypted copy? "
                                 (file-name-nondirectory plain-file)))
        (let ((content (buffer-string)))
          (with-temp-file plain-file
            (insert content))
          (delete-file file)
          (kill-buffer)
          (find-file plain-file)
          (message "Decrypted to %s" (file-name-nondirectory plain-file)))))))

(global-set-key (kbd "C-c s e") #'kdb-encrypt-file)
(global-set-key (kbd "C-c s d") #'kdb-decrypt-file)
(global-set-key (kbd "C-c s c") #'epa-dired-do-encrypt)

;; Load terminal optimizations (load immediately if in terminal mode, otherwise skip)
(when (not (display-graphic-p))
  (require 'init-terminal))



;; Marginalia ============================================== ;;

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0
        marginalia-align-offset 10))


;; HL Todo ================================================= ;;

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))


;; Corfu (Completion) ====================================== ;;

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil
        corfu-min-width 20
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))


;; Cape ==================================================== ;;

(use-package cape
  :config
  (dolist (func '(cape-dabbrev
                  cape-file
                  cape-keyword
                  ;; cape-elisp-symbol
                  cape-sgml))
    (add-to-list 'completion-at-point-functions func))

  ;; https://github.com/minad/corfu/wiki#continuously-update-the-candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


;; Tempel (Templates) ====================================== ;;

(use-package tempel
  :bind (("C-<tab>" . tempel-complete)
         ("M-+" . tempel-insert)
         ("C-1" . tempel-previous)
         ("C-2" . tempel-next)))

;; Tempel Collection ======================================= ;;

(use-package tempel-collection
  :after tempel)

;; Tree-sitter (Built-in Emacs 31) ======================== ;;

;; Automatically use tree-sitter modes when grammars are available
(setq treesit-enabled-modes t)              ; Remap all supported modes

;; Grammar sources for treesit-install-language-grammar
(with-eval-after-load 'treesit
  (dolist (source '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                    (python "https://github.com/tree-sitter/tree-sitter-python")
                    (json "https://github.com/tree-sitter/tree-sitter-json")
                    (css "https://github.com/tree-sitter/tree-sitter-css")
                    (html "https://github.com/tree-sitter/tree-sitter-html")
                    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
                    (toml "https://github.com/tree-sitter/tree-sitter-toml")
                    (bash "https://github.com/tree-sitter/tree-sitter-bash")
                    (c "https://github.com/tree-sitter/tree-sitter-c")
                    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                    (rust "https://github.com/tree-sitter/tree-sitter-rust")
                    (go "https://github.com/tree-sitter/tree-sitter-go")
                    (zig "https://github.com/maxxnino/tree-sitter-zig")
                    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                    (java "https://github.com/tree-sitter/tree-sitter-java")
                    (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
                    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))
    (add-to-list 'treesit-language-source-alist source)))

;;; ============================================================
;;;                  PROGRAMMING LANGUAGES
;;; ============================================================

;; Markdown Mode =========================================== ;;

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))



;; Tree-sitter Expand ====================================== ;;

(use-package treesit-expand
  :ensure nil
  :vc (:url "https://github.com/kborling/treesit-expand" :rev :newest)
  :bind (("C-=" . treesit-expand-dwim)
         ("C--" . treesit-contract-region)
         ("C-c e e" . treesit-expand-region)
         ("C-c e q" . treesit-expand-reset)))


;; Multiple Cursors ======================================== ;;

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-'" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)))


;; So Long ================================================= ;;

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))


;; Angular Mode ============================================ ;;

(use-package angular-mode
  :vc (:url "https://github.com/kborling/angular-mode" :rev :newest))

;; HTML Mode =============================================== ;;

(use-package mhtml-mode
  :ensure nil
  :bind (:map mhtml-mode-map
              ("C-c C-d" . nil))
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . mhtml-mode)))


;; Memmet Mode ============================================= ;;

(use-package memmet-mode
  :vc (:url "https://github.com/kborling/memmet-mode" :rev :newest)
  :bind (:map mhtml-mode-map
              ("C-<tab>" . memmet-expand))
  :hook (mhtml-mode . memmet-mode))


;; XML Mode ================================================ ;;

(use-package nxml-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode)))


;; Conf Mode =============================================== ;;

(use-package conf-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.sln\\'" . conf-mode)))


;; Dotnet ================================================== ;;

(defun kdb-dotnet--find-project ()
  "Find the nearest .csproj, .fsproj, or .sln file."
  (let ((root (or (locate-dominating-file default-directory
                    (lambda (d) (directory-files d nil "\\.\\(csproj\\|fsproj\\|sln\\)$")))
                  default-directory)))
    (car (directory-files root t "\\.\\(sln\\|csproj\\|fsproj\\)$"))))

(defun kdb-dotnet-run ()
  "Run the current .NET project."
  (interactive)
  (let ((proj (kdb-dotnet--find-project)))
    (compile (if proj
                 (format "dotnet run --project %s" (shell-quote-argument proj))
               "dotnet run"))))

(defun kdb-dotnet-build ()
  "Build the current .NET project."
  (interactive)
  (let ((proj (kdb-dotnet--find-project)))
    (compile (format "dotnet build -v n %s"
                     (if proj (shell-quote-argument proj) "")))))

(defun kdb-dotnet-test ()
  "Run tests for the current .NET project."
  (interactive)
  (let ((proj (kdb-dotnet--find-project)))
    (compile (format "dotnet test %s"
                     (if proj (shell-quote-argument proj) "")))))

(defun kdb-dotnet-command ()
  "Run a dotnet CLI command."
  (interactive)
  (let ((cmd (read-string "dotnet ")))
    (compile (concat "dotnet " cmd))))

(with-eval-after-load 'csharp-mode
  (dolist (binding '(("C-c n r" . kdb-dotnet-run)
                     ("C-c n b" . kdb-dotnet-build)
                     ("C-c n t" . kdb-dotnet-test)
                     ("C-c n !" . kdb-dotnet-command)))
    (define-key csharp-mode-map (kbd (car binding)) (cdr binding))))

;; EAT (Terminal) ========================================== ;;

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :ensure t
  :config
  ;; Performance settings
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t)

  ;; Windows-specific settings
  (when (eq system-type 'windows-nt)
    (setq eat-shell (or (executable-find "pwsh")
                        (executable-find "powershell")
                        (executable-find "bash")
                        "cmd.exe")))

  (setq eat-term-name "xterm-256color")

  ;; Eshell integration
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (add-hook 'eat-mode-hook
            (lambda ()
              (define-key eat-semi-char-mode-map (kbd "C-c C-c") 'eat-self-input)
              (define-key eat-semi-char-mode-map (kbd "C-c C-e") 'eat-emacs-mode)
              (define-key eat-semi-char-mode-map (kbd "C-c C-j") 'eat-char-mode))))


;; EXWM (Window Manager) =================================== ;;

;; Load EXWM early if we're in X11 session
(use-package exwm
  :ensure t
  :if (memq window-system '(x))
  :defer t
  :config
  ;; Shrink fringes to 1 pixel
  (fringe-mode 1))

(when (memq window-system '(x))
  (require 'init-exwm))

;; Tab Bar (Workspaces) ==================================== ;;

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t            ; Show tab numbers in tab names
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :bind (("C-c TAB n" . tab-bar-new-tab)
         ("C-c TAB r" . tab-bar-rename-tab)
         ("C-c TAB k" . tab-bar-close-tab)
         ("C-c TAB l" . tab-bar-switch-to-tab)
         ("C-c TAB f" . tab-bar-switch-to-next-tab)
         ("C-c TAB b" . tab-bar-switch-to-prev-tab)))


;; Evil Mode (Vim Emulation) =============================== ;;

(use-package evil
  :ensure t
  :defer t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-redo
        evil-respect-visual-line-mode t)
  :config
  (evil-set-leader 'normal (kbd "SPC")))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(defvar kdb-evil-buffer-list nil
  "List of buffer patterns where evil-mode is enabled.")

(defvar kdb-evil-project-list nil
  "List of project roots where evil-mode is enabled.
Set in personal.el, e.g. (setq kdb-evil-project-list \\='(\"/path/to/project\"))")

(defun kdb-evil-toggle ()
  "Toggle evil-mode globally."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (progn (evil-mode -1) (message "Evil mode disabled"))
    (evil-mode 1) (message "Evil mode enabled")))

(defun kdb-evil-toggle-buffer ()
  "Toggle evil-local-mode in current buffer."
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (progn (evil-local-mode -1) (message "Evil disabled in buffer"))
    (unless (bound-and-true-p evil-mode) (require 'evil))
    (evil-local-mode 1) (message "Evil enabled in buffer")))

(defun kdb-evil-auto-enable ()
  "Auto-enable evil-local-mode based on project or buffer patterns."
  (when (and (not (minibufferp))
             (not (bound-and-true-p evil-local-mode)))
    (let ((file (or buffer-file-name default-directory "")))
      (when (or (cl-some (lambda (pat) (string-match-p pat (buffer-name))) kdb-evil-buffer-list)
                (cl-some (lambda (root) (string-prefix-p (expand-file-name root) file)) kdb-evil-project-list))
        (unless (bound-and-true-p evil-mode) (require 'evil))
        (evil-local-mode 1)))))

(add-hook 'find-file-hook #'kdb-evil-auto-enable)

(global-set-key (kbd "C-c t v") #'kdb-evil-toggle)
(global-set-key (kbd "C-c t V") #'kdb-evil-toggle-buffer)

;; gptel (Claude Chat + Org Integration) ================== ;;

(use-package gptel
  :ensure t
  :config
  ;; Set API key in personal.el: (setq gptel-api-key "sk-ant-...")
  (setq gptel-model 'claude-sonnet-4-5
        gptel-default-mode 'org-mode     ; Chat buffers open in org-mode
        gptel-backend (gptel-make-anthropic "Claude" :stream t))

  ;; Org-mode integration: send subtrees, heading context, branching
  (setq gptel-org-branching-context t    ; Each org heading is its own context branch
        gptel-use-header-line t)

  ;; After a response, move point to end so next prompt is ready
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Tools — for actions Claude can take (context is handled by gptel-add/gptel-add-file)
  (gptel-make-tool
   :function (lambda (path old-text new-text)
               (let ((file (expand-file-name path)))
                 (unless (file-exists-p file)
                   (error "File not found: %s" path))
                 (with-current-buffer (find-file-noselect file)
                   (goto-char (point-min))
                   (if (search-forward old-text nil t)
                       (progn
                         (replace-match new-text t t)
                         (save-buffer)
                         (format "Replaced in %s" path))
                     (format "Text not found in %s" path)))))
   :name "edit_file"
   :description "Make a targeted edit in a file by replacing old_text with new_text."
   :args (list '(:name "path" :type string :description "File path")
               '(:name "old_text" :type string :description "Exact text to find and replace")
               '(:name "new_text" :type string :description "Replacement text"))
   :confirm t
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (command)
               (with-temp-buffer
                 (shell-command command (current-buffer))
                 (buffer-string)))
   :name "run_shell_command"
   :description "Run a shell command and return its output"
   :args (list '(:name "command" :type string :description "Shell command to execute"))
   :confirm t
   :category "system")

  ;; Quick coding session — adds current buffer as context
  (defun kdb-gptel-code ()
    "Start a gptel session with current buffer added as context."
    (interactive)
    (gptel-add-file (or buffer-file-name
                                (let ((tmp (make-temp-file "gptel-" nil
                                             (when (derived-mode-p 'prog-mode)
                                               (concat "." (file-name-extension (symbol-name major-mode)))))))
                                  (write-region (point-min) (point-max) tmp)
                                  tmp)))
    (gptel))

  ;; Add project directory as context
  (defun kdb-gptel-add-project ()
    "Add the current project's files to gptel context."
    (interactive)
    (let ((root (if (project-current)
                    (project-root (project-current))
                  (read-directory-name "Project root: "))))
      (gptel-add-file root)
      (message "Project %s added to context" root)))

  (setq gptel-log-file (expand-file-name "gptel-log.org" "~/.org/"))

  (defun kdb-gptel-send-capture ()
    "Send a :claude: tagged org entry to a gptel session.
If point is on a heading in an org buffer, use that entry.
Otherwise, search org files for :claude: tagged entries and prompt."
    (interactive)
    (let (heading entry-text)
      (if (and (derived-mode-p 'org-mode)
               (ignore-errors (org-back-to-heading t) t))
          ;; Use entry at point
          (save-excursion
            (org-back-to-heading t)
            (setq heading (org-get-heading t t t t)
                  entry-text (buffer-substring-no-properties
                              (point) (org-end-of-subtree t t))))
        ;; Search org files for :claude: entries
        (let* ((files (directory-files "~/.org" t "\\.org$"))
               (entries '()))
          (dolist (file files)
            (with-temp-buffer
              (insert-file-contents file)
              (org-mode)
              (goto-char (point-min))
              (while (re-search-forward "^\\*+ .+:claude:" nil t)
                (org-back-to-heading t)
                (let ((h (org-get-heading t t t t))
                      (text (buffer-substring-no-properties
                             (point) (org-end-of-subtree t t))))
                  (push (cons h text) entries)))))
          (unless entries
            (user-error "No :claude: tagged entries found in ~/.org/"))
          (let ((choice (completing-read "Send to Claude: "
                                         (mapcar #'car entries) nil t)))
            (setq heading choice
                  entry-text (cdr (assoc choice entries))))))
      ;; Extract prompt section if it exists
      (let* ((prompt (when (string-match
                            "^\\*+ Prompt\n\\(\\(?:.*\n?\\)*\\)" entry-text)
                       (string-trim (match-string 1 entry-text))))
             (buf-name (format "*Claude: %s*" heading))
             (buf (get-buffer-create buf-name)))
        (unless (and prompt (not (string-empty-p prompt)))
          (setq prompt (read-string "Prompt for Claude: ")))
        (with-current-buffer buf
          (org-mode)
          (erase-buffer)
          (gptel-mode 1)
          (insert "* Context\n\n")
          (insert entry-text)
          (insert "\n\n* Prompt\n\n")
          (insert prompt "\n"))
        (switch-to-buffer buf)
        (gptel-send))))

  :bind (("C-c l l" . gptel)            ; Open/switch to chat buffer
         ("C-c l s" . gptel-send)        ; Send prompt at point
         ("C-c l r" . gptel-rewrite)     ; Rewrite selected region in-place
         ("C-c l m" . gptel-menu)        ; Transient menu (model, params, etc.)
         ("C-c l a" . gptel-add)         ; Add region/buffer to context
         ("C-c l f" . gptel-add-file)    ; Add file to context
         ("C-c l c" . gptel-context-remove-all)    ; Clear context
         ("C-c l k" . kdb-gptel-code)              ; Code session (current buffer)
         ("C-c l p" . kdb-gptel-add-project)       ; Add project to context
         ("C-c l q" . kdb-gptel-send-capture)))    ; Send capture entry to Claude

;; Claude Session Archive ================================= ;;

;; Archive defaults to ~/.claude-archive
;; Set kdb-claude-export-directory in personal.el for Desktop exports
(autoload 'kdb-claude-sync "init-claude" "Sync all Claude sessions" t)
(autoload 'kdb-claude-browse "init-claude" "Browse Claude sessions" t)
(autoload 'kdb-claude-search "init-claude" "Search Claude sessions" t)
(autoload 'kdb-claude-import-export "init-claude" "Import Desktop export" t)
(global-set-key (kbd "C-c l b") #'kdb-claude-browse)
(global-set-key (kbd "C-c l /") #'kdb-claude-search)
(global-set-key (kbd "C-c l i") #'kdb-claude-import-export)
(global-set-key (kbd "C-c l S") #'kdb-claude-sync)


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
