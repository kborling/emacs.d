;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.6.0
;; Keywords: configuration
;; URL: https://github.com/kborling/emacs.d
;; Homepage: https://github.com/kborling/emacs.d
;; Package-Requires: ((emacs "31"))

;;; Commentary:

;; Copyright (C) 2026 Kevin Borling
;; My personal Emacs config.

;;; Code:

(require 'subr-x)

;;; ============================================================
;;;                    PERSONAL SETTINGS
;;; ============================================================

;; Load personal settings if they exist (not tracked in git)
(let ((personal-file (locate-user-emacs-file "personal.el")))
  (when (file-exists-p personal-file)
    (load personal-file)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Windows defaults to cp1252, which can't encode the quotes, ellipses and
;; emoji in Claude transcripts, so Emacs asked for a coding system on save.
(set-language-environment "UTF-8")

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
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Packages are already activated after early-init; doing it again here
;; (and parsing the archive lists) doubled the startup cost.  The archive
;; lists are only read when something actually needs installing.
(unless (bound-and-true-p package--activated)
  (package-activate-all))
;; Installs rebuild the quickstart file (early-init, Windows); build it the
;; first time too.
(when (and (bound-and-true-p package-quickstart)
           (not (file-exists-p (locate-user-emacs-file "package-quickstart.el"))))
  (run-with-idle-timer 2 nil (lambda ()
                               (require 'package)
                               (package-quickstart-refresh))))
;; A cached package list goes stale (MELPA drops old versions, deps move
;; on), so refresh it before the first install of the session.  Both entry
;; points: :vc dependencies go through `package-vc-install-dependencies',
;; which never calls `package-install' and only reports what's missing.
(defvar kdb--package-list-refreshed nil)
(defun kdb--refresh-package-list-once (&rest _)
  "Refresh the package list unless that already happened this session."
  (unless kdb--package-list-refreshed
    (setq kdb--package-list-refreshed t)
    (package-refresh-contents)))
(dolist (fn '(package-install package-vc-install-dependencies))
  (advice-add fn :before #'kdb--refresh-package-list-once))
;; use-package refreshes by itself when a package isn't in the (unread)
;; list; count that, or the first install downloads the list twice.
(advice-add 'package-refresh-contents :after
            (lambda (&rest _) (setq kdb--package-list-refreshed t)))

;;; Package Management ========================================= ;;

;; Checking installed packages loads package.el (slow on Windows); skip
;; it for anything already activated or on the load-path.
(defun kdb--package-present-p (name &rest _)
  "Non-nil if package NAME is activated or its library is found."
  (let ((name (use-package-as-symbol name)))
    (or (memq name (bound-and-true-p package-activated-list))
        (locate-library (symbol-name name)))))

(setq use-package-ensure-function
      (lambda (name args state &optional no-refresh)
        (unless (kdb--package-present-p name)
          (use-package-ensure-elpa name args state no-refresh))))
(advice-add 'use-package-vc-install :before-until
            (lambda (arg &rest _) (kdb--package-present-p (car arg))))

(setq use-package-always-ensure t
      package-native-compile t
      ;; :error, not :emergency, so failed package installs are visible.
      warning-minimum-level :error)

(setq package-install-upgrade-built-in t
      package-vc-register-as-project nil)

;; Popup/window management — consistent buffer placement
(setq display-buffer-alist
      '(;; Hide noise
        ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
         (display-buffer-no-window)
         (allow-no-window . t))
        ;; Bottom side window (ephemeral output)
        ("\\*\\(vc-git\\|vc-diff\\|Shell Command\\|Async Shell\\|compilation\\|grep\\|Occur\\).*"
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.3)
         (slot . 0))
        ;; Help/docs on the right
        ("\\*\\(Help\\|Apropos\\|info\\|Man \\|eldoc\\).*"
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.4)
         (slot . 0))
        ;; Completions small at bottom
        ("\\*Completions\\*"
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.25)
         (slot . 1))
        ;; Flymake/diagnostics at bottom
        ("\\*Flymake.*\\|\\*xref\\*"
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.25)
         (slot . 1))))

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

;; Clean mode line
(setq mode-line-compact 'long
      mode-line-position-column-format nil)    ; hide column, line is enough

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; Modified / read-only indicator
                (:eval (cond (buffer-read-only
                              (propertize " RO " 'face 'mode-line-emphasis))
                             ((buffer-modified-p)
                              (propertize " ** " 'face 'warning))
                             (t "    ")))
                ;; Buffer name
                mode-line-buffer-identification
                "  "
                ;; Major mode
                mode-line-modes
                "  "
                ;; VC branch
                (vc-mode vc-mode)
                "  "
                ;; Flymake errors/warnings
                (:eval (when (bound-and-true-p flymake-mode)
                         flymake-mode-line-counters))
                mode-line-format-right-align
                ;; Position (line number)
                mode-line-position
                "  "
                ;; Misc (time, battery, etc.)
                mode-line-misc-info
                mode-line-end-spaces))

;;; ============================================================
;;;                 UI AND APPEARANCE
;;; ============================================================

;; Cursor configuration - thick bar
(setq-default cursor-type '(bar . 3))  ; Bar cursor with width of 3 pixels
(setq cursor-in-non-selected-windows 'hollow)  ; Hollow cursor in inactive windows

;; Contextual menu with right mouse button
(when (or (display-graphic-p) (daemonp))
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

;; Frame sizing — large but not fullscreen (EXWM handles its own)
(unless (memq window-system '(x))  ; Skip on EXWM
  (when (or (display-graphic-p) (daemonp))
    (add-to-list 'default-frame-alist '(width . 120))
    (add-to-list 'default-frame-alist '(height . 45))))

;;; ============================================================
;;;                   SESSION MANAGEMENT
;;; ============================================================

;; Recent Files ============================================ ;;

(use-package recentf
  :ensure nil
  :defer t
  :init
  ;; setq, not :custom: customizing loads recentf at startup.
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-var-directory)
        recentf-max-saved-items 1000
        recentf-max-menu-items 20
        recentf-auto-cleanup 'never
        recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/opt/" "/.rustup/" "/ssh:" "/sudo:" "/node_modules/" "/nix/"))
  (run-with-idle-timer 1 nil #'recentf-mode)
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
(run-with-idle-timer 1 nil (lambda () (ignore-errors (display-battery-mode t))))

;; Smooth scrolling (GUI only)
(when (or (display-graphic-p) (daemonp))
  (setq pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-momentum-tick 0.016  ; ~60fps
        pixel-scroll-precision-momentum-seconds 0.75
        pixel-scroll-precision-large-scroll-height 40.0
        pixel-scroll-precision-interpolation-factor 30.0
        mouse-wheel-scroll-amount '(1 ((shift) . 5))
        mouse-wheel-progressive-speed nil
        scroll-preserve-screen-position t)
  (add-hook 'after-init-hook #'pixel-scroll-precision-mode))

;; Repeat mode — after C-x o, press o to keep switching windows, etc.
(add-hook 'after-init-hook #'repeat-mode)

;; Inline completion preview (ghost text)
(add-hook 'prog-mode-hook #'completion-preview-mode)

;; Spell checking — only when the checker actually works (a binary without
;; dictionaries, common on Windows, would warn in every buffer)
(defun kdb-spell-checker ()
  (when-let* ((prog (or (executable-find "aspell") (executable-find "hunspell"))))
    (with-temp-buffer
      (insert "hello\n")
      (when (zerop (call-process-region (point-min) (point-max) prog nil t nil "-a"))
        prog))))

(defun kdb-spell-setup ()
  "Enable flyspell if a working checker exists, in this and future buffers."
  (when-let* ((speller (kdb-spell-checker)))
    (setq ispell-program-name speller
          flyspell-issue-message-flag nil)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond ((derived-mode-p 'prog-mode) (flyspell-prog-mode))
              ((derived-mode-p 'text-mode) (flyspell-mode 1)))))))

;; Probing runs the checker (~0.3s), so do it once Emacs is idle.
(run-with-idle-timer 1.5 nil #'kdb-spell-setup)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;;; ============================================================
;;;                    VISUAL APPEARANCE
;;; ============================================================

;; Fonts ================================================ ;;

(setq-default line-spacing 0.20)        ; setopt would load cus-edit (~0.1s)

(defun kdb-apply-symbol-fonts ()
  "Draw the symbols agent-shell and org-modern use with Windows' own fonts.
Coding fonts lack most of them, and Emacs's fallback can pick a font
that shows boxes.  Box drawing falls back only where the font lacks it."
  (dolist (spec '(((#x2190 . #x21FF) . "Segoe UI Symbol")
                  ((#x2300 . #x23FF) . "Segoe UI Symbol")
                  ((#x25A0 . #x27BF) . "Segoe UI Symbol")
                  ((#x2980 . #x29FF) . "Cambria Math")
                  ((#x2B00 . #x2BFF) . "Segoe UI Symbol")))
    (set-fontset-font t (car spec) (cdr spec)))
  (set-fontset-font t '(#x2500 . #x259F) "Consolas" nil 'append)
  (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'prepend))

(defun kdb-apply-fonts ()
  "Set the default, fixed and variable-pitch fonts.
Needs a graphical frame: a daemon has none at startup, so fonts can't
be found until the first emacsclient frame opens."
  (when (display-graphic-p)
    (remove-hook 'server-after-make-frame-hook #'kdb-apply-fonts)
    (when (eq system-type 'windows-nt)
      (kdb-apply-symbol-fonts))
    (let* ((settings (cond
                      ((eq system-type 'windows-nt)    '(:size 110 :families ("Comic Code" "Rec Mono Semicasual" "Cascadia Code" "Consolas" "Courier New")))
                      ((eq system-type 'darwin)        '(:size 140 :families ("Overpass Mono" "Monaco" "Menlo" "monospace")))
                      (t '(:size 110 :families ("Comic Code" "JetBrains Mono" "DejaVu Sans Mono" "Liberation Mono" "monospace")))))  ; Linux, BSD, etc.
           (default-font-size (plist-get settings :size))
           (font-families (plist-get settings :families))
           (default-font-family (cl-find-if (lambda (font) (find-font (font-spec :family font))) font-families)))

      ;; Set the font if we found one available
      (when default-font-family
        (set-face-attribute 'default nil
                            :family default-font-family :weight 'medium :height default-font-size)
        (set-face-attribute 'fixed-pitch nil
                            :family default-font-family :height 1.0))

      (let ((vp-family (cl-find-if (lambda (f) (find-font (font-spec :family f)))
                                   '("FreeSans" "Helvetica Neue" "Segoe UI" "sans-serif"))))
        (when vp-family
          (set-face-attribute 'variable-pitch nil
                              :family vp-family :height 1.0 :weight 'regular))))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'kdb-apply-fonts)
  (kdb-apply-fonts))

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; Themes ================================================= ;;

(use-package uwu-theme
  :vc (:url "https://github.com/kborling/uwu-theme" :rev :newest)
  :config
  (setq
   uwu-distinct-line-numbers nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t
   uwu-cursor-color 'red)
  ;; (load-theme 'uwu t)
  )

(use-package fleury-theme
  :vc (:url "https://github.com/kborling/fleury-theme.el" :rev :newest)
  :config
  (setq fleury-distinct-line-numbers t)
  (load-theme 'fleury t))

(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package acme-theme
  :defer t
  :config
  ;; Accessibility fixes — keep acme character, improve readability
  (custom-theme-set-faces
   'acme
   '(font-lock-comment-face ((t (:foreground "#6E6E5E" :slant italic))))
   '(font-lock-doc-face ((t (:foreground "#6E6E5E" :slant italic))))
   '(font-lock-type-face ((t (:foreground "#5C5C00"))))
   '(font-lock-constant-face ((t (:foreground "#5C5C00"))))
   '(shadow ((t (:foreground "#7E7E6C"))))
   '(region ((t (:background "#C8DFC8"))))
   '(secondary-selection ((t (:background "#D8ECD8"))))))


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
            ("fd" "fd" t "Project file finder")
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
;; Declared special before the let below — eshell.el isn't loaded yet at
;; that point, and lexically binding it breaks eshell's own defvar.
(defvar eshell-buffer-name)
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
  "Create a new terminal: eat (GUI), term (terminal), or pwsh on Windows.
Names buffer after the directory for easier identification."
  (interactive)
  (let ((dir (abbreviate-file-name default-directory)))
    (cond ((eq system-type 'windows-nt)
           ;; eat launches everything through /usr/bin/env sh
           (let ((explicit-shell-file-name (or (executable-find "pwsh")
                                               (executable-find "powershell")
                                               "cmd.exe")))
             (shell (generate-new-buffer-name (format "*shell: %s*" dir)))))
          ((and (display-graphic-p) (fboundp 'eat))
           (eat nil (generate-new-buffer-name (format "*eat: %s*" dir))))
          (t (term (or (getenv "SHELL") "/bin/bash"))))))


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
    (dolist (binding '(("C-c h p" . hl-todo-previous)
                       ("C-c h n" . hl-todo-next)
                       ("C-c h o" . hl-todo-occur)))
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

    ;; Terminal management (C-c t)
    (dolist (binding '(("C-`" . kdb-eshell-toggle)
                       ("C-~" . kdb-eat-new)
                       ("C-c t e" . kdb-eshell-new)
                       ("C-c t t" . kdb-eat-new)
                       ("C-c t s" . shell)
                       ("C-c t d" . dired-jump-other-window)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Help extensions
    (dolist (binding '(("C-h M-k" . find-function-on-key)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Configuration shortcuts
    (dolist (binding '(("C-c e v" . config-visit)
                       ("C-c e r" . config-reload)
                       ("C-c e d" . kdb-doctor)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Toggles (C-c t)
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
  :defer t
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
                      (mode . agent-shell-mode)
                      (mode . kdb-claude-print-mode)))
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
                       (name . "^\\*Man ")))
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
   project-prune-zombie-projects '((prompt . project-prune-zombies-default)))

  ;; Use fd instead of find for project file listing (much faster)
  (when (executable-find "fd")
    (defvar kdb--project-files-cache (make-hash-table :test #'equal))
    (defvar kdb--project-files-refreshing (make-hash-table :test #'equal))

    (defun kdb--fd-args (roots)
      "fd arguments listing all files under ROOTS."
      (append '("--type" "f" "--hidden" "--follow" "--exclude" ".git")
              (mapcan (lambda (dir) (list "--search-path" (expand-file-name dir)))
                      roots)))

    (defun kdb--project-files-refresh (roots)
      "Re-list ROOTS with fd in the background and update the cache."
      (unless (gethash roots kdb--project-files-refreshing)
        (let ((buf (generate-new-buffer " *fd*")))
          (puthash roots t kdb--project-files-refreshing)
          (make-process
           :name "fd" :buffer buf :noquery t
           :command (cons "fd" (kdb--fd-args roots))
           :stderr (get-buffer-create " *fd-stderr*")
           :sentinel (lambda (proc _)
                       (unless (process-live-p proc)
                         (when (and (eq (process-status proc) 'exit)
                                    (zerop (process-exit-status proc)))
                           (puthash roots
                                    (with-current-buffer buf
                                      (split-string (buffer-string) "\n" t))
                                    kdb--project-files-cache))
                         (remhash roots kdb--project-files-refreshing)
                         (kill-buffer buf)))))))

    (defun kdb-project-files-fd (project &optional dirs)
      "Files in PROJECT (or DIRS) via fd, cached per root list.
The first call runs fd synchronously; later calls return the cache
and refresh it in the background."
      (let* ((roots (or dirs (list (project-root project))))
             (cached (gethash roots kdb--project-files-cache)))
        (if cached
            (progn (kdb--project-files-refresh roots) cached)
          (puthash roots (apply #'process-lines "fd" (kdb--fd-args roots))
                   kdb--project-files-cache))))
    (cl-defmethod project-files ((project (head vc)) &optional dirs)
      (kdb-project-files-fd project dirs))))


;; Eshell ================================================== ;;

(use-package eshell
  :ensure nil
  :commands eshell
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
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode css-ts-mode html-ts-mode
          c-ts-mode c++-ts-mode rust-ts-mode zig-ts-mode csharp-mode csharp-ts-mode)
         . eglot-ensure)
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
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("csharp-ls")))
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

  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Make sure Eldoc will show us all of the feedback at point."
              (setq-local eldoc-documentation-strategy
                          #'eldoc-documentation-compose)))

  ;; Remove inlay hints
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))

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


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-theme)   ; cross-platform theme switching (s-t / s-T)

;; Full path for askpass and for Claude sessions starting sibling sessions;
;; Emacs's bin dir isn't always on PATH (Windows).
(setenv "EMACSCLIENT"
        (expand-file-name (if (eq system-type 'windows-nt) "emacsclient.exe" "emacsclient")
                          invocation-directory))

;; SSH/Git passphrase prompts via emacs-askpass (subprocesses have no TTY).
(let ((askpass (seq-find #'file-exists-p
                         (list (expand-file-name "~/.local/bin/emacs-askpass")
                               (expand-file-name "~/dotfiles/bin/.local/bin/emacs-askpass")))))
  (when askpass
    (setenv "SSH_ASKPASS" askpass)
    (setenv "GIT_ASKPASS" askpass)
    ;; macOS/Windows have no DISPLAY, so ssh won't use askpass unforced
    (when (memq system-type '(darwin windows-nt))
      (setenv "SSH_ASKPASS_REQUIRE" "force"))))

;; server-running-p trusts any live PID; on Windows a reused PID leaves a
;; stale server file that blocks server-start and --daemon.
(when (eq system-type 'windows-nt)
  (let ((file (locate-user-emacs-file "server/server")))
    (when (file-exists-p file)
      (let ((pid (with-temp-buffer
                   (insert-file-contents-literally file)
                   (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
                        (string-to-number (match-string 1))))))
        (unless (and pid (string-prefix-p
                          "emacs" (or (alist-get 'comm (process-attributes pid)) "") t))
          (delete-file file))))))

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (or (daemonp) (server-running-p))
              (server-start))))

(defun kdb-load-init-vc ()
  "Load init-vc configuration once."
  (unless (featurep 'init-vc)
    (require 'init-vc)))

;; Only Git: otherwise every file visit probes 8 VC backends (slow on Windows).
(setq vc-handled-backends '(Git))
;; Shortly after startup, so diff-hl and the rest of init-vc are active.
(run-with-idle-timer 1 nil #'kdb-load-init-vc)

(autoload 'kdb-vc-transient "init-vc" "VC transient menu" t)
(global-set-key (kbd "C-c g") 'kdb-vc-transient)

(add-hook 'vc-dir-mode-hook #'kdb-load-init-vc)
(with-eval-after-load 'vc-dir (kdb-load-init-vc))
(with-eval-after-load 'log-view (kdb-load-init-vc))

(with-eval-after-load 'org
  (require 'init-org))

(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o d") #'deft)
;; Autoload from org, not init-org: loading org requires init-org, so
;; loading init-org directly would run it twice.
(autoload 'kdb/pandoc-convert "org" nil t)
(autoload 'kdb/markdown-export "org" nil t)
(dolist (binding '(("M" . kdb/markdown-to-org) ("o" . kdb/note-new)
                   ("RET" . kdb/note-capture) ("n" . kdb/add-contact)
                   ("m" . kdb/add-meeting-note) ("1" . kdb/log-1on1)
                   ("w" . kdb/log-win) ("i" . kdb/log-issue)
                   ("F" . kdb/log-feedback) ("u" . kdb/log-followup)
                   ("G" . kdb/set-goals) ("v" . kdb/view-person)
                   ("V" . kdb/view-person-all-years) ("r" . kdb/review-prep)
                   ("!" . kdb/attention) ("D" . kdb/daily-notes)
                   ("W" . kdb/weekly-review) ("f" . kdb/search-all)
                   ("j" . kdb/find-contact) ("s" . kdb/search-contacts)
                   ("C" . kdb/open-contacts) ("N" . kdb/open-notes)
                   ("p" . kdb/open-people)))
  (autoload (cdr binding) "org" nil t)
  (global-set-key (kbd (concat "C-c o " (car binding))) (cdr binding)))
(with-eval-after-load 'deft (require 'org))
(global-set-key (kbd "C-c o P") #'kdb/pandoc-convert)

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

(unless (or (display-graphic-p) (daemonp))
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
  :after corfu
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
         ("M-+" . tempel-insert))
  :config
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)
  (define-key tempel-map (kbd "C-g") #'tempel-abort))

;; Tempel Collection ======================================= ;;

(use-package tempel-collection
  :after tempel)

;; Tree-sitter (Built-in Emacs 31) ======================== ;;

;; Use tree-sitter modes for the languages whose grammar is installed.
;; `setq' skips the variable's :set, which is what adds the remaps; and t
;; would remap languages without a grammar too, which then ask to install
;; one on every visit.  Calling the setter directly avoids loading cus-edit.
(when (and (require 'treesit nil t) (treesit-available-p))
  (let ((langs '((csharp . c-sharp) (js . javascript) (c++ . cpp) (c-or-c++ . cpp)
                 (mhtml . html) (go-mod . gomod) (go-work . gowork))))
    (funcall (get 'treesit-enabled-modes 'custom-set) 'treesit-enabled-modes
             (seq-filter (lambda (mode)
                           (let ((name (intern (string-remove-suffix "-ts-mode" (symbol-name mode)))))
                             (treesit-language-available-p (alist-get name langs name))))
                         (seq-uniq (mapcar #'cdr treesit-major-mode-remap-alist))))))

;; Grammar sources for treesit-install-language-grammar
(with-eval-after-load 'treesit
  (dolist (source '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
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
  :init (setq markdown-command (if (executable-find "multimarkdown") "multimarkdown" "pandoc"))
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)
              ;; Export through org (C-c C-c e); the native one needs multimarkdown
              ([remap markdown-export] . kdb/markdown-export)))



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
  :vc (:url "https://github.com/kborling/angular-mode" :rev :newest)
  :defer t)

;; HTML Mode =============================================== ;;

(use-package mhtml-mode
  :ensure nil
  :mode "\\.cshtml\\'"
  :bind (:map mhtml-mode-map
              ("C-c C-d" . nil)))

(with-eval-after-load 'mhtml-ts-mode
  (define-key mhtml-ts-mode-map (kbd "C-c C-d") nil))


;; Memmet Mode ============================================= ;;

(use-package memmet-mode
  :vc (:url "https://github.com/kborling/memmet-mode" :rev :newest)
  :hook ((mhtml-mode mhtml-ts-mode) . memmet-mode)
  :config
  (with-eval-after-load 'mhtml-mode
    (keymap-set mhtml-mode-map "C-<tab>" #'memmet-expand))
  (with-eval-after-load 'mhtml-ts-mode
    (keymap-set mhtml-ts-mode-map "C-<tab>" #'memmet-expand)))


;; XML Mode ================================================ ;;

(use-package nxml-mode
  :ensure nil
  :defer t
  :mode "\\.csproj\\'")


;; Conf Mode =============================================== ;;

(use-package conf-mode
  :ensure nil
  :defer t
  :mode "\\.sln\\'")


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
    (define-key csharp-mode-map (kbd (car binding)) (cdr binding))
    (define-key csharp-ts-mode-map (kbd (car binding)) (cdr binding))))

;; EAT (Terminal) ========================================== ;;

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :ensure t
  :commands eat
  ;; Terminal rows are sized ignoring `line-spacing'; extra spacing overflows the window.
  :hook (eat-mode . (lambda () (setq-local line-spacing nil)))
  :init
  ;; Eshell integration (not on Windows: eat runs commands via /usr/bin/env sh)
  (unless (eq system-type 'windows-nt)
    (add-hook 'eshell-load-hook #'eat-eshell-mode)
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))
  :config
  ;; Performance settings
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t)

  (setq eat-term-name "xterm-256color")

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
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; One workspace (tab) per project, each with its own windows and buffer
;; list, so C-x b only offers this project's buffers.  Nothing is written
;; into projects: no todo file, no session files.
(use-package tabspaces
  :defer t
  :init
  ;; Loads dired and vc; keep it out of startup.
  (run-with-idle-timer 0.5 nil #'tabspaces-mode)
  :custom
  (tabspaces-keymap-prefix "C-c TAB")
  (tabspaces-use-filtered-buffers-as-default (not (eq window-system 'x))) ; EXWM apps stay global
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-initialize-project-with-vc nil)
  (tabspaces-session nil)
  :bind (:map tabspaces-command-map
              ("f" . tab-bar-switch-to-next-tab)
              ("p" . tab-bar-switch-to-prev-tab)))


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
  "List of buffer patterns where 'evil-mode' is enabled.")

(defvar kdb-evil-project-list nil
  "List of project roots where 'evil-mode' is enabled.
Set in personal.el, e.g. (setq kdb-evil-project-list \\='(\"/path/to/project\"))")

(defun kdb-evil-toggle ()
  "Toggle 'evil-mode' globally."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (progn (evil-mode -1) (message "Evil mode disabled"))
    (evil-mode 1) (message "Evil mode enabled")))

(defun kdb-evil-toggle-buffer ()
  "Toggle 'evil-local-mode' in current buffer."
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (progn (evil-local-mode -1) (message "Evil disabled in buffer"))
    (unless (bound-and-true-p evil-mode) (require 'evil))
    (evil-local-mode 1) (message "Evil enabled in buffer")))

(defun kdb-evil-auto-enable ()
  "Auto-enable 'evil-local-mode' based on project or buffer patterns."
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

;; Local models (Ollama) =================================== ;;
;; Coding models that fit a 4 GB GPU, run locally: completion as you type
;; (minuet) and chat/rewrite (gptel).  Setup: C-c a s.

(defvar kdb-local-models '("qwen2.5-coder:3b" "qwen2.5-coder:7b")
  "Ollama models: the first completes code as you type, the last chats.")

(defvar kdb-ollama-available 'unknown)

(defun kdb-ollama-available-p ()
  "Non-nil if Ollama is installed; looked up once."
  (when (eq kdb-ollama-available 'unknown)
    (setq kdb-ollama-available (and (executable-find "ollama") t)))
  kdb-ollama-available)

(defun kdb-ollama-ensure ()
  "Start the Ollama server unless it is already listening."
  (unless (ignore-errors
            (delete-process (make-network-process :name "ollama-probe" :host "localhost"
                                                  :service 11434 :nowait nil))
            t)
    (when (kdb-ollama-available-p)
      (let ((process-environment (cons "OLLAMA_KEEP_ALIVE=30m" process-environment)))
        (make-process :name "ollama" :buffer " *ollama*" :noquery t
                      :command '("ollama" "serve"))))))

(defun kdb-local-completion-maybe ()
  "Complete code as you type, if Ollama is installed."
  (when (kdb-ollama-available-p)
    (minuet-auto-suggestion-mode 1)))
(add-hook 'prog-mode-hook #'kdb-local-completion-maybe)

(defun kdb-local-models--ready ()
  "Turn on completion in open code buffers and say how to use it all."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'prog-mode) (minuet-auto-suggestion-mode 1))))
  (message "Local models ready: completion in code buffers (TAB accepts), C-c a a to chat"))

(defun kdb-local-models-setup ()
  "Get local coding models working: Ollama running, `kdb-local-models' pulled.
Safe to re-run; only missing models are downloaded."
  (interactive)
  (setq kdb-ollama-available 'unknown)
  (unless (kdb-ollama-available-p)
    (browse-url "https://ollama.com/download")
    (user-error "Install Ollama from the page just opened, then run C-c a s again"))
  (kdb-ollama-ensure)
  (let ((have :down))
    (dotimes (_ 20)
      (when (eq have :down)
        (setq have (condition-case nil
                       (mapcar (lambda (l) (car (split-string l)))
                               (cdr (process-lines "ollama" "list")))
                     (error (sleep-for 0.5) :down)))))
    (when (eq have :down)
      (user-error "Ollama didn't start; see the \" *ollama*\" buffer"))
    (if-let* ((missing (seq-difference kdb-local-models have)))
        (let ((buf "*Ollama setup*"))
          (async-shell-command (mapconcat (lambda (m) (concat "ollama pull " m)) missing " && ")
                               buf)
          (set-process-sentinel
           (get-buffer-process buf)
           (lambda (p _)
             (unless (process-live-p p)
               (if (zerop (process-exit-status p))
                   (kdb-local-models--ready)
                 (message "Pulling models failed; see %s" buf))))))
      (kdb-local-models--ready))))

(global-set-key (kbd "C-c a s") #'kdb-local-models-setup)

(use-package gptel
  :ensure t
  :bind (("C-c a a" . gptel)
         ("C-c a RET" . gptel-send)
         ("C-c a r" . gptel-rewrite)
         ("C-c a m" . gptel-menu))
  :config
  (kdb-ollama-ensure)
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434" :stream t
                        :models (mapcar #'intern (reverse kdb-local-models)))
        gptel-model (intern (car (last kdb-local-models)))))

(use-package minuet
  :ensure t
  :bind (("C-c a c" . minuet-show-suggestion)
         ("C-c a C" . minuet-auto-suggestion-mode)
         :map minuet-active-mode-map
         ("TAB" . minuet-accept-suggestion)
         ("M-a" . minuet-accept-suggestion-line)
         ("M-n" . minuet-next-suggestion)
         ("M-p" . minuet-previous-suggestion)
         ("M-e" . minuet-dismiss-suggestion))
  :config
  (kdb-ollama-ensure)
  (setq minuet-provider 'openai-fim-compatible
        minuet-n-completions 1
        minuet-context-window 1024
        ;; Room to load or swap in the model (one fits the GPU at a time).
        minuet-request-timeout 8)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key (lambda () "ollama"))
  (plist-put minuet-openai-fim-compatible-options :model (car kdb-local-models))
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

;; Claude ================================================= ;;
;; agent-shell runs Claude Code over ACP using the `claude' login (Max).
;; Needs node: npm install -g @agentclientprotocol/claude-agent-acp

;; Before any use-package below: installing a :vc package compiles it,
;; which loads agent-shell and runs its :config mid-install.
(autoload 'kdb-claude-agent-shell-subdir "init-claude")
(dolist (fn '(kdb-claude--capture-permission kdb-claude--track-permissions kdb-claude--notify
              kdb-claude--start-with kdb-claude-spawn kdb-claude-message
              kdb-claude--tag-client kdb-claude--render-markdown
              kdb-claude--track-artifacts kdb-claude--header-symbol-fonts
              kdb-claude--busy-frames kdb-claude--mask-icon kdb-claude--mask-header
              kdb-claude--steady-busy-frame))
  (autoload fn "init-claude-workflow"))
(dolist (spec '(("init-claude-workflow" kdb-claude kdb-claude-new
                 kdb-claude-send kdb-claude-start kdb-claude-answer kdb-claude-review
                 kdb-claude-watch-pr kdb-claude-schedule kdb-claude-run-block
                 kdb-claude-reply-to-org kdb-claude-usage kdb-claude-open-artifact
                 kdb-claude-set-busy-indicator kdb-claude-start-model)
                ("init-claude-print" kdb-claude-print)
                ("init-claude" kdb-claude-sync kdb-claude-sync-code-sessions
                 kdb-claude-search kdb-claude-import-export
                 kdb-claude-ask-archive kdb-claude-resume-archived kdb-claude-history
                 kdb-claude-setup)))
  (dolist (fn (cdr spec))
    (autoload fn (car spec) nil t)))

(use-package agent-shell
  :ensure t
  :commands (agent-shell-anthropic-start-claude-code agent-shell-send-file-to)
  :config
  ;; opencode's installer puts it in the user profile, not on PATH.
  (let ((opencode (expand-file-name ".opencode/bin" (or (getenv "USERPROFILE") "~"))))
    (when (file-directory-p opencode)
      (add-to-list 'exec-path opencode)))
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        ;; M-x agent-shell asks which agent; C-c l starts Claude directly.
        agent-shell-preferred-agent-config nil
        ;; Auto-approve file edits; shell commands still ask.
        agent-shell-anthropic-default-session-mode-id "acceptEdits"
        ;; Code block labels: RET copies (built in), x runs the block in
        ;; org babel, e opens it there to edit first.
        agent-shell-markdown-render-function #'kdb-claude--render-markdown
        ;; Resume from the history list (C-c l h), not a picker on every start.
        agent-shell-session-strategy 'new
        ;; Keep conversation data out of projects and this repo: the private
        ;; archive already has every transcript, so agent-shell's copy is off.
        agent-shell-transcript-file-path-function nil
        ;; ...and don't offer to save one when a session is killed.
        shell-maker-prompt-before-killing-buffer nil
        agent-shell-dot-subdir-function #'kdb-claude-agent-shell-subdir
        shell-maker-root-path (kdb-claude-agent-shell-subdir "")
        ;; Queue permission requests so C-c ' can answer them from anywhere.
        agent-shell-permission-responder-function #'kdb-claude--capture-permission
        ;; The adapter logs to stderr unless given a log dir, and acp.el
        ;; reports any stderr as an error, which aborts agent-review.
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "CLAUDE_AGENT_LOGS" (expand-file-name "claude-agent-logs" temporary-file-directory)))
  ;; The default, ImageMagick's import, is often missing on Linux; maim
  ;; (-s: drag a region) where installed.  macOS/Windows keep the default.
  (when (and (eq system-type 'gnu/linux) (executable-find "maim"))
    (setq agent-shell-screenshot-command '("maim" "-s")))
  (add-hook 'agent-shell-mode-hook #'kdb-claude--track-permissions)
  (add-hook 'agent-shell-mode-hook #'kdb-claude--track-artifacts)
  (setq agent-shell-busy-indicator-frames #'kdb-claude--busy-frames)
  (advice-add 'agent-shell--busy-indicator-frame :filter-return #'kdb-claude--steady-busy-frame)
  (advice-add 'agent-shell--fetch-agent-icon :before-until #'kdb-claude--mask-icon)
  (dolist (fn '(agent-shell--make-header-model agent-shell--header-model-refresh-frame))
    (advice-add fn :filter-return #'kdb-claude--mask-header))
  (when (eq system-type 'windows-nt)
    (advice-add 'agent-shell--make-header-model :filter-return #'kdb-claude--header-symbol-fonts))
  ;; Each session knows its id (KDB_CLAUDE_SESSION) for kdb-claude-message.
  (advice-add 'agent-shell-anthropic-make-claude-client :around #'kdb-claude--tag-client))

;; Peek: sessions grouped by project with live status; RET switches.
;; Label: Claude names the session.  (hq's HUD needs persp-mode; unused.)
(use-package agent-shell-hq
  :vc (:url "https://github.com/SreenivasVRao/agent-shell-hq" :rev :newest)
  :commands (agent-shell-hq-peek agent-shell-hq-label))

;; Second-opinion review of uncommitted changes; send findings back with S.
(use-package agent-review
  :vc (:url "https://github.com/nineluj/agent-review" :rev :newest)
  :commands agent-review)

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el" :rev :newest)
  :after agent-shell
  :demand t
  :config
  (setq agent-shell-attention-notify-function #'kdb-claude--notify)
  (agent-shell-attention-mode 1))

(global-set-key (kbd "C-c l") #'kdb-claude)
(global-set-key (kbd "C-c ;") #'agent-shell-hq-peek)
(global-set-key (kbd "C-c '") #'kdb-claude-answer)

;; Session transcripts stay out of recent files (and so out of var/recentf).
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (lambda (f)
                 (file-in-directory-p
                  f (or (bound-and-true-p kdb-claude-archive-dir) "~/.claude-archive")))))

;; Keep the archive current so search/ask/resume see recent sessions.
;; The conversion runs in a background Emacs, so idle time never freezes.
(run-with-idle-timer 300 t (lambda ()
                             (require 'init-claude)
                             (when (kdb-claude--archive-repo-p)
                               (with-demoted-errors "Claude archive sync: %S"
                                 (kdb-claude-sync-code-sessions-async)))))


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
