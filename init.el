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

;; Native compilation settings
(setq native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 4)

;; Rendering optimizations
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      jit-lock-defer-time 0
      ;; No stealth: it fontifies every buffer in the background,
      ;; multi-MB Claude sessions included, allocating as it goes.
      jit-lock-stealth-time nil
      bidi-inhibit-bpa t)

;; Timers (auto-revert, Claude sessions finishing, theme switches, agent
;; output) that message while you type a file name or command append
;; " [their text]" to it.  Not shown then; *Messages* still has them.
(defvar kdb-in-timer nil
  "While a timer runs, the minibuffer depth it started at.")

(defun kdb--mark-timer (fn &rest args)
  "Run timer handler FN with ARGS, `kdb-in-timer' bound."
  (let ((kdb-in-timer (minibuffer-depth)))
    (apply fn args)))
(advice-add 'timer-event-handler :around #'kdb--mark-timer)

(defun kdb-quiet-timer-message (_message)
  "Non-nil, so not shown, for a timer's message over a minibuffer in use.
Only one already open when the timer started: a prompt the timer opens
itself shows messages as usual.  For `set-message-functions'."
  (and kdb-in-timer (> kdb-in-timer 0) (= kdb-in-timer (minibuffer-depth)) t))
(setq set-message-functions '(kdb-quiet-timer-message set-minibuffer-message))

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
      warning-minimum-level :warning)

(setq package-install-upgrade-built-in t
      package-vc-register-as-project nil)

;; A new window takes its share from all those beside it, not half of one.
(setq window-combination-resize t)

;; Popup/window management — consistent buffer placement
(setq display-buffer-alist
      '(;; Hide noise
        ("\\`\\*Compile-Log\\*\\'"
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
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 set-mark-command-repeat-pop t
 global-mark-ring-max 50000
 ;; Built-in defaults
 view-read-only t
 completion-cycle-threshold 5
 delete-by-moving-to-trash t)

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
  (when (equal (buffer-name (get-buffer buffer)) "*Messages*")   ; BUFFER may be a name
    (with-current-buffer buffer
      (goto-char (point-max))
      (when-let* ((window (get-buffer-window buffer)))
        (set-window-point window (point-max))))))

(advice-add 'display-buffer :after #'kdb-messages-buffer-advice)

;; Frame sizing — large but not fullscreen (EXWM handles its own)
(unless (and (memq window-system '(x))
             (equal (getenv "DESKTOP_SESSION") "exwm"))
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
        recentf-autosave-interval 300
        recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/opt/" "/.rustup/" "/ssh:" "/sudo:" "/node_modules/" "/nix/"))
  (run-with-idle-timer 1 nil #'recentf-mode)
  :config
  ;; recentf-autosave-interval is new in Emacs 31; before it, a timer of our
  ;; own, in a defvar so a reload adds no second one.
  (when (< emacs-major-version 31)
    (defvar kdb-recentf-save-timer
      (run-at-time 300 300 (lambda () (let ((inhibit-message t)) (recentf-save-list)))))))

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
              fill-column 80
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
(setq-default word-wrap t)              ; wrap long lines between words, not inside them
(setq truncate-partial-width-windows nil) ; wrap side-by-side windows too, don't cut lines off

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
  :defer t
  :init
  ;; Before the theme file loads: it reads these as it defines its faces.
  (setq
   uwu-distinct-line-numbers nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t
   uwu-cursor-color 'red))

;; load-theme evaluates the theme file; a require first did it twice.
(use-package fleury-theme
  :vc (:url "https://github.com/kborling/fleury-theme.el" :rev :newest)
  :defer t
  :init
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
            ("sqlcmd" "sqlcmd" nil "Azure SQL (the Go sqlcmd: winget install sqlcmd, brew install sqlcmd)")
            ("psql" "psql" nil "PostgreSQL")
            ("az" "az" nil "Azure CLI: signs in to Azure SQL and PostgreSQL")
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
  "Reload ~/.emacs.d/init.el at runtime.
Then the Claude module again: init.el resets `display-buffer-alist', and
`require' wouldn't re-add what that module put there."
  (interactive)
  (load-file (expand-file-name (locate-user-emacs-file "init.el")))
  (load "init-claude-packages" nil t))

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

(defun kdb-shell--drop-bash-job-warnings (output)
  "OUTPUT without the job-control warnings bash prints on a pipe."
  (replace-regexp-in-string
   "^bash: \\(cannot set terminal process group.*\\|no job control in this shell\\)\r?\n" "" output))

(defun kdb-shell--quiet-bash ()
  (add-hook 'comint-preoutput-filter-functions #'kdb-shell--drop-bash-job-warnings nil t)
  ;; pwsh on a pipe echoes each command back; drop the copy.
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (when (string-match-p "pwsh\\|powershell" (car (process-command proc)))
      (setq-local comint-process-echoes t))))
(add-hook 'shell-mode-hook #'kdb-shell--quiet-bash)


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
              ;; After term's own sentinel, which this used to replace.
              (add-function :after (process-sentinel (get-buffer-process buf))
                            (lambda (_proc event)
                              (when (string= event "finished\n")
                                (kill-buffer buf)))))))

;;; ============================================================
;;;                   ESSENTIAL PACKAGES
;;; ============================================================

;; Auto Revert ============================================= ;;

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :init
  ;; File notifications instead of stat-ing every buffer's file every 5s.
  (setq auto-revert-avoid-polling t))


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

;; Azure SQL and Azure Database for PostgreSQL, signed in with Entra ID
;; through the Azure CLI (az login), the way VS Code's mssql extension and
;; `psql' with an az token do.  C-c C-a connects, C-c C-c runs the region
;; or the whole script, C-c C-u switches database, C-c C-o runs it into
;; org tables (C-c o x there exports CSV or Excel), C-c C-t lists tables.

(defvar kdb-sql-connections nil
  "Saved connections for `kdb-sql-connect', as (NAME . PLIST); set in personal.el.
PLIST has :product, `ms' (Azure SQL) or `postgres', :server, :database,
and for postgres :user, the Entra user or group the server knows (and
:port, default 5432).  VS Code's saved mssql connections are offered too:
  (setq kdb-sql-connections
        \\='((\"dev\" :product ms :server \"dev.database.windows.net\" :database \"app\")
          (\"pg-dev\" :product postgres :server \"pg-dev.postgres.database.azure.com\"
           :database \"postgres\" :user \"db-admins\")))")

(defvar kdb-sql--args nil
  "The exact program arguments for the connection being started.")

(defun kdb-sql--comint (product _options &optional buf-name)
  "Start PRODUCT's interpreter with exactly `kdb-sql--args'.
sql.el's own builders add -E (Windows sign-in) to sqlcmd when there is no
user or password, which overrides Entra ID, and fill in -S/-d/-U from the
previous connection's values: what made connecting work only sometimes."
  (unless kdb-sql--args
    (user-error "Connect with C-c C-a (kdb-sql-connect)"))
  (sql-comint product kdb-sql--args buf-name))

(defun kdb-sql--strip-jsonc ()
  "Turn the JSON with comments and trailing commas VS Code writes into JSON.
Strings are matched first and kept, so // in a URL stays.  The patterns
are unrolled, so a long string or comment can't overflow the matcher."
  (dolist (junk '("//[^\n]*\\|/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" ",\\([ \t\r\n]*[]}]\\)"))
    (goto-char (point-min))
    (while (re-search-forward (concat "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\"\\|\\(" junk "\\)") nil t)
      (when (match-beginning 1)
        (replace-match (or (match-string 2) "") t t)))))

(defun kdb-sql--vscode-connections ()
  "VS Code's saved mssql connections, as in `kdb-sql-connections'."
  (let ((file (expand-file-name
               "Code/User/settings.json"
               (pcase system-type
                 ('windows-nt (getenv "APPDATA"))
                 ('darwin "~/Library/Application Support")
                 (_ "~/.config")))))
    ;; Whatever is wrong with the file, saved connections still work.
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (kdb-sql--strip-jsonc)
          (goto-char (point-min))
          (mapcar (lambda (c)
                    (let-alist c
                      (list (or .profileName .server)
                            :product 'ms :server .server :database .database
                            :user (and (equal .authenticationType "SqlLogin") .user))))
                  (alist-get 'mssql.connections
                             (json-parse-buffer :object-type 'alist :array-type 'list
                                                :null-object nil))))))))

(defun kdb-sql--lines (program &rest args)
  "PROGRAM's output lines, without its stderr (az's warnings, for one).
Signals an error naming PROGRAM when it fails or isn't installed."
  (with-temp-buffer
    (let ((status (condition-case nil
                      (apply #'call-process program nil '(t nil) nil args)
                    (file-missing (user-error "%s isn't installed or isn't on PATH" program)))))
      (unless (eq status 0)
        (error "%s exited with %s" program status))
      (split-string (buffer-string) "[\r\n]+" t "[ \t]+"))))

(defvar kdb-sql--token nil
  "(TIME . TOKEN): the last Azure CLI token, reused for a few minutes.")

(defun kdb-sql--az-token ()
  "An Entra ID token for Azure Database for PostgreSQL, from the Azure CLI.
Kept 5 minutes, so listing databases and then connecting asks az once."
  (if (and kdb-sql--token (< (float-time (time-since (car kdb-sql--token))) 300))
      (cdr kdb-sql--token)
    (let ((token (condition-case nil
                     (car (kdb-sql--lines "az" "account" "get-access-token" "--resource-type"
                                          "oss-rdbms" "--query" "accessToken" "-o" "tsv"))
                   (error nil))))
      (unless token
        (user-error "No token from the Azure CLI: run az login, then connect again"))
      (setq kdb-sql--token (cons (current-time) token))
      token)))

(defun kdb-sql--pg-environment ()
  "psql's environment: the az token as password, SSL, a short connect timeout."
  (append (list (concat "PGPASSWORD=" (kdb-sql--az-token)) "PGSSLMODE=require"
                "PGCONNECT_TIMEOUT=10")
          process-environment))

(defvar-local kdb-sql--conn nil
  "In a SQLi buffer, the connection it was started with, database included.")

(defun kdb-sql--databases (product server user)
  "The databases on SERVER that USER can open, or nil if they can't be listed.
A SQL login (ms with USER) isn't listed: it would ask for the password twice."
  (ignore-errors
    (pcase product
      ('ms (unless user
             (kdb-sql--lines (or (bound-and-true-p sql-ms-program) "sqlcmd")
                             (concat "--server=" server)
                             "--authentication-method=ActiveDirectoryAzCli" "--login-timeOut=5"
                             (concat "--query=SET NOCOUNT ON; SELECT name FROM sys.databases"
                                     " WHERE state_desc = 'ONLINE' ORDER BY name")
                             "-h" "-1" "-W")))
      ('postgres
       (let ((process-environment (kdb-sql--pg-environment)))
         (kdb-sql--lines "psql" "-X" "-At" "-h" server "-U" user "-d" "postgres" "-c"
                         (concat "SELECT datname FROM pg_database"
                                 " WHERE datallowconn AND NOT datistemplate ORDER BY 1")))))))

(defun kdb-sql--pick-database (product server user)
  "Ask for a database on SERVER, from its list when it can be read."
  (if-let* ((dbs (kdb-sql--databases product server user)))
      (completing-read (format "Database on %s: " server) dbs nil t)
    (read-string (format "Database on %s (RET for its default): " server))))

(defun kdb-sql-connect (name)
  "Connect to the saved database NAME, or a new one typed in.
A session already open under NAME is replaced, so this also reconnects:
after an expired token, or to start over on another database."
  (interactive
   (list (completing-read "Connection: "
                          ;; A saved one of the same name comes first and wins.
                          (delete-dups (mapcar #'car (append kdb-sql-connections
                                                             (kdb-sql--vscode-connections)))))))
  (require 'sql)
  (let* ((from (current-buffer))         ; sql-ms/sql-postgres make the SQLi buffer current
         (conn (or (cdr (assoc name (append kdb-sql-connections (kdb-sql--vscode-connections))))
                   (let ((product (intern (completing-read "Product: " '("ms" "postgres") nil t))))
                     (list :product product
                           :server (read-string "Server: ")
                           :database (read-string "Database (RET to pick from the server): ")
                           :user (and (eq product 'postgres) (read-string "Entra user or group: "))))))
         (product (plist-get conn :product))
         (server (plist-get conn :server))
         (user (plist-get conn :user))
         (buf-name (format "*SQL: %s*" name)))
    (unless (memq product '(ms postgres))
      (user-error "%s needs :product ms or postgres, not %S" name product))
    ;; VS Code leaves it to a dropdown; without one the server's default
    ;; database is used, and the script's tables aren't there.
    (let ((database (let ((d (plist-get conn :database)))
                      (if (seq-empty-p d) (kdb-sql--pick-database product server user) d))))
      ;; sql.el only shows a live session of the same name, it doesn't reconnect.
      (when-let* ((proc (get-buffer-process buf-name)))
        (delete-process proc))
      (if (eq product 'ms)
          ;; Long options with =: the Go sqlcmd quietly falls back to Windows
          ;; sign-in when they're mixed with -G or follow -d.
          (let* ((kdb-sql--args
                  ;; -W: columns as wide as their values, not their types
                  (append (kdb-sql--ms-args (plist-put (copy-sequence conn) :database database))
                          (list "-W")))
                 (process-environment
                  (if user
                      (cons (concat "SQLCMDPASSWORD=" (read-passwd (format "Password for %s: " user)))
                            process-environment)
                    process-environment)))
            (sql-ms buf-name))
        (let ((kdb-sql--args (append (list "-X" "-P" "pager=off")
                                     (kdb-sql--pg-args
                                      (plist-put (copy-sequence conn) :database database))))
              ;; Read at sign-in only, so an expired token doesn't end the session.
              (process-environment (kdb-sql--pg-environment)))
          (sql-postgres buf-name)))
      (when-let* ((sqli (get-buffer buf-name)))
        (with-current-buffer sqli
          (setq kdb-sql--conn (plist-put (copy-sequence conn) :database database)))))
    ;; Back to the script, the results showing beside it: connecting moves
    ;; point to the empty SQLi buffer, which looks like a query that failed.
    (when (with-current-buffer from (derived-mode-p 'sql-mode))
      (with-current-buffer from (sql-set-product product))
      (when-let* ((win (get-buffer-window from)))
        (select-window win)))))

(defun kdb-sql--use-command (product database)
  "The command that switches a PRODUCT session to DATABASE, newline included."
  (if (eq product 'postgres)
      ;; psql reconnects with the az token it started with, good for about
      ;; an hour; after that, C-c C-a again.
      (format "\\connect \"%s\"\n" (string-replace "\"" "\"\"" database))
    (format "USE [%s];\ngo\n" (string-replace "]" "]]" database))))

(defun kdb-sql-use-database (database)
  "Switch the connected session to DATABASE, picked from the server's list.
From a script, its session; from a SQLi buffer, that one."
  (interactive
   (let* ((sqli (kdb-sql--session))
          (conn (or (buffer-local-value 'kdb-sql--conn sqli)
                    (user-error "%s wasn't started by kdb-sql-connect" (buffer-name sqli)))))
     (list (kdb-sql--pick-database (plist-get conn :product) (plist-get conn :server)
                                   (plist-get conn :user)))))
  (let ((sqli (kdb-sql--session)))
    (with-current-buffer sqli
      (setq kdb-sql--conn (plist-put kdb-sql--conn :database database))
      ;; Straight to the process, not sql-send-string: that adds psql's \g,
      ;; which would run the previous query again on the new database.
      (comint-send-string (get-buffer-process sqli)
                          (kdb-sql--use-command (plist-get kdb-sql--conn :product) database)))
    (message "%s: now on %s" (buffer-name sqli) database)))

(defun kdb-sql--session ()
  "The SQLi buffer for the current buffer: itself, or its script's session."
  (cond ((derived-mode-p 'sql-interactive-mode) (current-buffer))
        ((sql-buffer-live-p sql-buffer) (get-buffer sql-buffer))
        (t (user-error "Not connected: C-c C-a first"))))

(defun kdb-sql-send-dwim ()
  "Run the region, or else the whole buffer, as VS Code does.
Not the paragraph: a script's DECLAREs and the statements using them,
often with blank lines between, have to go as one batch."
  (interactive)
  (if (use-region-p)
      (sql-send-region (region-beginning) (region-end))
    (sql-send-buffer)))

;; Results as org tables, and a list of tables.  Both run apart from the
;; session, in the background, on its server and database: what the
;; session set up itself (USE, temp tables) isn't there.

(defconst kdb-sql--us (string #x1f) "Column separator in output read by machine.")
(defconst kdb-sql--rs (string #x1e) "Row separator in psql's output read by machine.")

(defun kdb-sql--ms-args (conn)
  "sqlcmd's sign-in arguments for CONN: long options only (see `kdb-sql-connect')."
  (append (list (concat "--server=" (plist-get conn :server)))
          (if (plist-get conn :user)
              (list (concat "--user-name=" (plist-get conn :user)))
            (list "--authentication-method=ActiveDirectoryAzCli"))
          (unless (seq-empty-p (plist-get conn :database))
            (list (concat "--database-name=" (plist-get conn :database))))
          (list "--login-timeOut=10")))

(defun kdb-sql--pg-args (conn)
  "psql's connection arguments for CONN."
  (list "-h" (plist-get conn :server)
        "-p" (number-to-string (or (plist-get conn :port) 5432))
        "-U" (plist-get conn :user) "-d" (plist-get conn :database)))

(defun kdb-sql--parse (product output)
  "PRODUCT's machine-read OUTPUT as a list of (table HEADER ROWS) and (text STRING)."
  (let ((split (lambda (rec) (split-string rec kdb-sql--us)))
        (output (string-replace "\r\n" "\n" output)))
    (if (eq product 'postgres)
        ;; header RS row RS ... RS (N rows) LF: one result set after another.
        ;; The LF, which psql always ends the footer with, tells it from a
        ;; value starting "(1 row)".  A value holding RS or US still splits.
        (let ((footer (concat kdb-sql--rs "([0-9]+ rows?)\n"))
              (pos 0) results)
          (while (string-match footer output pos)
            ;; Positions first: split-string and string-trim reset the match data.
            (let* ((end (match-end 0))
                   (recs (split-string (substring output pos (match-beginning 0)) kdb-sql--rs)))
              (push (list 'table (funcall split (string-trim-left (car recs)))
                          (mapcar split (cdr recs)))
                    results)
              (setq pos end)))
          (let ((rest (string-trim (substring output pos))))
            (unless (string-empty-p rest) (push (list 'text rest) results)))
          (nreverse results))
      ;; sqlcmd: blocks between blank lines; a table's second line is dashes.
      ;; A lone unnamed column (SELECT COUNT(*)) has a blank header, which
      ;; the trimming takes: the block then starts with the dashes.
      (mapcar (lambda (block)
                (let ((lines (split-string block "\n")))
                  (when (string-match-p (concat "\\`[-" kdb-sql--us "]+\\'") (car lines))
                    (push "" lines))
                  (if (and (cdr lines)
                           (string-match-p (concat "\\`[-" kdb-sql--us "]+\\'") (cadr lines)))
                      (list 'table (funcall split (car lines)) (mapcar split (cddr lines)))
                    (list 'text block))))
              (split-string output "\n[ \t]*\n" t "[ \t\n]+")))))

(defun kdb-sql--query (conn sql callback)
  "Run SQL on CONN in the background, then call CALLBACK with its results.
They're as `kdb-sql--parse' gives them, with anything on stderr (errors,
notices) last, as text."
  (let* ((product (plist-get conn :product))
         ;; First: the password prompt and the az token can quit or fail,
         ;; before there's a file, buffer or pipe to leave behind.
         (process-environment
          (cond ((eq product 'postgres)
                 (cons "PGCLIENTENCODING=UTF8" (kdb-sql--pg-environment)))
                ((plist-get conn :user)
                 (cons (concat "SQLCMDPASSWORD="
                               (read-passwd (format "Password for %s: " (plist-get conn :user))))
                       process-environment))
                (t process-environment)))
         (file (make-temp-file "kdb-sql" nil ".sql"))
         (out (generate-new-buffer " *kdb-sql out*"))
         (err (generate-new-buffer " *kdb-sql err*"))
         ;; Its own pipe, quiet: the default sentinel would write
         ;; "Process ... finished" into the errors shown with the results.
         (err-pipe (make-pipe-process :name "kdb-sql-query stderr" :buffer err
                                      :noquery t :sentinel #'ignore))
         (coding-system-for-write 'utf-8))
    (with-temp-file file
      ;; In a batch of its own: CREATE VIEW, PROCEDURE and the like must
      ;; come first in theirs.  It's a session setting, so it carries over.
      (insert (if (eq product 'postgres) sql (concat "SET NOCOUNT ON;\nGO\n" sql)) "\n"))
    (condition-case err-data
        (make-process
         :name "kdb-sql-query" :buffer out :stderr err-pipe :noquery t
         :coding 'utf-8-dos               ; sqlcmd ends lines with CRLF on Windows
         :command (if (eq product 'postgres)
                      (append (list "psql" "-X" "-q" "-A" "-F" kdb-sql--us "-R" kdb-sql--rs
                                    "-P" "footer=on")
                              (kdb-sql--pg-args conn) (list "-f" file))
                    ;; -s before -i: -i takes the rest as more input files.
                    ;; -y 8000: (max) and xml values whole, not cut at 256.
                    ;; -r 1: PRINT and errors to stderr, not between tables.
                    ;; -x: no $(name) substitution in the script.
                    (append (list (or (bound-and-true-p sql-ms-program) "sqlcmd"))
                            (kdb-sql--ms-args conn)
                            (list "-s" kdb-sql--us "-W" "-y" "8000" "-r" "1" "-x" "-i" file)))
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             ;; stderr comes through its own pipe, which may still be draining:
             ;; until it's quiet for 0.2 s.
             (while (accept-process-output err-pipe 0.2))
             (delete-process err-pipe)
             (let ((results (kdb-sql--parse product (with-current-buffer out (buffer-string))))
                   (errors (string-trim (with-current-buffer err (buffer-string)))))
               (kill-buffer out)
               (kill-buffer err)
               (delete-file file)
               (funcall callback (if (string-empty-p errors) results
                                   (append results (list (list 'text errors)))))))))
      ;; sqlcmd or psql not found: nothing to leave behind.
      (error (delete-process err-pipe)
             (kill-buffer out)
             (kill-buffer err)
             (delete-file file)
             (signal (car err-data) (cdr err-data))))))

(defun kdb-sql--org-cell (value)
  "VALUE as an org table cell: | and newlines would break the table."
  (replace-regexp-in-string "[\r\n]+" " " (string-replace "|" "¦" value)))

(defun kdb-sql--show-results (name results title)
  "Show RESULTS in org buffer NAME, under TITLE; return the buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t) (n 0))
      (erase-buffer)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (insert "#+TITLE: " title "\n"
              "# " (format-time-string "%F %T")
              ".  In a table, C-c o x exports it to CSV or Excel.\n")
      (dolist (r results)
        (pcase r
          (`(table ,header ,rows)
           (insert (format "\n* Result %d (%d row%s)\n" (setq n (1+ n)) (length rows)
                           (if (= 1 (length rows)) "" "s")))
           (let ((start (point)))
             (dolist (row (cons header rows))
               (insert "| " (mapconcat #'kdb-sql--org-cell row " | ") " |\n")
               (when (eq row header) (insert "|-\n")))
             ;; Aligning is slow on big results; unaligned, it's still a table.
             (when (< (length rows) 2000)
               (save-excursion (goto-char start) (org-table-align)))))
          (`(text ,text)
           (insert "\n#+begin_example\n" text "\n#+end_example\n"))))
      ;; The Go sqlcmd prints nothing at all for a result set without rows.
      (unless results
        (insert "\nNo rows.\n"))
      (goto-char (point-min))
      (re-search-forward "^|" nil t)
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun kdb-sql--session-conn ()
  "A copy of the connection of the current script's session (or SQLi buffer's).
A copy: switching the session's database (C-c C-u) changes its own in place,
and a table list opened before must keep the database it lists."
  (let ((sqli (kdb-sql--session)))
    (copy-sequence
     (or (buffer-local-value 'kdb-sql--conn sqli)
         (user-error "%s wasn't started by kdb-sql-connect" (buffer-name sqli))))))

(defun kdb-sql--run-and-show (conn sql name title)
  "Run SQL on CONN in the background, then show its results in NAME under TITLE."
  (message "Running on %s..." (or (plist-get conn :database) (plist-get conn :server)))
  (kdb-sql--query conn sql
                  (lambda (results)
                    (display-buffer (kdb-sql--show-results name results title))
                    (message "%s: %d result set(s)" title
                             (seq-count (lambda (r) (eq (car r) 'table)) results)))))

(defun kdb-sql-run-to-org ()
  "Run the region, or else the whole script, with the results as org tables.
On the database of the script's session (C-c C-a), in the background."
  (interactive)
  (let* ((conn (kdb-sql--session-conn))
         (sql (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (kdb-sql--run-and-show conn sql (format "*SQL results: %s*" (buffer-name))
                           (format "%s on %s" (buffer-name)
                                   (or (plist-get conn :database) (plist-get conn :server))))))

(defun kdb-sql--ident (product name)
  "NAME quoted as a PRODUCT identifier."
  (if (eq product 'postgres)
      (format "\"%s\"" (string-replace "\"" "\"\"" name))
    (format "[%s]" (string-replace "]" "]]" name))))

(defun kdb-sql--literal (value)
  "VALUE as an SQL string literal."
  (format "'%s'" (string-replace "'" "''" value)))

(defvar-local kdb-sql--script nil
  "In a table list, the script it was opened from, for `kdb-sql-tables-insert'.")

(defvar-keymap kdb-sql-tables-mode-map
  :doc "Keys in a list of tables."
  "RET" #'kdb-sql-tables-columns
  "s" #'kdb-sql-tables-select
  "w" #'kdb-sql-tables-copy
  "i" #'kdb-sql-tables-insert
  "g" #'kdb-sql-tables-refresh)

(define-derived-mode kdb-sql-tables-mode tabulated-list-mode "SQL Tables"
  "The tables and views of a database.
\\<kdb-sql-tables-mode-map>\\[kdb-sql-tables-columns] its columns, \\[kdb-sql-tables-select] its first 100 rows, \\[kdb-sql-tables-insert] its name into the script, \\[kdb-sql-tables-copy] copy the name, \\[kdb-sql-tables-refresh] refresh."
  (setq tabulated-list-format [("Schema" 24 t) ("Name" 56 t) ("Type" 12 t)]
        tabulated-list-sort-key '("Schema" . nil))
  (tabulated-list-init-header))

(defun kdb-sql-tables ()
  "List the tables and views of the script's session database, dired style."
  (interactive)
  (let ((conn (kdb-sql--session-conn))
        (script (current-buffer)))
    (message "Listing tables on %s..." (plist-get conn :database))
    (kdb-sql--query
     ;; Upper case for SQL Server databases with a case-sensitive collation;
     ;; PostgreSQL folds unquoted names to lower case.
     conn (concat "SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES"
                  " WHERE TABLE_SCHEMA NOT IN ('pg_catalog', 'information_schema', 'sys')"
                  " ORDER BY 1, 2;")
     (lambda (results)
       (let ((table (seq-find (lambda (r) (eq (car r) 'table)) results)))
         (unless table
           (user-error "No tables listed: %s" (or (cadr (assq 'text results)) "no output")))
         (with-current-buffer (get-buffer-create
                               (format "*SQL tables: %s*" (or (plist-get conn :database)
                                                              (plist-get conn :server))))
           (kdb-sql-tables-mode)
           (setq kdb-sql--conn conn
                 kdb-sql--script script
                 tabulated-list-entries
                 (mapcar (lambda (row)
                           (list (cons (nth 0 row) (nth 1 row))
                                 (vector (nth 0 row) (nth 1 row)
                                         (string-remove-prefix "BASE " (nth 2 row)))))
                         (nth 2 table)))
           (tabulated-list-print)
           (pop-to-buffer (current-buffer))))))))

(defun kdb-sql--tables-at-point ()
  "The (SCHEMA . TABLE) on this line."
  (or (tabulated-list-get-id) (user-error "No table on this line")))

(defun kdb-sql--qualified (product id)
  "ID, (SCHEMA . TABLE), as a quoted PRODUCT name."
  (concat (kdb-sql--ident product (car id)) "." (kdb-sql--ident product (cdr id))))

(defun kdb-sql-tables-columns ()
  "Show the columns of the table on this line."
  (interactive)
  (let ((id (kdb-sql--tables-at-point)))
    (kdb-sql--run-and-show
     kdb-sql--conn
     (format (concat "SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, IS_NULLABLE,"
                     " COLUMN_DEFAULT FROM INFORMATION_SCHEMA.COLUMNS"
                     " WHERE TABLE_SCHEMA = %s AND TABLE_NAME = %s ORDER BY ORDINAL_POSITION;")
             (kdb-sql--literal (car id)) (kdb-sql--literal (cdr id)))
     (format "*SQL columns: %s.%s*" (car id) (cdr id))
     (format "Columns of %s.%s" (car id) (cdr id)))))

(defun kdb-sql-tables-select ()
  "Show the first 100 rows of the table on this line."
  (interactive)
  (let* ((id (kdb-sql--tables-at-point))
         (product (plist-get kdb-sql--conn :product))
         (name (kdb-sql--qualified product id)))
    (kdb-sql--run-and-show
     kdb-sql--conn
     (if (eq product 'postgres)
         (format "SELECT * FROM %s LIMIT 100;" name)
       (format "SELECT TOP 100 * FROM %s;" name))
     (format "*SQL rows: %s.%s*" (car id) (cdr id))
     (format "First 100 rows of %s.%s" (car id) (cdr id)))))

(defun kdb-sql-tables-copy ()
  "Copy the quoted name of the table on this line."
  (interactive)
  (let ((name (kdb-sql--qualified (plist-get kdb-sql--conn :product) (kdb-sql--tables-at-point))))
    (kill-new name)
    (message "Copied %s" name)))

(defun kdb-sql-tables-insert ()
  "Insert the quoted name of the table on this line into the script."
  (interactive)
  (let ((name (kdb-sql--qualified (plist-get kdb-sql--conn :product) (kdb-sql--tables-at-point))))
    (unless (buffer-live-p kdb-sql--script) (user-error "The script is gone"))
    (with-current-buffer kdb-sql--script (insert name))
    (if-let* ((win (get-buffer-window kdb-sql--script)))
        (select-window win)
      (pop-to-buffer kdb-sql--script))))

(defun kdb-sql-tables-refresh ()
  "List the tables again."
  (interactive)
  (if (buffer-live-p kdb-sql--script)
      (with-current-buffer kdb-sql--script (kdb-sql-tables))
    (user-error "The script is gone; list tables from another with C-c C-t")))

(use-package sql
  :ensure nil
  :defer t
  :config
  (setq sql-ms-program (or (executable-find "sqlcmd") "sqlcmd")
        ;; sqlcmd runs nothing until GO, psql nothing until ;
        sql-send-terminator t
        ;; No login prompts: kdb-sql-connect has everything
        sql-ms-login-params nil
        sql-postgres-login-params nil)
  (dolist (product '(ms postgres))
    (sql-set-product-feature product :sqli-comint-func #'kdb-sql--comint))
  :bind (:map sql-mode-map
              ("C-c C-c" . kdb-sql-send-dwim)
              ("C-c C-a" . kdb-sql-connect)
              ("C-c C-u" . kdb-sql-use-database)
              ("C-c C-o" . kdb-sql-run-to-org)
              ("C-c C-t" . kdb-sql-tables)))


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
   dabbrev-case-fold-search nil
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
   diff-font-lock-syntax nil  ; Disable syntax highlighting in diffs
   diff-font-lock-leading-indicator nil  ; Disable special indicator formatting
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
  :defer t
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

  ;; fido-mode forces completion-styles to (flex) on every minibuffer entry,
  ;; puts flex in front of each category's styles, and turns off scrolling.
  ;; Override it after fido's setup hook runs.
  (defun kdb-orderless-override-fido ()
    "Restore orderless as the completion style in fido-mode."
    (when (and (bound-and-true-p icomplete-mode)
               (not (eq (car-safe completion-styles) 'orderless)))
      (setq-local completion-styles '(orderless basic)
                  completion-category-defaults nil
                  icomplete-scroll t)))
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
   completions-detailed t
   completion-ignore-case t
   read-buffer-completion-ignore-case t
   completions-max-height 14
   completions-header-format nil
   enable-recursive-minibuffers t
   completions-sort 'historical
   read-answer-short t
   ;; Leaving a prompt keeps windows opened meanwhile (by a Claude session,
   ;; say) instead of putting back the layout it started with.
   read-minibuffer-restore-windows nil))

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
  :init
  ;; No events buffer: logging every JSON message slows big servers.
  (setq eglot-events-buffer-config '(:size 0 :format full))
  :config
  (setq
   eglot-sync-connect 0
   eglot-send-changes-idle-time 0
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentHighlightProvider))

  ;; Language Servers
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("csharp-ls")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(html-ts-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-ts-mode . ("vscode-json-language-server" "--stdio")))
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

;; `when', not :if: :ensure installs the package before :if is checked.
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-variables '("PATH" "SSH_AUTH_SOCK")
          exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize)))


;; Tools installed in the user profile (opencode's installer, npm's global
;; prefix: claude-agent-acp, ccusage) aren't always on PATH.  Both lists:
;; exec-path for programs Emacs starts, PATH for shell commands.
(dolist (dir '(".opencode/bin" ".npm-global/bin"))
  (kdb-add-to-path (expand-file-name dir (or (getenv "USERPROFILE") "~"))))

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
  (let* ((langs '((csharp . c-sharp) (js . javascript) (c++ . cpp) (c-or-c++ . cpp)
                  (mhtml . html) (go-mod . gomod) (go-work . gowork)))
         (grammar-p (lambda (mode)
                      (let ((name (intern (string-remove-suffix "-ts-mode" (symbol-name mode)))))
                        (and (fboundp mode) (treesit-language-available-p (alist-get name langs name)))))))
    (if (boundp 'treesit-major-mode-remap-alist)
        (funcall (get 'treesit-enabled-modes 'custom-set) 'treesit-enabled-modes
                 (seq-filter grammar-p (seq-uniq (mapcar #'cdr treesit-major-mode-remap-alist))))
      ;; Before Emacs 31 there's no remap registry, and the modes with no
      ;; older mode to replace aren't in auto-mode-alist: both by hand.
      (dolist (remap '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode) (c-or-c++-mode . c-or-c++-ts-mode)
                       (csharp-mode . csharp-ts-mode) (js-mode . js-ts-mode) (javascript-mode . js-ts-mode)
                       (js-json-mode . json-ts-mode) (css-mode . css-ts-mode) (python-mode . python-ts-mode)
                       (sh-mode . bash-ts-mode) (java-mode . java-ts-mode) (ruby-mode . ruby-ts-mode)
                       (conf-toml-mode . toml-ts-mode)))
        (when (funcall grammar-p (cdr remap))
          (add-to-list 'major-mode-remap-alist remap)))
      (dolist (mode '(("\\.ts\\'" . typescript-ts-mode) ("\\.tsx\\'" . tsx-ts-mode)
                      ("\\.ya?ml\\'" . yaml-ts-mode) ("\\.rs\\'" . rust-ts-mode)
                      ("\\.go\\'" . go-ts-mode) ("/go\\.mod\\'" . go-mod-ts-mode)
                      ("\\(?:Dockerfile\\|\\.dockerfile\\)\\'" . dockerfile-ts-mode)))
        (when (funcall grammar-p (cdr mode))
          (add-to-list 'auto-mode-alist mode))))))

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


;; PowerShell ============================================== ;;

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))


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

(defvar eat--install-path)
(defvar eat-term-terminfo-directory)

(defun kdb-eat-local-terminfo ()
  "Point eat at terminfo compiled here by macOS's own `tic'.
eat ships it built by ncurses 6, which macOS's ncurses 5.7 can't read:
zsh says \"'eat-truecolor': unknown terminal type\".  Compiled once, and
again when eat's source changes; kept out of eat's own checkout."
  (let ((dir (locate-user-emacs-file "eat-terminfo/"))
        (src (expand-file-name "eat.ti" eat--install-path)))
    ;; Up to date unless the source is newer: equal times (same second) too.
    (unless (seq-some (lambda (f) (not (file-newer-than-file-p src f)))
                      (and (file-directory-p dir)
                           (directory-files-recursively dir "\\`eat-truecolor\\'")))
      (make-directory dir t)
      (let ((process-environment (cons (concat "TERMINFO=" dir) process-environment)))
        (call-process "/usr/bin/tic" nil nil nil "-x" src)))
    (setq eat-term-terminfo-directory dir)))

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
  (when (eq system-type 'darwin)
    (kdb-eat-local-terminfo))
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

;; Only when Emacs is the window manager: init-exwm binds s-t, s-p, s-x and
;; the other launcher keys globally, which under StumpWM etc. take over
;; keys meant for Emacs (s-t's theme cycle).  `when', not :if, which
;; :ensure ignores: exwm got installed everywhere.
(when (and (memq window-system '(x))
           (equal (getenv "DESKTOP_SESSION") "exwm"))
  (use-package exwm
    :ensure t
    :defer t
    :config
    ;; Shrink fringes to 1 pixel
    (fringe-mode 1))
  (require 'init-exwm))

;; Tab Bar (Workspaces) ==================================== ;;

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t)           ; Show tab numbers in tab names
  ;; Unless EXWM already put its workspace and clock there.
  (unless (memq 'kdb-exwm-tab-bar-workspace-name tab-bar-format)
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
  ;; M-1..M-8 go to that tab, M-9 the last, M-0 the one you were just in.
  ;; Not in terminal Emacs, where M-digits split windows (init-terminal).
  (unless (bound-and-true-p kdb-terminal-mode)
    (customize-set-variable 'tab-bar-select-tab-modifiers '(meta))))

;; One workspace (tab) per project, each with its own windows and buffer
;; list, so C-x b only offers this project's buffers.  Nothing is written
;; into projects: no todo file, no session files.
(defvar kdb-tabs--restored nil
  "Non-nil once the saved tabs have been restored, or there were none.")

(defun kdb-tabs--own-frame-p (&optional frame)
  "Non-nil unless FRAME is a daemon's hidden startup frame, which has no tabs."
  (not (and (daemonp) (eq (or frame (selected-frame)) terminal-frame))))

(defun kdb-tabs-restore (&optional _frame)
  "Bring back last session's tabs, once, into a frame you can see.
A daemon runs this when its first client frame opens."
  (when (and (not kdb-tabs--restored) (kdb-tabs--own-frame-p))
    (setq kdb-tabs--restored t)
    (remove-hook 'server-after-make-frame-hook #'kdb-tabs-restore)
    (unless (bound-and-true-p tabspaces-mode) (tabspaces-mode 1))
    (when (file-exists-p tabspaces-session-file)
      (tabspaces-restore-session tabspaces-session-file))))

(defun kdb-tabs--may-save (&rest _)
  "Non-nil when saving the tabs won't wipe the saved session.
Not before they are restored, nor from a daemon's hidden frame."
  (and kdb-tabs--restored (kdb-tabs--own-frame-p)))

(defun kdb-tabs--save-closing-frame (frame)
  "Save the tabs when the last frame you can see closes.
A daemon keeps running, and has nothing to save from afterwards."
  (when (and (bound-and-true-p tabspaces-mode)
             (kdb-tabs--own-frame-p frame)
             (null (seq-remove (lambda (f) (or (eq f frame) (not (kdb-tabs--own-frame-p f))))
                               (frame-list))))
    (with-selected-frame frame
      (tabspaces--save-session-smart))))

(use-package tabspaces
  :defer t
  :init
  ;; Loads dired and vc; keep it out of startup.
  (run-with-idle-timer 0.5 nil (lambda ()
                                 (unless (bound-and-true-p tabspaces-mode) (tabspaces-mode 1))
                                 (unless (daemonp) (kdb-tabs-restore))))
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook #'kdb-tabs-restore))
  :custom
  (tabspaces-keymap-prefix "C-c TAB")
  (tabspaces-use-filtered-buffers-as-default (not (eq window-system 'x))) ; EXWM apps stay global
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-initialize-project-with-vc nil)
  ;; Tabs and their buffers (Claude sessions included) come back after a
  ;; restart, from ~/.emacs.d/tabsession.el; saved on exit, when the last
  ;; frame closes, and every 10 idle minutes in case Emacs dies.
  (tabspaces-session t)
  (tabspaces-session-auto-save-delay 600)
  ;; All tabs in that one file: per-project files would land in projects.
  (tabspaces-session-project-session-store nil)
  :bind (:map tabspaces-command-map
              ("f" . tab-bar-switch-to-next-tab)
              ("p" . tab-bar-switch-to-prev-tab))
  :config
  (advice-add 'tabspaces--save-session-smart :before-while #'kdb-tabs--may-save)
  (add-hook 'delete-frame-functions #'kdb-tabs--save-closing-frame))


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
  (evil-collection-init)
  ;; It deletes these from `special-mode-map' itself, for every buffer, so
  ;; g stopped refreshing lists (Claude history, Dired-like modes) outside
  ;; evil too.  Evil's own state maps come first, so its buffers keep its keys.
  (define-key special-mode-map "g" #'revert-buffer)
  (define-key special-mode-map "h" #'describe-mode)
  (define-key special-mode-map "?" #'describe-mode)
  (define-key special-mode-map "0" #'digit-argument))

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
;; Open coding models run locally: completion and chat (gptel, minuet).
(require 'init-local-models)

;; Claude ================================================= ;;
;; agent-shell and friends, keys, and what runs in the background.  The
;; commands themselves load on first use (lisp/init-claude*.el).
(require 'init-claude-packages)


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
