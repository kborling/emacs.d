;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.6.0
;; Keywords: configuration
;; URL: https://github.com/kborling/emacs.d
;; Homepage: https://github.com/kborling/emacs.d
;; Package-Requires: ((emacs "30"))

;;; Commentary:

;; Copyright (C) 2025 Kevin Borling
;; My personal Emacs config.

;;; Code:

;; User ============================================= ;;
(setq user-full-name "Kevin Borling"
      user-mail-address "kborling@protonmail.com")

;; Custom ========================================== ;;

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Package ============================================= ;;

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
(unless package--initialized
  (package-initialize))
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

;; Backups & File Organization ====================== ;;

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

 ;; Additional file redirections
 bookmark-default-file (expand-file-name "bookmarks" user-emacs-var-directory)
 abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-var-directory)
 nsm-settings-file (expand-file-name "network-security.data" user-emacs-var-directory))

;; Performance Optimizations ====================== ;;

;; Additional performance tweaks
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      jit-lock-defer-time 0
      jit-lock-stealth-time 0.2)

;; Defaults ========================================= ;;

(setq
 sentence-end-double-space nil
 fill-column 80
 column-number-mode t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 set-mark-command-repeat-pop t
 global-mark-ring-max 50000)

;; Cursor configuration - thick bar
(setq-default cursor-type '(bar . 3))  ; Bar cursor with width of 3 pixels
(setq cursor-in-non-selected-windows 'hollow)  ; Hollow cursor in inactive windows

;; Contextual menu with right mouse button
(when (display-graphic-p)
  (context-menu-mode))

;; Persistent Features ============================== ;;

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-var-directory))
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpaca/" "/elpa/" "/opt/" "/.rustup/" "/elpa/" "/ssh:" "/sudo:" "/node_modules/" "/nix/"))
  :config
  ;; Save silently every 5 minutes
  (run-at-time nil (* 5 60) (lambda () 
                               (let ((inhibit-message t))
                                 (recentf-save-list)))))

;; Save cursor position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-var-directory)))

;; Minibuffer history
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

;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil
              tab-stop-list ()
              tab-width 4
              c-basic-offset 4
              sgml-basic-offset 4
              typescript-ts-mode-indent-offset 4
              js-switch-indent-offset 4)

;; Treat Camelcase as words
(use-package subword-mode
  :ensure nil
  :hook (after-init . global-subword-mode))

;; Delete selection on insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Smooth scrolling
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Fonts ================================================ ;;

(setopt line-spacing 4)

(let* ((settings (cond
                  ((eq system-type 'windows-nt) '(:size 110 :family "Rec Mono Semicasual"))
                  ((eq system-type 'gnu/linux)  '(:size 120 :family "Inconsolata"))
                  ((eq system-type 'darwin)     '(:size 150 :family "Overpass Mono"))))
       (default-font-size (plist-get settings :size))
       (default-font-family (plist-get settings :family)))
  (set-face-attribute 'default nil
                      :family default-font-family :weight 'light :height default-font-size)
  (set-face-attribute 'fixed-pitch nil
                      :family default-font-family :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "FreeSans" :height 1.0 :weight 'regular))

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; Themes ================================================= ;;

(use-package uwu-theme
  :vc (:url "https://github.com/kborling/uwu-theme" :rev :newest)
  :config
  (setq
   uwu-distinct-line-numbers 'nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t)
  (load-theme 'uwu t))

(use-package acme-theme)

;; UI Enhancements ======================================== ;;

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

;; Custom Functions ======================================= ;;

(defun config-visit ()
  "Load ~/.emacs.d/init.el for editing."
  (interactive)
  (find-file (expand-file-name (locate-user-emacs-file "init.el"))))

(defun config-reload ()
  "Reload ~/.emacs.d/init.el at runtime."
  (interactive)
  (load-file (expand-file-name (locate-user-emacs-file "init.el"))))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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
  "Copy the current buffer while maintaining cursor position."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (push-mark (point-max) nil t)
    (copy-region-as-kill 1 (buffer-size))))

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

;; TAGS ============================================== ;;

(defun kdb-create-tags (dir-name)
  "Create tags file using DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" (locate-file "ctags" exec-path) (directory-file-name dir-name))))

;; Keybindings ======================================= ;;

(defun my/setup-keybindings ()
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
                       ("C-c C-d" . duplicate-line)
                       ("C-x C-r" . recentf)
                       ("C-x f" . project-find-file)
                       ("C-c C-r" . replace-regexp)
                       ("C-\\" . hippie-expand)
                       ("M-z" . zap-up-to-char)
                       ("C-M-s" . isearch-forward-symbol-at-point)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Insert pair shortcuts
    (dolist (key '("M-(" "M-\"" "M-{" "M-["))
      (define-key map (kbd key) #'insert-pair))

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

    ;; Opening tools
    (dolist (binding '(("C-c t e" . eshell)
                       ("C-c t v" . ansi-term)
                       ("C-c t d" . dired-jump-other-window)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Configuration shortcuts
    (dolist (binding '(("C-c e v" . config-visit)
                       ("C-c e r" . config-reload)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Toggling features
    (dolist (binding '(("C-c t t" . toggle-theme)
                       ("C-c t f" . toggle-frame-fullscreen)))
      (define-key map (kbd (car binding)) (cdr binding)))))

;; Set up keybindings after everything else loads
(add-hook 'after-init-hook #'my/setup-keybindings)

(defun kdb/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'kdb/keyboard-quit-dwim)

;; OSX ============================================= ;;
;; (when (eq system-type 'darwin)
;;   (select-frame-set-input-focus (selected-frame))
;;   (setq mac-option-modifier nil
;;         ns-function-modifier 'super
;;         mac-right-command-modifier 'hyper
;;         mac-right-option-modifier 'alt
;;         mac-command-modifier 'meta))

;; Ansi-term ====================================== ;;

(defadvice kdb-ansi-term (before force-bash)
  "Set the default shell to bash."
  (interactive (list (or (locate-file "zsh" exec-path)
                         "bash"))))
(ad-activate 'kdb-ansi-term)

(defun kdb-term-exec-hook ()
  "Kill the terminal after exit."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'kdb-term-exec-hook)

;; Paste into term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Auto Revert ====================================== ;;

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))


;; Grep ============================================== ;;

(use-package grep
  :ensure nil
  :config
  ;; Use ripgrep for grep-command
  (setq grep-command "rg --color=never --no-heading --line-number --smart-case "
        ;; grep-find-command
        ;; (concat "fd --type f --hidden --follow --exclude .git | "
        ;;         "xargs rg --color=never --no-heading --line-number --smart-case ")
        grep-use-null-device nil))

;; Deadgrep ========================================== ;;

(use-package deadgrep
  :ensure t
  :config
  (setq deadgrep-extra-arguments '("--no-config" "--multiline"))
  (global-set-key (kbd "C-x g") #'deadgrep))

;; Xref ============================================== ;;

(use-package xref
  :ensure nil
  :config
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-search-program 'ripgrep))


;; Which Key ========================================= ;;

(use-package which-key
  :ensure nil
  :defer 0
  :config
  (which-key-mode))

;; EditorConfig ======================================== ;;

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

;; ISearch =========================================== ;;

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-g" . isearch-cancel)
              ("M-/" . isearch-complete)
              ("M-e" . isearch-edit-string)
              ("M-s M-<" . isearch-beginning-of-buffer)
              ("M-s M->" . isearch-end-of-buffer)
              ("C-n" . isearch-repeat-forward)
              ("C-p" . isearch-repeat-backward)
              ("<down>" . isearch-repeat-forward)
              ("<up>" . isearch-repeat-backward))
  :config
  (setq
   search-whitespace-regexp ".*?"
   isearch-lazy-count t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " (%s/%s)"
   isearch-wrap-pause 'no
   isearch-yank-on-move 'shift
   isearch-allow-scroll 'unlimited
   isearch-allow-motion t ; Allow motion commands during isearch
   isearch-motion-changes-direction t ; Motion commands change search direction
   isearch-repeat-on-direction-change t
   isearch-regexp-lax-whitespace t ; More flexible whitespace matching in regexp search
   isearch-lax-whitespace t ; More flexible whitespace matching
   lazy-highlight-initial-delay 0.1 ; Faster highlighting
   lazy-highlight-interval 0 ; Highlight all matches immediately
   lazy-highlight-max-at-a-time nil) ; No limit on highlighting
  
  ;; Occur integration - show all matches in a buffer
  (define-key isearch-mode-map (kbd "M-s o") 'isearch-occur)
  
  ;; Better visual feedback
  (defun my/isearch-remove-failed-part ()
    "Remove the failing part of the search string."
    (interactive)
    (while (isearch-fail-pos)
      (isearch-pop-state)))
  (define-key isearch-mode-map (kbd "C-<backspace>") 'my/isearch-remove-failed-part)
  
  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit))

;; Dabbrev =========================================== ;;

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

;; Diff ========================================= ;;

(use-package diff-mode
  :ensure nil
  :defer t
  :hook ((diff-mode . (lambda ()
                        (setq-local truncate-lines t)
                        (diff-refine-hunk)
                        (outline-minor-mode 1))))
  :config
  ;; Enhanced diff settings for magit-like appearance
  (setq
   diff-refine nil  ; Disable refinement
   diff-font-lock-prettify nil  ; Disable prettification
   diff-font-lock-syntax nil  ; Disable syntax highlighting in diffs
   diff-font-lock-leading-indicator nil  ; Disable special indicator formatting
   diff-update-on-the-fly t
   diff-advance-after-apply-hunk t
   diff-default-read-only t)  ; Make diff buffers read-only by default
  
  ;; Diff face customizations are now in uwu-theme.el
  
  ;; Custom function to toggle refinement
  (defun my/diff-toggle-refine ()
    "Toggle diff refinement between 'navigation and nil."
    (interactive)
    (setq diff-refine (if diff-refine nil 'navigation))
    (if diff-refine
        (diff-refine-hunk)
      (diff-unrefine-hunk))
    (message "Diff refinement: %s" (if diff-refine "enabled" "disabled")))
  
  ;; Better navigation
  (defun my/diff-navigate-and-refine (orig-fun &rest args)
    "Automatically refine hunk after navigation."
    (apply orig-fun args)
    (when diff-refine
      (diff-refine-hunk)))
  
  (advice-add 'diff-hunk-next :around #'my/diff-navigate-and-refine)
  (advice-add 'diff-hunk-prev :around #'my/diff-navigate-and-refine)
  
  :bind (:map diff-mode-map
              ("C-c C-r" . my/diff-toggle-refine)
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

;; Dired ============================================= ;;

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
    (setq dired-use-ls-dired nil))     ; macOS ls doesn't support --dired

  ;; Better keybindings
  :bind (:map dired-mode-map
              ("E" . dired-toggle-read-only)
              ("?" . dired-summary)
              ("C-c C-e" . wdired-change-to-wdired-mode)))

;; Ibuffer ============================================== ;;

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

   ;; Additional useful settings
   ibuffer-show-empty-filter-groups nil  ; Hide empty groups
   ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs Config" (filename . "\\.emacs\\.d"))
      ("Projects" (filename . "Projects/"))
      ("Programming" (derived-mode . prog-mode))
      ("Dired" (mode . dired-mode))
      ("Org" (or (mode . org-mode)
                 (mode . org-agenda-mode)))
      ("Magit" (name . "^\\*magit"))
      ("Help" (or (name . "^\\*Help\\*")
                  (name . "^\\*Apropos\\*")
                  (name . "^\\*info\\*")
                  (name . "^\\*Messages\\*")))
      ("Special" (name . "^\\*")))))

  ;; Auto-use the saved filter groups
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))

  ;; Better keybindings
  :bind (:map ibuffer-mode-map
              ("M-o" . other-window)
              ("/" . ibuffer-filter-by-name)))

;; Project ============================================ ;;

(use-package project
  :ensure nil
  :config
  (setq
   vc-directory-exclusion-list (nconc vc-directory-exclusion-list '("node_modules" "elpa" ".sl"))
   project-vc-extra-root-markers '(".envrc" "package.json" ".project" ".sl")))

;; Async ============================================== ;;

(use-package async
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'dired
    (require 'dired-async)
    (dired-async-mode 1))
  :config
  ;; Enable async byte compilation
  (async-bytecomp-package-mode 1))

;; Orderless ========================================== ;;

(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  :commands (orderless-filter)
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil))
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)))

;; Fussy =============================================== ;;

(use-package fussy
  :config
  (setq fussy-use-cache t
        ;; fussy-filter-fn 'fussy-filter-default
        fussy-filter-fn 'fussy-filter-orderless-flex
        fussy-compare-same-score-fn 'fussy-histlen->strlen<)

  (fussy-setup)
  (fussy-eglot-setup)

  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit (if (eq system-type 'windows-nt) 3000 5000)
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

;; Icomplete ========================================= ;;

(use-package icomplete
  :ensure nil
  :init
  (fido-mode)
  :config
  (defun fussy-fido-setup ()
    "Use `fussy' with `fido-mode'."
    (setq-local completion-styles '(fussy basic)))
  (advice-add 'icomplete--fido-mode-setup :after 'fussy-fido-setup)
  (setq icomplete-tidy-shadowed-file-names t
        icomplete-show-matches-on-no-input t
        icomplete-compute-delay 0
        icomplete-max-delay-chars 0
        icomplete-delay-completions-threshold 0
        icomplete-hide-common-prefix nil
        icomplete-prospects-height 2
        icomplete-with-completion-tables t
        icomplete-scroll t)

  (global-set-key (kbd "C-`") 'fido-vertical-mode))

;; Minibuffer ======================================== ;;

(use-package minibuffer
  :ensure nil
  :demand t
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
   completions-max-height 20
   completions-header-format nil
   minibuffer-visible-completions nil
   enable-recursive-minibuffers t
   completions-sort 'historical
   read-answer-short t)
  :bind (:map minibuffer-local-map
              ("C-p" . minibuffer-previous-completion)
              ("C-n" . minibuffer-next-completion))
  :bind (:map completion-in-region-mode-map
              ("C-p" . minibuffer-previous-completion)
              ("C-n" . minibuffer-next-completion)
              ("RET" . minibuffer-choose-completion)))

;; Eglot ============================================== ;;

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
  ;; See https://github.com/olrtg/emmet-language-server
  (add-to-list 'eglot-server-programs '(html-ts-mode . ("emmet-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-ts-mode . ("emmet-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode)
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

  ;; FIXME: This doesn't always work initially (eval-buffer usually fixes it)
  (let* ((global-prefix (string-trim (shell-command-to-string "npm config get --global prefix")))
         (modules-path (if (eq system-type 'windows-nt)
                           "node_modules"
                         "lib/node_modules"))
         (node-modules-path (expand-file-name modules-path global-prefix)))
    ;; See https://v17.angular.io/guide/language-service#neovim
    (add-to-list 'eglot-server-programs
                 `(angular-template-mode . ("ngserver"
                                            "--stdio"
                                            "--tsProbeLocations"
                                            ,(concat node-modules-path "/typescript/lib")
                                            "--ngProbeLocations"
                                            ,(concat node-modules-path "/@angular/language-server/bin")))))

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
                  css-mode
                  js-mode
                  c-mode
                  c++-mode
                  rust-mode
                  csharp-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'eglot-ensure)))

;; NOTE: Be sure to grab the latest release 'https://github.com/blahgeek/emacs-lsp-booster/releases'
;; and place in PATH
(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :config (eglot-booster-mode))

;; Flymake ========================================= ;;
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

  ;; Custom faces for better visibility
  (set-face-attribute 'flymake-error nil
                      :underline '(:style wave :color "red"))
  (set-face-attribute 'flymake-warning nil
                      :underline '(:style wave :color "yellow"))
  (set-face-attribute 'flymake-note nil
                      :underline '(:style wave :color "blue"))

  ;; Quick fix function
  (defun my/flymake-quickfix ()
    "Apply quickfix at point if available."
    (interactive)
    (when-let ((diag (get-char-property (point) 'flymake-diagnostic)))
      (when-let ((data (flymake-diagnostic-data diag)))
        (when (plist-get data :quickfix)
          (funcall (plist-get data :quickfix))))))

  ;; Navigate only errors (skip warnings/notes)
  (defun my/flymake-goto-next-error-only ()
    "Go to next error, skipping warnings and notes."
    (interactive)
    (flymake-goto-next-error nil '(:error)))
  
  (defun my/flymake-goto-prev-error-only ()
    "Go to previous error, skipping warnings and notes."
    (interactive)
    (flymake-goto-prev-error nil '(:error)))

  (define-key ctl-x-x-map "m" #'flymake-mode)

  :bind (:map flymake-mode-map
              ("C-c f s" . flymake-start)
              ("C-c f d" . flymake-show-buffer-diagnostics)
              ("C-c f D" . flymake-show-project-diagnostics)
              ("C-c f n" . my/flymake-goto-next-error-only)
              ("C-c f p" . my/flymake-goto-prev-error-only)
              ("C-c f q" . my/flymake-quickfix)
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

;; Eldoc ============================================ ;;

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode 1)
  (setq
   eldoc-echo-area-use-multiline-p t))

;; (use-package eldoc-box
;;   :after eldoc
;;   :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))

;; Exec Path ========================================= ;;

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Version Control =============================================== ;;

(use-package vc
  :ensure nil
  :config
  (setq vc-follow-symlinks t
        vc-handled-backends '(Git)
        vc-git-diff-switches '("--histogram" "--patience" "--no-prefix")
        vc-git-print-log-follow t
        vc-git-log-edit-summary-target-len 50
        vc-git-log-edit-summary-max-len 70
        ;; Cleaner log output with pretty format
        vc-git-log-switches '("--graph" 
                              "--pretty=format:%h %s" 
                              "--abbrev-commit"
                              "--date=short"))
  
  ;; Enhanced vc-diff appearance
  (defun my/vc-diff-enhanced (&optional historic not-urgent)
    "Enhanced vc-diff with better visual presentation."
    (interactive "P")
    (call-interactively 'vc-diff)
    (when (get-buffer "*vc-diff*")
      (with-current-buffer "*vc-diff*"
        (diff-mode)
        (setq-local truncate-lines t)
        (when diff-refine
          (diff-refine-hunk))
        (goto-char (point-min))
        (when (re-search-forward "^@@" nil t)
          (beginning-of-line)))))
  
  ;; Hook to enhance vc-diff buffers
  (add-hook 'vc-diff-finish-functions
            (lambda ()
              (when (string-match-p "\\*vc-diff\\*" (buffer-name))
                (setq-local truncate-lines t)
                (when diff-refine
                  (diff-refine-hunk)))))
  
  ;; Custom navigation for git log format
  (defun my/vc-log-next-commit ()
    "Navigate to next commit in log."
    (interactive)
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at "^\\*\\s-+[0-9a-f]\\{7,\\}")))
      (forward-line 1))
    (beginning-of-line))
  
  (defun my/vc-log-prev-commit ()
    "Navigate to previous commit in log."
    (interactive)
    (forward-line -1)
    (while (and (not (bobp))
                (not (looking-at "^\\*\\s-+[0-9a-f]\\{7,\\}")))
      (forward-line -1))
    (beginning-of-line))
  
  ;; Enhanced vc-log appearance
  (defun my/vc-log-enhanced ()
    "Enhanced vc-log with better colors and navigation."
    (interactive)
    (call-interactively 'vc-print-log)
    (when (get-buffer "*vc-change-log*")
      (with-current-buffer "*vc-change-log*"
        ;; Add custom keybindings for navigation
        (local-set-key (kbd "n") 'my/vc-log-next-commit)
        (local-set-key (kbd "p") 'my/vc-log-prev-commit)
        (local-set-key (kbd "TAB") 'my/vc-log-next-commit)
        (local-set-key (kbd "<backtab>") 'my/vc-log-prev-commit)
        ;; Add font-lock keywords
        (font-lock-add-keywords
         nil
         '(;; Branch merge/split indicators (|/ or |\)
           ("^\\(|[/\\\\]\\)\\s-*$" (0 'font-lock-warning-face t))
           ;; Branch lines with commits (| * hash)
           ("^\\(|\\)\\s-\\*" (1 'font-lock-warning-face t))
           ;; Vertical lines alone
           ("^\\(|\\)\\s-*$" (1 'font-lock-comment-face t))
           ;; Commit hash
           ("\\b\\([0-9a-f]\\{7,\\}\\)\\b" (1 'font-lock-type-face t))
           ;; Commit message after hash
           ("[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face t)))
         t)
        (font-lock-fontify-buffer))))
  
  ;; Enhanced functions available in VC transient menu
  )

;; Log-view configuration
(use-package log-view
  :ensure nil
  :after vc
  :config
  ;; Match git log --graph --oneline format
  (setq log-view-message-re "^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)\\s-+\\(.*\\)$"
        log-view-file-re "^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)"
        log-view-font-lock-keywords
        '(("^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)" (1 'log-view-message))
          ("^\\*\\s-+[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face))))
  
  ;; Make commit hashes clickable and copyable
  (defun my/make-hashes-clickable ()
    "Make commit hashes clickable and copyable."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" nil t)
        (let ((hash (match-string 1))
              (start (match-beginning 1))
              (end (match-end 1)))
          (put-text-property start end 'mouse-face 'highlight)
          (put-text-property start end 'help-echo "Click: show commit details | Right-click: copy hash")
          (put-text-property start end 'keymap 
                             (let ((map (make-sparse-keymap)))
                               (define-key map [mouse-1] 
                                 (lambda (e) (interactive "e") 
                                   (my/vc-commit-show hash)))
                               (define-key map [mouse-3] 
                                 (lambda (e) (interactive "e") 
                                   (kill-new hash) 
                                   (message "Copied hash: %s" hash)))
                               (define-key map (kbd "RET") 
                                 (lambda () (interactive) 
                                   (my/vc-commit-show hash)))
                               (define-key map (kbd "c") 
                                 (lambda () (interactive) 
                                   (kill-new hash) 
                                   (message "Copied hash: %s" hash)))
                               map))))))
  
  ;; Add branch styling font-lock patterns
  (defun my/add-log-styling ()
    "Add font-lock patterns for branch indicators and styling."
    (font-lock-add-keywords
     nil
     '(;; Branch merge/split indicators on their own line
       ("^\\(|[/\\\\]\\|[/\\\\]|\\)\\s-*$" . 'font-lock-warning-face)
       ;; Branch lines with commits (| * hash message)
       ("^\\(|\\)\\s-\\*" (1 'font-lock-warning-face))
       ;; Vertical branch lines alone
       ("^\\(|\\)\\s-*$" . 'font-lock-comment-face)
       ;; Commit hash (7+ hex characters)
       ("\\b\\([0-9a-f]\\{7,\\}\\)\\b" (1 'log-view-message))
       ;; Everything after the hash is the commit message
       ("[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face))
       ;; Branch/tag names in parentheses
       ("(\\([^)]+\\))" (1 'font-lock-keyword-face))
       ;; Merge indicators
       ("\\<Merge:\\s-" . 'font-lock-builtin-face)
       ;; Issue numbers
       ("#[0-9]+" . 'font-lock-constant-face))
     t))
  
  ;; Enhanced display settings
  (add-hook 'log-view-mode-hook
            (lambda ()
              (setq-local truncate-lines t)
              (hl-line-mode 1)
              ;; Ensure proper mode setup
              (setq-local log-view-per-file-logs nil)
              (setq-local log-view-message-face 'log-view-message)
              ;; Apply styling and make hashes clickable
              (my/add-log-styling)
              (font-lock-fontify-buffer)
              (my/make-hashes-clickable)))
  
  ;; Also apply to vc-git-log-view-mode
  (add-hook 'vc-git-log-view-mode-hook
            (lambda ()
              (my/add-log-styling)
              (font-lock-fontify-buffer)
              (my/make-hashes-clickable))))

(use-package ssh-agency
  :if (eq system-type 'windows-nt)
  :vc (:url "https://github.com/magit/ssh-agency" :rev :newest))


;; VC-msg - Show commit messages for current line
(use-package vc-msg
  :ensure t
  :config
  ;; Customize popup appearance - cleaner style
  (setq vc-msg-show-author t
        vc-msg-time-format "%Y-%m-%d %H:%M"
        vc-msg-persist-popup t)
  
  ;; Use built-in vc backend
  (setq vc-msg-git-show-commit-function 'vc-msg-git-show-commit-internal)
  
  ;; Remove box/border from popup
  (with-eval-after-load 'vc-msg
    (set-face-attribute 'vc-msg-face nil
                        :box nil
                        :background "#232A2C"  ; uwu-bright-black
                        :foreground "#C5C8C9")) ; uwu-fg
  
  ;; Available in VC transient menu as 'm'
  )

;; VC Transient Menu - provides Magit-like interface for VC
(defun my/vc-transient ()
  "Custom VC transient menu with Magit-like keybindings."
  (interactive)
  (require 'transient)
  (transient-define-prefix my/vc-menu ()
    "VC operations menu"
    [["File Operations"
      ("s" "Status" vc-dir)
      ("d" "Enhanced Diff" my/vc-diff-enhanced)
      ("l" "Enhanced Log" my/vc-log-enhanced)
      ("B" "Blame" vc-annotate)
      ("=" "Basic Diff" vc-diff)]
     ["Branch Operations"
      ("b b" "Switch Branch" vc-switch-branch)
      ("b n" "New Branch" vc-create-branch)
      ""
      ""
      "Remote Operations"
      ("f" "Pull" vc-update)
      ("F" "Fetch" vc-update)
      ("p" "Push" vc-push)
      ("C" "Clone" vc-clone)]
     ["Changes"
      ("a" "Add/Register" vc-register)
      ("c" "Commit" vc-next-action)
      ("u" "Revert" vc-revert)
      ("k" "Delete" vc-delete-file)
      ""
      "Stashing"
      ("z z" "Stash" vc-git-stash)
      ("z a" "Apply Stash" vc-git-stash-apply-at-point)
      ("z p" "Pop Stash" vc-git-stash-pop-at-point)]
     ["Commit Tools"
      ("h s" "Show Commit" my/vc-commit-show)
      ("h d" "Commit Diff" my/vc-commit-diff)
      ("h t" "Commit Stats" my/vc-commit-stats)
      ("m" "Show Line Commit" vc-msg-show)
      ""
      "Search & Info"
      ("g" "Log Search" vc-log-search)
      ("i" "Init Repo" vc-create-repo)
      ("t" "Tag" vc-create-tag)
      ""
      "Remotes"
      ("r u" "Set Remote URL" my/vc-remote-set-url)
      ("r l" "List Remotes" my/vc-remote-list)
      ""
      ("q" "Quit" transient-quit-one)]])
  (my/vc-menu))

;; Custom VC helper functions
(defun my/vc-remote-set-url ()
  "Set remote URL for current repository."
  (interactive)
  (let* ((remote (read-string "Remote name (default: origin): " "origin"))
         (url (read-string "New URL: ")))
    (when (and remote url (not (string-empty-p url)))
      (shell-command (format "git remote set-url %s %s" remote url))
      (message "Remote '%s' URL updated to: %s" remote url))))

;; Commit hash lookup functions
(defun my/vc-commit-show (hash)
  "Show detailed information about a commit hash."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --stat --pretty=fuller %s" (shell-quote-argument hash)))))
    (with-current-buffer (get-buffer-create "*Commit Details*")
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun my/vc-commit-diff (hash)
  "Show diff for a specific commit."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --format= %s" (shell-quote-argument hash)))))
    (with-current-buffer (get-buffer-create "*Commit Diff*")
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun my/vc-commit-stats (hash)
  "Show stats for a specific commit HASH."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --stat --format='%%h %%s%%n%%aD %%an' %s" (shell-quote-argument hash)))))
    (message "📊 %s" (string-trim output))))

(defun my/vc-remote-list ()
  "List all remotes with their URLs."
  (interactive)
  (let ((output (shell-command-to-string "git remote -v")))
    (if (string-empty-p (string-trim output))
        (message "No remotes configured")
      (with-current-buffer (get-buffer-create "*VC Remotes*")
        (erase-buffer)
        (insert "Git Remotes:\n\n")
        (insert output)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;; VC-dir Enhancements
(use-package vc-dir
  :ensure nil
  :after vc
  :config
  ;; Better defaults for cleaner UI
  (setq vc-dir-hide-up-to-date t           ; Hide unchanged files by default
        vc-dir-hide-unregistered nil       ; Show untracked files
        vc-stay-local t                    ; Faster for remote repos
        vc-directory-exclusion-list '(".git" ".hg" ".svn" "node_modules" ".venv"))
  
  ;; Custom function to hide uninteresting files
  (defun my/vc-dir-hide-unmodified ()
    "Hide up-to-date and ignored files in vc-dir."
    (interactive)
    (vc-dir-hide-state 'up-to-date)
    (vc-dir-hide-state 'ignored))
  
  ;; Show only modified files
  (defun my/vc-dir-show-only-modified ()
    "Show only modified files in vc-dir."
    (interactive)
    (vc-dir-hide-state 'up-to-date)
    (vc-dir-hide-state 'ignored)
    (vc-dir-hide-state 'unregistered))
  
  ;; Toggle showing all files
  (defun my/vc-dir-toggle-all ()
    "Toggle between showing all files and only modified files."
    (interactive)
    (if (get 'my/vc-dir-toggle-all 'showing-all)
        (progn
          (my/vc-dir-show-only-modified)
          (put 'my/vc-dir-toggle-all 'showing-all nil)
          (message "Showing only modified files"))
      (progn
        (vc-dir-unmark-all-files t)
        (revert-buffer)
        (put 'my/vc-dir-toggle-all 'showing-all t)
        (message "Showing all files"))))
  
  ;; Quick commit with message
  (defun my/vc-dir-quick-commit ()
    "Quick commit with a simple message prompt."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (vc-next-action nil)
      (insert msg)
      (log-edit-done)))
  
  ;; Stage/unstage helpers
  (defun my/vc-dir-stage-all ()
    "Stage all changes."
    (interactive)
    (vc-dir-mark-all-files t)
    (vc-next-action nil)
    (message "All changes staged"))
  
  ;; Show summary of changes
  (defun my/vc-dir-summary ()
    "Show a summary of repository status."
    (interactive)
    (let ((modified 0) (added 0) (removed 0) (unregistered 0))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "^[ *][ *]\\s-+\\(\\S-+\\)\\s-+")
            (let ((state (match-string 1)))
              (cond ((string= state "edited") (cl-incf modified))
                    ((string= state "added") (cl-incf added))
                    ((string= state "removed") (cl-incf removed))
                    ((string= state "unregistered") (cl-incf unregistered)))))
          (forward-line 1)))
      (message "📊 Modified: %d | Added: %d | Removed: %d | Untracked: %d" 
               modified added removed unregistered)))
  
  ;; Enhanced display with icons
  (defun my/vc-dir-prettify ()
    "Add icons and better formatting to vc-dir."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(edited\\)" nil t)
        (replace-match "modified" nil nil nil 1))
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(added\\)" nil t)
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face '(:foreground "#6BB05D" :weight bold)))
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(removed\\)" nil t)
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face '(:foreground "#F65B5B" :weight bold)))))
  
  ;; Enhanced keybindings
  :bind (:map vc-dir-mode-map
              ("h" . my/vc-dir-hide-unmodified)
              ("H" . my/vc-dir-toggle-all)
              ("M" . my/vc-dir-show-only-modified)
              ("c" . my/vc-dir-quick-commit)
              ("a" . my/vc-dir-stage-all)
              ("s" . my/vc-dir-summary)
              ("P" . vc-push)
              ("F" . vc-update)
              ("b" . vc-switch-branch)
              ("l" . vc-print-log)
              ("=" . vc-diff)
              ("g" . revert-buffer)
              ("k" . vc-dir-delete-file)
              ("!" . vc-dir-ignore)
              ("?" . my/vc-dir-help))
  
  ;; Help function
  (defun my/vc-dir-help ()
    "Show vc-dir keybinding help."
    (interactive)
    (message "VC-Dir: [m]ark [u]nmark [c]ommit [=]diff [l]og [g]refresh [h]ide [s]ummary [a]dd-all [P]ush [F]etch"))
  
  ;; Auto-refresh and enhance display
  (add-hook 'vc-dir-mode-hook
            (lambda ()
              (auto-revert-mode 1)
              (hl-line-mode 1)
              (display-line-numbers-mode -1)  ; Cleaner without line numbers
              (my/vc-dir-prettify)             ; Apply prettification
              (setq-local revert-buffer-function
                          (lambda (_ignore-auto _noconfirm)
                            (vc-dir-refresh)
                            (my/vc-dir-prettify)))
              ;; Show summary on startup
              (run-at-time 0.1 nil 'my/vc-dir-summary))))

;; Additional VC customizations for better experience
(with-eval-after-load 'vc
  ;; Faster git operations
  (setq vc-git-diff-switches '("--histogram" "--color=never")
        vc-suppress-confirm t  ; Don't ask for confirmation on every operation
        vc-command-messages t)  ; Show VC command messages
  
  ;; Better commit message interface
  (setq log-edit-confirm 'changed  ; Only confirm if buffer changed
        log-edit-keep-buffer nil  ; Don't keep log buffer after commit
        log-edit-require-final-newline t
        log-edit-setup-add-author nil))

(global-set-key (kbd "C-c g") 'my/vc-transient)


;; Marginalia ======================================== ;;

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0
        marginalia-align-offset 10))

;; Highlight TODOs ===================================== ;;

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)))

;; Corfu ============================================== ;;

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

;; Cape ================================================ ;;

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

;; Templates =========================================== ;;

(use-package tempel
  :bind (("C-<tab>" . tempel-complete)
         ("M-+" . tempel-insert)
         ("C-1" . tempel-previous)
         ("C-2" . tempel-next)))

(use-package tempel-collection
  :after tempel)

;; Treesitter ========================================== ;;

(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :custom
  ;; Change the prefix key to something more convenient
  (combobulate-key-prefix "C-c o")
  (combobulate-flash-node t)
  (combobulate-dimmer-mode t)
  ;; Auto-expand region to meaningful boundaries
  (combobulate-envelope-indent-region-function #'indent-region)
  :hook
  ;; Enable for specific modes that have tree-sitter support
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (toml-ts-mode . combobulate-mode)
   (go-ts-mode . combobulate-mode)
   (rust-ts-mode . combobulate-mode))
  :bind
  ;; Override some default keybindings for easier access
  (:map combobulate-key-map
        ;; Navigation
        ("C-M-f" . combobulate-navigate-next)
        ("C-M-b" . combobulate-navigate-previous)
        ("C-M-u" . combobulate-navigate-up)
        ("C-M-d" . combobulate-navigate-down)
        ("C-M-n" . combobulate-navigate-sequence-next)
        ("C-M-p" . combobulate-navigate-sequence-previous)
        ;; Logical navigation (beginning/end of blocks)
        ("C-M-a" . combobulate-navigate-logical-previous)
        ("C-M-e" . combobulate-navigate-logical-next)
        ;; Marking and selection
        ("C-M-SPC" . combobulate-mark-node-dwim)
        ;; Editing
        ("C-M-k" . combobulate-kill-node-dwim)
        ("C-M-y" . combobulate-yank)
        ("C-M-/" . combobulate-comment-dwim)
        ;; Manipulation
        ("M-<up>" . combobulate-drag-up)
        ("M-<down>" . combobulate-drag-down)
        ("M-<left>" . combobulate-splice-self)
        ("M-<right>" . combobulate-splice-parent)
        ;; Envelopes (wrapping)
        ("C-c o e" . combobulate-envelope-dwim)))

;; Treesit Expand ==================================== ;;

(use-package treesit-expand
  :ensure nil
  :vc (:url "https://github.com/kborling/treesit-expand" :rev :newest)
  :bind (("C-=" . treesit-expand-dwim)
         ("C--" . treesit-contract-region)
         ("C-c e e" . treesit-expand-region)
         ("C-c e q" . treesit-expand-reset)))

;; Multiple Cursors ================================== ;;

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-'" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)))

;; So Long =========================================== ;;

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; Angular =========================================== ;;

(use-package angular-mode
  :vc (:url "https://github.com/kborling/angular-mode" :rev :newest)
  :config
  (defun angular-console-log-thing-at-point ()
    "Insert a console.log statement for the full JavaScript expression at point, including function calls."
    (interactive)
    (let ((expr ""))
      (save-excursion
        ;; Move backward to get start of dotted expression
        (while (or (looking-back "\\(?:\\sw\\|\\s_\\|\\.\\)" (1- (point)))
                   (looking-back ")" (1- (point))))
          (backward-char))
        (let ((start (point)))
          ;; Move forward through symbol, dot, and optional function calls
          (while (looking-at "\\(?:\\sw\\|\\s_\\|\\.\\)+")
            (forward-sexp))
          ;; If it's a function call, include the parentheses
          (when (looking-at "(")
            (forward-sexp))
          (setq expr (buffer-substring-no-properties start (point)))))
      (when (not (string-empty-p expr))
        (save-excursion
          (end-of-line)
          (newline-and-indent)
          (insert (format "console.log('%s', %s);" expr expr))))))

  (defun angular-remove-all-console-logs ()
    "Delete all lines containing console.log(...) in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      ;; Loop through each occurrence and delete the whole line
      (while (re-search-forward "\\bconsole\\.log\\s-*(" nil t)
        (beginning-of-line)
        (kill-whole-line))))

  (defun angular-open-interface ()
    "Open an Angular interface file in the project."
    (interactive)
    (angular-open-file "interface"))
  :bind (:map angular-mode-map
              ("C-c a o f" . angular-open-interface)
              ("C-c a c" . angular-console-log-thing-at-point)
              ("C-c a d" . angular-remove-all-console-logs)))

(define-derived-mode angular-template-mode html-ts-mode "Angular Template "
  "A major mode derived from 'html-ts-mode', for editing angular template files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-template-mode))

;; HTML Mode ======================================= ;;

(use-package html-mode
  :ensure nil
  :bind (:map html-mode-map
              ("C-c C-d" . nil))
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode)))

;; Memmet Mode ======================================= ;;

(use-package memmet-mode
  :vc (:url "https://github.com/kborling/memmet-mode" :rev :newest)
  :bind (:map html-mode-map
              ("C-<tab>" . memmet-expand))
  :hook (html-mode . memmet-mode))

;; XML Mode ======================================= ;;

(use-package xml-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode)))

;; Conf Mode ====================================== ;;

(use-package conf-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.sln\\'" . conf-mode)))

;; Rust ============================================ ;;

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;; .NET ============================================ ;;

(use-package dotnet
  :hook (csharp-mode)
  :bind ((("C-c n n" . dotnet-new)
          ("C-c n c" . dotnet-clean)
          ("C-c n t" . dotnet-test)
          ("C-c n r" . dotnet-run)
          ("C-c n a" . dotnet-run-with-args)
          ("C-c n b" . dotnet-build))))

;; EAT ============================================ ;;

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
    ;; Use PowerShell or Git Bash on Windows
    (setq eat-shell (or (executable-find "pwsh")
                        (executable-find "powershell")
                        (executable-find "bash")
                        "cmd.exe")))
  
  ;; Better colors and display
  (setq eat-term-name "xterm-256color")
  
  ;; Eshell integration
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  
  ;; Better keybindings in eat mode
  (add-hook 'eat-mode-hook
            (lambda ()
              (define-key eat-semi-char-mode-map (kbd "C-c C-c") 'eat-self-input)
              (define-key eat-semi-char-mode-map (kbd "C-c C-e") 'eat-emacs-mode)
              (define-key eat-semi-char-mode-map (kbd "C-c C-j") 'eat-char-mode))))

;; Org Mode ===================================== ;;

(use-package org
  ;; :ensure nil
  :config
  ;; Basic Org settings
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

  ;; Source code blocks
  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil)

  ;; Todo keywords for task management
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "FOLLOWUP(f)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Agenda configuration
  (setq org-agenda-files '("~/.org/contacts.org" "~/.org/notes.org")
        org-log-done 'time
        org-agenda-include-diary nil
        org-agenda-start-on-weekday nil
        org-agenda-span 7
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  ;; Custom agenda views for work
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

  ;; Structure templates
  (setq org-structure-template-alist
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))

  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (restclient . t)
     (python . t)))

  ;; Org keybindings
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  ;; Windows font compatibility fixes
  (when (eq system-type 'windows-nt)
    ;; Use simple ASCII bullets instead of Unicode symbols
    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          ;; Alternative: use even simpler bullets if above don't work
          ;; org-modern-star '("●" "○" "◦" "▪" "▫" "▸" "▹" "▸")
          ;; Or minimal ASCII-only option:
          ;; org-modern-star '("*" "+" "-" "*" "+" "-" "*" "+")

          ;; Disable problematic Unicode elements
          org-modern-list '((?+ . "•") (?- . "–") (?* . "•"))
          org-modern-block-name '("▼" . "▶")  ; Simple arrows
          org-modern-keyword nil  ; Disable fancy keywords
          org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐"))
          org-modern-horizontal-rule "─"  ; Simple line
          )))

;; Contact Management ================================ ;;

;; Load org-templates and org-contacts-simple
(with-eval-after-load 'org
  (require 'org-templates)
  (require 'org-contacts-simple))

;; Winum - Window Numbers ========================== ;;

(use-package winum
  :ensure t
  :config
  (winum-mode)
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
