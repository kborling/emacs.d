;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.6.0
;; Keywords: configuration
;; URL: https://github.com/kborling/emacs.d
;; Homepage: https://github.com/kborling/emacs.d
;; Package-Requires: ((emacs "30"))

;;; Commentary:

;; Copyright (C) 2024 Kevin Borling
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

;; Backups ========================================== ;;

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
 kept-old-versions 2
 create-lockfiles nil)

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
 global-mark-ring-max 50000
 bookmark-save-flag 1)

;; Remember cursor place
(setq
 save-place-file (locate-user-emacs-file "saveplace")
 save-place-forget-unreadable-files t)

(save-place-mode 1)

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

(let* ((settings (cond
                  ((eq system-type 'windows-nt) '(:size 100 :family "Cascadia Code"))
                  ((eq system-type 'gnu/linux)  '(:size 120 :family "Inconsolata"))
                  ((eq system-type 'darwin)     '(:size 160 :family "Inconsolata"))))
       (default-font-size (plist-get settings :size))
       (default-font-family (plist-get settings :family)))
  (set-face-attribute 'default nil
                      :family default-font-family :weight 'regular :height default-font-size)
  (set-face-attribute 'fixed-pitch nil
                      :family default-font-family :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Roboto" :height 1.0 :weight 'regular))

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; Themes ================================================= ;;

(use-package uwu-theme
  :config
  (setq
   uwu-distinct-line-numbers 'nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t)
  (load-theme 'uwu t))

(use-package acme-theme)

(use-package standard-themes)

(use-package ef-themes)

;; UI Enhancements ======================================== ;;

(use-package spacious-padding
  :custom
  (line-spacing 3)
  (spacious-padding-subtle-mode-line t)
  :init
  (spacious-padding-mode 1))

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

;; Keybindings ======================================= ;;

(let ((map global-map))
  ;; Remove suspend keys
  (dolist (key '("C-z" "C-x C-z"))
    (define-key map (kbd key) nil))

  ;; General keybindings
  (dolist (binding '(("C-h C-r" . restart-emacs)
                     ("C-;" . comment-line)
                     ("C-a" . kdb-move-to-beginning-of-line)
                     ("C-k" . kdb-kill-line)
                     ("C-c b" . copy-whole-buffer)
                     ("C-c C-d" . duplicate-line)
                     ("C-x C-r" . recentf)
                     ("C-x f" . project-find-file)
                     ("C-c C-r" . replace-regexp)
                     ("M-z" . zap-up-to-char)
                     ("C-z" . zap-to-char)
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
    (define-key map (kbd (car binding)) (cdr binding))))

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
(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame))
  (setq mac-option-modifier nil
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta))

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
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;; Save History ====================================== ;;

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq
   history-length 10000
   history-delete-duplicates t))

;; Xref ============================================== ;;

(use-package xref
  :ensure nil
  :config
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-file-name-display 'project-relative
   xref-search-program 'ripgrep))

;; Recent Files ====================================== ;;

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq
   recentf-save-file (locate-user-emacs-file "recentf")
   recentf-max-saved-items 50
   recentf-exclude '(".gz" ".xz" ".zip" "/elpaca/" "/elpa/" "/opt/" "/.rustup/" "/elpa/" "/ssh:" "/sudo:" "/node_modules/" "/nix/")))

;; Which Key ========================================= ;;

(use-package which-key
  :ensure nil
  :defer 0
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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
              ("M-/" . isearch-complete))
  :config
  (setq
   search-highlight t
   search-whitespace-regexp ".*?"
   isearch-lax-whitespace t
   isearch-regexp-lax-whitespace nil
   isearch-lazy-highlight t
   isearch-lazy-count t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " (%s/%s)"
   isearch-yank-on-move 'shift
   isearch-allow-scroll 'unlimited
   isearch-repeat-on-direction-change t
   lazy-highlight-initial-delay 0.5
   lazy-highlight-no-delay-length 3
   isearch-wrap-pause t)
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
  :config
  (setq
   diff-advance-after-apply-hunk t
   diff-update-on-the-fly t
   diff-refine nil
   diff-font-lock-prettify t
   diff-font-lock-syntax 'hunk-also))

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
   dired-dwim-target t))

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
   ibuffer-default-shrink-to-minimum-size nil))

;; Project ============================================ ;;

(use-package project
  :ensure nil
  :config
  (setq
   vc-directory-exclusion-list (nconc vc-directory-exclusion-list '("node_modules" "elpa" ".sl"))
   project-vc-extra-root-markers '(".envrc" "package.json" ".project" ".sl")))

;; Orderless ========================================= ;;

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
              (setq-local fussy-max-candidate-limit 5000
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
        icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil
        icomplete-prospects-height 2
        ;; icomplete-separator " . "
        icomplete-with-completion-tables t
        ;; icomplete-in-buffer t
        icomplete-scroll t)
  (global-set-key (kbd "C-=") 'fido-vertical-mode))

(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)

;; Minibuffer ======================================== ;;

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq
   completions-format 'one-column
   completion-show-help nil
   completion-show-inline-help nil
   completion-auto-help 'always
   completion-auto-select nil
   completions-detailed t
   completion-ignore-case t
   read-buffer-completion-ignore-case t
   completions-max-height 20
   completion-flex-nospace nil
   completions-header-format nil
   completions-highlight-face 'completions-highlight
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

;; (defun update-completions-on-typing ()
;;   "Show or hide the *Completions* buffer based on minibuffer input length.
;; The *Completions* buffer is shown after typing at least 2 characters,
;; hidden if fewer than 2 characters are present, and ignores navigation commands."
;;   (when (and (minibufferp)
;;              (not (member this-command '(minibuffer-previous-completion minibuffer-next-completion))))
;;     (if (>= (length (minibuffer-contents)) 2)
;;         ;; Show the *Completions* buffer if input length is 2 or more
;;         (minibuffer-completion-help)
;;       ;; Hide the *Completions* buffer if input length is less than 2
;;       (when-let ((win (get-buffer-window "*Completions*")))
;;         (delete-window win)))))

;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (add-hook 'post-command-hook #'update-completions-on-typing nil t)))

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
   eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentHighlightProvider))

  ;; Language Servers
  (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp" "-lsp")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  ;; See https://github.com/olrtg/emmet-language-server
  (add-to-list 'eglot-server-programs '(html-ts-mode . ("emmet-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-ts-mode . ("emmet-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls" "--stdio")))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rls" "--stdio")))
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

;; NOTE: Be sure to grab the laotest release 'https://github.com/blahgeek/emacs-lsp-booster/releases'
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
   flymake-fringe-indicator-position 'left-fringe
   flymake-suppress-zero-counters t
   flymake-start-on-flymake-mode t
   flymake-no-changes-timeout 0
   ;; flymake-start-on-save-buffer t
   flymake-wrap-around nil
   flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters)
   flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ""))

  (define-key ctl-x-x-map "m" #'flymake-mode)

  :bind (:map flymake-mode-map
              ("C-c f s" . flymake-start)
              ("C-c f d" . flymake-show-buffer-diagnostics)
              ("C-c f D" . flymake-show-project-diagnostics)
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error))
  :hook (prog-mode-hook . flymake-mode)
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
   eldoc-echo-area-use-multiline-p t
   eldoc-idle-delay 0.75))

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
  :init
  (setq vc-follow-symlinks t)
  :config
  (setq vc-handled-backends '(Git)))

(use-package ssh-agency
  :if (eq system-type 'windows-nt)
  :vc (:url "https://github.com/magit/ssh-agency" :rev :newest))

(use-package transient
  :defer t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq
   magit-status-headers-hook nil
   magit-status-sections-hook'(magit-insert-error-header
                               magit-insert-head-branch-header
                               magit-insert-unstaged-changes
                               magit-insert-staged-changes
                               magit-insert-unpushed-to-pushremote)))

;; Marginalia ======================================== ;;

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0))

;; Highlight TODOs ===================================== ;;

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c p" . hl-todo-previous)
              ("C-c n" . hl-todo-next)
              ("C-c o" . hl-todo-occur)
              ("C-c i" . hl-todo-insert)))

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
  :preface
  (setq combobulate-key-prefix "C-c b")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode)
         (angular-template-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; Multiple Cursors ================================== ;;

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)))

;; So Long =========================================== ;;

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; Angular =========================================== ;;

(use-package angular-mode
  :vc (:url "https://github.com/kborling/angular-mode" :rev :newest)
  :config
  (defun angular-open-interface ()
    "Open an Angular interface file in the project."
    (interactive)
    (angular-open-file "interface"))
  (global-set-key (kbd "C-c a o f") 'angular-open-interface))

(define-derived-mode angular-template-mode html-ts-mode "Angular Template"
  "A major mode derived from 'html-ts-mode', for editing angular template files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-template-mode))

;; Typescript ===================================== ;;

(use-package typescript-ts-mode
  :ensure nil
  :config
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (setq-local indent-line-function 'js-indent-line)
              (setq-local indent-region-function
                          (lambda (start end)
                            (save-excursion
                              (goto-char start)
                              (while (< (point) end)
                                (js-indent-line)
                                (forward-line 1)))))))
  (setq-local indent-line-function 'js-indent-line))

;; EAT ============================================ ;;

(use-package eat
  :ensure t
  :hook (eshell-load-hook . eat-eshell-mode))


;; Dape =========================================== ;;
(use-package dape
  :preface
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-hide-mode-line nil)
  (setq dape-inlay-hints t)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer))

;; Org Mode ===================================== ;;

;;; Org Mode
(defun kdb/org-mode-setup ()
  "Setup org mode."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (electric-indent-local-mode -1)
  ;; (auto-fill-mode 1)
  (setq cursor-type 'bar))

(use-package org
  :ensure nil
  :commands (org-capture org-agenda)
  :hook (org-mode . kdb/org-mode-setup)
  :config

  (setq
   org-ellipsis "…"
   org-use-sub-superscripts "{}"
   org-pretty-entities t
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-hide-emphasis-markers t
   org-confirm-babel-evaluate nil
   org-export-with-smart-quotes t
   org-src-window-setup 'current-window
   org-directory "~/org/"
   org-todo-keyword
   '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED" "BLOCKED" "FIXME"))
   org-structure-template-alist
   '(("s" . "src")
     ("E" . "src emacs-lisp")
     ("e" . "example")
     ("q" . "quote")
     ("v" . "verse")
     ("V" . "verbatim")
     ("c" . "center")
     ("C" . "comment"))
   org-confirm-babel-evaluate nil
   org-src-window-setup 'current-window
   org-edit-src-persistent-message nil
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 0
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-export-with-drawers nil
   org-export-with-todo-keywords nil
   org-export-with-broken-links t
   org-export-with-toc nil
   org-export-with-smart-quotes t
   org-export-headline-levels 8
   org-export-dispatch-use-expert-ui nil
   org-html-htmlize-output-type nil
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (shell . t)
      (restclient . t)
      (python . t)))))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

;; Redacted ===================================== ;;

(use-package obfuscate-mode
  :vc (:url "https://github.com/kborling/obfuscate-mode.el" :rev :newest)
  :ensure t
  :config
  (global-set-key (kbd "C-c t r") 'obfuscate-mode)

  (defvar kdb-idle-timer nil
    "Timer to toggle `obfuscate-mode` on inactivity for all windows.")

  (defvar kdb-idle-timeout 300
    "Number of seconds of inactivity before toggling `obfuscate-mode` in all buffers.")

  (defvar kdb-obfuscate-active nil
    "Track whether `obfuscate-mode` is currently active in all windows.")

  (defun kdb-enable-obfuscate-mode-for-all-windows ()
    "Enable `obfuscate-mode` for all visible buffers."
    (unless kdb-obfuscate-active
      (message "Enabling obfuscate-mode for all windows due to inactivity")
      (setq kdb-obfuscate-active t)
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          (obfuscate-mode 1)))))

  (defun kdb-disable-obfuscate-mode-for-all-windows ()
    "Disable `obfuscate-mode` for all visible buffers."
    (when kdb-obfuscate-active
      (message "Disabling obfuscate-mode for all windows due to activity")
      (setq kdb-obfuscate-active nil)
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          (obfuscate-mode -1)))))

  (defun kdb-setup-idle-obfuscate-mode-for-all-windows ()
    "Set up idle toggling for `obfuscate-mode` across all windows."
    (setq kdb-idle-timer
          (run-with-idle-timer kdb-idle-timeout t #'kdb-enable-obfuscate-mode-for-all-windows))
    (add-hook 'post-command-hook #'kdb-disable-obfuscate-mode-for-all-windows))

  (defun kdb-disable-idle-obfuscate-mode-for-all-windows ()
    "Remove idle toggling for `obfuscate-mode` across all windows."
    (when kdb-idle-timer
      (cancel-timer kdb-idle-timer)
      (setq kdb-idle-timer nil))
    (remove-hook 'post-command-hook #'kdb-disable-obfuscate-mode-for-all-windows))

  ;; Enable the setup
  (kdb-setup-idle-obfuscate-mode-for-all-windows)

  ;; To disable the functionality, call:
  ;; (kdb-disable-idle-obfuscate-mode-for-all-windows)
  )

;; Meow ========================================= ;;

(use-package meow
  :ensure t
  :hook (meow-mode . meow-setup)
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     '("<escape>" . ignore))
    ;; custom keybinding for motion state
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore))))
(global-set-key (kbd "C-c t m") 'meow-global-mode)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
