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
 global-mark-ring-max 50000)

;; Contextual menu with right mouse button
(when (display-graphic-p)
  (context-menu-mode))

;; Remember cursor place
(setq
 save-place-file (locate-user-emacs-file "saveplace"))
(save-place-mode 1)

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
                      :family "FreeSans" :height 1.2 :weight 'regular))

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
  ;; (load-theme 'uwu t)
  )

(use-package acme-theme)

(use-package standard-themes)

(use-package fleury-theme
  :vc (:url "https://github.com/ShamsParvezArka/fleury-theme.el" :rev :newest)
  :config
  (global-hl-line-mode +1)
  (setq-default cursor-type '(bar . 3))
  (load-theme 'fleury t))

(use-package distinguished-theme)

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

;; Save History ====================================== ;;

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq
   history-length 10000
   history-delete-duplicates t))

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
              ("M-/" . isearch-complete))
  :config
  (setq
   search-whitespace-regexp ".*?"
   isearch-lazy-count t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " (%s/%s)"
   isearch-wrap-pause 'no
   isearch-yank-on-move 'shift
   isearch-allow-scroll 'unlimited
   isearch-repeat-on-direction-change t
   lazy-highlight-initial-delay 0.5)
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

;; Async ============================================== ;;

(use-package async
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'dired
    (require 'dired-async)
    (dired-async-mode 1))
  :config
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
   eglot-extend-to-xref t
   eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentHighlightProvider))

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
   flymake-no-changes-timeout 0
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
  :bind
  (:map global-map
        ("C-x v B" . vc-annotate) ; blame
        ("C-x v e" . vc-ediff)
        ("C-x v k" . vc-delete-file)
        ("C-x v g" . vc-log-search)
        ("C-x v i" . vc-create-repo) ; initialize
        ("C-x v a" . vc-register) ; add
        ("C-x v t" . vc-create-tag)
        ("C-x v f" . vc-update) ; pull
        ("C-x v p" . vc-push)
        ("C-x v c" . vc-clone)
        ("C-x v d" . vc-diff)
        ("C-x v s" . vc-dir)
        ("C-x v ." . vc-dir-root)
        ("C-x v <return>" . vc-dir-root)
        ("C-x v b b" . vc-switch-branch)
        ("C-x v b n" . vc-create-branch)
        :map vc-dir-mode-map
        ("t" . vc-create-tag)
        ("O" . vc-log-outgoing)
        ("o" . vc-dir-find-file-other-window)
        ("d" . vc-diff)         ; parallel to D: `vc-root-diff'
        ("k" . vc-dir-delete-file)
        ("G" . vc-revert)
        :map vc-git-stash-shared-map
        ("a" . vc-git-stash-apply-at-point)
        ("c" . vc-git-stash) ; "create" named stash
        ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
        ("p" . vc-git-stash-pop-at-point)
        ("s" . vc-git-stash-snapshot)
        :map vc-annotate-mode-map
        ("M-q" . vc-annotate-toggle-annotation-visibility)
        ("C-c C-c" . vc-annotate-goto-line)
        ("<return>" . vc-annotate-find-revision-at-line)
        ;; :map log-edit-mode-map
        ;; ("M-s" . nil)
        ;; ("M-r" . nil)
        :map log-view-mode-map
        ("<tab>" . log-view-toggle-entry-display)
        ("<return>" . log-view-find-revision)
        ("s" . vc-log-search)
        ("o" . vc-log-outgoing)
        ("f" . vc-log-incoming)
        ("F" . vc-update)
        ("P" . vc-push))
  :init
  (setq vc-follow-symlinks t)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  (setq vc-handled-backends '(Git))

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))

(use-package ssh-agency
  :if (eq system-type 'windows-nt)
  :vc (:url "https://github.com/magit/ssh-agency" :rev :newest))

(use-package transient
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-c g g" . magit-status)
         ("C-c g s" . magit-status)
         ("C-c g i" . magit-init)
         ("C-c g c" . magit-clone)
         ("C-c g l" . magit-pull)
         ("C-c g p" . magit-push)
         ("C-c g f" . magit-fetch-all)
         ("C-c g b" . magit-branch)
         ("C-c g B" . magit-blame)
         ("C-c g d" . magit-diff)
         ("C-c g r" . magit-remote)
         ("C-c g z" . magit-stash)
         ("C-c g Z" . magit-apply))
  :init
  (setq magit-define-global-key-bindings nil))

(use-package magit-prime
  :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

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
              ("C-c o" . hl-todo-occur)))

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
  ;; (combobulate-key-prefix "C-c ,")
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

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
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

;; Org Mode ===================================== ;;

(use-package org
  ;; :ensure nil
  :config
  (setq
   org-ellipsis "…"
   org-use-sub-superscripts "{}"
   org-pretty-entities t
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-confirm-babel-evaluate nil
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
   org-startup-indented t
   org-src-preserve-indentation t
   org-edit-src-content-indentation 0
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (shell . t)
      (restclient . t)
      (python . t)))))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

;; Hyperbole ========================================= ;;

(use-package hyperbole
  :ensure t
  :hook (after-init . hyperbole-mode))

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
