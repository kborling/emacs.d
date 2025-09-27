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

;; Load personal settings if they exist (not tracked in git)
(let ((personal-file (locate-user-emacs-file "personal.el")))
  (when (file-exists-p personal-file)
    (load personal-file)))


(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


(fset #'jsonrpc--log-event #'ignore)

;; Native compilation settings to avoid battery drain
(setq native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 4)

(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      jit-lock-defer-time 0
      jit-lock-stealth-time 0.2)


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

;; Focus cursor when Messages buffer is displayed
(defun kdb-focus-messages-buffer ()
  "Focus the *Messages* buffer when it's displayed."
  (when (string= (buffer-name) "*Messages*")
    (goto-char (point-max))))

(add-hook 'buffer-list-update-hook
          (lambda ()
            (when (and (get-buffer-window "*Messages*")
                       (string= (buffer-name (window-buffer (get-buffer-window "*Messages*"))) "*Messages*"))
              (with-current-buffer "*Messages*"
                (goto-char (point-max))))))

;; Alternative: advice on display-buffer for Messages
(defun kdb-messages-buffer-advice (buffer &optional action frame)
  "Focus Messages buffer when displayed."
  (when (and buffer (string= (buffer-name buffer) "*Messages*"))
    (with-current-buffer buffer
      (goto-char (point-max)))
    (let ((window (get-buffer-window buffer)))
      (when window
        (select-window window)
        (goto-char (point-max))))))

(advice-add 'display-buffer :after #'kdb-messages-buffer-advice)

;; Platform-specific window configuration
(when (display-graphic-p)
  (cond
   ;; macOS: open maximized
   ((eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))
   ;; Windows: split to right side of screen
   ((eq system-type 'windows-nt)
    (add-to-list 'default-frame-alist '(left . 0.5))
    (add-to-list 'default-frame-alist '(width . 0.5))
    (add-to-list 'default-frame-alist '(top . 0))
    (add-to-list 'default-frame-alist '(height . 1.0)))))


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
                  ((eq system-type 'windows-nt) '(:size 110 :families ("Rec Mono Semicasual" "Cascadia Code" "Consolas" "Courier New")))
                  ((eq system-type 'gnu/linux)  '(:size 120 :families ("Inconsolata" "DejaVu Sans Mono" "Liberation Mono" "monospace")))
                  ((eq system-type 'darwin)     '(:size 150 :families ("Overpass Mono" "Monaco" "Menlo" "monospace")))))
       (default-font-size (plist-get settings :size))
       (font-families (plist-get settings :families))
       (default-font-family (cl-find-if (lambda (font) (find-font (font-spec :family font))) font-families)))

  ;; Set the font if we found one available
  (when default-font-family
    (set-face-attribute 'default nil
                        :family default-font-family :weight 'light :height default-font-size)
    (set-face-attribute 'fixed-pitch nil
                        :family default-font-family :height 1.0))

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

;; Terminal management functions (defined early for keybindings)
(defun kdb-eshell-new ()
  "Create a new eshell buffer with a unique name."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*eshell*")))
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
  "Create a new terminal with unique name, using best available for platform."
  (interactive)
  (cond
   ;; In terminal mode, always use ansi-term
   ((not (display-graphic-p))
    (if (fboundp 'ansi-term)
        (ansi-term (or (getenv "SHELL") (getenv "COMSPEC") "/bin/bash"))
      (term (or (getenv "SHELL") (getenv "COMSPEC") "/bin/bash"))))
   ;; On Windows, use ansi-term or term
   ((eq system-type 'windows-nt)
    (if (fboundp 'ansi-term)
        (ansi-term (or (getenv "COMSPEC") "cmd.exe"))
      (term (or (getenv "COMSPEC") "cmd.exe"))))
   ;; On GUI systems, try eat first, fall back to ansi-term
   ((and (fboundp 'eat) (display-graphic-p))
    (let ((eat-buffer-name (generate-new-buffer-name "*eat*")))
      (eat)))
   ;; Fallback to ansi-term
   ((fboundp 'ansi-term)
    (ansi-term (or (getenv "SHELL") "/bin/bash")))
   ;; Last resort: basic term
   (t
    (term (or (getenv "SHELL") "/bin/bash")))))


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


(defun kdb-create-tags (dir-name)
  "Create tags file using DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" (locate-file "ctags" exec-path) (directory-file-name dir-name))))


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
                       ("C-c C-d" . duplicate-line)
                       ("C-x C-r" . recentf)
                       ("C-x f" . project-find-file)
                       ("C-c C-r" . replace-regexp)
                       ("C-\\" . hippie-expand)
                       ("M-z" . zap-up-to-char)
                       ("C-M-s" . isearch-forward-symbol-at-point)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; TODOO navigation (hl-todo)
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

    ;; Configuration shortcuts
    (dolist (binding '(("C-c e v" . config-visit)
                       ("C-c e r" . config-reload)))
      (define-key map (kbd (car binding)) (cdr binding)))

    ;; Toggling features
    (dolist (binding '(("C-c t h" . toggle-theme)  ; Changed from t t to t h
                       ("C-c t f" . toggle-frame-fullscreen)))
      (define-key map (kbd (car binding)) (cdr binding)))))

(add-hook 'after-init-hook #'kdb-setup-keybindings)

(defun kdb-keyboard-quit-dwim ()
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

(define-key global-map (kbd "C-g") #'kdb-keyboard-quit-dwim)

;; (when (eq system-type 'darwin)
;;   (select-frame-set-input-focus (selected-frame))
;;   (setq mac-option-modifier nil
;;         ns-function-modifier 'super
;;         mac-right-command-modifier 'hyper
;;         mac-right-option-modifier 'alt
;;         mac-command-modifier 'meta))


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

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))


(use-package grep
  :ensure nil
  :config
  (setq grep-command "rg --color=never --no-heading --line-number --smart-case "
        ;; grep-find-command
        ;; (concat "fd --type f --hidden --follow --exclude .git | "
        ;;         "xargs rg --color=never --no-heading --line-number --smart-case ")
        grep-use-null-device nil))


(use-package deadgrep
  :ensure t
  :config
  (setq deadgrep-extra-arguments '("--no-config" "--multiline"))
  (global-set-key (kbd "C-x g") #'deadgrep))


(use-package xref
  :ensure nil
  :config
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-search-program 'ripgrep))



(use-package which-key
  :ensure nil
  :defer 0
  :config
  (which-key-mode))


(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))


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
      ("Emacs Config" (or (filename . "\\.emacs\\.d")
                          (name . "^\\*scratch\\*")
                          (name . "^\\*Messages\\*")))
      ("VC/Git" (or (name . "^\\*magit")
                    (name . "^\\*vc-")
                    (name . "^\\*git-")
                    (mode . diff-mode)
                    (mode . log-view-mode)))
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
                       (mode . helpful-mode)))
      ("Special" (name . "^\\*")))))

  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))

  :bind (:map ibuffer-mode-map
              ("M-o" . other-window)
              ("/" . ibuffer-filter-by-name)))


(use-package project
  :ensure nil
  :config
  (setq
   vc-directory-exclusion-list (nconc vc-directory-exclusion-list '("node_modules" "elpa" ".sl"))
   project-vc-extra-root-markers '(".envrc" "package.json" ".project" ".sl")))


(use-package async
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'dired
    (require 'dired-async)
    (dired-async-mode 1))
  :config
  (async-bytecomp-package-mode 1))


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

(defun kdb-eshell-new ()
  "Create a new eshell buffer with a unique name."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*eshell*")))
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

(when (package-installed-p 'eat)
  (use-package eat
    :config
    (setq eat-kill-buffer-on-exit t
          eat-enable-mouse t)))

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
        icomplete-scroll t))


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
   completions-max-height 14
   completions-header-format nil
   minibuffer-visible-completions nil
   enable-recursive-minibuffers t
   completions-sort 'historical
   read-answer-short t))
  ;; :bind (:map minibuffer-local-map
  ;;             ("C-p" . minibuffer-previous-completion)
  ;;             ("C-n" . minibuffer-next-completion))
  ;; :bind (:map completion-in-region-mode-map
  ;;             ("C-p" . minibuffer-previous-completion)
  ;;             ("C-n" . minibuffer-next-completion)
  ;;             ("RET" . minibuffer-choose-completion)))

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

  (set-face-attribute 'flymake-error nil
                      :underline '(:style wave :color "red"))
  (set-face-attribute 'flymake-warning nil
                      :underline '(:style wave :color "yellow"))
  (set-face-attribute 'flymake-note nil
                      :underline '(:style wave :color "blue"))

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


(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode 1)
  (setq
   eldoc-echo-area-use-multiline-p t))

;; (use-package eldoc-box
;;   :after eldoc
;;   :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))


(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))


;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load VC configuration  
(require 'init-vc)

;; Load Org configuration
(require 'init-org)

;; Load encryption configuration
(require 'init-encryption)

;; Load terminal optimizations
(require 'init-terminal)



(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0
        marginalia-align-offset 10))


(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))


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


(use-package tempel
  :bind (("C-<tab>" . tempel-complete)
         ("M-+" . tempel-insert)
         ("C-1" . tempel-previous)
         ("C-2" . tempel-next)))

(use-package tempel-collection
  :after tempel)


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

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Enable markdown treesitter support if available
(when (treesit-language-available-p 'markdown)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode)))

(when (treesit-language-available-p 'markdown-inline)
  (add-to-list 'treesit-auto-langs 'markdown-inline))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :custom
  (combobulate-key-prefix "C-c O")
  (combobulate-flash-node t)
  (combobulate-dimmer-mode t)
  (combobulate-envelope-indent-region-function #'indent-region)
  :hook
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


(use-package treesit-expand
  :ensure nil
  :vc (:url "https://github.com/kborling/treesit-expand" :rev :newest)
  :bind (("C-=" . treesit-expand-dwim)
         ("C--" . treesit-contract-region)
         ("C-c e e" . treesit-expand-region)
         ("C-c e q" . treesit-expand-reset)))

;; Agent Shell for AI integration
(use-package shell-maker
  :ensure t)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest)
  :after (shell-maker acp)
  :config
  (defun agent-shell-anthropic-key ()
    "Get Anthropic API key from environment or auth-source."
    (or (getenv "ANTHROPIC_API_KEY")
        (getenv "CLAUDE_API_KEY")))

  (defun agent-shell-start-claude-code-agent ()
    "Start an interactive Claude Code agent shell."
    (interactive)
    (agent-shell--start
     :new-session t
     :mode-line-name "Claude Code"
     :buffer-name "Claude Code"
     :shell-prompt "Claude Code> "
     :shell-prompt-regexp "Claude Code> "
     :client-maker (lambda ()
                     (acp-make-client :command "claude-code-acp"
                                      :environment-variables
                                      (when (agent-shell-anthropic-key)
                                        (list (format "ANTHROPIC_API_KEY=%s" (agent-shell-anthropic-key))))))))

  :commands (agent-shell-start-claude-code-agent))

;; Test function to check if agent-shell is working
(defun test-agent-shell ()
  "Test if agent-shell packages are loaded."
  (interactive)
  (if (fboundp 'agent-shell-start-claude-code-agent)
      (progn
        (message "Agent-shell is loaded, starting Claude Code...")
        (agent-shell-start-claude-code-agent))
    (message "Agent-shell packages not yet loaded. Try: M-x package-install RET shell-maker")))

;; Claude Code EAT-based integration (autonomous AI coding)
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :after eat
  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'eat)
  :bind (("C-c c" . claude-code-transient)))
(global-set-key (kbd "C-c a c") 'agent-shell-start-claude-code-agent)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-'" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)))


(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))


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


(use-package memmet-mode
  :vc (:url "https://github.com/kborling/memmet-mode" :rev :newest)
  :bind (:map html-mode-map
              ("C-<tab>" . memmet-expand))
  :hook (html-mode . memmet-mode))


(use-package xml-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode)))


(use-package conf-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.sln\\'" . conf-mode)))


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
