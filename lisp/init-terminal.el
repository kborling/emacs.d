;;; init-terminal.el --- Terminal mode optimizations -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimizations for running Emacs in terminal mode (emacs -nw)
;; especially within tmux

;;; Code:

;; Detect if we're running in terminal mode
(defvar kdb-terminal-mode (not (display-graphic-p))
  "Non-nil if Emacs is running in terminal mode.")

;; Detect if we're running in tmux
(defvar kdb-in-tmux (getenv "TMUX")
  "Non-nil if Emacs is running inside tmux.")

;; Terminal-specific settings
(when kdb-terminal-mode
  ;; Enable mouse support in terminal
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  
  ;; Better clipboard integration
  (setq select-enable-clipboard t
        select-enable-primary t)
  
  ;; Use clipboard for kill/yank when in terminal
  (when (executable-find "pbcopy")  ; macOS
    (defun kdb-copy-to-osx-clipboard ()
      "Copy region to macOS clipboard."
      (interactive)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc (buffer-substring (region-beginning) (region-end)))
          (process-send-eof proc))))
    
    (defun kdb-paste-from-osx-clipboard ()
      "Paste from macOS clipboard."
      (interactive)
      (insert (shell-command-to-string "pbpaste"))))
  
  ;; Disable features that don't work well in terminal
  (menu-bar-mode -1)
  (setq-default cursor-type 'bar)
  
  ;; Fix colors for terminal
  (setq frame-background-mode 'dark))

;; Tmux-specific optimizations
(when kdb-in-tmux
  ;; Fix escape key delay in tmux
  (setq ttymouse-mode 'xterm2)
  
  ;; Better key handling
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2D" [S-left])
  
  ;; Make C-t available since tmux uses it as prefix
  ;; You're already using C-t as tmux prefix, so this is handled
  
  ;; Enable 256 colors
  (setq xterm-color-names-bright
        ["#3c3836" ; black
         "#fb4934" ; red
         "#b8bb26" ; green
         "#fabd2f" ; yellow
         "#83a598" ; blue
         "#d3869b" ; magenta
         "#8ec07c" ; cyan
         "#ebdbb2"]; white
        ))

;; Terminal-friendly keybindings
(when kdb-terminal-mode
  ;; Alternative bindings for common operations that might conflict
  ;; Since C-t is your tmux prefix, provide alternatives
  (global-set-key (kbd "C-c C-t") 'transpose-chars)
  
  ;; Terminal-friendly window management
  (global-set-key (kbd "M-1") 'delete-other-windows)
  (global-set-key (kbd "M-2") 'split-window-below)
  (global-set-key (kbd "M-3") 'split-window-right)
  (global-set-key (kbd "M-0") 'delete-window)
  
  ;; Since tmux handles panes, make Emacs window switching easier
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1))))

;; Fix terminal paste issues
(when kdb-terminal-mode
  ;; Bracketed paste mode support
  (defun kdb-bracketed-paste-enable ()
    "Enable bracketed paste mode."
    (send-string-to-terminal "\e[?2004h"))
  
  (defun kdb-bracketed-paste-disable ()
    "Disable bracketed paste mode."
    (send-string-to-terminal "\e[?2004l"))
  
  (add-hook 'after-init-hook 'kdb-bracketed-paste-enable)
  (add-hook 'kill-emacs-hook 'kdb-bracketed-paste-disable))

;; Terminal color theme adjustments
(defun kdb-optimize-theme-for-terminal ()
  "Adjust current theme for better terminal visibility."
  (when kdb-terminal-mode
    ;; Make comments more visible
    (set-face-attribute 'font-lock-comment-face nil :foreground "brightblack")
    ;; Better region visibility
    (set-face-attribute 'region nil :background "brightblack")
    ;; Clearer mode line
    (set-face-attribute 'mode-line nil :background "brightblack" :foreground "white")
    (set-face-attribute 'mode-line-inactive nil :background "black" :foreground "brightblack")))

(add-hook 'after-load-theme-hook 'kdb-optimize-theme-for-terminal)

;; Special handling for eat terminal in terminal mode
(when kdb-terminal-mode
  ;; Don't try to use eat inside terminal Emacs
  (with-eval-after-load 'init
    (defun kdb-eat-new ()
      "In terminal mode, use ansi-term instead of eat."
      (interactive)
      (if (fboundp 'ansi-term)
          (ansi-term (or (getenv "SHELL") "/bin/bash"))
        (term (or (getenv "SHELL") "/bin/bash"))))))

;; Tmux integration commands
(when kdb-in-tmux
  (defun kdb-tmux-new-window ()
    "Create a new tmux window."
    (interactive)
    (shell-command "tmux new-window"))
  
  (defun kdb-tmux-split-horizontal ()
    "Split tmux pane horizontally."
    (interactive)
    (shell-command "tmux split-window -h"))
  
  (defun kdb-tmux-split-vertical ()
    "Split tmux pane vertically."
    (interactive)
    (shell-command "tmux split-window -v"))
  
  ;; Keybindings for tmux operations
  (global-set-key (kbd "C-c T n") 'kdb-tmux-new-window)
  (global-set-key (kbd "C-c T |") 'kdb-tmux-split-horizontal)
  (global-set-key (kbd "C-c T -") 'kdb-tmux-split-vertical))

;; Message about terminal mode
(when kdb-terminal-mode
  (message "Terminal mode detected. Mouse support enabled.%s"
           (if kdb-in-tmux " Running in tmux." "")))

(provide 'init-terminal)
;;; init-terminal.el ends here