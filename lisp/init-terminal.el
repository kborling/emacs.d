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
  ;; Better clipboard integration
  (setq select-enable-clipboard t
        select-enable-primary t)
  
  ;; Clipboard integration for terminal mode
  (let ((copy-cmd (cond
                   ((executable-find "pbcopy") "pbcopy")       ; macOS
                   ((executable-find "xclip") "xclip -selection clipboard") ; Linux X11
                   ((executable-find "xsel") "xsel --clipboard --input")    ; Linux X11 alt
                   ((executable-find "wl-copy") "wl-copy")     ; Linux Wayland
                   ((executable-find "clip.exe") "clip.exe"))) ; Windows/WSL
        (paste-cmd (cond
                    ((executable-find "pbpaste") "pbpaste")
                    ((executable-find "xclip") "xclip -selection clipboard -o")
                    ((executable-find "xsel") "xsel --clipboard --output")
                    ((executable-find "wl-paste") "wl-paste --no-newline")
                    ((executable-find "powershell.exe") "powershell.exe -command Get-Clipboard"))))
    (when copy-cmd
      (setq interprogram-cut-function
            (lambda (text)
              (let ((process-connection-type nil))
                (let ((proc (start-process "clipboard-copy" nil shell-file-name "-c" copy-cmd)))
                  (process-send-string proc text)
                  (process-send-eof proc))))))
    (when paste-cmd
      (setq interprogram-paste-function
            (lambda ()
              (let ((output (shell-command-to-string paste-cmd)))
                (unless (string-empty-p output) output))))))
  
  ;; Disable features that don't work well in terminal
  (menu-bar-mode -1)
  (setq-default cursor-type 'bar)
  
  ;; Fix colors for terminal
  (setq frame-background-mode 'dark))

;; Tmux-specific optimizations  
(when kdb-in-tmux
  ;; Better key handling
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2D" [S-left])
  
)

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
  (message "Terminal mode detected.%s"
           (if kdb-in-tmux " Running in tmux." "")))

(provide 'init-terminal)
;;; init-terminal.el ends here