;;; init-exwm.el --- EXWM window manager configuration -*- lexical-binding: t; -*-

;; Author: Kevin Borling
;; Keywords: exwm window-manager

;;; Commentary:

;; Comprehensive EXWM (Emacs X Window Manager) configuration
;; Provides a complete window management environment using Emacs

;;; Code:

(require 'exwm)

;;; Basic EXWM Setup =====================================================

;; Set the number of workspaces
(setq exwm-workspace-number 10)

;; Allow buffers to be accessed from any workspace
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Mouse workflow improvements
;; Automatically send the mouse cursor to the selected workspace's display
(setq exwm-workspace-warp-cursor t)

;;; Workspace Names ======================================================

(defun kdb-exwm-workspace-names (index)
  "Return workspace name for INDEX (1-based numbering)."
  (number-to-string (1+ index)))

(setq exwm-workspace-index-map #'kdb-exwm-workspace-names)

;; Custom tab-bar format to show workspace info
(defun kdb-exwm-tab-bar-workspace-name ()
  "Show current EXWM workspace in tab bar."
  (let ((ws-name (if (and (featurep 'exwm)
                          (boundp 'exwm-workspace-current-index))
                     (funcall exwm-workspace-index-map exwm-workspace-current-index)
                   "N/A")))
    `((exwm-workspace menu-item
                      ,(format " [%s] " ws-name)
                      ignore
                      :help "Current workspace"))))

(defun kdb-exwm-tab-bar-time ()
  "Display current time in 24-hour format."
  `((time menu-item
          ,(format-time-string " %H:%M ")
          ignore
          :help ,(format-time-string "%A, %B %d, %Y %H:%M"))))

(defun kdb-exwm-tab-bar-spacer ()
  "Add spacing for systemtray on the right."
  '((systemtray-spacer menu-item "     " ignore)))

(defun kdb-exwm-setup-tab-bar ()
  "Configure tab-bar for EXWM."
  (setq tab-bar-format '(kdb-exwm-tab-bar-workspace-name
                         tab-bar-format-align-right
                         kdb-exwm-tab-bar-time
                         kdb-exwm-tab-bar-spacer))
  ;; Update time display every minute
  (run-with-timer 0 60 #'force-mode-line-update t))

;;; Window Management ====================================================

;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook
          (lambda ()
            (pcase exwm-class-name
              ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
              ("Google-chrome" (exwm-workspace-rename-buffer (format "Chrome: %s" exwm-title)))
              (_ (exwm-workspace-rename-buffer exwm-title)))))

;; Configure window behavior
;; Note: Commented out workspace assignments so windows open on current workspace
;; Uncomment specific rules if you want apps to open on specific workspaces
(setq exwm-manage-configurations
      '(;; ((string= exwm-class-name "firefox")
        ;;  workspace 1
        ;;  char-mode t)
        ;; ((string= exwm-class-name "Google-chrome")
        ;;  workspace 1
        ;;  char-mode t)
        ;; ((string= exwm-class-name "Slack")
        ;;  workspace 3
        ;;  char-mode t)
        ;; ((string= exwm-class-name "Discord")
        ;;  workspace 3
        ;;  char-mode t)
        ;; ((string= exwm-class-name "Spotify")
        ;;  workspace 4
        ;;  char-mode t)
        ))

;;; Multi-Monitor Support ================================================

(defun kdb-exwm-update-displays ()
  "Update display configuration."
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output LVDS-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal"))

;; Enable RANDR support for multi-monitor
(defun kdb-exwm-enable-randr ()
  "Enable EXWM RANDR support."
  (condition-case err
      (progn
        (require 'exwm-randr)
        (setq exwm-randr-workspace-monitor-plist '(0 "LVDS-1" 1 "VGA-1"))
        (add-hook 'exwm-randr-screen-change-hook #'kdb-exwm-update-displays)
        ;; Use new API if available
        (if (fboundp 'exwm-randr-mode)
            (exwm-randr-mode 1)
          (exwm-randr-enable)))
    (error (message "EXWM RANDR not available: %s" err))))

;;; System Tray ==========================================================

;; Configuration variable to choose between EXWM systemtray and Polybar
(defvar kdb-exwm-use-polybar t
  "If non-nil, use Polybar instead of EXWM's built-in systemtray.
Toggle with `kdb-exwm-toggle-panel-mode'.")

(defun kdb-exwm-enable-systemtray ()
  "Enable EXWM's built-in system tray."
  (condition-case err
      (progn
        (require 'exwm-systemtray)
        ;; Configure systemtray
        (setq exwm-systemtray-height 20)
        ;; Position systemtray at top-right corner
        (setq exwm-systemtray-position 'top)
        ;; Set background color to improve visibility in dark mode
        ;; This will be dynamically updated based on theme
        (setq exwm-systemtray-background-color
              (face-attribute 'default :background nil 'default))
        ;; Enable systemtray
        (if (fboundp 'exwm-systemtray-mode)
            (exwm-systemtray-mode 1)
          (exwm-systemtray-enable)))
    (error (message "EXWM systemtray not available: %s" err))))

;;; Polybar Panel ========================================================

(defvar kdb-exwm-polybar-process nil
  "Holds the process of the running Polybar instance, if any.")

(defun kdb-exwm-kill-panel ()
  "Kill the running Polybar panel."
  (interactive)
  (when kdb-exwm-polybar-process
    (ignore-errors
      (kill-process kdb-exwm-polybar-process)))
  (setq kdb-exwm-polybar-process nil))

(defun kdb-exwm-start-panel ()
  "Start the Polybar panel."
  (interactive)
  (kdb-exwm-kill-panel)
  (setq kdb-exwm-polybar-process
        (start-process-shell-command "polybar" nil "polybar exwm-panel")))

(defun kdb-exwm-send-polybar-hook (module-name hook-index)
  "Send a hook message to Polybar MODULE-NAME at HOOK-INDEX."
  (start-process-shell-command
   "polybar-msg" nil
   (format "polybar-msg hook %s %s" module-name hook-index)))

(defun kdb-exwm-send-polybar-exwm-workspace ()
  "Update Polybar with current EXWM workspace."
  (kdb-exwm-send-polybar-hook "exwm-workspace" 1))

(defun kdb-exwm-toggle-panel-mode ()
  "Toggle between EXWM systemtray and Polybar panel."
  (interactive)
  (if kdb-exwm-use-polybar
      ;; Switch to EXWM systemtray
      (progn
        (setq kdb-exwm-use-polybar nil)
        (kdb-exwm-kill-panel)
        ;; Enable tab-bar for systemtray mode
        (setq tab-bar-show t
              tab-bar-close-button-show nil
              tab-bar-new-button-show nil)
        (kdb-exwm-setup-tab-bar)
        (tab-bar-mode 1)
        (kdb-exwm-enable-systemtray)
        (message "Switched to EXWM built-in systemtray with tab-bar"))
    ;; Switch to Polybar
    (progn
      (setq kdb-exwm-use-polybar t)
      ;; Disable tab-bar (Polybar replaces it)
      (tab-bar-mode -1)
      ;; Disable EXWM systemtray
      (when (featurep 'exwm-systemtray)
        (if (fboundp 'exwm-systemtray-mode)
            (exwm-systemtray-mode -1)
          (exwm-systemtray-disable)))
      (kdb-exwm-start-panel)
      (message "Switched to Polybar panel (tab-bar disabled)"))))

;; Update Polybar workspace indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'kdb-exwm-send-polybar-exwm-workspace)

;;; Global Keybindings ===================================================

;; Set the Super key as the EXWM modifier
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ?\C-\M-j  ;; Buffer list
        ?\C-\ ))  ;; Ctrl+Space

;; Ctrl+q to enable the next key to be sent directly to application
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings (Super key bindings that work everywhere)
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (from char-mode)
        ([?\s-r] . exwm-reset)

        ;; Switch workspace with Super+number (1-9 map to workspaces 0-8, 0 maps to workspace 9)
        ,@(mapcar (lambda (i)
                    (let ((workspace (if (= i 0) 9 (1- i))))
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,workspace)))))
                  (number-sequence 0 9))

        ;; Move window to workspace with Super+Shift+number
        ,@(mapcar (lambda (i)
                    (let ((workspace (if (= i 0) 9 (1- i))))
                      `(,(kbd (format "s-S-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-move-window ,workspace)))))
                  (number-sequence 0 9))

        ;; Alternative bindings for shifted numbers (!, @, #, etc.) - map to workspaces 0-8
        ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 0)))
        ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 1)))
        ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 2)))
        ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 3)))
        ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 4)))
        ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 5)))
        ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 7)))
        ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 8)))
        ([?\s-)] . (lambda () (interactive) (exwm-workspace-move-window 9)))

        ;; Launch applications (keeping s-& for shell command)
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch to buffer or window
        ([?\s-b] . ibuffer)
        ([?\s-w] . exwm-workspace-switch)

        ;; Window management
        ([?\s-h] . windmove-left)
        ([?\s-j] . windmove-down)
        ([?\s-k] . windmove-up)
        ([?\s-l] . windmove-right)

        ;; Window resizing
        ([?\s-H] . shrink-window-horizontally)
        ([?\s-L] . enlarge-window-horizontally)
        ([?\s-K] . shrink-window)
        ([?\s-J] . enlarge-window)

        ;; Toggle fullscreen
        ([?\s-f] . exwm-layout-toggle-fullscreen)

        ;; Toggle floating
        ([?\s-F] . exwm-floating-toggle-floating)

        ;; Close window
        ([?\s-q] . (lambda () (interactive) (kill-buffer (current-buffer))))))

;;; Simulation Keys ======================================================

;; These keys should always pass through to Emacs
(setq exwm-input-simulation-keys
      '(
        ;; Movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; Cut/paste
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; Search
        ([?\C-s] . [?\C-f])))

;;; Application Launcher =================================================

(defun kdb-exwm-counsel-linux-app ()
  "Launch a Linux application using Emacs completion."
  (interactive)
  (let* ((apps-dir '("/usr/share/applications" "~/.local/share/applications"))
         (desktop-files '())
         (apps '()))
    ;; Find all .desktop files
    (dolist (dir apps-dir)
      (when (file-directory-p dir)
        (setq desktop-files (append desktop-files
                                   (directory-files dir t "\\.desktop$")))))
    ;; Parse desktop files
    (dolist (file desktop-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((name nil) (exec nil))
          (when (re-search-forward "^Name=\\(.*\\)$" nil t)
            (setq name (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward "^Exec=\\(.*\\)$" nil t)
            (setq exec (match-string 1)))
          (when (and name exec)
            ;; Clean up Exec line (remove %f %u etc)
            (setq exec (replace-regexp-in-string " %[fFuU].*$" "" exec))
            (push (cons name exec) apps)))))
    ;; Prompt and launch
    (let* ((choice (completing-read "Launch: " (mapcar #'car apps)))
           (command (cdr (assoc choice apps))))
      (when command
        (start-process-shell-command choice nil command)))))

(defun kdb-exwm-app-launcher ()
  "Launch an application using the best available method."
  (interactive)
  (cond
   ;; Try Emacs built-in launcher first
   (t (kdb-exwm-counsel-linux-app))))

(exwm-input-set-key (kbd "s-SPC") #'kdb-exwm-app-launcher)

;;; Common Application Launchers =========================================

(defun kdb-exwm-launch-browser ()
  "Launch web browser."
  (interactive)
  (cond
   ((executable-find "firefox") (start-process "" nil "firefox"))
   ((executable-find "google-chrome") (start-process "" nil "google-chrome"))
   ((executable-find "chromium") (start-process "" nil "chromium"))
   (t (message "No browser found"))))

(defun kdb-exwm-launch-terminal ()
  "Launch terminal emulator."
  (interactive)
  (cond
   ((executable-find "alacritty") (start-process "" nil "alacritty"))
   ((executable-find "kitty") (start-process "" nil "kitty"))
   ((executable-find "xterm") (start-process "" nil "xterm"))
   (t (eshell))))

(defun kdb-exwm-launch-file-manager ()
  "Launch file manager."
  (interactive)
  (cond
   ((executable-find "thunar") (start-process "" nil "thunar"))
   ((executable-find "nautilus") (start-process "" nil "nautilus"))
   ((executable-find "pcmanfm") (start-process "" nil "pcmanfm"))
   (t (dired "~"))))

;; Bind application launchers
(exwm-input-set-key (kbd "s-<return>") #'kdb-exwm-launch-terminal)
(exwm-input-set-key (kbd "s-e") #'kdb-exwm-launch-file-manager)
(exwm-input-set-key (kbd "s-c") #'kdb-exwm-launch-browser)

;;; Screen Lock ==========================================================

(defun kdb-exwm-lock-screen ()
  "Lock the screen."
  (interactive)
  (cond
   ((executable-find "i3lock")
    (start-process "" nil "i3lock" "-c" "000000"))
   ((executable-find "xscreensaver-command")
    (start-process "" nil "xscreensaver-command" "-lock"))
   (t (message "No screen locker found"))))

(exwm-input-set-key (kbd "s-x") #'kdb-exwm-lock-screen)

;;; Audio Control ========================================================

(defun kdb-exwm-volume-up ()
  "Increase volume."
  (interactive)
  (start-process-shell-command "volume-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%"))

(defun kdb-exwm-volume-down ()
  "Decrease volume."
  (interactive)
  (start-process-shell-command "volume-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%"))

(defun kdb-exwm-volume-mute ()
  "Toggle mute."
  (interactive)
  (start-process-shell-command "volume-mute" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

;; Media keys
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'kdb-exwm-volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'kdb-exwm-volume-down)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'kdb-exwm-volume-mute)

;;; Brightness Control ===================================================

(defun kdb-exwm-brightness-up ()
  "Increase brightness."
  (interactive)
  (start-process-shell-command "brightness-up" nil "brightnessctl set +10%"))

(defun kdb-exwm-brightness-down ()
  "Decrease brightness."
  (interactive)
  (start-process-shell-command "brightness-down" nil "brightnessctl set 10%-"))

;; Brightness keys
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'kdb-exwm-brightness-up)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'kdb-exwm-brightness-down)

;;; Startup Programs =====================================================

(defun kdb-exwm-run-autostart ()
  "Run programs at EXWM startup."
  (interactive)
  ;; Set wallpaper
  (when (executable-find "feh")
    (start-process-shell-command "wallpaper" nil "feh --bg-scale ~/Pictures/wallpaper.jpg"))

  ;; Start compositor for transparency and effects
  (when (executable-find "picom")
    (start-process-shell-command "picom" nil "picom -b"))

  ;; Network manager applet
  (when (executable-find "nm-applet")
    (start-process-shell-command "nm-applet" nil "nm-applet"))

  ;; Bluetooth manager
  (when (executable-find "blueman-applet")
    (start-process-shell-command "blueman" nil "blueman-applet"))

  ;; Screen saver
  (when (executable-find "xscreensaver")
    (start-process-shell-command "xscreensaver" nil "xscreensaver -no-splash"))

  ;; Notification daemon
  (when (executable-find "dunst")
    (start-process-shell-command "dunst" nil "dunst"))

  ;; Red night light (automatic based on time of day)
  ;; Use -l to specify location manually, avoiding GeoClue issues
  ;; Only start redshift during evening/night hours (18:00-06:00)
  (when (executable-find "redshift")
    (let ((hour (string-to-number (format-time-string "%H"))))
      (when (or (>= hour 18) (< hour 6))
        (start-process-shell-command "redshift" nil "redshift -l 40.7:-74.0 -t 6500:3500"))))

  ;; Set X resources
  (when (file-exists-p "~/.Xresources")
    (start-process-shell-command "xrdb" nil "xrdb -merge ~/.Xresources"))

  ;; Load SSH keys into agent
  (when (file-exists-p "~/.ssh/id_ed25519")
    (start-process-shell-command
     "ssh-add" nil
     "SSH_ASKPASS=~/.local/bin/ssh-askpass DISPLAY=:0 ssh-add ~/.ssh/id_ed25519 < /dev/null"))

  ;; Set keyboard layout and options
  (start-process-shell-command "setxkbmap" nil "setxkbmap -option ctrl:nocaps")

  ;; Set keyboard rate (delay 250ms, rate 40/sec for fast repeat)
  (start-process-shell-command "xset" nil "xset r rate 250 40"))

;;; Modeline Integration =================================================

(defun kdb-exwm-modeline-segment ()
  "Display current workspace in modeline."
  (format " [WS:%d] " exwm-workspace-current-index))

;; Add to mode-line-format if desired
;; (setq-default mode-line-format
;;               (cons '(:eval (kdb-exwm-modeline-segment))
;;                     mode-line-format))

;;; Theme Management - Light/Dark Mode ===================================

(defvar kdb-exwm-current-mode 'dark
  "Current theme mode: 'light or 'dark.")

(defvar kdb-exwm-light-theme 'acme
  "Theme to use for light mode.")

(defvar kdb-exwm-dark-theme 'fleury
  "Theme to use for dark mode.")


(defun kdb-exwm-set-kitty-theme (theme)
  "Set kitty terminal theme to THEME (light or dark)."
  ;; Directly execute the sed command and reload kitty
  (let ((config-file (expand-file-name "~/.config/kitty/kitty.conf"))
        (old-theme (if (string= theme "light") "dark" "light")))
    (start-process-shell-command
     "kitty-theme" nil
     (format "sed -i 's|include themes/%s.conf|include themes/%s.conf|g' %s && kill -SIGUSR1 $(pgrep kitty)"
             old-theme theme config-file))
    (message "Set kitty theme to %s" theme)))

(defun kdb-exwm-set-dunst-theme (theme)
  "Set dunst notification theme to THEME (light or dark)."
  (let ((dunst-config (expand-file-name "~/.config/dunst/dunstrc"))
        (theme-config (expand-file-name (format "~/.config/dunst/dunstrc-%s" theme))))
    ;; Update symlink
    (when (file-exists-p theme-config)
      (delete-file dunst-config)
      (make-symbolic-link theme-config dunst-config)
      ;; Restart dunst to apply new config
      (start-process-shell-command
       "dunst-restart" nil
       "killall dunst; dunst &")
      (message "Set dunst theme to %s" theme))))

(defun kdb-exwm-set-gtk-theme (theme)
  "Set GTK theme preference to THEME (light or dark) for Firefox prefers-color-scheme."
  (let ((gtk-theme (if (string= theme "light") "Adwaita" "Adwaita-dark")))
    ;; Set GTK theme which Firefox respects for prefers-color-scheme
    (start-process-shell-command
     "gtk-theme" nil
     (format "gsettings set org.gnome.desktop.interface gtk-theme '%s'" gtk-theme))
    (start-process-shell-command
     "color-scheme" nil
     (format "gsettings set org.gnome.desktop.interface color-scheme 'prefer-%s'" theme))
    (message "Set system color scheme to %s" theme)))

(defun kdb-exwm-set-light-mode ()
  "Switch to light mode: Acme theme, light kitty, light dunst."
  (interactive)
  (setq kdb-exwm-current-mode 'light)

  ;; Disable all themes first, then load light theme
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme kdb-exwm-light-theme t)

  ;; Set system color scheme (for Firefox prefers-color-scheme)
  (kdb-exwm-set-gtk-theme "light")

  ;; Set kitty to light theme
  (kdb-exwm-set-kitty-theme "light")

  ;; Set dunst to light theme
  (kdb-exwm-set-dunst-theme "light")

  ;; Update systemtray background color
  (when (featurep 'exwm-systemtray)
    (setq exwm-systemtray-background-color
          (face-attribute 'default :background nil 'default)))

  ;; Switch Polybar to light theme if using Polybar
  (when kdb-exwm-use-polybar
    (start-process-shell-command
     "polybar-theme" nil
     "cd ~/.config/polybar && ln -sf config-light.ini config && polybar-msg cmd restart"))

  ;; Send message
  (message "Light mode activated"))

(defun kdb-exwm-set-dark-mode ()
  "Switch to dark mode: Uwu theme, dark kitty, dark dunst."
  (interactive)
  (setq kdb-exwm-current-mode 'dark)

  ;; Disable all themes first, then load dark theme
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme kdb-exwm-dark-theme t)

  ;; Set system color scheme (for Firefox prefers-color-scheme)
  (kdb-exwm-set-gtk-theme "dark")

  ;; Set kitty to dark theme
  (kdb-exwm-set-kitty-theme "dark")

  ;; Set dunst to dark theme
  (kdb-exwm-set-dunst-theme "dark")

  ;; Update systemtray background color
  (when (featurep 'exwm-systemtray)
    (setq exwm-systemtray-background-color
          (face-attribute 'default :background nil 'default)))

  ;; Switch Polybar to dark theme if using Polybar
  (when kdb-exwm-use-polybar
    (start-process-shell-command
     "polybar-theme" nil
     "cd ~/.config/polybar && ln -sf config.ini config && polybar-msg cmd restart"))

  ;; Send message
  (message "Dark mode activated"))

(defun kdb-exwm-toggle-theme-mode ()
  "Toggle between light and dark mode system-wide."
  (interactive)
  (if (eq kdb-exwm-current-mode 'dark)
      (kdb-exwm-set-light-mode)
    (kdb-exwm-set-dark-mode)))

;; Bind theme toggle to Super+t
(exwm-input-set-key (kbd "s-t") #'kdb-exwm-toggle-theme-mode)

;;; Power Management =====================================================

(defun kdb-exwm-restart ()
  "Restart EXWM cleanly by killing child processes first."
  (interactive)
  (when (y-or-n-p "Restart EXWM? ")
    ;; Kill common systemtray/background processes that will be restarted
    (start-process-shell-command "cleanup" nil
                                 "killall nm-applet blueman-applet dunst picom 2>/dev/null || true")
    ;; Wait a moment for cleanup
    (run-with-timer 0.5 nil
                    (lambda ()
                      ;; Restart Emacs
                      (restart-emacs)))))

(defun kdb-exwm-power-menu ()
  "Show power management menu."
  (interactive)
  (let ((choice (completing-read "Power: "
                                 '("Lock" "Logout" "Restart EXWM" "Suspend" "Reboot" "Shutdown"))))
    (pcase choice
      ("Lock" (kdb-exwm-lock-screen))
      ("Logout" (save-buffers-kill-emacs))
      ("Restart EXWM" (kdb-exwm-restart))
      ("Suspend" (start-process "" nil "systemctl" "suspend"))
      ("Reboot" (start-process "" nil "systemctl" "reboot"))
      ("Shutdown" (start-process "" nil "systemctl" "poweroff")))))

(exwm-input-set-key (kbd "s-p") #'kdb-exwm-power-menu)

;;; Buffer Management ====================================================

(defun kdb-exwm-buffer-switch ()
  "Switch to EXWM buffer or workspace."
  (interactive)
  (let* ((exwm-buffers (seq-filter
                        (lambda (buffer)
                          (with-current-buffer buffer
                            (eq major-mode 'exwm-mode)))
                        (buffer-list)))
         (buffer-names (mapcar #'buffer-name exwm-buffers))
         (choice (completing-read "Switch to window: " buffer-names)))
    (when choice
      (switch-to-buffer choice))))

(exwm-input-set-key (kbd "s-TAB") #'kdb-exwm-buffer-switch)

;;; Layout Management ====================================================

(defun kdb-exwm-split-window-right ()
  "Split window right and switch to it."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kdb-exwm-split-window-below ()
  "Split window below and switch to it."
  (interactive)
  (split-window-below)
  (other-window 1))

(exwm-input-set-key (kbd "s-v") #'kdb-exwm-split-window-right)
(exwm-input-set-key (kbd "s-s") #'kdb-exwm-split-window-below)
(exwm-input-set-key (kbd "s-d") #'delete-window)
(exwm-input-set-key (kbd "s-D") #'delete-other-windows)

;;; Screenshot ===========================================================

(defun kdb-exwm-screenshot ()
  "Take a screenshot."
  (interactive)
  (cond
   ((executable-find "scrot")
    (start-process-shell-command "screenshot" nil
                                 (format "scrot ~/Pictures/screenshot-%s.png"
                                         (format-time-string "%Y%m%d-%H%M%S"))))
   ((executable-find "maim")
    (start-process-shell-command "screenshot" nil
                                 (format "maim ~/Pictures/screenshot-%s.png"
                                         (format-time-string "%Y%m%d-%H%M%S"))))
   (t (message "No screenshot tool found"))))

(defun kdb-exwm-screenshot-region ()
  "Take a screenshot of selected region."
  (interactive)
  (cond
   ((executable-find "scrot")
    (start-process-shell-command "screenshot" nil
                                 (format "scrot -s ~/Pictures/screenshot-%s.png"
                                         (format-time-string "%Y%m%d-%H%M%S"))))
   ((executable-find "maim")
    (start-process-shell-command "screenshot" nil
                                 (format "maim -s ~/Pictures/screenshot-%s.png"
                                         (format-time-string "%Y%m%d-%H%M%S"))))
   (t (message "No screenshot tool found"))))

(exwm-input-set-key (kbd "s-S") #'kdb-exwm-screenshot)
(exwm-input-set-key (kbd "s-C-s") #'kdb-exwm-screenshot-region)

;;; Helper Functions =====================================================

(defun kdb-exwm-toggle-char-mode ()
  "Toggle between char-mode and line-mode."
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (if (equal exwm--input-mode 'line-mode)
          (exwm-input-release-keyboard)
        (exwm-input-grab-keyboard))
    (message "Not in EXWM buffer")))

(exwm-input-set-key (kbd "s-i") #'kdb-exwm-toggle-char-mode)

;;; Workspace Switching ==================================================

(defun kdb-exwm-workspace-next ()
  "Switch to next workspace."
  (interactive)
  (let ((next (mod (1+ exwm-workspace-current-index) exwm-workspace-number)))
    (exwm-workspace-switch next)))

(defun kdb-exwm-workspace-prev ()
  "Switch to previous workspace."
  (interactive)
  (let ((prev (mod (1- exwm-workspace-current-index) exwm-workspace-number)))
    (exwm-workspace-switch prev)))

(exwm-input-set-key (kbd "s-]") #'kdb-exwm-workspace-next)
(exwm-input-set-key (kbd "s-[") #'kdb-exwm-workspace-prev)

;;; Notifications ========================================================

(defun kdb-exwm-notify (message)
  "Send a notification MESSAGE."
  (start-process-shell-command
   "notify" nil
   (format "notify-send 'EXWM' '%s'" message)))

;;; Initialize EXWM ======================================================

(defun kdb-exwm-init ()
  "Initialize EXWM."
  (interactive)
  ;; Tab-bar is disabled when using Polybar
  ;; Polybar provides workspace info and system tray
  (when (not kdb-exwm-use-polybar)
    ;; Only enable tab-bar if using EXWM systemtray
    (setq tab-bar-show t
          tab-bar-close-button-show nil
          tab-bar-new-button-show nil)
    (kdb-exwm-setup-tab-bar)
    (tab-bar-mode 1))

  ;; Maximize the frame
  (set-frame-parameter nil 'fullscreen 'maximized)

  ;; Run autostart programs
  (kdb-exwm-run-autostart)

  ;; Enable EXWM first (use new API if available)
  (if (fboundp 'exwm-wm-mode)
      (exwm-wm-mode 1)
    (exwm-enable))

  ;; Enable optional features after EXWM is running
  (run-with-timer 1 nil
    (lambda ()
      ;; Enable RANDR (multi-monitor support)
      (kdb-exwm-enable-randr)

      ;; Enable system tray or Polybar based on configuration
      (if kdb-exwm-use-polybar
          (kdb-exwm-start-panel)
        (kdb-exwm-enable-systemtray))

      ;; Place *Messages* and *Warnings* buffers on specific workspaces
      (run-with-timer 2 nil
        (lambda ()
          ;; *Messages* buffer on workspace 9 (displays as "10")
          (when (get-buffer "*Messages*")
            (with-current-buffer "*Messages*"
              (exwm-workspace-move-window 9)))
          ;; *Warnings* buffer on workspace 8 (displays as "9")
          (when (get-buffer "*Warnings*")
            (with-current-buffer "*Warnings*"
              (exwm-workspace-move-window 8)))))

      ;; Show a welcome notification
      (run-with-timer 1 nil #'kdb-exwm-notify "EXWM initialized successfully!"))))

;;; Start EXWM automatically ============================================

;; Auto-start EXWM when running as a window manager
(when (and (eq window-system 'x)
           (not (daemonp))
           (getenv "DESKTOP_SESSION"))
  (kdb-exwm-init))

(provide 'init-exwm)
;;; init-exwm.el ends here
