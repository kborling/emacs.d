;;; init-theme.el --- Cross-platform theme switching -*- lexical-binding: t; -*-

;; Switch between the three themes from Emacs, so the SAME keybinding works on
;; Void/StumpWM and on Windows (where there's no WM / no Komorebi to bind to).
;; On Linux it also syncs kitty + wallpaper via ~/.local/bin/theme; on Windows
;; it just sets the Emacs theme.
;;
;; Keys:  s-t  cycle  fleury -> uwu -> acme
;;        s-T  toggle light (acme) <-> last dark

;;; Code:

(defvar kdb-themes '(fleury uwu acme)
  "Theme cycle order.  fleury/uwu are dark, acme is light.")

(defvar kdb-light-themes '(acme)
  "Themes treated as \"light\" for the light/dark toggle.")

(defvar kdb-dark-default 'fleury
  "Dark theme to fall back to.")

(defvar kdb-current-theme kdb-dark-default
  "Last theme set through this module.")

(defvar kdb-last-dark-theme kdb-dark-default
  "Most recent dark theme, for the light/dark toggle.")

(defconst kdb-theme-script (expand-file-name "~/.local/bin/theme")
  "Helper that syncs kitty + wallpaper on Linux.")

(defun kdb--wt-settings-file ()
  "Locate the Windows Terminal settings.json (Store or unpackaged install)."
  (let ((local (getenv "LOCALAPPDATA")))
    (when local
      (seq-find
       #'file-exists-p
       (list
        (expand-file-name "Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json" local)
        (expand-file-name "Packages/Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe/LocalState/settings.json" local)
        (expand-file-name "Microsoft/Windows Terminal/settings.json" local))))))

(defun kdb--sync-windows-terminal (theme)
  "Point Windows Terminal at THEME's color scheme and font weight via settings.json.
Only rewrites colorScheme values already set to one of our themes, and font
faces already set to JetBrains Mono, so other profiles are untouched.  The light
theme (acme) uses a heavier weight since dark-on-light reads thin.  WT live-reloads."
  (let ((file (kdb--wt-settings-file))
        (face (if (string= theme "acme") "Comic Code Medium" "Comic Code")))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((changed nil))
          (goto-char (point-min))
          (while (re-search-forward
                  "\"colorScheme\"[[:space:]]*:[[:space:]]*\"\\(?:fleury\\|uwu\\|acme\\)\""
                  nil t)
            (replace-match (format "\"colorScheme\": \"%s\"" theme) t t)
            (setq changed t))
          (goto-char (point-min))
          (while (re-search-forward
                  "\"face\"[[:space:]]*:[[:space:]]*\"\\(?:JetBrains Mono\\|Comic Code\\)\\(?: Medium\\| SemiBold\\)?\""
                  nil t)
            (replace-match (format "\"face\": \"%s\"" face) t t)
            (setq changed t))
          (when changed
            (write-region (point-min) (point-max) file nil 'quiet)))))))

(defconst kdb-windows-wallpaper-dir (expand-file-name "~/wallpapers")
  "Where the void-<theme>.png wallpapers live on the Windows VM.")

(defconst kdb-windows-wallpaper-ps1 (expand-file-name "~/set-wallpaper.ps1")
  "PowerShell helper that sets the Windows desktop wallpaper.")

(defun kdb--sync-windows-wallpaper (theme)
  "Set the Windows desktop wallpaper to match THEME."
  (let ((wall (expand-file-name (format "void-%s.png" theme) kdb-windows-wallpaper-dir)))
    (when (and (file-exists-p kdb-windows-wallpaper-ps1) (file-exists-p wall))
      (call-process "powershell" nil 0 nil "-NoProfile" "-ExecutionPolicy" "Bypass"
                    "-File" kdb-windows-wallpaper-ps1 wall))))

(defun kdb--sync-desktop-theme (theme)
  "Sync the rest of the desktop to THEME (Emacs sets its own theme separately).
Linux: kitty + wallpaper via the shell helper.  Windows: Windows Terminal +
desktop wallpaper."
  (let ((name (symbol-name theme)))
    (cond
     ((eq system-type 'gnu/linux)
      (when (file-executable-p kdb-theme-script)
        (call-process kdb-theme-script nil 0 nil name "--no-emacs")))
     ((eq system-type 'windows-nt)
      (kdb--sync-windows-terminal name)
      (kdb--sync-windows-wallpaper name)))))

(defun kdb/theme-set (theme)
  "Load THEME in Emacs and, on Linux, sync the rest of the desktop."
  (interactive
   (list (intern (completing-read "Theme: "
                                  (mapcar #'symbol-name kdb-themes) nil t))))
  ;; acme: authentic Plan 9 true-black text (must be set before the theme loads).
  (setq acme-theme-black-fg t)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  ;; Dark-on-light reads thinner than light-on-dark: heavier weight for light.
  (set-face-attribute 'default nil :weight
                      (if (memq theme kdb-light-themes) 'medium 'normal))
  (setq kdb-current-theme theme)
  (unless (memq theme kdb-light-themes)
    (setq kdb-last-dark-theme theme))
  (kdb--sync-desktop-theme theme)
  (message "theme: %s" theme))

(defun kdb/theme-cycle ()
  "Cycle to the next theme in `kdb-themes'."
  (interactive)
  (kdb/theme-set (or (cadr (memq kdb-current-theme kdb-themes))
                     (car kdb-themes))))

(defun kdb/theme-toggle ()
  "Toggle between light (acme) and the last dark theme."
  (interactive)
  (kdb/theme-set (if (memq kdb-current-theme kdb-light-themes)
                     kdb-last-dark-theme
                   'acme)))

;; On Windows, make the Windows key act as super so `s-t` reaches Emacs
;; (otherwise the OS swallows Win+T). You rarely use Win-key shortcuts anyway.
(when (eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super
        w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil))

;; super-t: avoids the C-c t (eat) prefix and matches the old EXWM binding.
(global-set-key (kbd "s-t") #'kdb/theme-cycle)
(global-set-key (kbd "s-T") #'kdb/theme-toggle)

(provide 'init-theme)
;;; init-theme.el ends here
