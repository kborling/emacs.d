;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Author: Kevin Borling <kborling@protonmail.com>
;; Package-Requires: ((emacs "31"))

;;; Commentary:

;; Performance and UI settings that must run before init.el.

;;; Code:

;; Maximize GC threshold during startup, restore after
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.5)))

(setq large-file-warning-threshold 200000000)

;; Windows: file and process I/O is the bottleneck there
(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)
        ;; Skip the slow security-descriptor lookup on every file stat
        w32-get-true-file-attributes nil
        ;; Don't compact font caches on GC; costly with many fonts
        inhibit-compacting-font-caches t
        ;; One pre-built autoloads file instead of one per package
        package-quickstart t)
  ;; Every `load' during startup matches against these; restore after.
  ;; jka-compr stays so libraries shipped only as .el.gz still load.
  (let ((handlers file-name-handler-alist))
    (setq file-name-handler-alist
          (delq nil (list (rassq 'jka-compr-handler handlers))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist handlers))))))
  ;; Explorer keeps a stale PATH until sign-in; npm global bin may be missing.
  (let ((npm (expand-file-name "npm" (getenv "APPDATA"))))
    (when (and (file-directory-p npm) (not (member npm exec-path)))
      (add-to-list 'exec-path npm)
      (setenv "PATH" (concat npm path-separator (getenv "PATH")))))
  ;; Git's bin first, so "bash" (org babel, Claude code blocks) is Git Bash,
  ;; not WSL's System32\bash.exe.
  (when-let* ((git (executable-find "git"))
              (bin (expand-file-name "../bin" (file-name-directory git)))
              ((file-executable-p (expand-file-name "bash.exe" bin))))
    (add-to-list 'exec-path bin)))

;; UI chrome — disable before frame draws to avoid flicker
;; Keep macOS menu bar (integrates with system top bar)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

;; Frame and startup settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      visible-bell nil
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t
      initial-scratch-message nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      package-enable-at-startup t)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; early-init.el ends here
