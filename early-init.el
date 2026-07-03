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

;; Windows pipe optimizations
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
(when (boundp 'w32-pipe-buffer-size)
  (setq w32-pipe-buffer-size (* 64 1024)))

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
