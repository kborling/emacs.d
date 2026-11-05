;;; path-test.el --- Tools in the user profile reach shell commands -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/path-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)

(ert-deftest add-to-path-feeds-emacs-and-shells ()
  (let* ((dir (file-name-as-directory (make-temp-file "tools" t)))
         (dir (directory-file-name dir))
         (exec-path '("/usr/bin"))
         (process-environment (cons "PATH=/usr/bin" process-environment)))
    (kdb-add-to-path dir)
    (kdb-add-to-path dir)                      ; twice: still once
    (should (equal exec-path (list dir "/usr/bin")))
    ;; Shells get Windows-style backslashes on Windows.
    (should (equal (getenv "PATH")
                   (concat (if (eq system-type 'windows-nt) (subst-char-in-string ?/ ?\\ dir) dir)
                           path-separator "/usr/bin")))
    ;; A folder that isn't there is left out.
    (kdb-add-to-path (expand-file-name "missing" dir))
    (should (= 2 (length exec-path)))
    (delete-directory dir t)))

(ert-deftest add-to-path-writes-windows-paths-for-cmd ()
  ;; As on Windows: PATH gets backslashes and ; between entries, for cmd.
  (let* ((dir (directory-file-name (make-temp-file "tools" t)))
         (system-type 'windows-nt)
         (path-separator ";")
         (exec-path nil)
         (process-environment (cons "PATH=C:\\Windows" process-environment)))
    (kdb-add-to-path dir)
    (should (equal (car exec-path) dir))
    (should (equal (getenv "PATH")
                   (concat (subst-char-in-string ?/ ?\\ dir) ";C:\\Windows")))
    (should-not (string-search "/" (car (split-string (getenv "PATH") ";"))))
    (delete-directory dir t)))

(ert-deftest shell-commands-find-npm-tools ()
  ;; The user-profile folders were added at startup: a shell finds them.
  (when (file-executable-p (expand-file-name "~/.npm-global/bin/ccusage"))
    (should (string-suffix-p "/ccusage"
                             (string-trim (shell-command-to-string "command -v ccusage"))))))

(ert-deftest display-buffer-by-name-is-fine ()
  ;; The *Messages* scroll advice takes a buffer's name as well as the buffer.
  (let ((b (get-buffer-create "*by name*")))
    (should (windowp (display-buffer "*by name*")))
    (kill-buffer b))
  (should (windowp (display-buffer "*Messages*"))))

;;; path-test.el ends here
