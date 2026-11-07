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

(defvar eat--install-path)
(defvar eat-term-terminfo-directory)

(ert-deftest eat-terminfo-compiled-here-on-macos ()
  ;; macOS's ncurses 5.7 can't read eat's ncurses-6 terminfo: compile it
  ;; with the system tic, once, into a folder of our own.
  (let* ((home (file-name-as-directory (make-temp-file "emacsd" t)))
         (user-emacs-directory home)
         (src-dir (file-name-as-directory (make-temp-file "eat" t)))
         (eat--install-path src-dir)
         (eat-term-terminfo-directory "/shipped/")
         (runs nil))
    (with-temp-file (expand-file-name "eat.ti" src-dir) (insert "eat-truecolor|x,\n"))
    (cl-letf (((symbol-function 'call-process)
               (lambda (prog &rest args)
                 (push (cons prog (append args (list (getenv "TERMINFO")))) runs)
                 (let ((out (expand-file-name "65/eat-truecolor" (getenv "TERMINFO"))))
                   (make-directory (file-name-directory out) t)
                   (with-temp-file out (insert "compiled")))
                 0)))
      (kdb-eat-local-terminfo)
      (should (equal eat-term-terminfo-directory (expand-file-name "eat-terminfo/" home)))
      (should (equal (car runs) (list "/usr/bin/tic" nil nil nil "-x"
                                      (expand-file-name "eat.ti" src-dir)
                                      (expand-file-name "eat-terminfo/" home))))
      ;; Up to date: not compiled again.
      (kdb-eat-local-terminfo)
      (should (= 1 (length runs)))
      ;; eat updated its source: compiled again.
      (set-file-times (expand-file-name "eat.ti" src-dir) (time-add nil 60))
      (kdb-eat-local-terminfo)
      (should (= 2 (length runs))))
    (delete-directory home t)
    (delete-directory src-dir t)))

;;; path-test.el ends here
