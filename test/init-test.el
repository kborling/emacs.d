;;; init-test.el --- init.el's own helpers do what they say -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/init-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

(ert-deftest messages-window-goes-to-its-own-end ()
  ;; Shown from a short buffer: *Messages*' end, not the caller's.
  (with-current-buffer (messages-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (make-string 500 ?x) "\n")))
  (let ((win (display-buffer "*Messages*")))
    (set-window-point win (point-min))
    (with-temp-buffer
      (kdb-messages-buffer-advice "*Messages*"))
    (should (= (window-point win)
               (with-current-buffer "*Messages*" (point-max))))))

(ert-deftest sql-connect-names-buffer-and-skips-login ()
  (require 'sql)
  (let (seen)
    (cl-letf (((symbol-function 'sql-ms)
               (lambda (&optional buffer)
                 (setq seen (list buffer sql-ms-login-params sql-ms-options))))
              ((symbol-function 'executable-find)
               (lambda (prog &rest _) (and (equal prog "go-sqlcmd") prog))))
      (kdb-sql-connect-internal "srv" "db" "dev")
      (should (equal seen '("*SQL: dev*" nil ("-S" "srv" "-d" "db" "--authentication-method"
                                             "ActiveDirectoryDefault"))))
      ;; Unnamed: the buffer is named after the database.
      (kdb-sql-azure "srv" "db")
      (should (equal (car seen) "*SQL: db*")))))

(ert-deftest term-buffer-closes-after-terms-own-sentinel ()
  (let* ((buf (generate-new-buffer " *term test*"))
         (proc (make-pipe-process :name "term-test" :buffer buf :noquery t))
         (live-in-term-sentinel nil))
    (set-process-sentinel proc (lambda (&rest _)
                                 (push (buffer-live-p buf) live-in-term-sentinel)))
    (with-current-buffer buf (run-hooks 'term-exec-hook))
    (funcall (process-sentinel proc) proc "finished\n")
    (should (car (last live-in-term-sentinel)))   ; term's ran, buffer still there
    (should-not (buffer-live-p buf))))

(ert-deftest fido-keeps-orderless-and-scrolling ()
  (require 'icomplete)
  (require 'orderless)
  (with-temp-buffer
    (let ((icomplete-mode t))
      ;; As fido's setup leaves the minibuffer.
      (setq-local completion-styles '(flex)
                  completion-category-defaults '((buffer (styles flex basic)))
                  icomplete-scroll nil)
      (kdb-orderless-override-fido)
      (should (equal completion-styles '(orderless basic)))
      (should-not completion-category-defaults)
      (should icomplete-scroll))))

;;; init-test.el ends here
