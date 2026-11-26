;;; sql-test.el --- SQL sessions: az tokens, database lists, big results -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/sql-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'init-sql)

(ert-deftest sql-az-token-kept-until-a-minute-before-it-expires ()
  (let ((kdb-sql--token nil) asked answer)
    (cl-letf (((symbol-function 'kdb-sql--lines) (lambda (&rest args) (push args asked) answer)))
      ;; One line with a tab: kept until a minute before expires_on.
      (let ((expires (+ (truncate (float-time)) 3600)))
        (setq answer (list (format "tok1\t%d" expires)))
        (should (equal (kdb-sql--az-token) "tok1"))
        (should (member "[accessToken,expires_on]" (car asked)))
        (should (time-equal-p (car kdb-sql--token) (- expires 60)))
        (setq answer '("other"))
        (should (equal (kdb-sql--az-token) "tok1"))
        (should (= 1 (length asked))))
      ;; A line each; expiring within the minute, so asked again next time.
      (setq kdb-sql--token nil asked nil
            answer (list "tok2" (number-to-string (+ (truncate (float-time)) 30))))
      (should (equal (kdb-sql--az-token) "tok2"))
      ;; Older az has no expires_on: 5 minutes.
      (setq answer '("tok3\tNone"))
      (should (equal (kdb-sql--az-token) "tok3"))
      (should (= 2 (length asked)))
      (should (< 290 (float-time (time-subtract (car kdb-sql--token) nil)) 301)))))

(ert-deftest sql-az-token-says-what-went-wrong ()
  (let ((kdb-sql--token nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (signal 'file-missing '("Searching for program" "No such file" "az")))))
      (should (string-match-p "az isn.t installed"   ; ' or ’: user-error curves it
                              (cadr (should-error (kdb-sql--az-token) :type 'user-error)))))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))  ; signed out
      (should (string-match-p "run az login"
                              (cadr (should-error (kdb-sql--az-token) :type 'user-error)))))))

(ert-deftest sql-database-listing-says-what-it-does ()
  (let (msgs prompt)
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) msgs)))
              ((symbol-function 'read-string) (lambda (p &rest _) (setq prompt p) ""))
              ((symbol-function 'kdb-sql--lines) (lambda (&rest _) (error "sqlcmd exited with 1"))))
      (should (equal (kdb-sql--pick-database 'ms "srv" nil) ""))
      (should (member "Listing databases on srv..." msgs))
      (should (string-match-p "couldn't list them: sqlcmd exited with 1" prompt))
      ;; A SQL login isn't listed: nothing to say.
      (setq msgs nil)
      (kdb-sql--pick-database 'ms "srv" "user")
      (should-not msgs)
      (should-not (string-match-p "couldn't" prompt)))))

(ert-deftest sql-output-without-cr-isnt-copied ()
  (let ((replace (symbol-function 'string-replace))
        (us (string #x1f))
        (replaced 0))
    (cl-letf (((symbol-function 'string-replace)
               (lambda (from &rest more) (when (equal from "\r\n") (cl-incf replaced)) (apply replace from more))))
      (should (equal (kdb-sql--parse 'ms (concat "a" us "b\n-" us "-\n1" us "2\n"))
                     '((table ("a" "b") (("1" "2"))))))
      (should (= replaced 0))
      (should (equal (kdb-sql--parse 'ms (concat "a" us "b\r\n-" us "-\r\n1" us "2\r\n"))
                     '((table ("a" "b") (("1" "2"))))))
      (should (= replaced 1)))))

(ert-deftest sql-results-aligned-unless-too-many-cells ()
  (require 'org)
  (let* ((header (mapcar (lambda (i) (format "c%d" i)) (number-sequence 1 300)))
         (row (cons "a-long-value" (make-list 299 "b")))
         ;; 100 rows, but of 300 columns: too many cells to align.
         (big (kdb-sql--show-results " *test big*" `((table ,header ,(make-list 100 row))) "big"))
         (small (kdb-sql--show-results " *test small*" '((table ("x" "y") (("a-long-value" "b")))) "small")))
    (unwind-protect
        (progn
          ;; A search, not the buffer's text: a failure would print all of it.
          (with-current-buffer big
            (goto-char (point-min))
            (should (re-search-forward "^| c1 | c2 |" nil t)))
          (with-current-buffer small
            (goto-char (point-min))
            (should (re-search-forward "^| x            | y |$" nil t))
            ;; Inserted with org's change hooks off: its element cache still matches.
            (should (re-search-forward "^| a-long" nil t))
            (let ((cached (org-element-at-point))
                  (parsed (let ((org-element-use-cache nil)) (org-element-at-point))))
              (should (eq (org-element-type cached) 'table-row))
              (should (= (org-element-property :begin cached) (org-element-property :begin parsed))))))
      (kill-buffer big)
      (kill-buffer small))))

;;; sql-test.el ends here
