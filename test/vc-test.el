;;; vc-test.el --- The C-c g git commands do what they say -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/vc-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'init-vc)

(defmacro test-in-repo (&rest body)
  "Run BODY in a fresh git repo with one commit of src/f.txt."
  `(let* ((root (file-name-as-directory (make-temp-file "vc" t)))
          (default-directory root))
     (dolist (args '(("init" "-q") ("config" "user.email" "t@t") ("config" "user.name" "t")))
       (apply #'call-process "git" nil nil nil args))
     (make-directory "src")
     (with-temp-file "src/f.txt" (insert "one\n"))
     (call-process "git" nil nil nil "add" ".")
     (call-process "git" nil nil nil "commit" "-qm" "init")
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun test-git-status ()
  (string-trim-right (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "git" nil t nil "status" "--porcelain")))))

(ert-deftest stage-and-unstage-a-tracked-file-in-a-subdirectory ()
  (test-in-repo
   (with-current-buffer (find-file-noselect "src/f.txt")
     (insert "changed\n") (save-buffer)
     (kdb-vc-git-stage)
     (should (equal (test-git-status) "M  src/f.txt"))   ; staged
     (kdb-vc-git-unstage)
     (should (equal (test-git-status) " M src/f.txt"))   ; back to unstaged
     (kill-buffer))))

(ert-deftest discard-restores-the-committed-file ()
  (test-in-repo
   (with-current-buffer (find-file-noselect "src/f.txt")
     (insert "changed\n") (save-buffer)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
       (kdb-vc-revert-file))
     (should (equal (test-git-status) ""))
     (should (equal (buffer-string) "one\n"))            ; the buffer too
     (kill-buffer))))

(ert-deftest odd-file-names-reach-git-untouched ()
  (test-in-repo
   (let ((odd "x;echo pwned>p.txt"))
     (with-temp-file odd (insert "x\n"))
     (call-process "git" nil nil nil "add" ".")
     (call-process "git" nil nil nil "commit" "-qm" "odd")
     (should (equal (kdb-vc-git-output "show" (concat "HEAD:" odd)) "x\n"))
     (should-not (file-exists-p "p.txt")))))

(ert-deftest git-errors-come-back-as-errors ()
  (test-in-repo
   (should-error (kdb-vc-git-run "branch" "-d" "no-such-branch") :type 'user-error)))

(ert-deftest git-log-keeps-commit-lines-log-view-can-read ()
  (require 'vc-git)
  (should-not (seq-some (lambda (s) (string-prefix-p "--pretty" s)) vc-git-log-switches)))

;;; vc-test.el ends here
