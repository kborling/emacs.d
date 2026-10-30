;;; claude-sync-test.el --- Archive sync skips unreadable transcripts -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-sync-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'init-claude)

(defun test-jsonl (dir name text)
  (make-directory dir t)
  (with-temp-file (expand-file-name (concat name ".jsonl") dir)
    (insert (json-serialize `((type . "user")
                              (message . ((role . "user") (content . ,text)))
                              (cwd . "/tmp/x")))
            "\n")))

(ert-deftest sync-skips-unreadable-transcript ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (archive (expand-file-name "archive/" root))
         (convert (symbol-function 'kdb-claude--jsonl-to-org))
         msgs)
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "first session")
    (test-jsonl (expand-file-name "-proj-a/" projects) "s2" "unreadable one")
    (test-jsonl (expand-file-name "-proj-b/" projects) "s3" "third session")
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive))
              ((symbol-function 'kdb-claude--jsonl-to-org)
               (lambda (jsonl org)
                 (if (string-match-p "/s2\\.jsonl\\'" jsonl)
                     (signal 'file-missing (list "Opening input file" "No such file or directory" jsonl))
                   (funcall convert jsonl org))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (car (push (apply #'format fmt args) msgs)))))
      (should (equal (kdb-claude-sync-code-sessions)
                     "Synced 2 Claude Code sessions (1 unreadable, skipped)"))
      (should (seq-find (lambda (m) (string-match-p "\\`Skipped .*/s2\\.jsonl: " m)) msgs))
      (should (file-exists-p (expand-file-name "code-sessions/-proj-a/s1.org" archive)))
      (should (file-exists-p (expand-file-name "code-sessions/-proj-b/s3.org" archive)))
      (should-not (file-exists-p (expand-file-name "code-sessions/-proj-a/s2.org" archive)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "code-sessions/-proj-b/s3.org" archive))
        (should (string-match-p "third session" (buffer-string))))
      ;; Converted ones are up to date; the unreadable one is retried each sync.
      (should (equal (kdb-claude-sync-code-sessions)
                     "Synced 0 Claude Code sessions (1 unreadable, skipped)"))
      ;; Nothing skipped: the plain message.
      (delete-file (expand-file-name "-proj-a/s2.jsonl" projects))
      (should (equal (kdb-claude-sync-code-sessions) "Synced 0 Claude Code sessions")))
    (delete-directory root t)))

(ert-deftest sync-skips-unlistable-project-dir ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (archive (expand-file-name "archive/" root))
         (locked (expand-file-name "-proj-locked/" projects)))
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "readable")
    (test-jsonl locked "s2" "can't even list this dir")
    (set-file-modes locked #o000)
    (unwind-protect
        (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
                  ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive)))
          (should (equal (kdb-claude-sync-code-sessions) "Synced 1 Claude Code sessions"))
          (should (file-exists-p (expand-file-name "code-sessions/-proj-a/s1.org" archive))))
      (set-file-modes locked #o755)
      (delete-directory root t))))

(ert-deftest sync-other-errors-still-surface ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root)))
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "x")
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () (expand-file-name "archive/" root)))
              ((symbol-function 'kdb-claude--jsonl-to-org) (lambda (&rest _) (error "Converter bug"))))
      (should-error (kdb-claude-sync-code-sessions)))
    (delete-directory root t)))

;;; claude-sync-test.el ends here
