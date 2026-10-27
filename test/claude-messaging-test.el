;;; claude-messaging-test.el --- Sessions spawning and messaging sessions -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-messaging-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Loads the real agent-shell; no session or process is started.

(require 'ert)
(require 'agent-shell)
(require 'init-claude-workflow)

(defvar test-busy nil)
(defvar test-delivered nil "(BUFFER . PROMPT) submitted, newest first.")
(defvar test-started nil "(DIR PROMPT ENV) per started session.")

(defun shell-maker-busy () test-busy)
(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  (setq-local test-queue (append (bound-and-true-p test-queue) (list prompt))))
(defun agent-shell--prompt-queue-process-next ()
  (when test-busy (error "Busy, try later"))
  (when (bound-and-true-p test-queue)
    (push (cons (current-buffer) (pop test-queue)) test-delivered)))

;; Stand-in for starting a session: a buffer, plus the client-maker call
;; agent-shell-start makes synchronously, which is where the id is set.
(defun kdb-claude--start-with (dir &optional prompt _session-id)
  (let* ((buf (generate-new-buffer "*Claude test session*"))
         (client (with-current-buffer buf
                   (setq default-directory dir)
                   (agent-shell-anthropic-make-claude-client :buffer buf))))
    (push (list dir prompt (map-elt client :environment-variables)) test-started)
    buf))

(defun test-session (id)
  (with-current-buffer (generate-new-buffer "*Claude test parent*")
    (setq kdb-claude--id id)
    (current-buffer)))

(defun test-reset ()
  (setq test-busy nil test-delivered nil test-started nil))

(defun test-run-report-back (prompt summary)
  "Run the report-back form PROMPT tells the child to run, with SUMMARY."
  (string-match "<<'EOF'\n\\(.*\\)\nEOF" prompt)
  (eval (car (read-from-string
              (string-replace "\"SUMMARY\"" (prin1-to-string summary) (match-string 1 prompt))))
        t))

(ert-deftest messaging-advice-installed ()
  (should (advice-member-p #'kdb-claude--tag-client 'agent-shell-anthropic-make-claude-client)))

(ert-deftest messaging-every-session-gets-an-id ()
  (test-reset)
  (let* ((buf (generate-new-buffer "*Claude any*"))
         (client (with-current-buffer buf
                   (setq default-directory "/tmp/my-repo/")
                   (agent-shell-anthropic-make-claude-client :buffer buf)))
         (id (buffer-local-value 'kdb-claude--id buf)))
    (should (string-match-p "\\`my-repo-[0-9a-f]\\{4\\}\\'" id))
    (should (member (concat "KDB_CLAUDE_SESSION=" id) (map-elt client :environment-variables)))
    ;; Re-creating the client (resume, restart) keeps the id.
    (with-current-buffer buf (agent-shell-anthropic-make-claude-client :buffer buf))
    (should (equal id (buffer-local-value 'kdb-claude--id buf)))
    ;; The id survives a major-mode change.
    (with-current-buffer buf (fundamental-mode))
    (should (equal id (buffer-local-value 'kdb-claude--id buf)))
    (should (eq buf (kdb-claude--session-buffer id)))
    (kill-buffer buf)))

(ert-deftest messaging-round-trip ()
  (test-reset)
  (let* ((parent (test-session "parent-1"))
         (child-id (kdb-claude-spawn "/tmp/" "Fix the flaky test" "parent-1"))
         (child (kdb-claude--session-buffer child-id)))
    (should child)
    (pcase-let ((`(,dir ,prompt ,env) (car test-started)))
      (should (equal dir (expand-file-name "/tmp/")))
      (should (string-prefix-p "Fix the flaky test" prompt))
      (should (string-match-p (format "your session id is %s" child-id) prompt))
      ;; The child's process knows the same id the footer tells it.
      (should (member (concat "KDB_CLAUDE_SESSION=" child-id) env))
      ;; Child reports back, exactly as its prompt says to.
      (should (equal (test-run-report-back prompt "Fixed; PR #42 is green, don't merge yet")
                     "Queued for parent-1")))
    (should (equal (car test-delivered)
                   (cons parent (format "[Message from Claude session %s]\nFixed; PR #42 is green, don't merge yet"
                                        child-id))))
    ;; Parent follows up with the child.
    (kdb-claude-message child-id "Now rebase on main" "parent-1")
    (should (equal (car test-delivered)
                   (cons child "[Message from Claude session parent-1]\nNow rebase on main")))
    (kill-buffer parent)
    (kill-buffer child)))

(ert-deftest messaging-waits-for-busy-session ()
  (test-reset)
  (let ((buf (test-session "busy-1")))
    (setq test-busy t)
    (kdb-claude-message "busy-1" "later")
    (should-not test-delivered)
    (should (equal (buffer-local-value 'test-queue buf) '("later")))
    (setq test-busy nil)
    (with-current-buffer buf (agent-shell--prompt-queue-process-next))  ; turn end
    (should (equal (car test-delivered) (cons buf "later")))
    (kill-buffer buf)))

(ert-deftest messaging-unknown-session-errors ()
  (test-reset)
  (should-error (kdb-claude-message "nobody-0000" "hi") :type 'user-error)
  ;; A buffer that isn't a Claude session doesn't count either.
  (with-current-buffer (get-buffer-create "*scratch*")
    (should-error (kdb-claude-message "*scratch*" "hi") :type 'user-error)))

(ert-deftest messaging-spawn-without-parent ()
  (test-reset)
  (let ((id (kdb-claude-spawn "/tmp/" "Just do it")))
    (should (equal (nth 1 (car test-started)) "Just do it"))   ; no report-back footer
    (kill-buffer (kdb-claude--session-buffer id))))

(ert-deftest messaging-spawn-on-branch-uses-worktree ()
  (test-reset)
  (let* ((repo (file-name-as-directory (make-temp-file "repo" t)))
         (default-directory repo))
    (call-process "git" nil nil nil "init" "-q")
    (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "x")
    (let* ((id (kdb-claude-spawn repo "Go" "parent-1" "feature/x"))
           (dir (nth 0 (car test-started))))
      (should (string-suffix-p "-feature-x/" dir))
      (should (equal (car (process-lines "git" "-C" dir "branch" "--show-current")) "feature/x"))
      (should (string-match-p "\\`repo.*-feature-x-[0-9a-f]\\{4\\}\\'" id))
      (kill-buffer (kdb-claude--session-buffer id))
      (delete-directory dir t))
    (delete-directory repo t)))

;;; claude-messaging-test.el ends here
