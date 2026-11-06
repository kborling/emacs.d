;;; claude-messaging-test.el --- Sessions spawning and messaging sessions -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-messaging-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Loads the real agent-shell; no session or process is started.

(require 'ert)
(require 'agent-shell)
(require 'init-claude-workflow)

(defvar test-busy nil)
(defvar test-delivered nil "(BUFFER . PROMPT) submitted, newest first.")
(defvar test-started nil "(DIR PROMPT ENV QUIETLY) per started session.")

(defun shell-maker-busy () test-busy)
(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  (setq-local test-queue (append (bound-and-true-p test-queue) (list prompt))))
(defun agent-shell--prompt-queue-process-next ()
  (when test-busy (error "Busy, try later"))
  (when (bound-and-true-p test-queue)
    (push (cons (current-buffer) (pop test-queue)) test-delivered)))

;; Stand-in for starting a session: a buffer, plus the client-maker call
;; agent-shell-start makes synchronously, which is where the id is set.
(defun kdb-claude--start-with (dir &optional prompt _session-id quietly)
  (let* ((buf (generate-new-buffer "*Claude test session*"))
         (client (with-current-buffer buf
                   (setq default-directory dir)
                   (agent-shell-anthropic-make-claude-client :buffer buf))))
    (push (list dir prompt (map-elt client :environment-variables) quietly) test-started)
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
    (should (eq (nth 3 (car test-started)) 'background))      ; focus stays with you
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

;;; Other models

(defun test-env-value (env name)
  (when-let* ((e (seq-find (lambda (e) (string-prefix-p (concat name "=") e)) env)))
    (substring e (1+ (length name)))))

(ert-deftest start-model-points-claude-code-at-provider ()
  (test-reset)
  (let ((process-environment (cons "ZAI_API_KEY=test-key" process-environment)))
    (let* ((buf (kdb-claude-start-model "GLM (Z.ai)" "glm-4.6" "/tmp/"))
           (env (nth 2 (car test-started))))
      (should (equal (test-env-value env "ANTHROPIC_BASE_URL") "https://api.z.ai/api/anthropic"))
      (should (equal (test-env-value env "ANTHROPIC_AUTH_TOKEN") "test-key"))
      (dolist (v '("ANTHROPIC_MODEL" "ANTHROPIC_SMALL_FAST_MODEL" "ANTHROPIC_DEFAULT_OPUS_MODEL"
                   "ANTHROPIC_DEFAULT_SONNET_MODEL" "ANTHROPIC_DEFAULT_HAIKU_MODEL"))
        (should (equal (test-env-value env v) "glm-4.6")))
      (should (test-env-value env "KDB_CLAUDE_SESSION"))       ; messaging still works
      ;; agent-shell rebuilds the client later (reconnects): same provider.
      (let ((later (map-elt (agent-shell-anthropic-make-claude-client :buffer buf) :environment-variables)))
        (should (equal (test-env-value later "ANTHROPIC_BASE_URL") "https://api.z.ai/api/anthropic")))
      (kill-buffer buf)))
  ;; A normal session afterwards is untouched.
  (let* ((id (kdb-claude-spawn "/tmp/" "plain"))
         (env (nth 2 (car test-started))))
    (should-not (test-env-value env "ANTHROPIC_BASE_URL"))
    (kill-buffer (kdb-claude--session-buffer id))))

(ert-deftest start-model-keys-and-local ()
  (test-reset)
  ;; Key from ~/.authinfo.gpg when the variable isn't set.
  (let ((process-environment (cons "OPENROUTER_API_KEY" process-environment)))
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest spec) (and (equal (plist-get spec :host) "openrouter.ai") "from-authinfo"))))
      (let ((buf (kdb-claude-start-model "OpenRouter (fastest host per model)" "qwen/qwen3-coder:nitro" "/tmp/")))
        (should (equal (test-env-value (nth 2 (car test-started)) "ANTHROPIC_AUTH_TOKEN") "from-authinfo"))
        (kill-buffer buf)))
    ;; No key anywhere: a clear error naming both places.
    (cl-letf (((symbol-function 'auth-source-pick-first-password) #'ignore))
      (should (string-match-p "OPENROUTER_API_KEY.*machine openrouter.ai"
                              (cadr (should-error (kdb-claude-start-model "OpenRouter (fastest host per model)" "x" "/tmp/")
                                                  :type 'user-error))))))
  ;; Ollama needs no key.
  (let ((buf (kdb-claude-start-model "Ollama (local)" "qwen3:4b" "/tmp/"))
        (env (nth 2 (car test-started))))
    (should (equal (test-env-value env "ANTHROPIC_BASE_URL") "http://localhost:11434"))
    (should (equal (test-env-value env "ANTHROPIC_AUTH_TOKEN") "ollama"))
    (kill-buffer buf)))

(ert-deftest queue-edit-in-place ()
  (require 'agent-shell)
  (let ((shell (generate-new-buffer "*Claude queue test*")))
    (with-current-buffer shell
      (setq-local agent-shell--state (list (cons :pending-prompts (list "first" "second\nlines" "third")))))
    (cl-letf (((symbol-function 'agent-shell--shell-buffer) (lambda (&rest _) shell))
              ((symbol-function 'completing-read)
               (lambda (_p table &rest _) (nth 1 (all-completions "" table)))))   ; "2: second lines"
      ;; Changed in place, others kept in order.
      (cl-letf (((symbol-function 'agent-shell--prompt-queue-read)
                 (lambda (&key initial) (should (equal initial "second\nlines")) "second, better")))
        (kdb-claude-queue-edit))
      (should (equal (map-elt (buffer-local-value 'agent-shell--state shell) :pending-prompts)
                     '("first" "second, better" "third")))
      ;; Emptied: taken out.
      (cl-letf (((symbol-function 'agent-shell--prompt-queue-read) (lambda (&rest _) "  ")))
        (kdb-claude-queue-edit))
      (should (equal (map-elt (buffer-local-value 'agent-shell--state shell) :pending-prompts)
                     '("first" "third")))
      ;; Sent while you were editing: not put back.
      (cl-letf (((symbol-function 'agent-shell--prompt-queue-read)
                 (lambda (&rest _)
                   (with-current-buffer shell (map-put! agent-shell--state :pending-prompts nil))
                   "late")))
        (should-error (kdb-claude-queue-edit) :type 'user-error))
      (should-not (map-elt (buffer-local-value 'agent-shell--state shell) :pending-prompts))
      (should-error (kdb-claude-queue-edit) :type 'user-error))     ; nothing queued
    (kill-buffer shell)))

(defun test-permission (call respond)
  "A permission request for tool CALL, answered by calling RESPOND with an option id."
  (list (cons :tool-call call)
        (cons :options (list '((:kind . "allow_once") (:option . "Allow") (:option-id . "yes"))
                             '((:kind . "reject_once") (:option . "Reject") (:option-id . "no"))))
        (cons :respond (or respond #'ignore))))

(ert-deftest answer-panel-reads-at-a-glance ()
  (let ((buf (generate-new-buffer "Claude Agent @ api")))
    (with-current-buffer buf (setq default-directory "/tmp/api/"))
    ;; A command: what it's for, then the command itself.
    (let ((text (kdb-claude--permission-text
                 buf (test-permission '((:title . "ls -la src/ && make test")
                                        (:description . "Run the tests")
                                        (:command . "ls -la src/ && make test"))
                                      nil))))
      (should (string-match-p "\\`api\n\nRun the tests\n\n\\$ ls -la src/ && make test" text)))
    ;; An edit: the file, and its lines out and in.
    (let ((text (kdb-claude--permission-text
                 buf (test-permission '((:title . "Edit") (:raw-input . ((file_path . "/tmp/api/a.el")))
                                        (:diffs . (((:old . "(foo)") (:new . "(bar)\n(baz)") (:file . "/tmp/api/a.el")))))
                                      nil))))
      (should (string-match-p "/tmp/api/a.el\n- (foo)\n\\+ (bar)\n\\+ (baz)" text)))
    ;; Named after its title, the project isn't repeated.
    (with-current-buffer buf (rename-buffer "api · Fix flaky test"))
    (should (string-prefix-p "api  ·  Fix flaky test\n"
                             (kdb-claude--permission-text buf (test-permission '((:title . "x")) nil))))
    ;; A labeled session shows its name beside the project.
    (with-current-buffer buf (rename-buffer "Fix flaky test"))
    (should (string-prefix-p "api  ·  Fix flaky test"
                             (kdb-claude--permission-text buf (test-permission '((:title . "x")) nil))))
    (kill-buffer buf)))

(ert-deftest answer-two-in-a-row-then-later ()
  (require 'agent-shell)
  (let* ((a (generate-new-buffer "Claude Agent @ a"))
         (answered nil)
         (respond (lambda (id) (push id answered)))
         (kdb-claude--permissions
          (list (list "r1" a (test-permission '((:title . "one") (:command . "echo 1")) respond))
                (list "r2" a (test-permission '((:title . "two") (:command . "echo 2")) respond))
                (list "r3" a (test-permission '((:title . "three") (:command . "echo 3")) respond))))
         (keys (list ?y ?y ?q))
         prompts panels)
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (prompt _choices &rest _)
                 (push prompt prompts)
                 (push (with-current-buffer kdb-claude--permission-panel (buffer-string)) panels)
                 (should (get-buffer-window kdb-claude--permission-panel))   ; shown while asking
                 (list (pop keys))))
              ((symbol-function 'agent-shell-attention-jump) (lambda () (error "Shouldn't jump")))
              ((symbol-function 'kdb-claude--permission-open-p) #'always))
      (kdb-claude-answer))
    (should (equal answered '("yes" "yes")))
    (should (equal (reverse prompts) '("Answer (2 more waiting)" "Answer (1 more waiting)" "Answer")))
    (should (string-match-p "echo 1" (car (last panels))))
    (should (string-match-p "echo 3" (car panels)))
    ;; "later" leaves it queued, and the panel goes.
    (should (equal (mapcar #'car kdb-claude--permissions) '("r3")))
    (should-not (get-buffer-window kdb-claude--permission-panel))
    ;; g: to the session, by way of its tab.
    (let (went)
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (list ?g)))
                ((symbol-function 'kdb-claude--permission-open-p) #'always)
                ((symbol-function 'kdb-claude--goto-session) (lambda (b) (setq went b)))
                ((symbol-function 'agent-shell-jump-to-latest-permission-button-row) #'ignore))
        (kdb-claude-answer))
      (should (eq went a)))
    (kill-buffer a)))

(ert-deftest permission-ids-are-per-session ()
  "Two sessions can both have a request 11; answering one keeps the other."
  (let* ((a (generate-new-buffer "Claude Agent @ a"))
         (b (generate-new-buffer "Claude Agent @ b"))
         (kdb-claude--permissions (list (list 11 a 'pa) (list 11 b 'pb))))
    (kdb-claude--forget-permission a 11)
    (should (equal kdb-claude--permissions (list (list 11 b 'pb))))
    (kill-buffer a)
    (kill-buffer b)))

(ert-deftest permission-notice-in-plain-words ()
  (require 'agent-shell-attention)
  (let ((buf (generate-new-buffer "Fix the parser"))
        shown notified passed)
    (with-current-buffer buf
      (setq-local agent-shell--state
                  (list (cons :tool-calls
                              (list (cons "toolu_1" (list (cons :title "```console\nnpm test -- --runInBand\n```")
                                                          (cons :description "Run the parser tests")
                                                          (cons :permission-request-id 7))))))))
    (cl-letf (((symbol-function 'agent-shell-attention--should-notify-buffer) #'always)
              ((symbol-function 'agent-shell-attention--maybe-notify)
               (lambda (_b title body) (setq notified (list title body))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (substring-no-properties (apply #'format fmt args)))))
              ((symbol-function 'active-minibuffer-window) #'ignore))
      (cl-flet ((orig (b text) (setq passed (list b text))))
        ;; A permission request: who asks, for what, and the key.
        (kdb-claude--permission-message #'orig buf "Permission: ```console npm test``` (execute)")
        (should (equal shown "Fix the parser asks: Run the parser tests  (C-c ' to answer)"))
        (should (equal notified '("Fix the parser" "Fix the parser asks: Run the parser tests")))
        (should-not passed)
        ;; Anything else is agent-shell-attention's as before.
        (kdb-claude--permission-message #'orig buf "Turn complete")
        (should (equal passed (list buf "Turn complete")))
        ;; Not over a prompt you're answering; the notification still goes.
        (setq shown nil notified nil)
        (cl-letf (((symbol-function 'active-minibuffer-window) #'always))
          (kdb-claude--permission-message #'orig buf "Permission: x"))
        (should-not shown)
        (should notified)))
    ;; No description: the command, without its fence.
    (should (equal (kdb-claude--permission-gist '((:title . "```console\nls -la\n```"))) "ls -la"))
    (kill-buffer buf))
  ;; The card: what it's for, above the command.
  (let ((orig (lambda (&rest _) "```console\nnpm test\n```")))
    (should (equal (kdb-claude--permission-title orig :tool-call '((:description . "Run the tests")))
                   "Run the tests\n\n```console\nnpm test\n```"))
    (should (equal (kdb-claude--permission-title orig :tool-call '((:title . "x")))
                   "```console\nnpm test\n```"))))

;;; claude-messaging-test.el ends here
