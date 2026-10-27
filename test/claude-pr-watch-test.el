;;; claude-pr-watch-test.el --- PR/CI watcher with a stubbed gh -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-pr-watch-test.el \
;;   -f ert-run-tests-batch-and-exit
;; The fake gh is a sh script; on Windows it runs through Git's sh.exe,
;; and the suite is skipped without one.  GH_STUB_DELAY (seconds) slows
;; it down, e.g. to rehearse Windows spawn times elsewhere.

(require 'ert)

(defvar test-autoloaded
  (and (not (featurep 'init-claude-workflow))
       (seq-every-p (lambda (f) (autoloadp (symbol-function f)))
                    '(kdb-claude-watch-pr kdb-claude-schedule
                      kdb-claude--start-with kdb-claude-spawn kdb-claude-message))))

(require 'init-claude-workflow)

(defvar test-dir (make-temp-file "ghstub" t))
(defvar test-busy nil)
(defvar test-queue nil "Stand-in for agent-shell's :pending-prompts.")
(defvar test-prompts nil "Prompts submitted, newest first.")
(defvar test-notes nil)

(define-derived-mode agent-shell-mode fundamental-mode "Agent")
(defun shell-maker-busy () test-busy)
(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  (setq test-queue (append test-queue (list prompt))))
(defun agent-shell--prompt-queue-process-next ()
  (when test-busy (error "Busy, try later"))
  (when test-queue (push (pop test-queue) test-prompts)))
(defun kdb-claude--notify (_buf title body) (push (concat title ": " body) test-notes))

(with-temp-file (expand-file-name "gh" test-dir)
  (insert "#!/bin/sh
d=$(dirname \"$0\")
echo \"$*\" >> \"$d/calls\"
sleep ${GH_STUB_DELAY:-0.3}
case \"$*\" in
  'pr view --json'*) cat \"$d/pr.json\" ;;
  'pr view '*' --json state') cat \"$d/state.json\" ;;
  'pr checks'*) cat \"$d/checks.json\"; grep -q fail \"$d/checks.json\" && exit 1 ;;
  'run view '*)
    if [ -e \"$d/log-$3.fail\" ]; then echo \"run $3 is still in progress\" >&2; exit 1; fi
    cat \"$d/log-$3.txt\" ;;
esac
"))
(set-file-modes (expand-file-name "gh" test-dir) #o755)

(defvar test-sh
  (if (not (eq system-type 'windows-nt))
      "sh"
    (seq-find (lambda (f) (and f (file-executable-p f)))
              (cons (executable-find "sh")
                    (when-let* ((git (executable-find "git")))
                      (list (expand-file-name "../bin/sh.exe" (file-name-directory git))
                            (expand-file-name "../usr/bin/sh.exe" (file-name-directory git))))))))

;; exec-suffixes would find the real gh.exe before an extensionless stub.
(when (and test-sh (eq system-type 'windows-nt))
  (with-temp-file (expand-file-name "gh.cmd" test-dir)
    (insert (format "@\"%s\" \"%%~dp0gh\" %%*\r\n"
                    (subst-char-in-string ?/ ?\\ test-sh)))))
(setq exec-path (cons test-dir exec-path))
(setenv "PATH" (concat test-dir path-separator (getenv "PATH")))

(defun test-file (name content)
  (with-temp-file (expand-file-name name test-dir) (insert content)))

(defun test-calls (prefix)
  (let ((f (expand-file-name "calls" test-dir)))
    (if (file-exists-p f)
        (seq-count (lambda (l) (string-prefix-p prefix l))
                   (with-temp-buffer (insert-file-contents f) (split-string (buffer-string) "\n" t)))
      0)))

(defun test-wait (pred)
  (let ((end (+ (float-time) 30)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(defun test-settle ()
  "Wait until no gh is running, so every reply in flight has arrived."
  (accept-process-output nil 0.1)
  (test-wait (lambda ()
               (not (seq-find (lambda (p) (string-prefix-p "gh" (process-name p)))
                              (process-list))))))

(defun test-reset ()
  (unless test-sh (ert-skip "no sh for the fake gh"))
  (dolist (w kdb-claude--watches) (kdb-claude--watch-stop w "reset"))
  (setq kdb-claude--pr-lookups nil)
  (dolist (f (directory-files test-dir t "\\`\\(calls\\|.*\\.fail\\)\\'"))
    (delete-file f))
  (setq test-busy nil test-queue nil test-prompts nil test-notes nil))

(defun test-shell ()
  (with-current-buffer (generate-new-buffer "*claude test*")
    (agent-shell-mode)
    (setq default-directory test-dir)
    (current-buffer)))

(defun test-poll (w)
  (kdb-claude--pr-poll)
  (test-wait (lambda () (not (kdb-claude--watch-polling w)))))

(defun test-check (name bucket run job)
  (format "{\"name\":%S,\"state\":\"x\",\"bucket\":%S,\"link\":\"https://github.com/o/r/actions/runs/%s/job/%s\"}"
          name bucket run job))

(defun test-checks (&rest checks)
  (concat "[" (string-join checks ",") "]"))

(defconst test-failing
  (test-checks (test-check "build" "fail" 101 1) (test-check "lint" "fail" 101 2)
               (test-check "e2e" "pending" 102 3)))

(defconst test-passing
  "[{\"name\":\"build\",\"state\":\"SUCCESS\",\"bucket\":\"pass\",\"link\":\"x\"},
    {\"name\":\"docs\",\"state\":\"SKIPPED\",\"bucket\":\"skipping\",\"link\":\"y\"}]")

(ert-deftest pr-watch-startup-untouched ()
  (should test-autoloaded))

(ert-deftest pr-watch-one-prompt-per-failed-run ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" test-failing)
  (test-file "log-101.txt" (mapconcat (lambda (i) (format "build\tstep\tline %d" i)) (number-sequence 1 5) "\n"))
  (let ((kdb-claude-pr-log-lines 3)
        (buf (test-shell))
        (t0 (float-time)))
    (setq test-busy t)
    (let ((w (kdb-claude--watch-start buf test-dir 7)))
      (should (< (- (float-time) t0) 0.25))      ; gh takes >= 0.3s: nothing waited on it
      (should (timerp kdb-claude--pr-timer))
      (test-wait (lambda () test-queue))
      (should-not test-prompts)                  ; busy: left in agent-shell's queue
      (setq test-busy nil)
      (agent-shell--prompt-queue-process-next)   ; what agent-shell does at turn end
      (should (= 1 (length test-prompts)))
      (let ((p (car test-prompts)))
        (should (string-prefix-p "CI check build, lint failed on PR #7. Log below. Diagnose, fix, commit and push." p))
        (should (string-match-p "line 5" p))
        (should-not (string-match-p "line 2" p)))  ; only the last 3 lines
      (should (member "PR #7 CI failed: build, lint" test-notes))
      ;; Same failed jobs on later polls: nothing new.
      (dotimes (_ 2) (test-poll w))
      (should (= 1 (length test-prompts)))
      (should (= 1 (test-calls "run view 101")))
      ;; Re-running failed jobs keeps the run id but gives new job links.
      (test-file "checks.json" (test-checks (test-check "build" "fail" 101 11)
                                            (test-check "lint" "pass" 101 12)))
      (test-poll w)
      (test-wait (lambda () (= 2 (length test-prompts))))
      (should (string-prefix-p "CI check build failed" (car test-prompts)))
      ;; A new run failing gets its own prompt; sent at once when idle.
      (test-file "checks.json" (test-checks (test-check "build" "fail" 103 21)))
      (test-file "log-103.txt" "build\tstep\tagain")
      (test-poll w)
      (test-wait (lambda () (= 3 (length test-prompts))))
      (kill-buffer buf))))

(ert-deftest pr-watch-waits-for-run-to-finish ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "lint" "fail" 201 1)
                                        (test-check "test" "pending" 201 2)))
  (test-file "log-201.txt" "lint\tstep\tlint broke\ntest\tstep\ttests broke")
  (let* ((buf (test-shell))
         (w (kdb-claude--watch-start buf test-dir 20)))
    (test-wait (lambda () (not (kdb-claude--watch-polling w))))
    (test-settle)
    (should-not test-prompts)
    (should (= 0 (test-calls "run view")))
    ;; Run finished with a second failure: one prompt naming both.
    (test-file "checks.json" (test-checks (test-check "lint" "fail" 201 1)
                                          (test-check "test" "fail" 201 2)))
    (test-poll w)
    (test-wait (lambda () test-prompts))
    (should (string-prefix-p "CI check lint, test failed on PR #20" (car test-prompts)))
    (should (string-match-p "tests broke" (car test-prompts)))
    (kill-buffer buf)))

(ert-deftest pr-watch-retries-unavailable-log ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "build" "fail" 301 1)))
  (test-file "log-301.txt" "build\tstep\tthe real log")
  (test-file "log-301.fail" "")
  (let* ((buf (test-shell))
         (w (kdb-claude--watch-start buf test-dir 30)))
    (test-wait (lambda () (= 1 (test-calls "run view 301"))))
    (test-settle)
    (should-not test-prompts)                    ; no "(no log available)" prompt
    (delete-file (expand-file-name "log-301.fail" test-dir))
    (test-poll w)
    (test-wait (lambda () test-prompts))
    (should (string-match-p "the real log" (car test-prompts)))
    (should (= 1 (length test-prompts)))
    (kill-buffer buf)))

(ert-deftest pr-watch-failures-without-links-are-separate ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" "[{\"name\":\"sonar\",\"bucket\":\"fail\",\"link\":\"\"},
                             {\"name\":\"jenkins\",\"bucket\":\"fail\"}]")
  (let ((buf (test-shell)))
    (kdb-claude--watch-start buf test-dir 40)
    (test-wait (lambda () (= 2 (length test-prompts))))
    (should (= 0 (test-calls "run view")))
    (should (seq-find (lambda (p) (string-prefix-p "CI check sonar failed" p)) test-prompts))
    (should (seq-find (lambda (p) (string-prefix-p "CI check jenkins failed" p)) test-prompts))
    (kill-buffer buf)))

(ert-deftest pr-watch-stops-when-checks-pass ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" test-passing)
  (let ((buf (test-shell)))
    (kdb-claude--watch-start buf test-dir 8)
    (test-wait (lambda () (null kdb-claude--watches)))
    (should-not kdb-claude--pr-timer)
    (should-not test-prompts)
    (should (member "PR #8: all checks passed" test-notes))
    (kill-buffer buf)))

(ert-deftest pr-watch-cancelled-checks-count-as-done ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "build" "pass" 1 1)
                                        (test-check "old" "cancel" 2 2)))
  (let ((buf (test-shell)))
    (kdb-claude--watch-start buf test-dir 41)
    (test-wait (lambda () (null kdb-claude--watches)))
    (should-not test-prompts)
    (kill-buffer buf)))

(ert-deftest pr-watch-merged-runs-hook ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"MERGED\"}")
  (let* ((buf (test-shell))
         (got nil)
         (kdb-claude-pr-merged-functions (list (lambda (n b) (setq got (list n b))))))
    (kdb-claude--watch-start buf test-dir 9)
    (test-wait (lambda () got))
    (should (equal got (list 9 buf)))
    (should-not kdb-claude--watches)
    (should (= 0 (test-calls "pr checks")))
    (kill-buffer buf)))

(ert-deftest pr-watch-stopped-ignores-late-merge ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"MERGED\"}")
  (let* ((buf (test-shell))
         (got nil)
         (kdb-claude-pr-merged-functions (list (lambda (&rest _) (setq got t))))
         (w (kdb-claude--watch-start buf test-dir 14)))
    (kdb-claude--watch-stop w "stopped")
    (test-settle)
    (should-not got)
    (should (equal test-notes '("PR #14: stopped")))
    (should-not (kdb-claude--watch-polling w))
    (kill-buffer buf)))

(ert-deftest pr-watch-closed-stops ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"CLOSED\"}")
  (let ((buf (test-shell)))
    (kdb-claude--watch-start buf test-dir 10)
    (test-wait (lambda () (null kdb-claude--watches)))
    (should (member "PR #10: closed" test-notes))
    (kill-buffer buf)))

(ert-deftest pr-watch-stops-when-buffer-killed ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" test-failing)
  (let ((buf (test-shell)))
    (kdb-claude--watch-start buf test-dir 11)
    (kill-buffer buf)
    (should-not kdb-claude--watches)
    (should-not kdb-claude--pr-timer)
    (test-settle)
    (should-not test-prompts)
    (should (= 0 (test-calls "pr checks")))))  ; no spawn after the stop

(ert-deftest pr-watch-stopped-mid-fetch-sends-nothing ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "build" "fail" 101 1)))
  (test-file "log-101.txt" "build\tstep\tboom")
  (let* ((buf (test-shell))
         (w (kdb-claude--watch-start buf test-dir 13)))
    (test-wait (lambda () (= 1 (test-calls "run view"))))
    (kdb-claude--watch-stop w "stopped")
    (test-settle)
    (should-not test-prompts)
    (should-not test-queue)
    (kill-buffer buf)))

(ert-deftest pr-watch-spawn-failure-keeps-polling ()
  (test-reset)
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "build" "fail" 101 1)))
  (test-file "log-101.txt" "build\tstep\tboom")
  (let* ((buf (test-shell))
         (w (kdb-claude--watch-start buf "/nonexistent-dir/" 15)))
    (test-wait (lambda () (not (kdb-claude--watch-polling w))))
    (setf (kdb-claude--watch-dir w) test-dir)  ; directory back: next poll works
    (test-poll w)
    (test-wait (lambda () test-prompts))
    (kill-buffer buf)))

(ert-deftest pr-watch-command-finds-branch-pr ()
  (test-reset)
  (test-file "pr.json" "{\"number\":12,\"url\":\"u\",\"headRefName\":\"feat\",\"state\":\"OPEN\"}")
  (test-file "state.json" "{\"state\":\"OPEN\"}")
  (test-file "checks.json" (test-checks (test-check "build" "fail" 101 1)))
  (test-file "log-101.txt" "build\tstep\tboom")
  (let ((buf (test-shell)))
    (with-current-buffer buf
      (kdb-claude-watch-pr)
      (kdb-claude-watch-pr))                     ; pressed twice before gh answers
    (test-wait (lambda () test-prompts))
    (test-settle)
    (should (= 1 (length kdb-claude--watches)))
    (should (= 12 (kdb-claude--watch-number (car kdb-claude--watches))))
    (should (= 1 (test-calls "pr view --json")))
    (should (= 1 (length test-prompts)))
    (should-not kdb-claude--pr-lookups)
    (kill-buffer buf)))

(ert-deftest schedule-past-time-means-tomorrow ()
  (let* ((past (format-time-string "%H:%M" (time-subtract nil 3600)))
         (fired nil))
    (cl-letf (((symbol-function 'kdb-claude--start-with) (lambda (&rest a) (setq fired a))))
      (let ((timer (kdb-claude-schedule past "/tmp/" "go")))
        (should (time-less-p (time-add nil (* 22 3600)) (timer--time timer)))
        (cancel-timer timer))
      (let ((timer (kdb-claude-schedule "1 sec" "/tmp/" "go")))
        (should (time-less-p nil (timer--time timer)))
        (sleep-for 1.5)
        (accept-process-output nil 0.1)
        (should (equal fired '("/tmp/" "go")))))
    (should-error (kdb-claude-schedule "  " "/tmp/" "go") :type 'user-error)))

;;; claude-pr-watch-test.el ends here
