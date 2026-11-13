;;; tabs-session-test.el --- Tabs, files and Claude sessions survive a restart -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/tabs-session-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Uses a temporary session file; the real ~/.emacs.d/tabsession.el is untouched.

;; These open, close and reset tabs and windows, and kill buffers: loaded
;; into a working Emacs they wipe its tabs.
(unless noninteractive
  (user-error "Run %s in batch Emacs (emacs --batch), not in a live one"
              (file-name-nondirectory (or load-file-name buffer-file-name "these tests"))))
(require 'ert)
(require 'tabspaces)
(require 'init-claude-workflow)

(defun test-claude-buffer (dir id)
  "A stand-in Claude shell in DIR whose session is ID."
  (with-current-buffer (generate-new-buffer "Claude Agent @ test")
    (setq major-mode 'agent-shell-mode
          default-directory dir)
    (setq-local test-session-id id)
    (kdb-claude--close-session-window)
    (current-buffer)))

(ert-deftest tabs-round-trip-with-claude-session ()
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "tabs" t))))
         (tabspaces-session-file (expand-file-name "tabsession.el" root))
         (file (expand-file-name "notes.txt" root))
         (resumed nil))
    (with-temp-file file (insert "hello"))
    (cl-letf (((symbol-function 'agent-shell-session-id)
               (lambda (&key shell-buffer) (buffer-local-value 'test-session-id shell-buffer)))
              ((symbol-function 'kdb-claude--start-with)
               (lambda (dir &optional _prompt id _quietly)
                 (push (list dir id) resumed)
                 (test-claude-buffer dir id))))
      (unwind-protect
          (progn
            (tab-bar-mode 1)
            (tabspaces-mode 1)
            (tabspaces-switch-or-create-workspace "notes")
            (find-file file)
            (tabspaces-switch-or-create-workspace "claude")
            (switch-to-buffer (test-claude-buffer root "sess-123"))
            (tabspaces-save-session)
            ;; What was written: the file, and the Claude session by id.
            (let ((saved (with-temp-buffer (insert-file-contents tabspaces-session-file) (buffer-string))))
              (should (string-match-p (regexp-quote file) saved))
              (should (string-match-p ":kind claude" saved))
              (should (string-match-p "sess-123" saved)))
            ;; Restart: everything gone.
            (tab-bar-close-other-tabs)
            (mapc (lambda (b) (unless (string-prefix-p " " (buffer-name b)) (kill-buffer b)))
                  (seq-filter (lambda (b) (or (buffer-file-name b) (string-prefix-p "Claude" (buffer-name b))))
                              (buffer-list)))
            (let ((kdb-tabs--restored nil))
              (kdb-tabs-restore)
              (should kdb-tabs--restored)
              (let ((names (mapcar (lambda (tab) (alist-get 'name tab)) (frame-parameter nil 'tabs))))
                (should (member "notes" names))
                (should (member "claude" names)))
              (should (get-file-buffer file))
              ;; The same folder, however Windows spells it (KEVINB~1 or not).
              (should (= 1 (length resumed)))
              (should (equal (file-truename (car (car resumed))) (file-truename root)))
              (should (equal (cadr (car resumed)) "sess-123"))
              ;; Only once.
              (kdb-tabs-restore)
              (should (= 1 (length resumed)))))
        (tabspaces-mode -1)
        (ignore-errors (tab-bar-close-other-tabs))
        (tab-bar-mode -1)
        (delete-directory root t)))))

(ert-deftest closing-a-session-leaves-the-others-running ()
  ;; agent-shell's own kill hook shuts down the current buffer's agent, so
  ;; ours must leave the dying session current, whatever buffer it visits.
  (let* ((dir (file-name-as-directory temporary-file-directory))
         (other (test-claude-buffer dir "sess-b"))
         (closing (test-claude-buffer dir "sess-a"))
         (shut-down nil))
    (with-current-buffer closing
      (setq kdb-claude--id "sess-a")
      (add-hook 'kill-buffer-hook (lambda () (setq shut-down (current-buffer))) t t))
    (cl-letf (((symbol-function 'kdb-claude-cleanup) (lambda (_) (set-buffer other))))
      (unwind-protect
          (progn (kill-buffer closing)
                 (should (eq shut-down closing)))
        (kill-buffer other)))))

(ert-deftest tabs-are-never-saved-over-with-nothing ()
  ;; Before the restore: saving would replace last session with this one.
  (let ((kdb-tabs--restored nil))
    (should-not (kdb-tabs--may-save)))
  (let ((kdb-tabs--restored t))
    (should (kdb-tabs--may-save))
    ;; A daemon's hidden startup frame has no tabs to save.
    (cl-letf (((symbol-function 'daemonp) (lambda () t)))
      (with-selected-frame terminal-frame
        (should-not (kdb-tabs--may-save))))))

(ert-deftest tabs-saved-when-the-last-frame-closes ()
  (let ((saved 0)
        (tabspaces-mode t))
    (cl-letf (((symbol-function 'tabspaces--save-session-smart) (lambda () (cl-incf saved))))
      ;; Another frame you can see is still open: nothing yet.
      (cl-letf (((symbol-function 'frame-list) (lambda () (list 'mine 'other))))
        (kdb-tabs--save-closing-frame 'mine))
      (should (= saved 0))
      ;; Last one: save.  The daemon's hidden frame doesn't count.  (Batch
      ;; Emacs runs in its own startup frame, so the hidden one is a stand-in.)
      (let ((terminal-frame 'hidden))
        (cl-letf (((symbol-function 'frame-list) (lambda () (list (selected-frame) 'hidden)))
                  ((symbol-function 'daemonp) (lambda () t)))
          (kdb-tabs--save-closing-frame (selected-frame))))
      (should (= saved 1))
      ;; Closing the hidden frame itself never saves.
      (let ((terminal-frame (selected-frame)))
        (cl-letf (((symbol-function 'daemonp) (lambda () t)))
          (kdb-tabs--save-closing-frame (selected-frame))))
      (should (= saved 1)))))

(ert-deftest claude-tab-record-only-for-claude-sessions ()
  (let ((claude (test-claude-buffer "/tmp/" "sess-9")))
    (cl-letf (((symbol-function 'agent-shell-session-id)
               (lambda (&key shell-buffer) (buffer-local-value 'test-session-id shell-buffer))))
      (should (equal (kdb-claude--tab-save claude)
                     (list :kind 'claude :dir "/tmp/" :name (buffer-name claude) :session "sess-9")))
      (with-temp-buffer (should-not (kdb-claude--tab-save (current-buffer)))))
    ;; A project folder that's gone is skipped rather than failing the restore.
    (should-not (kdb-claude--tab-restore '(:kind claude :dir "/nonexistent/" :session "x")))
    (kill-buffer claude)))

(defmacro test-with-tabs (&rest body)
  `(let ((tabspaces-project-tab-map nil))
     (unwind-protect
         (progn (tab-bar-mode 1) (tabspaces-mode 1) (delete-other-windows) ,@body)
       (tab-bar-new-tab)
       (ignore-errors (tab-bar-close-other-tabs))
       (tabspaces-mode -1)
       (tab-bar-mode -1)
       (delete-other-windows)
       (dolist (b (buffer-list))
         (when (string-prefix-p "Claude Agent @ test" (buffer-name b)) (kill-buffer b))))))

(defun test-project (name)
  ;; Truename: Windows gives the temp folder as KEVINB~1, files open under the long name.
  (let ((dir (file-name-as-directory (expand-file-name name (file-truename (make-temp-file "proj" t))))))
    (make-directory dir t)
    dir))

(defun test-tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(ert-deftest each-session-in-a-tab-of-its-own ()
  (require 'agent-shell)
  (test-with-tabs
   (let* ((api (test-project "api"))
          (a (test-claude-buffer api "a"))
          (b (test-claude-buffer api "b")))
     ;; Same project, still a tab each, first named after its folder.
     (should (eq (kdb-claude--display-in-own-tab a nil) (selected-window)))
     (should (one-window-p t))
     (kdb-claude--display-in-own-tab b nil)
     (should (equal (cdr (test-tab-names)) '("api" "api")))
     (should (one-window-p t))
     ;; Titled: the tab takes the title too.
     (with-current-buffer b
       (setq-local agent-shell--state (list (cons :session (list (cons :title "Fix the parser"))))))
     (kdb-claude--name-session b)
     (should (equal (buffer-name b) "Fix the parser"))
     (should (equal (alist-get 'name (tab-bar--current-tab)) "Fix the parser"))
     ;; A tab you or a PR review named keeps its name.
     (tab-bar-rename-tab "PR apps#9")
     (with-current-buffer b
       (map-put! (map-elt agent-shell--state :session) :title "Something else"))
     (kdb-claude--name-session b)
     (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#9"))
     ;; The session ends: its tab goes; never the last one.
     (kill-buffer b)
     (should (equal (cdr (test-tab-names)) '("api")))
     (tab-bar-close-other-tabs)
     (switch-to-buffer a)
     (kill-buffer a)
     (should (= 1 (length (test-tab-names)))))))

(ert-deftest renamed-session-keeps-working ()
  ;; shell-maker finds a session's buffer by the name it was given.  A
  ;; renamed session must not hand its output to a new one taking that name.
  (require 'agent-shell)
  (let ((old (test-claude-buffer "/tmp/" "old")))
    (with-current-buffer old
      (rename-buffer "Claude Agent @ api" t)
      (setq-local shell-maker--buffer-name-override (buffer-name))
      (setq-local agent-shell--state (list (cons :session (list (cons :title "Old work"))))))
    (kdb-claude--name-session old)
    (let ((new (get-buffer-create "Claude Agent @ api")))   ; a new session, the freed name
      (should-not (eq new old))
      (with-current-buffer old
        (should (equal (shell-maker-buffer-name nil) (buffer-name old))))
      ;; By hand too.
      (with-current-buffer old (rename-buffer "my name"))
      (with-current-buffer old
        (should (equal (shell-maker-buffer-name nil) "my name")))
      (kill-buffer new))
    (kill-buffer old)))

(ert-deftest background-session-keeps-you-where-you-are ()
  ;; A spawned or scheduled session lands in its tab; focus doesn't move.
  (test-with-tabs
   (let ((api (test-project "api"))
         (win (selected-window))
         (here (window-buffer (selected-window))))
     (kdb-claude--show-in-background (test-claude-buffer api "a"))
     (should (member "api" (test-tab-names)))
     (should-not (equal (alist-get 'name (tab-bar--current-tab)) "api"))
     (should (eq (selected-window) win))
     (should (eq (window-buffer win) here)))))

(ert-deftest sessions-without-tabs-show-as-before ()
  (tab-bar-mode -1)
  (should-not (kdb-claude--display-in-own-tab (test-claude-buffer "/tmp/" "x") nil)))

(ert-deftest session-shown-in-its-tab-is-not-split-again ()
  (test-with-tabs
   (let* ((api (test-project "api"))
          (a (test-claude-buffer api "a"))
          (win (kdb-claude--display-in-own-tab a nil)))
     (tab-bar-switch-to-tab (car (test-tab-names)))
     (should (eq (kdb-claude--display-in-own-tab a nil) win))
     (should (one-window-p t)))))

(ert-deftest session-in-a-custom-named-tab-is-gone-to ()
  ;; A PR review's session lives in "PR apps#1"; showing it again (agent-shell
  ;; does when it's ready) goes there, not to a second tab.
  (test-with-tabs
   (let* ((wt (test-project "apps-review-1"))
          (r (test-claude-buffer wt "r")))
     (tab-bar-new-tab)
     (tab-bar-rename-tab "PR apps#1")
     (switch-to-buffer r)
     (tab-bar-switch-to-tab (car (test-tab-names)))
     (let ((count (length (test-tab-names)))
           (win (kdb-claude--display-in-own-tab r nil)))
       (should (= count (length (test-tab-names))))
       (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#1"))
       (should (eq (window-buffer win) r))
       ;; From its own tab too.
       (should (eq (kdb-claude--display-in-own-tab r nil) win))
       (should (= count (length (test-tab-names))))))))

(ert-deftest goto-session-in-another-tab ()
  ;; Picking an open session in C-c l c goes to the tab showing it.
  (test-with-tabs
   (let ((r (test-claude-buffer (test-project "api") "r")))
     (tab-bar-new-tab)
     (tab-bar-rename-tab "api")
     (switch-to-buffer r)
     (tab-bar-switch-to-tab (car (test-tab-names)))
     (kdb-claude--goto-session r)
     (should (equal (alist-get 'name (tab-bar--current-tab)) "api"))
     (should (eq (window-buffer (selected-window)) r)))))

(ert-deftest send-to-a-session-or-a-new-one ()
  (require 'agent-shell)
  (let* ((api (test-project "api"))
         (a (test-claude-buffer api "a"))
         (from (test-claude-buffer api "from"))
         offered started sent)
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list from a)))
              ((symbol-function 'agent-shell--context) (lambda (&rest _) "the region"))
              ((symbol-function 'agent-shell--display-buffer) #'ignore)
              ((symbol-function 'agent-shell--insert-to-shell-buffer)
               (lambda (&rest args) (setq sent args)))
              ((symbol-function 'kdb-claude--start-on-branch) (lambda (dir) (setq started dir)))
              ((symbol-function 'kdb-claude-read-directory) (lambda (&rest _) "/elsewhere/")))
      (with-current-buffer from
        ;; Open sessions first, not the one you're in; then new ones.
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p table &rest _) (setq offered (all-completions "" table)) (car offered))))
          (kdb-claude-send))
        (should (= 3 (length offered)))
        (should (string-match-p "\\`api +test" (nth 0 offered)))      ; project once, then its name
        (should (equal sent (list :text "the region" :shell-buffer a)))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (nth 1 offered))))
          (kdb-claude-send))
        (should (equal started api))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (nth 2 offered))))
          (kdb-claude-send))
        (should (equal started "/elsewhere/"))))
    (kill-buffer a) (kill-buffer from)))

(ert-deftest pr-url-parts ()
  (should (equal (kdb-claude--pr-url-parts "https://github.com/acme/apps/pull/123") '("apps" "123")))
  (should (equal (kdb-claude--pr-url-parts "https://github.com/acme/data/pull/45/files#diff-1") '("data" "45")))
  (should (equal (kdb-claude--pr-url-parts "https://github.acme.com/team/apps/pull/7") '("apps" "7")))
  (should (equal (kdb-claude--pr-url-parts "https://github.com/acme/apps.git") '("apps" nil)))
  (should-not (kdb-claude--pr-url-parts "not a url")))

(defun test-wait (pred)
  (let ((end (+ (float-time) 10)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))))

(ert-deftest pr-review-in-its-own-worktree-and-tab ()
  (test-with-tabs
   (let* ((root (file-name-as-directory (file-truename (make-temp-file "prs" t))))
          (apps (file-name-as-directory (expand-file-name "apps" root)))
          (bin (expand-file-name "bin/" root))
          (gh (expand-file-name "gh" bin))
          (exec-path (cons bin exec-path))
          (kdb-claude-review-skill "team-code-review")
          (kdb-claude-review-repos (list (cons "apps" apps)))
          started)
     ;; A repo, and a gh whose `pr checkout N' puts the worktree on pr-N.
     (make-directory apps t)
     (make-directory bin t)
     (let ((default-directory apps))
       (dolist (args '(("init" "-q" "-b" "main") ("-c" "user.name=t" "-c" "user.email=t@t"
                                                  "commit" "-q" "--allow-empty" "-m" "x")))
         (apply #'call-process "git" nil nil nil args)))
     ;; A script Windows runs is a .bat; it's found before the real gh.exe.
     (if (eq system-type 'windows-nt)
         (with-temp-file (concat gh ".bat")
           (insert "@echo off\r\n"
                   "if \"%3\"==\"9\" (\r\n  echo no pull request 9 1>&2\r\n  exit /b 1\r\n)\r\n"
                   "if not \"%1 %2\"==\"pr checkout\" exit /b 1\r\n"
                   "git checkout -q -B pr-%3\r\n"))
       (with-temp-file gh
         (insert "#!/bin/sh\n[ \"$3\" = 9 ] && { echo 'no pull request 9' >&2; exit 1; }\n"
                 "[ \"$1 $2\" = \"pr checkout\" ] && exec git checkout -q -B \"pr-$3\"\nexit 1\n"))
       (set-file-modes gh #o755))
     (should (string-prefix-p (expand-file-name "gh" bin) (executable-find "gh")))
     (cl-letf (((symbol-function 'kdb-claude--start-with)
                (lambda (dir prompt &rest _)
                  (setq started (list dir prompt))
                  (let ((buf (test-claude-buffer dir "r")))
                    (funcall (caar agent-shell-display-action) buf nil)
                    buf))))
       (kdb-claude-review-pr "https://github.com/acme/apps/pull/123")
       (test-wait (lambda () started))
       (let ((wt (file-name-as-directory (expand-file-name "apps-review-123" root))))
         ;; The session runs in the worktree, on the PR's branch, with the bare skill.
         (should (equal started (list wt "/team-code-review")))
         (should (equal (car (kdb-claude--git-lines wt "branch" "--show-current")) "pr-123"))
         (should (equal (car (kdb-claude--git-lines apps "branch" "--show-current")) "main"))
         (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#123"))
         ;; Again with its tab closed: the worktree is reused.
         (tab-bar-close-tab)
         (setq started nil)
         (kdb-claude-review-pr "https://github.com/acme/apps/pull/123/files")
         (test-wait (lambda () started))
         (should (equal (car started) wt)))
       ;; gh failing: its error is shown, no session starts.
       (setq started nil)
       (let (shown)
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
           (kdb-claude-review-pr "https://github.com/acme/apps/pull/9")
           (test-wait (lambda () (seq-some (lambda (m) (string-match-p "no pull request 9" m)) shown))))
         (should (seq-some (lambda (m) (string-match-p "failed: no pull request 9" m)) shown)))
       (should-not started)
       ;; Not a PR link.
       (should-error (kdb-claude-review-pr "https://github.com/acme/apps") :type 'user-error))
     (delete-directory root t))))

(ert-deftest pr-review-checkout-found-by-repo-name ()
  (let ((kdb-claude-review-repos '(("data" . "/src/data-repo/"))))
    (should (equal (kdb-claude--repo-checkout "data") "/src/data-repo/")))
  (cl-letf (((symbol-function 'project-known-project-roots) (lambda () '("/x/data/" "/y/apps/"))))
    (should (equal (kdb-claude--repo-checkout "apps") "/y/apps/"))))

(ert-deftest session-shows-and-runs-in-its-own-tab-then-cleans-up ()
  (test-with-tabs
   (let* ((api (test-project "api"))
          (notes (expand-file-name "notes.txt" api))
          (mine (expand-file-name "mine.txt" api))
          (s (test-claude-buffer api "s")))
     (with-temp-file notes (insert "one\ntwo\nthree\n"))
     (with-temp-file mine (insert "my file\n"))
     (with-current-buffer s (setq kdb-claude--id "api-1"))
     (kdb-claude--display-in-own-tab s nil)           ; the session's tab, "api"
     (tab-bar-switch-to-tab (car (test-tab-names)))       ; you, elsewhere
     (let ((here (selected-window))
           (mine-buf (find-file-noselect mine)))          ; already open, yours
       ;; Show: in the session's tab; you stay put.
       (should (string-match-p "Showing notes.txt" (kdb-claude-show "api-1" "notes.txt" 2)))
       (kdb-claude-show "api-1" mine)
       (should (eq (selected-window) here))
       (should-not (equal (alist-get 'name (tab-bar--current-tab)) "api"))
       (tab-bar-switch-to-tab "api")
       (should (= 2 (length (window-list))))               ; one split, reused
       (should (get-buffer-window mine-buf))
       (tab-bar-switch-to-tab (car (test-tab-names)))
       ;; Run: in the project shell, in the session's tab; output readable.
       (let ((shell (kdb-claude-run "api-1" "echo hi-from-claude")))
         (let ((end (+ (float-time) 10)))
           (while (and (not (string-match-p "\\(^\\|[$>] \\)hi-from-claude\r?$" (kdb-claude-output shell)))
                       (< (float-time) end))
             (accept-process-output nil 0.05)))
         (should (string-match-p "\\(^\\|[$>] \\)hi-from-claude\r?$" (kdb-claude-output shell)))
         (should (string-match-p "[$>] echo hi-from-claude" (kdb-claude-output shell)))
         (should-not (process-query-on-exit-flag (get-buffer-process shell)))
         (should (memq #'comint-osc-process-output    ; Git Bash's title codes stripped
                       (buffer-local-value 'comint-output-filter-functions (get-buffer shell))))
         (should (memq #'comint-truncate-buffer       ; dev server logs don't grow without end
                       (buffer-local-value 'comint-output-filter-functions (get-buffer shell))))
         (should (eq (selected-window) here))
         ;; Cleanup: what it opened goes; your file stays, and so does unsaved work.
         (with-current-buffer (get-file-buffer notes) (insert "edit"))
         (let (said)
           (cl-letf (((symbol-function 'message)
                      (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
             (should (string-match-p "kept unsaved notes.txt" (kdb-claude-cleanup "api-1"))))
           ;; Announced, so a window vanishing isn't a mystery.
           (should (seq-some (lambda (m) (string-match-p "closed 2 window(s), 1 buffer(s) it had opened" m))
                             said)))
         (should-not (get-buffer shell))
         (should (buffer-live-p mine-buf))
         (tab-bar-switch-to-tab "api")
         (should (one-window-p t))
         (with-current-buffer (get-file-buffer notes) (set-buffer-modified-p nil)))
       ;; Kept for its unsaved changes, notes.txt is yours now.
       (kill-buffer (get-file-buffer notes))
       ;; The session ending cleans up too.
       (let ((later (expand-file-name "later.txt" api)))
         (with-temp-file later (insert "x\n"))
         (kdb-claude-show "api-1" later)
         (should (= 2 (length (window-list))))
         (kill-buffer s)
         (should-not (get-file-buffer later))
         (should (one-window-p t)))
       (kill-buffer mine-buf)))))

(ert-deftest bash-is-never-wsl ()
  ;; On Windows, System32\bash.exe starts WSL, not Git Bash.
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "C:\\Windows\\System32\\bash.exe")))
    (should-not (kdb-claude--bash)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "C:/Program Files/Git/bin/bash.exe")))
    (should (equal (kdb-claude--bash) "C:/Program Files/Git/bin/bash.exe"))))

(ert-deftest sessions-sharing-a-project-dont-close-each-others-things ()
  (test-with-tabs
   (let* ((api (test-project "api"))
          (notes (expand-file-name "notes.txt" api))
          (a (test-claude-buffer api "a"))
          (b (test-claude-buffer api "b")))
     (with-temp-file notes (insert "x\n"))
     (with-current-buffer a (setq kdb-claude--id "api-a"))
     (with-current-buffer b (setq kdb-claude--id "api-b"))
     (kdb-claude--display-in-own-tab a nil)
     (kdb-claude--display-in-own-tab b nil)            ; a tab each
     ;; Both use the project's one shell, and the same file.
     (let ((shell (kdb-claude-run "api-a" "true")))
       (should (equal (kdb-claude-run "api-b" "true") shell))
       (kdb-claude-show "api-a" "notes.txt")
       (kdb-claude-show "api-b" "notes.txt")
       (let ((b-win (seq-find (lambda (w) (equal (window-parameter w 'kdb-claude-session) "api-b"))
                              (window-list))))
         ;; a is done: its window goes; what b still uses stays.
         (kdb-claude-cleanup "api-a")
         (should (get-buffer shell))
         (should (get-file-buffer notes))
         (should (window-live-p b-win))
         (should-not (seq-find (lambda (w) (equal (window-parameter w 'kdb-claude-session) "api-a"))
                               (window-list)))
         ;; b done too: now they go.
         (kdb-claude-cleanup "api-b")
         (should-not (get-buffer shell))
         (should-not (get-file-buffer notes))))
     ;; A shell you ran a block in (C-c l x) is yours: no session closes it.
     (let ((shell (kdb-claude-run "api-a" "true")))
       (with-temp-buffer
         (setq default-directory api)
         (insert "```bash\necho mine\n```\n")
         (kdb-claude-run-block t))
       (kdb-claude-cleanup "api-a")
       (should (get-buffer shell))
       (let ((kill-buffer-query-functions nil)) (kill-buffer shell)))
     (kill-buffer a) (kill-buffer b))))

(ert-deftest renamed-while-hidden-still-found-in-its-tab ()
  ;; A review's session, titled while you're in another tab, then shown
  ;; again (a reply, C-c ' g, C-c l c): back to its tab, not a second one.
  (require 'agent-shell)
  (test-with-tabs
   (let ((r (test-claude-buffer (test-project "apps-review-1") "r")))
     (tab-bar-new-tab)
     (tab-bar-rename-tab "PR apps#1")
     (switch-to-buffer r)
     (tab-bar-switch-to-tab (car (test-tab-names)))
     (with-current-buffer r (rename-buffer "apps-review-1 · Review PR 1"))
     (should (tab-bar-get-buffer-tab r))
     (let ((count (length (test-tab-names))))
       (kdb-claude--display-in-own-tab r nil)
       (should (= count (length (test-tab-names))))
       (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#1")))
     ;; The same by the title naming, a default-named session's tab hidden.
     (let ((r2 (test-claude-buffer (test-project "apps-review-2") "r2")))
       (tab-bar-new-tab)
       (tab-bar-rename-tab "PR apps#2")
       (switch-to-buffer r2)
       (tab-bar-switch-to-tab (car (test-tab-names)))
       (with-current-buffer r2
         (setq-local agent-shell--state (list (cons :session (list (cons :title "Reviewing the parser"))))))
       (kdb-claude--name-session r2)
       (should (string-match-p "Reviewing the parser" (buffer-name r2)))
       (should (tab-bar-get-buffer-tab r2))))))

(defun test-git (dir &rest args)
  (let ((default-directory dir))
    (apply #'call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t" args)))

(defun test-git-out (dir &rest args)
  (car (apply #'kdb-claude--git-lines dir args)))

(ert-deftest new-session-on-a-branch-from-origins-default ()
  (require 'agent-shell)
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "wt" t))))
         (origin (expand-file-name "origin.git" root))
         (seed (file-name-as-directory (expand-file-name "seed" root)))
         (repo (file-name-as-directory (expand-file-name "apps" root)))
         (note (expand-file-name "note.txt" root))
         answer asked annotate started sent)
    ;; origin: main at A, a colleague's branch; the clone, then main moves on to B.
    (test-git root "init" "-q" "--bare" "-b" "main" origin)
    (test-git root "clone" "-q" origin seed)
    (test-git seed "commit" "-q" "--allow-empty" "-m" "A")
    (test-git seed "push" "-q" "origin" "HEAD:main")
    (test-git seed "push" "-q" "origin" "HEAD:colleague")
    (test-git root "clone" "-q" origin repo)
    (test-git seed "commit" "-q" "--allow-empty" "-m" "B")
    (test-git seed "push" "-q" "origin" "HEAD:main")
    ;; The main checkout is off on another branch, with unmerged work.
    (test-git repo "switch" "-q" "-c" "feature/old")
    (test-git repo "commit" "-q" "--allow-empty" "-m" "C")
    (with-temp-file note (insert "x\n"))
    (cl-flet ((same (a b) (equal (file-truename (file-name-as-directory a))
                                 (file-truename (file-name-as-directory b))))
              (subject (dir) (test-git-out dir "log" "-1" "--format=%s"))
              (wait () (let ((end (+ (float-time) 15)))
                         (while (and (not started) (< (float-time) end))
                           (accept-process-output nil 0.05)))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p table &rest _)
                   (setq asked (all-completions "" table)
                         annotate (alist-get 'annotation-function (cdr (funcall table "" nil 'metadata))))
                   answer))
                ((symbol-function 'kdb-claude--start-with) (lambda (dir &rest _) (setq started dir) 'shell))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq sent (plist-get args :text)))))
        ;; A new branch: from origin's main, fetched (B), not the checkout (C).
        ;; What was at point (this file) is taken now and reaches the session.
        (with-current-buffer (find-file-noselect note)
          (let ((default-directory repo))
            (setq answer "fix/login" started nil)
            (kdb-claude--start-on-branch repo)
            (wait))
          (kill-buffer))
        (should (same started (expand-file-name "apps-fix-login" root)))
        (should (equal (subject started) "B"))
        (should (string-match-p "note.txt" sent))
        (should (equal (car asked) "(current checkout)"))
        (should (member "origin/colleague" asked))
        ;; An existing branch: its worktree, then labelled as one.
        (setq answer "fix/login" started nil)
        (kdb-claude--start-on-branch repo)
        (should (same started (expand-file-name "apps-fix-login" root)))
        (should (equal (funcall annotate "fix/login") "  worktree"))
        ;; A colleague's branch, only on origin: a local branch tracking it.
        (setq answer "origin/colleague" started nil)
        (kdb-claude--start-on-branch repo)
        (should (same started (expand-file-name "apps-colleague" root)))
        (should (equal (test-git-out started "rev-parse" "--abbrev-ref" "@{upstream}") "origin/colleague"))
        ;; Can't fetch: from the local default branch, as it is here.
        (test-git repo "remote" "set-url" "origin" (expand-file-name "gone.git" root))
        (setq answer "fix/offline" started nil)
        (kdb-claude--start-on-branch repo)
        (wait)
        (should (equal (subject started) "A"))
        ;; No origin at all: the local default branch.
        (test-git repo "remote" "remove" "origin")
        (setq answer "fix/local" started nil)
        (kdb-claude--start-on-branch repo)
        (should (equal (subject started) "A"))
        ;; RET: the current checkout.
        (setq answer "(current checkout)" started nil)
        (kdb-claude--start-on-branch repo)
        (should (same started repo))))
    (delete-directory root t)))

(ert-deftest big-session-loses-oldest-turns-only ()
  (require 'agent-shell-prompt)
  (with-temp-buffer
    (setq-local comint-prompt-regexp "Claude> ")
    (let ((kdb-claude-session-max-chars 1000)
          (prompt (propertize "Claude> " 'font-lock-face 'comint-highlight-prompt)))
      (dotimes (i 10)
        (insert prompt (format "question %d\n" i)
                "Claude> quoted in a reply, not a prompt\n"
                (make-string 200 ?x) "\n"))
      (insert prompt)
      (kdb-claude--trim-buffer)
      (should (<= (buffer-size) 1000))
      (should (string-prefix-p "Earlier turns trimmed" (buffer-string)))
      (should (string-match-p "question 9" (buffer-string)))
      (should-not (string-match-p "question 0" (buffer-string)))
      ;; Cut at a real prompt, so the first turn kept is whole.
      (should (string-match-p "trimmed[^\n]*\n\nClaude> question" (buffer-string)))
      ;; Under the limit: nothing happens.
      (let ((before (buffer-string)))
        (kdb-claude--trim-buffer)
        (should (equal before (buffer-string))))
      ;; Only one huge turn after the note: nothing worth freeing, left alone
      ;; rather than the note rewritten after every turn.
      (erase-buffer)
      (insert "Earlier turns trimmed\n\n" prompt "q\n" (make-string 3000 ?y))
      (let ((before (buffer-string)))
        (kdb-claude--trim-buffer)
        (should (equal before (buffer-string))))))
  ;; Narrowed (say to one prompt), still the oldest turns go.
  (with-temp-buffer
    (setq-local comint-prompt-regexp "Claude> ")
    (let ((kdb-claude-session-max-chars 1000)
          (prompt (propertize "Claude> " 'font-lock-face 'comint-highlight-prompt)))
      (dotimes (i 10)
        (insert prompt (format "question %d\n" i) (make-string 200 ?x) "\n"))
      (narrow-to-region (- (point-max) 300) (point-max))
      (kdb-claude--trim-buffer)
      (widen)
      (should (string-prefix-p "Earlier turns trimmed" (buffer-string)))
      (should (string-match-p "question 9" (buffer-string))))))

(ert-deftest agent-dying-on-its-own-is-reported-not-a-shutdown ()
  (let* ((buf (generate-new-buffer "Claude Agent @ died"))
         (proc (make-process :name "fake-agent" :command '("true") :noquery t))
         (client (list (cons :process proc) (cons :context-buffer buf)
                       (cons :command "claude") (cons :instance-count 1)))
         shown)
    (with-current-buffer buf (setq major-mode 'agent-shell-mode))
    (while (process-live-p proc) (accept-process-output proc 0.05))
    (cl-letf (((symbol-function 'agent-shell--state) #'ignore)
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (push (plist-get args :body) shown)))
              ((symbol-function 'kdb-claude--notify) #'ignore))
      (kdb-claude--agent-died client proc "finished\n")
      (should (string-match-p "agent process finished" (car shown)))
      (should (string-match-p "agent-shell-reload" (car shown)))
      ;; acp-shutdown forgets the process first: a deliberate stop says nothing.
      (setf (alist-get :process client) nil)
      (kdb-claude--agent-died client proc "finished\n")
      (should (= 1 (length shown)))
      ;; An agent-review client: its buffer isn't a session.  Nothing here
      ;; may signal, or acp's own sentinel, after this one, never runs.
      (let ((review (generate-new-buffer "agent-review")))
        (setf (alist-get :process client) proc
              (alist-get :context-buffer client) review)
        (cl-letf (((symbol-function 'agent-shell--state) (lambda () (error "Not a session"))))
          (kdb-claude--agent-died client proc "finished\n"))
        (kill-buffer review))
      (should (= 1 (length shown)))
      (setf (alist-get :context-buffer client) buf)
      (cl-letf (((symbol-function 'agent-shell--update-fragment) (lambda (&rest _) (error "Boom"))))
        (kdb-claude--agent-died client proc "finished\n")))
    (kill-buffer buf)))

(ert-deftest prompt-for-a-quiet-session-leaves-focus-alone ()
  ;; A spawned or scheduled session's first prompt, once it's ready, must
  ;; not pull you to its tab.
  (require 'agent-shell)
  (let ((buf (generate-new-buffer "Claude Agent @ test"))
        on-ready inserted)
    (cl-letf (((symbol-function 'agent-shell-anthropic-make-claude-code-config) #'ignore)
              ((symbol-function 'agent-shell--start) (lambda (&rest _) buf))
              ((symbol-function 'agent-shell-subscribe-to)
               (lambda (&rest args) (setq on-ready (plist-get args :on-event)) 'token))
              ((symbol-function 'agent-shell-unsubscribe) #'ignore)
              ((symbol-function 'agent-shell-insert) (lambda (&rest args) (setq inserted args))))
      (kdb-claude--start-with "/tmp/" "go" nil 'hidden)
      (funcall on-ready nil)
      (should (equal (plist-get inserted :text) "go"))
      (should (plist-get inserted :no-focus)))
    (kill-buffer buf)))

(ert-deftest restarted-session-keeps-its-tab-id-and-things ()
  ;; agent-shell-restart (and -reload) kills the buffer and starts another
  ;; under its name: the same session, not one closed and a new one.
  (require 'agent-shell)
  (test-with-tabs
   (let* ((api (test-project "api"))
          (old (test-claude-buffer api "a"))
          cleaned new)
     (with-current-buffer old
       (setq kdb-claude--id "api-1"
             kdb-claude--provider-env '("ANTHROPIC_MODEL=m")))
     (kdb-claude--display-in-own-tab old nil)
     (let ((tabs (test-tab-names)))
       (cl-letf (((symbol-function 'kdb-claude-cleanup) (lambda (id) (push id cleaned))))
         (with-current-buffer old
           (kdb-claude--keep-across-restart
            (lambda ()
              ;; As agent-shell does: kill it, then start anew, client and all.
              (let ((name (buffer-name old)))
                (kill-buffer old)
                (setq new (test-claude-buffer api "a"))
                (kdb-claude--tag-client #'ignore :buffer new)
                (with-current-buffer new (rename-buffer name))
                (set-window-buffer (selected-window) new))))))
       (should-not cleaned)
       (should (equal (test-tab-names) tabs))
       (with-current-buffer new
         (should (equal kdb-claude--id "api-1"))
         (should (equal kdb-claude--provider-env '("ANTHROPIC_MODEL=m")))
         (should (equal kdb-claude--tab-auto "api"))
         ;; Declined: nothing is left set for the next session started.
         (should-error (kdb-claude--keep-across-restart (lambda () (user-error "Cancelled")))
                       :type 'user-error))
       (should-not kdb-claude--restarting)
       (should-not kdb-claude--next-id)))))

(ert-deftest busy-animation-waits-while-emacs-is-unfocused ()
  (cl-letf (((symbol-function 'frame-focus-state) #'ignore))
    (should-not (kdb-claude--focused-tick-p :cache-enabled t))   ; a tick: skipped
    (should (kdb-claude--focused-tick-p)))                       ; a real update: drawn
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
    (should (kdb-claude--focused-tick-p :cache-enabled t))))

(ert-deftest timer-messages-stay-out-of-an-open-minibuffer ()
  (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
    (let ((kdb-in-timer 1))                 ; minibuffer open when the timer fired
      (should (kdb-quiet-timer-message "Reverting buffer")))
    (let ((kdb-in-timer 0))                 ; the timer opened this prompt itself
      (should-not (kdb-quiet-timer-message "Mark set")))
    (let ((kdb-in-timer nil))               ; your own command
      (should-not (kdb-quiet-timer-message "Mark set")))))

(ert-deftest orphaned-agents-are-stopped-owned-ones-kept ()
  (require 'acp)
  (let* ((start (lambda (buf)
                  ;; The keys `acp-shutdown' updates in place, as a real client has.
                  (list (cons :process (make-process :name "fake-agent" :command '("sleep" "60") :noquery t))
                        (cons :context-buffer buf) (cons :command "fake") (cons :instance-count 0)
                        (cons :error-handlers nil) (cons :notification-handlers nil)
                        (cons :request-handlers nil) (cons :pending-requests nil))))
         (session (generate-new-buffer "session"))
         (review (generate-new-buffer "agent-review"))           ; not a session
         (closed (generate-new-buffer "closed session"))
         (owned (funcall start session))
         (replaced (funcall start session))                      ; session moved on
         (other (funcall start review))
         (gone (funcall start closed))
         (kdb-claude--clients (list owned replaced other gone)))
    (with-current-buffer session
      (setq major-mode 'agent-shell-mode)
      (setq-local agent-shell--state (list (cons :client owned))))
    (kill-buffer closed)
    (should (= 2 (kdb-claude-reap-agents)))
    (should (process-live-p (map-elt owned :process)))
    (should (process-live-p (map-elt other :process)))
    (should-not (map-elt replaced :process))                    ; acp-shutdown let go of them
    (should-not (map-elt gone :process))
    (should (equal kdb-claude--clients (list owned other)))
    (dolist (c (list owned other)) (delete-process (map-elt c :process)))
    (kill-buffer session) (kill-buffer review)))

(ert-deftest quiet-agent-mid-turn-is-noted-once ()
  (let* ((buf (generate-new-buffer "session"))
         (kdb-claude--permissions nil)
         (last (time-subtract nil (* 60 20)))          ; 20 minutes ago
         (busy t) notes)
    (with-current-buffer buf (setq major-mode 'agent-shell-mode))
    (cl-letf (((symbol-function 'agent-shell--state) #'ignore)
              ((symbol-function 'agent-shell--active-requests-p) (lambda (_) busy))
              ((symbol-function 'agent-shell-last-activity-time) (lambda (&rest _) last))
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (push (plist-get args :body) notes)))
              ((symbol-function 'kdb-claude--notify) #'ignore))
      (kdb-claude-check-stalls)
      (should (string-match-p "may be stuck" (car notes)))
      (kdb-claude-check-stalls)                        ; same silence: not again
      (should (= 1 (length notes)))
      (setq last (time-subtract nil 60))               ; heard from a minute ago
      (kdb-claude-check-stalls)
      (should (= 1 (length notes)))
      (setq last (time-subtract nil (* 60 20)))        ; quiet, but waiting on you
      (let ((kdb-claude--permissions (list (list "id" buf nil))))
        (kdb-claude-check-stalls))
      (should (= 1 (length notes)))
      (setq busy nil)                                  ; not mid-turn
      (kdb-claude-check-stalls)
      (should (= 1 (length notes))))
    (kill-buffer buf)))

;;; tabs-session-test.el ends here
