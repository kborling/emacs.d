;;; tabs-session-test.el --- Tabs, files and Claude sessions survive a restart -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/tabs-session-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Uses a temporary session file; the real ~/.emacs.d/tabsession.el is untouched.

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
  (let* ((root (file-name-as-directory (make-temp-file "tabs" t)))
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
              (should (equal resumed (list (list root "sess-123"))))
              ;; Only once.
              (kdb-tabs-restore)
              (should (= 1 (length resumed)))))
        (tabspaces-mode -1)
        (ignore-errors (tab-bar-close-other-tabs))
        (tab-bar-mode -1)
        (delete-directory root t)))))

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
  (let ((dir (file-name-as-directory (expand-file-name name (make-temp-file "proj" t)))))
    (make-directory dir t)
    dir))

(defun test-tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(ert-deftest sessions-open-in-their-project-tab ()
  (test-with-tabs
   (let ((api (test-project "api"))
         (web (test-project "web")))
     ;; First session: a new tab named after the project, the session alone.
     (let ((a (test-claude-buffer api "a")))
       (should (eq (kdb-claude--display-in-project-tab a nil) (selected-window)))
       (should (equal (alist-get 'name (tab-bar--current-tab)) "api"))
       (should (one-window-p t))
       ;; Second in the same project, from another tab: back to it, split.
       (tab-bar-switch-to-tab (car (test-tab-names)))
       (let* ((b (test-claude-buffer api "b"))
              (win (kdb-claude--display-in-project-tab b nil)))
         (should (equal (alist-get 'name (tab-bar--current-tab)) "api"))
         (should (eq (window-buffer win) b))
         (should (equal (mapcar #'window-buffer (kdb-claude--session-windows)) (list a b)))
         ;; Killing a session closes its split.
         (kill-buffer b)
         (should (one-window-p t))))
     ;; Another project: its own tab.
     (kdb-claude--display-in-project-tab (test-claude-buffer web "c") nil)
     (should (equal (alist-get 'name (tab-bar--current-tab)) "web"))
     (should (= 3 (length (test-tab-names)))))))

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
  (should-not (kdb-claude--display-in-project-tab (test-claude-buffer "/tmp/" "x") nil)))

(ert-deftest session-shown-in-its-tab-is-not-split-again ()
  (test-with-tabs
   (let* ((api (test-project "api"))
          (a (test-claude-buffer api "a"))
          (win (kdb-claude--display-in-project-tab a nil)))
     (tab-bar-switch-to-tab (car (test-tab-names)))
     (should (eq (kdb-claude--display-in-project-tab a nil) win))
     (should (one-window-p t)))))

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
              ((symbol-function 'kdb-claude--new-in) (lambda (dir) (setq started dir)))
              ((symbol-function 'kdb-claude-read-directory) (lambda (&rest _) "/elsewhere/")))
      (with-current-buffer from
        ;; Open sessions first, not the one you're in; then new ones.
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p table &rest _) (setq offered (all-completions "" table)) (car offered))))
          (kdb-claude-send))
        (should (= 3 (length offered)))
        (should (string-match-p "\\`api +Claude Agent @ test" (nth 0 offered)))
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

(ert-deftest pr-review-opens-its-own-tab ()
  (test-with-tabs
   (let* ((apps (test-project "apps"))
          (kdb-claude-review-skill "team-review")
          (kdb-claude-review-repos nil)
          started)
     (cl-letf (((symbol-function 'project-known-project-roots) (lambda () (list "/x/data/" apps)))
               ((symbol-function 'kdb-claude--start-with)
                (lambda (dir prompt &rest _)
                  (setq started (list dir prompt))
                  ;; As agent-shell shows a new session.
                  (let ((buf (test-claude-buffer dir "r")))
                    (funcall (caar agent-shell-display-action) buf nil)
                    buf))))
       (kdb-claude-review-pr "https://github.com/acme/apps/pull/123")
       (should (equal started (list apps "/team-review https://github.com/acme/apps/pull/123")))
       (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#123"))
       (should (one-window-p t))
       ;; The same PR again: back to its tab, nothing new started.
       (tab-bar-switch-to-tab (car (test-tab-names)))
       (setq started nil)
       (kdb-claude-review-pr "https://github.com/acme/apps/pull/123/files")
       (should-not started)
       (should (equal (alist-get 'name (tab-bar--current-tab)) "PR apps#123"))
       ;; A checkout named differently comes from the configured list.
       (let ((kdb-claude-review-repos '(("data" . "/src/data-repo/"))))
         (should (equal (kdb-claude--repo-checkout "data") "/src/data-repo/")))))))

;;; tabs-session-test.el ends here
