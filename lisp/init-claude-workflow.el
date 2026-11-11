;;; init-claude-workflow.el --- Claude workflow on agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; One transient menu over agent-shell (ACP) sessions: start them per
;; project, branch or note, send them context, jump to whichever needs
;; you (agent-shell-attention), and search or resume past sessions.

;;; Code:

(require 'cl-lib)
(require 'init-claude)
(require 'transient)

(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-send-dwim "agent-shell")
(defvar agent-shell--state)
(declare-function agent-shell--start "agent-shell")
(declare-function comint-osc-process-output "comint")
(declare-function kdb-claude--goto-session "init-claude")

(declare-function agent-shell--prompt-queue-read "agent-shell")
(declare-function agent-shell--prompt-queue-migrate "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--context "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function dired-get-filename "dired")
(declare-function dbus-call-method "dbus")
(declare-function w32-shell-execute "w32fns.c")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-review "agent-review")
(declare-function agent-shell-attention-jump "agent-shell-attention")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function notifications-notify "notifications")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(declare-function org-back-to-heading "org")
(declare-function org-mark-subtree "org")
(defvar agent-shell-cwd-function)
(defvar agent-shell-busy-indicator-frames)

;;; ============================================================
;;; Attention
;;; ============================================================

(defvar kdb-claude--responders (make-hash-table :test #'equal)
  "Permission request id -> agent-shell permission alist (with :respond).")

(defvar kdb-claude--permissions nil
  "Pending permission requests, oldest first, as (ID BUFFER PERMISSION).")

;;;###autoload
(defun kdb-claude--capture-permission (permission)
  "Remember PERMISSION so it can be answered from anywhere.
Return nil so agent-shell still shows its inline card."
  (puthash (map-elt (map-elt permission :tool-call) :permission-request-id)
           permission kdb-claude--responders)
  nil)

(defun kdb-claude--forget-permission (buf id)
  "Drop session BUF's permission request ID from the queue.
Each session numbers its own requests, so another session's ID is kept."
  (setq kdb-claude--permissions (seq-remove (lambda (e) (and (equal (car e) id) (eq (nth 1 e) buf)))
                                            kdb-claude--permissions)))

(defun kdb-claude--permission-open-p (buf id)
  "Whether session BUF still waits on permission request ID.
A request the agent cancels sends no response event, so it'd stay queued."
  (seq-some (lambda (tc) (equal (map-elt (cdr tc) :permission-request-id) id))
            (map-elt (buffer-local-value 'agent-shell--state buf) :tool-calls)))

;;;###autoload
(defun kdb-claude--track-permissions ()
  "Queue this shell's permission requests for `kdb-claude-answer'."
  (let ((buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buf
     :on-event (lambda (event)
                 (let ((id (map-elt (map-elt event :data) :request-id)))
                   (pcase (map-elt event :event)
                     ('permission-request
                      ;; Captured just now, in this same request: take it before another session reuses the id.
                      (when-let* ((perm (gethash id kdb-claude--responders)))
                        (remhash id kdb-claude--responders)
                        (setq kdb-claude--permissions
                              (append kdb-claude--permissions (list (list id buf perm))))))
                     ('permission-response (kdb-claude--forget-permission buf id))
                     ('clean-up
                      (setq kdb-claude--permissions
                            (seq-remove (lambda (e) (eq (nth 1 e) buf)) kdb-claude--permissions)))))))))

(defconst kdb-claude--permission-keys
  '(("allow_once" . ?y) ("allow_always" . ?!) ("reject_once" . ?n) ("reject_always" . ?N)))

(defconst kdb-claude--permission-panel "*Claude permission*")

;;;###autoload
(defun kdb-claude-answer ()
  "Answer the oldest permission request from any session, without leaving
the current buffer, then the next one.  Each is shown in a panel: the
session, what it wants to do, and the command or the change.  With none
pending, jump to the next session waiting on you."
  (interactive)
  (unwind-protect (kdb-claude--answer-next)
    (when-let* ((win (get-buffer-window kdb-claude--permission-panel)))
      (quit-window t win))))

(defun kdb-claude--answer-next ()
  "Answer the oldest pending permission request, then the next."
  (setq kdb-claude--permissions (seq-filter (lambda (e) (and (buffer-live-p (nth 1 e))
                                                             (kdb-claude--permission-open-p (nth 1 e) (car e))))
                                            kdb-claude--permissions))
  (pcase (car kdb-claude--permissions)
    ('nil (unless (get-buffer-window kdb-claude--permission-panel) ; none were waiting
            (agent-shell-attention-jump)))
    (`(,id ,buf ,perm)
     (let* ((options (seq-filter (lambda (o) (assoc (map-elt o :kind) kdb-claude--permission-keys))
                                 (map-elt perm :options)))
            (choices (append (mapcar (lambda (o)
                                       (list (cdr (assoc (map-elt o :kind) kdb-claude--permission-keys))
                                             (map-elt o :option) o))
                                     options)
                             '((?g "go to session") (?q "later"))))
            (more (1- (length kdb-claude--permissions))))
       (kdb-claude--show-permission buf perm)
       (let* ((key (car (read-multiple-choice
                         (if (> more 0) (format "Answer (%d more waiting)" more) "Answer")
                         (mapcar (lambda (c) (seq-take c 2)) choices))))
              (opt (nth 2 (assq key choices))))
         (cond (opt
                (with-current-buffer buf
                  (funcall (map-elt perm :respond) (map-elt opt :option-id))
                  (when (equal (map-elt opt :kind) "reject_once")
                    (agent-shell-interrupt t)))
                (kdb-claude--forget-permission buf id)
                (when kdb-claude--permissions (kdb-claude--answer-next)))
               ((eq key ?g)
                (kdb-claude--goto-session buf)   ; in its tab, if it has one
                (agent-shell-jump-to-latest-permission-button-row))))))))

(defun kdb-claude--session-label (buf project)
  "Session BUF's name without PROJECT, which is shown beside it."
  (string-remove-prefix (concat project " · ")
                        (string-remove-prefix "Claude Agent @ " (buffer-name buf))))

(defun kdb-claude--permission-gist (call)
  "One line saying what tool CALL is for: its description, else its title."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n]+" " "
    (replace-regexp-in-string "```[a-z]*" ""
                              (or (map-elt call :description)
                                  (map-elt (map-elt call :raw-input) 'description)
                                  (map-elt call :title)
                                  "use a tool")))))

;;;###autoload
(defun kdb-claude--permission-message (message buffer text)
  "Around agent-shell-attention's notice: a permission request in plain words.
Instead of its raw \"Permission: <command>\", say which session asks for
what, and how to answer; not over a prompt you're answering."
  (if-let* (((string-prefix-p "Permission:" text))
            ((buffer-live-p buffer))
            ((agent-shell-attention--should-notify-buffer buffer))
            (call (seq-some (lambda (tc) (and (map-elt (cdr tc) :permission-request-id) (cdr tc)))
                            (reverse (map-elt (buffer-local-value 'agent-shell--state buffer)
                                              :tool-calls)))))
      (let* ((session (string-remove-prefix "Claude Agent @ " (buffer-name buffer)))
             (asks (format "%s asks: %s" session (kdb-claude--permission-gist call))))
        (unless (active-minibuffer-window)
          (message "%s  %s" (truncate-string-to-width asks (max 20 (- (frame-width) 22)) nil nil "…")
                   (propertize "(C-c ' to answer)" 'face 'shadow)))
        (agent-shell-attention--maybe-notify buffer session asks))
    (funcall message buffer text)))

;;;###autoload
(defun kdb-claude--permission-title (title-fn &rest args)
  "Around agent-shell's permission card title: what it's for, then the command."
  (let ((title (apply title-fn args))
        (what (map-elt (plist-get args :tool-call) :description)))
    (if (and what title (not (string-match-p (regexp-quote what) title)))
        (concat what "\n\n" title)
      title)))

(defun kdb-claude--permission-text (buf perm)
  "What session BUF asks to do in permission request PERM, to read at a glance."
  (let* ((call (map-elt perm :tool-call))
         (input (map-elt call :raw-input))
         (project (file-name-nondirectory
                   (directory-file-name (kdb-claude--project-dir
                                         (buffer-local-value 'default-directory buf)))))
         (session (kdb-claude--session-label buf project))
         (command (let ((c (or (map-elt call :command) (map-elt input 'command))))
                    (and (stringp c) (not (string-empty-p c)) c)))
         (file (or (map-elt input 'file_path) (map-elt input 'notebook_path)))
         (lines (lambda (text face prefix)
                  (let* ((ls (split-string (or text "") "\n"))
                         (shown (seq-take ls 8)))
                    (concat (mapconcat (lambda (l) (propertize (concat prefix l) 'face face)) shown "\n")
                            (if (> (length ls) 8)
                                (propertize (format "\n%s… %d more lines" prefix (- (length ls) 8)) 'face 'shadow)
                              ""))))))
    (concat
     (propertize project 'face 'bold)
     (if (equal session project) "" (concat (propertize "  ·  " 'face 'shadow) session))
     "\n\n"
     (propertize (or (map-elt call :description) (map-elt input 'description)
                     (map-elt call :title) "Use a tool")
                 'face 'font-lock-keyword-face)
     "\n"
     (cond (command (concat "\n" (propertize "$ " 'face 'shadow)
                            (propertize command 'face 'font-lock-string-face) "\n"))
           ((map-elt call :diffs)
            (mapconcat (lambda (d)
                         (concat "\n" (propertize (abbreviate-file-name (or (map-elt d :file) file "")) 'face 'bold)
                                 "\n" (funcall lines (map-elt d :old) 'diff-removed "- ")
                                 "\n" (funcall lines (map-elt d :new) 'diff-added "+ ") "\n"))
                       (seq-take (map-elt call :diffs) 2) ""))
           (file (concat "\n" (propertize (abbreviate-file-name file) 'face 'bold) "\n"))
           (t "")))))

(defun kdb-claude--show-permission (buf perm)
  "Show permission request PERM from session BUF in the panel above the prompt."
  (with-current-buffer (get-buffer-create kdb-claude--permission-panel)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (kdb-claude--permission-text buf perm))
      (goto-char (point-min)))
    (setq buffer-read-only t)
    (visual-line-mode 1)
    (display-buffer (current-buffer)
                    '((display-buffer-in-side-window)
                      (side . bottom) (slot . 1)
                      (window-height . fit-window-to-buffer)))))

(defvar kdb-claude--notification nil
  "Id of the last Windows notification, which must be closed before the next.")

(defun kdb-claude--emacs-focused-p ()
  "Non-nil when some Emacs frame has focus."
  (seq-some #'frame-focus-state (frame-list)))

;;;###autoload
(defun kdb-claude--notify (_buffer title body)
  "Show TITLE/BODY as a desktop notification, only when Emacs isn't focused."
  (unless (kdb-claude--emacs-focused-p)
    (cond ((fboundp 'w32-notification-notify)
           (when kdb-claude--notification
             (w32-notification-close kdb-claude--notification))
           (setq kdb-claude--notification
                 (w32-notification-notify :title title :body (concat body "  (C-c ' in Emacs)"))))
          ((require 'notifications nil t)
           (notifications-notify :app-name "Emacs" :title title
                                 :body (concat body "  (C-c ' in Emacs)"))))))

;;; ============================================================
;;; Starting
;;; ============================================================

(defun kdb-claude--mark-org-entry ()
  "In org, with no region, mark the entry at point so it is carried over."
  (when (and (derived-mode-p 'org-mode) (not (use-region-p))
             (ignore-errors (org-back-to-heading t) t))
    (org-mark-subtree)))

(declare-function agent-shell--get-files-context "agent-shell")
(defvar agent-shell-context-sources)

(defun kdb-claude--this-file ()
  "This buffer's file, as context for a session: when nothing is selected,
the file, not agent-shell's default of the line point is on."
  (when buffer-file-name
    (agent-shell--get-files-context :files (list buffer-file-name))))

(defconst kdb-claude--context-sources '(files region error kdb-claude--this-file)
  "What a new or sent-to session gets: Dired files, the region, the error
at point, else this file.")

(defun kdb-claude--new-in (dir)
  "Start a new Claude shell in DIR (nil = the current project root).
The region, Dired files or org entry at point become its first input."
  (require 'agent-shell)
  (kdb-claude--mark-org-entry)
  (let ((agent-shell-cwd-function (if dir (lambda () dir) agent-shell-cwd-function))
        (agent-shell-context-sources kdb-claude--context-sources))
    (agent-shell-anthropic-start-claude-code)))


(defvar project-find-functions)
(declare-function project-try-vc "project")

(defun kdb-claude--project-dir (dir)
  "DIR's project root, or DIR outside any project.
Only DIR itself decides: tabspaces' fallback to the current tab's
project would put a session outside any project in whatever tab you're in."
  (if-let* ((project (let ((project-find-functions (list #'project-try-vc)))
                       (project-current nil dir))))
      (project-root project)
    dir))

(defvar-local kdb-claude--tab-auto nil
  "Name given to this session's tab here, renamed with the title.
Nil when the tab was named otherwise (a PR review's, say).")
(put 'kdb-claude--tab-auto 'permanent-local t)

;;;###autoload
(defun kdb-claude--display-in-own-tab (buffer _alist)
  "Show Claude session BUFFER in a tab of its own, named after it.
A tab already showing it is gone to instead.  The tab is first named
after the session's folder, then after its title once it has one.
Returns the window, for `agent-shell-display-action'."
  (when (bound-and-true-p tab-bar-mode)
    (if-let* ((shown (tab-bar-get-buffer-tab buffer)))
        (progn (tab-bar-select-tab (1+ (alist-get 'index shown)))
               (get-buffer-window buffer))
      (let ((name (file-name-nondirectory
                   (directory-file-name
                    (kdb-claude--project-dir (buffer-local-value 'default-directory buffer))))))
        (tab-bar-new-tab)
        (tab-bar-rename-tab name)
        (with-current-buffer buffer (setq kdb-claude--tab-auto name))
        (delete-other-windows)
        (switch-to-buffer buffer nil t)
        (selected-window)))))

(defun kdb-claude--show-in-background (buf)
  "Show session BUF in its tab, then come back to this tab and window."
  (let ((from (tab-bar--current-tab-index))
        (win (selected-window)))
    (when (kdb-claude--display-in-own-tab buf nil)
      (unless (eql from (tab-bar--current-tab-index))
        (tab-bar-select-tab (1+ from)))
      (when (window-live-p win) (select-window win)))))

(defvar kdb-claude--id)
(defvar kdb-claude--users)

;;;###autoload
(defun kdb-claude--close-session-window ()
  "When a session is killed, close its tab, or its window if it shares one.
What it opened (`kdb-claude-show', `kdb-claude-run') is closed too."
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when kdb-claude--id
                (ignore-errors (kdb-claude-cleanup kdb-claude--id)))
              (ignore-errors (kdb-claude--close-session-tab (current-buffer)))
              (dolist (w (get-buffer-window-list (current-buffer) nil))
                (ignore-errors (delete-window w))))
            nil t))

(defun kdb-claude--close-session-tab (buf)
  "Close the tab showing session BUF, when BUF is all it shows."
  (when-let* (((bound-and-true-p tab-bar-mode))
              ((cdr (tab-bar-tabs)))                 ; never the last tab
              (tab (tab-bar-get-buffer-tab buf)))
    (when (if (eq (car tab) 'current-tab)
              (and (one-window-p t) (eq (window-buffer) buf))
            (equal (window-state-buffers (alist-get 'ws tab)) (list (buffer-name buf))))
      (tab-bar-close-tab (1+ (alist-get 'index tab))))))

(defun kdb-claude--start-with (dir &optional prompt session-id quietly)
  "Start a Claude shell in DIR, resuming SESSION-ID if non-nil.
Submit PROMPT once the shell is ready.  Return the shell buffer.
QUIETLY `background': shown in its project tab, focus stays here;
`hidden': not shown at all."
  (require 'agent-shell)
  (let* ((default-directory dir)
         (agent-shell-cwd-function (lambda () dir))
         (buf (agent-shell--start :config (agent-shell-anthropic-make-claude-code-config)
                                  :no-focus quietly :new-session t :session-id session-id)))
    (when (eq quietly 'background)
      (kdb-claude--show-in-background buf))
    (when prompt
      (let (token)
        (setq token (agent-shell-subscribe-to
                     :shell-buffer buf :event 'prompt-ready
                     :on-event (lambda (_)
                                 (with-current-buffer buf
                                   (agent-shell-unsubscribe :subscription token))
                                 (agent-shell-insert :text prompt :submit t :shell-buffer buf))))))
    buf))

;;;###autoload
(defun kdb-claude-new ()
  "Start a new Claude session in this project, on a branch you pick.
A branch gets its own worktree, so sessions on different things don't
collide; a new one starts from origin's default branch, fetched.
\"(current checkout)\", the first choice, stays put.  Outside a project,
it asks for a folder.  The region, Dired files, org entry or file at
point are carried over."
  (interactive)
  (kdb-claude--start-on-branch
   (if (project-current)
       (kdb-claude--project-dir default-directory)
     (file-name-as-directory (expand-file-name (read-directory-name "Start Claude in: " nil nil t))))))

;;;###autoload
(defun kdb-claude-new-in-folder (dir)
  "Start a new Claude session in folder DIR, project or not.
Asked with Emacs's usual folder prompt, from the current folder; in a
git repo, then for a branch, as `kdb-claude-new' does.  The region,
Dired files or org entry at point are carried over."
  (interactive (list (read-directory-name "Start Claude in: " nil nil t)))
  (kdb-claude--start-on-branch (file-name-as-directory (expand-file-name dir))))

;;;###autoload
(defun kdb-claude-scratch ()
  "Start a throwaway session for a one-off question, outside any project.
It runs in a scratch folder under the temp directory, gets its own tab,
and stays out of `kdb-claude-recent'.  The region, Dired files or org
entry at point are carried over, as with `kdb-claude-new'."
  (interactive)
  (let ((dir (file-name-as-directory (expand-file-name "claude-scratch" temporary-file-directory))))
    (make-directory dir t)
    (kdb-claude--new-in dir)))

;;;###autoload
(defun kdb-claude-queue-edit ()
  "Change a message waiting in this project's session queue, in place.
Emptied, it is taken out of the queue."
  (interactive)
  (require 'agent-shell)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let* ((pending (or (map-elt agent-shell--state :pending-prompts)
                        (user-error "Nothing is queued")))
           (choices (seq-map-indexed
                     (lambda (p i)
                       (cons (format "%d: %s" (1+ i)
                                     (truncate-string-to-width
                                      (replace-regexp-in-string "\n" " " (substring-no-properties p))
                                      70 nil nil "…"))
                             i))
                     pending))
           (i (cdr (assoc (completing-read
                           "Edit queued: "
                           (lambda (s p a)
                             (if (eq a 'metadata)
                                 '(metadata (display-sort-function . identity)
                                            (cycle-sort-function . identity))
                               (complete-with-action a choices s p)))
                           nil t)
                          choices)))
           (text (string-trim (agent-shell--prompt-queue-read :initial (nth i pending))))
           ;; Read again: the turn may have ended, and the queue gone out, meanwhile.
           (now (map-elt agent-shell--state :pending-prompts)))
      (unless (equal (nth i now) (nth i pending))
        (user-error "That message has already been sent"))
      (map-put! agent-shell--state :pending-prompts
                (if (string-empty-p text)
                    (append (seq-take now i) (seq-drop now (1+ i)))
                  (append (seq-take now i) (list text) (seq-drop now (1+ i)))))
      (message (if (string-empty-p text) "Removed from the queue" "Queued message changed")))))

;;;###autoload
(defun kdb-claude-send ()
  "Send the region, Dired files, org entry at point, or else this file, to a
session you pick.  Or hand it off to a new session, in this project or
another."
  (interactive)
  (require 'agent-shell)
  (kdb-claude--mark-org-entry)
  (let* ((here (kdb-claude--project-dir default-directory))
         (new-here (format "+ New session in %s" (abbreviate-file-name here)))
         (new-other "+ New session in another project…")
         (sessions (mapcar (lambda (b)
                             (let ((project (file-name-nondirectory
                                             (directory-file-name
                                              (kdb-claude--project-dir (buffer-local-value 'default-directory b))))))
                               (cons (format "%-16s %s" (truncate-string-to-width project 16 nil nil "…")
                                             (kdb-claude--session-label b project))
                                     b)))
                           (remq (current-buffer) (agent-shell-buffers))))
         (pick (completing-read
                "Send to: "
                (lambda (s p a)
                  (if (eq a 'metadata)
                      '(metadata (display-sort-function . identity) (cycle-sort-function . identity))
                    (complete-with-action a (append (mapcar #'car sessions) (list new-here new-other)) s p)))
                nil t)))
    (cond ((equal pick new-here) (kdb-claude--start-on-branch here))
          ((equal pick new-other) (kdb-claude--start-on-branch (kdb-claude-read-directory "Project: ")))
          (t (let* ((buf (cdr (assoc pick sessions)))
                    (text (let ((agent-shell-context-sources kdb-claude--context-sources))
                            (agent-shell--context :shell-buffer buf))))
               (agent-shell--display-buffer buf)
               (when text
                 (agent-shell--insert-to-shell-buffer :text text :shell-buffer buf)))))))

;;;###autoload
(defun kdb-claude-review ()
  "Have Claude review this repo's uncommitted changes (agent-review).
In the results, mark findings with m and send them to a session with S."
  (interactive)
  (require 'agent-shell)
  (agent-review (agent-shell-anthropic-make-claude-code-config)))

;;; ============================================================
;;; Reviewing a GitHub pull request with the team's skill
;;; ============================================================

(defvar agent-shell-display-action)

(defvar kdb-claude-review-skill "app-dev-code-review"
  "The skill a PR review runs, as /NAME, in the PR's worktree.")

(defvar kdb-claude-review-repos nil
  "Alist of GitHub repo name -> local checkout, for PR reviews.
Only needed when the checkout's folder isn't named like the repo; those
are found among known projects.  Set it in personal.el.")

(defun kdb-claude--pr-url-parts (url)
  "(REPO NUMBER) of GitHub pull request URL, NUMBER nil if not a PR."
  (when (string-match "\\`https?://[^/]+/[^/]+/\\([^/#?]+\\)\\(?:/pull/\\([0-9]+\\)\\)?" url)
    (list (string-remove-suffix ".git" (match-string 1 url)) (match-string 2 url))))

(defun kdb-claude--run-steps (dir steps done)
  "Run STEPS, each a command list, one after another in DIR; then call DONE.
In the background, so a slow fetch doesn't freeze Emacs.  A failing step
stops there and shows its output."
  (if (null steps)
      (funcall done)
    (let ((default-directory dir)
          (out (get-buffer-create " *kdb-claude-steps*")))
      (with-current-buffer out (erase-buffer))
      (make-process
       :name "kdb-claude-step" :buffer out :command (car steps) :noquery t
       :sentinel (lambda (proc _)
                   (unless (process-live-p proc)
                     (if (zerop (process-exit-status proc))
                         (kdb-claude--run-steps dir (cdr steps) done)
                       (message "%s failed: %s" (string-join (car steps) " ")
                                (with-current-buffer out (string-trim (buffer-string)))))))))))

(defun kdb-claude--pr-worktree (checkout repo number done)
  "Make worktree REPO-review-NUMBER beside CHECKOUT on the PR's branch.
Then call DONE with its directory.  CHECKOUT itself is never switched.
An existing one is checked out again, for the PR's latest commits."
  (let ((dir (file-name-as-directory
              (expand-file-name (format "%s-review-%s" repo number)
                                (file-name-directory (directory-file-name checkout)))))
        (gh (or (executable-find "gh") (user-error "Needs gh, GitHub's CLI"))))
    (message "Checking out PR #%s in %s…" number (abbreviate-file-name dir))
    (kdb-claude--run-steps
     checkout
     (unless (file-directory-p dir)
       (list (list "git" "worktree" "add" "--detach" (directory-file-name dir))))
     (lambda ()
       (kdb-claude--run-steps dir (list (list gh "pr" "checkout" number))
                              (lambda () (funcall done dir)))))))

(defun kdb-claude--repo-checkout (repo)
  "Local checkout of GitHub REPO: configured, a known project, or asked."
  (or (cdr (assoc repo kdb-claude-review-repos))
      (seq-find (lambda (dir) (equal (file-name-nondirectory (directory-file-name dir)) repo))
                (and (fboundp 'project-known-project-roots) (project-known-project-roots)))
      (kdb-claude-read-directory (format "Checkout of %s: " repo))))

;;;###autoload
(defun kdb-claude-review-pr (url)
  "Review the GitHub pull request at URL with `kdb-claude-review-skill'.
Its branch is checked out in a worktree, REPO-review-N beside the repo's
checkout, and a session there runs /SKILL, in its own tab named after
the PR.  Reviewing the same PR again goes back to that tab.
Interactively, a PR URL just copied is the default."
  (interactive
   (let* ((copied (ignore-errors (string-trim (current-kill 0 t))))
          (copied (and copied (cadr (kdb-claude--pr-url-parts copied)) copied)))
     (list (read-string (format-prompt "GitHub PR URL" copied) nil nil copied))))
  (pcase-let* ((`(,repo ,number) (kdb-claude--pr-url-parts url))
               (tab (format "PR %s#%s" repo number)))
    (unless number (user-error "Not a GitHub pull request URL: %s" url))
    (if (and (bound-and-true-p tab-bar-mode)
             (member tab (mapcar (lambda (tb) (alist-get 'name tb)) (tab-bar-tabs))))
        (progn (tab-bar-switch-to-tab tab)
               (message "Already reviewing %s here" tab))
      (kdb-claude--pr-worktree
       (file-name-as-directory (expand-file-name (kdb-claude--repo-checkout repo)))
       repo number
       (lambda (dir)
         (let ((agent-shell-display-action
                (if (bound-and-true-p tab-bar-mode)
                    `((,(lambda (buf _)
                          (tab-bar-new-tab)
                          (tab-bar-rename-tab tab)
                          (delete-other-windows)
                          (switch-to-buffer buf nil t)
                          (selected-window))))
                  agent-shell-display-action)))
           (kdb-claude--start-with dir (concat "/" kdb-claude-review-skill))))))))

;;; ============================================================
;;; Worktrees
;;; ============================================================

(defun kdb-claude--git-lines (dir &rest args)
  "Return output lines of git ARGS in DIR, or nil on failure."
  (let ((default-directory dir))
    (ignore-errors (apply #'process-lines "git" args))))

(defun kdb-claude--branch-worktree (project branch)
  "Return the directory where BRANCH of PROJECT is checked out, or nil."
  (let (dir found)
    (dolist (line (kdb-claude--git-lines project "worktree" "list" "--porcelain"))
      (cond ((string-prefix-p "worktree " line) (setq dir (substring line 9)))
            ((equal line (concat "branch refs/heads/" branch)) (setq found dir))))
    (and found (file-name-as-directory found))))

(defun kdb-claude--worktree (project branch &optional base)
  "Return a worktree of PROJECT on BRANCH, creating it beside PROJECT.
A BRANCH that doesn't exist yet is created from BASE, else PROJECT's HEAD.
The main checkout is never switched, so parallel sessions stay put."
  (or (kdb-claude--branch-worktree project branch)
      (let ((dir (expand-file-name
                  (format "%s-%s"
                          (file-name-nondirectory (directory-file-name project))
                          (replace-regexp-in-string "/" "-" branch))
                  (file-name-directory (directory-file-name project)))))
        (when (file-exists-p dir)
          (user-error "%s exists but is not a worktree of %s" dir branch))
        (let ((default-directory project))
          (unless (zerop (apply #'call-process "git" nil nil nil "worktree" "add"
                                (if (kdb-claude--git-lines project "rev-parse" "--verify" "--quiet"
                                                           (concat "refs/heads/" branch))
                                    (list dir branch)
                                  (append (list "-b" branch dir) (and base (list base))))))
            (user-error "git worktree add failed for %s" branch)))
        (file-name-as-directory dir))))

(defun kdb-claude--main-checkout (dir)
  "The main checkout of the repo DIR is in, even from one of its worktrees."
  (if-let* ((common (car (kdb-claude--git-lines dir "rev-parse" "--path-format=absolute"
                                                "--git-common-dir")))
            ((string-suffix-p "/.git" (directory-file-name common))))
      (file-name-directory (directory-file-name common))
    dir))

(defun kdb-claude--default-base (main callback)
  "Call CALLBACK with what a new branch in MAIN starts from.
Origin's default branch, fetched first, in the background; if the fetch
fails, the local default branch; without an origin, MAIN's HEAD."
  (let* ((remote (car (kdb-claude--git-lines main "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
         (name (if remote (string-remove-prefix "origin/" remote)
                 (seq-find (lambda (b) (kdb-claude--git-lines main "rev-parse" "--verify" "--quiet"
                                                               (concat "refs/heads/" b)))
                           '("main" "master"))))
         (local (and name (kdb-claude--git-lines main "rev-parse" "--verify" "--quiet"
                                                  (concat "refs/heads/" name))
                     name)))
    (if (not remote)
        (funcall callback local)
      (message "Fetching origin/%s…" name)
      (let ((default-directory main))
        (make-process
         :name "claude-fetch" :buffer nil :noquery t
         :command (list "git" "fetch" "--quiet" "origin" name)
         :sentinel (lambda (proc _)
                     (unless (process-live-p proc)
                       (if (zerop (process-exit-status proc))
                           (funcall callback remote)
                         (message "Couldn't fetch origin; starting from %s as it is here"
                                  (or local "the current checkout"))
                         (funcall callback local)))))))))

(defun kdb-claude--pick-branch (dir callback)
  "Ask which branch a session in DIR's repo works on; call CALLBACK with where.
\"(current checkout)\", first, stays in DIR.  An existing branch, local or
only on origin, opens in its worktree, made if need be; a new name gets a
new branch from origin's default branch, fetched, in a new worktree beside
the main checkout.  Outside git, DIR, without asking."
  (if-let* ((locals (kdb-claude--git-lines dir "branch" "--format=%(refname:short)")))
      (let* ((main (kdb-claude--main-checkout dir))
             (remotes (seq-remove (lambda (r) (or (not (string-match-p "/" r))
                                                   (string-suffix-p "/HEAD" r)
                                                   (member (string-remove-prefix "origin/" r) locals)))
                                  (kdb-claude--git-lines dir "branch" "-r" "--format=%(refname:short)")))
             (worktrees (seq-keep (lambda (line) (and (string-prefix-p "branch refs/heads/" line)
                                                      (substring line 18)))
                                  (kdb-claude--git-lines dir "worktree" "list" "--porcelain")))
             (here "(current checkout)")
             (choices (append (list here) locals remotes))
             (b (completing-read
                 "Branch (new name = new worktree; M-j for exactly what you typed): "
                 (lambda (str pred action)
                   (if (eq action 'metadata)
                       `(metadata (display-sort-function . identity) (cycle-sort-function . identity)
                                  (annotation-function
                                   . ,(lambda (c) (cond ((member c worktrees) "  worktree")
                                                        ((member c remotes) "  on origin")))))
                     (complete-with-action action choices str pred))))))
        (cond ((or (string-empty-p b) (equal b here)) (funcall callback dir))
              ((member b locals) (funcall callback (kdb-claude--worktree main b)))
              ((member b remotes)
               (funcall callback (kdb-claude--worktree main (string-remove-prefix "origin/" b) b)))
              (t (kdb-claude--default-base
                  main (lambda (base) (funcall callback (kdb-claude--worktree main b base)))))))
    (funcall callback dir)))

(defun kdb-claude--start-on-branch (dir)
  "Start a session in DIR's repo on a branch you pick, with what's at point.
The region, Dired files, org entry or file is taken now: a new branch
waits for a fetch, and you may have moved on by then."
  (require 'agent-shell)
  (kdb-claude--mark-org-entry)
  (let ((text (let ((agent-shell-context-sources kdb-claude--context-sources))
                (agent-shell--context))))
    (kdb-claude--pick-branch
     dir (lambda (where)
           (let ((buf (kdb-claude--start-with where)))
             (when text
               (agent-shell--insert-to-shell-buffer :text text :shell-buffer buf)))))))

;;;###autoload
(defun kdb-claude--header-symbol-fonts (model)
  "Give agent-shell's graphical header MODEL fallback fonts for symbols.
The header is an SVG image drawn by librsvg, which ignores Emacs's
fontsets, so symbols the main font lacks need fallbacks named here."
  (let ((family (map-elt model :font-family)))
    (if (stringp family)
        (cons (cons :font-family (concat family ", Segoe UI Symbol, Segoe UI Emoji"))
              (assq-delete-all :font-family (copy-alist model)))
      model)))

;;; ============================================================
;;; Busy animation
;;; ============================================================

(defvar kdb-claude-busy-indicator 'claude
  "Busy animation in Claude sessions: `claude' (the header's Claude icon
hopping beside a sliding bar), or any value
`agent-shell-busy-indicator-frames' takes, e.g. `wave'.")

(defconst kdb-claude--busy-bar
  (let ((bar (lambda (n) (string-pad (make-string n ?░) 8))))
    (append (mapcar bar (number-sequence 1 8)) (mapcar bar '(6 4 2 0))))
  "An 8-cell bar that grows and shrinks, 10 frames a second.
Listed here, not agent-shell's `wide', so its width can be held when idle.
Its 12 frames are one hop of `kdb-claude--logo-hop', so bar and icon
repeat together and the header has few distinct frames to draw.")

(defconst kdb-claude--logo-hop
  (let ((hop '((0 1.14 0.84 0) (-6 0.92 1.12 3) (-14 0.96 1.06 5) (-19 1.0 1.0 6)
               (-21 1.0 1.0 6) (-19 1.0 1.0 5) (-14 0.98 1.03 3) (-6 0.95 1.07 1)
               (0 1.14 0.84 0) (0 1.05 0.95 0) (0 1.0 1.0 0) (0 1.0 1.0 0))))
    (append (mapcar (lambda (f) (append (butlast f) (list (- (car (last f)))))) hop)
            hop))
  "Icon (DY SX SY ANGLE) per tick: two hops, leaning left then right.
Each squashes, stretches up, hangs, lands and settles.  DY is in
hundredths of the icon's height; it tilts about its feet.")

(defconst kdb-claude--logo-size 0.82
  "Icon size within its slot, leaving room to hop.")

(declare-function agent-shell--image-type-to-mime "agent-shell")
(declare-function agent-shell--fetch-agent-icon "agent-shell")

(defun kdb-claude--logo-svg (icon step)
  "SVG of image file ICON moved for STEP of `kdb-claude--logo-hop'.
The whole icon moves; its artwork is never altered."
  (let* ((hop (nth step kdb-claude--logo-hop))
         (mime (or (agent-shell--image-type-to-mime icon) "image/png"))
         (href (concat "data:" mime ";base64,"
                       (base64-encode-string
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally icon)
                          (buffer-string))
                        t))))
    (format (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                    "xmlns:xlink=\"http://www.w3.org/1999/xlink\" "
                    "viewBox=\"0 0 100 100\" width=\"100\" height=\"100\">"
                    "<image width=\"100\" height=\"100\" href=\"%s\" xlink:href=\"%s\" "
                    "transform=\"translate(50 %d) rotate(%d) scale(%s %s) translate(-50 -100)\"/>"
                    "</svg>")
            href href (+ 100 (nth 0 hop)) (nth 3 hop)
            (* kdb-claude--logo-size (nth 1 hop)) (* kdb-claude--logo-size (nth 2 hop)))))

(defvar kdb-claude--logo-files (make-hash-table :test #'equal)
  "Header icon name -> its hop frame's file: each is drawn once, not per tick.")

;;;###autoload
(defun kdb-claude--logo-icon (name)
  "Icon file for header icon NAME if it is a hopping-logo frame, else nil.
Before-until advice on `agent-shell--fetch-agent-icon'.  NAME is
kdb-logo-STEP|ICON, ICON being the name agent-shell would fetch."
  (when (and (stringp name) (string-prefix-p "kdb-logo-" name))
    (with-memoization (gethash name kdb-claude--logo-files)
      (kdb-claude--logo-file name))))

(defun kdb-claude--logo-file (name)
  "Write the hop frame NAME (kdb-logo-STEP|ICON) names; return its file."
  (when (string-match "\\`kdb-logo-\\([0-9]+\\)|\\(.+\\)" name)
    (let ((step (min (string-to-number (match-string 1 name)) (1- (length kdb-claude--logo-hop)))))
      (when-let* ((icon (agent-shell--fetch-agent-icon (match-string 2 name))))
        (let* ((svg (kdb-claude--logo-svg icon step))
               ;; Named by content, so a new icon never shows a stale copy.
               (file (expand-file-name (format "kdb-logo-%d-%x.svg" step (sxhash-equal svg))
                                       (expand-file-name "kdb-claude-logo" temporary-file-directory))))
          (unless (file-exists-p file)
            (make-directory (file-name-directory file) t)
            (with-temp-file file (insert svg)))
          file)))))

;;;###autoload
(defun kdb-claude--logo-header (model)
  "While busy in `claude' style, move MODEL's icon a hop step every tick.
Filter on agent-shell's header model, run in the shell buffer.  The bar
changes every tick too, so each step is drawn: agent-shell only redraws
when the busy text changes."
  (if-let* ((icon (map-elt model :icon-name))
            (base (if (string-match "\\`kdb-logo-[0-9]+|\\(.+\\)" icon) (match-string 1 icon) icon))
            (name (if-let* (((eq kdb-claude-busy-indicator 'claude))
                            ((eq (map-nested-elt (agent-shell--state) '(:heartbeat :status)) 'busy))
                            (tick (map-nested-elt (agent-shell--state) '(:heartbeat :value))))
                      (format "kdb-logo-%d|%s" (mod tick (length kdb-claude--logo-hop)) base)
                    base))
            ((not (equal name icon))))
      ;; Busy ticks reuse the last full header, whose icon may already be
      ;; a frame: step on from its base icon, or it freezes there.
      (cons (cons :icon-name name)
            (assq-delete-all :icon-name (copy-alist model)))
    model))

(defvar kdb-claude-busy-beats-per-second
  (if (string-prefix-p "RDP-" (or (getenv "SESSIONNAME") "")) 5 10)
  "Busy animation steps a second.  Halved over Remote Desktop (and AVD),
where each redraw is slow.")

;;;###autoload
(defun kdb-claude--heartbeat-rate (args)
  "ARGS for `agent-shell-heartbeat-make', at `kdb-claude-busy-beats-per-second'."
  (append (list :beats-per-second kdb-claude-busy-beats-per-second) args))

;;;###autoload
(defun kdb-claude--focused-tick-p (&rest args)
  "Nil for a busy animation tick (ARGS has :cache-enabled) if Emacs is unfocused.
Before-while advice on `agent-shell--update-header-and-mode-line': each
tick redraws the header, so sessions working while you're in another
app animate for no one.  The first tick after you come back redraws."
  (or (not (plist-get args :cache-enabled))
      (kdb-claude--emacs-focused-p)))

;;;###autoload
(defun kdb-claude--busy-frames ()
  "Frames for `agent-shell-busy-indicator-frames', per `kdb-claude-busy-indicator'."
  (pcase kdb-claude-busy-indicator
    ('claude kdb-claude--busy-bar)
    (other other)))

;;;###autoload
(defun kdb-claude--steady-busy-frame (frame)
  "FRAME, or when idle, blanks as wide as the animation.
Filter on `agent-shell--busy-indicator-frame': the frame ends the mode
line's agent-shell part, so without this everything after it shifts
each time a turn starts or ends."
  (or frame
      (and (bound-and-true-p agent-shell-show-busy-indicator)
           (eq agent-shell-busy-indicator-frames #'kdb-claude--busy-frames)
           (let ((frames (kdb-claude--busy-frames)))
             (and (consp frames) (stringp (car frames))
                  (make-string (1+ (string-width (car frames))) ?\s))))))

;;;###autoload
(defun kdb-claude-set-busy-indicator (style)
  "Switch the busy animation of Claude sessions to STYLE."
  (interactive
   (list (intern (completing-read "Busy animation: "
                                  '("claude" "wide" "wave" "dots-block" "circle")
                                  nil t))))
  (setq kdb-claude-busy-indicator style))

;;; ============================================================
;;; Tabs: mark the ones with a session waiting for you
;;; ============================================================

(defvar agent-shell-attention--pending)
(defvar agent-shell-attention--busy)
(declare-function agent-shell-attention--tab-displays-buffer-p "agent-shell-attention")
(declare-function agent-shell-attention--pending-entry-permission-p "agent-shell-attention")
(declare-function agent-shell-attention--should-notify-buffer "agent-shell-attention")
(declare-function agent-shell-attention--maybe-notify "agent-shell-attention")

(defun kdb-claude--waiting ()
  "Sessions waiting for you or still working, as (BUFFER . KIND).
KIND is `input' for a permission request, `done' for a finished reply,
`busy' for a request still in flight or a background command running."
  (when (boundp 'agent-shell-attention--pending)
    (let (waiting)
      (maphash (lambda (b entry)
                 (when (buffer-live-p b)
                   (push (cons b (if (agent-shell-attention--pending-entry-permission-p entry)
                                     'input
                                   'done))
                         waiting)))
               agent-shell-attention--pending)
      (maphash (lambda (b _)
                 (when (and (buffer-live-p b) (not (assq b waiting)))
                   (push (cons b 'busy) waiting)))
               agent-shell-attention--busy)
      ;; Ready, but a command it started is still running: working too.
      (dolist (b (kdb-claude--working-in-background))
        (unless (assq b waiting)
          (push (cons b 'busy) waiting)))
      waiting)))

(defun kdb-claude--tab-waiting (tab _index)
  "State of the sessions TAB shows, or nil if none.
`input', `done' or `busy': a request for input outranks a finished
reply, which outranks working."
  (when-let* ((waiting (kdb-claude--waiting))
              ;; Shown there, not just visited there once (tabspaces' list).
              (kinds (mapcar #'cdr
                             (seq-filter
                              (lambda (w)
                                (if (eq (car tab) 'current-tab)
                                    (get-buffer-window (car w))
                                  (agent-shell-attention--tab-displays-buffer-p tab (car w))))
                              waiting))))
    (seq-find (lambda (k) (memq k kinds)) '(input done busy))))

(defface kdb-claude-tab-input '((t :inherit warning :weight bold))
  "Tab mark: a session there needs your input."
  :group 'tab-bar)
(defface kdb-claude-tab-done '((t :inherit success :weight bold))
  "Tab mark: a session there has finished a reply."
  :group 'tab-bar)
(defface kdb-claude-tab-busy '((t :inherit shadow))
  "Tab mark: a session there is still working."
  :group 'tab-bar)

;;;###autoload
(defun kdb-claude--tab-name-waiting (name tab i)
  "NAME marked when TAB, number I, has a session waiting for you or working.
● in the warning color: it needs your input (a permission request).
✓ in the success color: it has finished a reply.
◌ dimmed: it is still working.
The mark goes after the tab's number, on the tab's own face, so it is
highlighted with the tab it belongs to.  For
`tab-bar-tab-name-format-functions'."
  (if-let* ((state (kdb-claude--tab-waiting tab (1- i)))
            (mark (pcase state ('input "● ") ('done "✓ ") ('busy "◌ ")))
            (face (pcase state ('input 'kdb-claude-tab-input) ('done 'kdb-claude-tab-done)
                    ('busy 'kdb-claude-tab-busy))))
      (let ((tab-face (get-text-property 0 'face name))
            (at (if (string-match "\\`[0-9]+ " name) (match-end 0) 0)))
        (concat (substring name 0 at)
                (propertize mark 'face (if tab-face (list face tab-face) face))
                (substring name at)))
    name))

(defvar kdb-claude--waiting-drawn nil
  "The sessions waiting for you, and on what, when the tab bar was last drawn.")

;;;###autoload
(defun kdb-claude--redraw-tabs (&rest _)
  "Redraw the tab bar when which sessions wait for you, or on what, changed.
It can change with the count unchanged.  Runs every second, so it
redraws only on a change."
  (kdb-claude--scan-background)
  (let ((waiting (kdb-claude--waiting)))
    (unless (and (= (length waiting) (length kdb-claude--waiting-drawn))
                 (seq-every-p (lambda (w) (member w kdb-claude--waiting-drawn)) waiting))
      (setq kdb-claude--waiting-drawn waiting)
      (force-mode-line-update t))))

;;;###autoload
(defun kdb-claude--tab-save (buf)
  "Session record for Claude shell BUF, for tabspaces to restore, or nil."
  (with-current-buffer buf
    (when-let* (((derived-mode-p 'agent-shell-mode))
                (id (agent-shell-session-id :shell-buffer buf)))
      (list :kind 'claude :dir default-directory :name (buffer-name) :session id))))

;;;###autoload
(defun kdb-claude--tab-restore (record)
  "Resume the Claude session in RECORD, as `kdb-claude--tab-save' made it."
  (let ((dir (plist-get record :dir)))
    (when (file-directory-p dir)
      (kdb-claude--start-with (file-name-as-directory dir) nil (plist-get record :session) 'hidden))))

;;; ============================================================
;;; Artifacts: files Claude writes
;;; ============================================================

(defvar kdb-claude-artifact-auto-show nil
  "Non-nil to show documents Claude writes in the artifact pane as they land.
Off: they're links in the chat instead, opening in that pane when clicked.")

(defvar kdb-claude-artifact-extensions
  '("md" "markdown" "org" "txt" "csv" "svg" "png" "jpg" "jpeg" "gif" "pdf")
  "Extensions of files shown when Claude writes them; code is only listed.")

(defvar-local kdb-claude--artifacts nil
  "Files this session wrote, newest first.")
(defvar-local kdb-claude--artifact-calls nil
  "Tool calls already handled, since updates repeat.")
(defvar kdb-claude--artifact-pane nil
  "(WINDOW . BUFFER): the pane and the document last put in it.")

(defun kdb-claude--tool-call-file (tool-call)
  "The file a Write/Edit TOOL-CALL wrote, or nil."
  (let ((input (map-elt tool-call :raw-input)))
    (when-let* ((file (or (map-elt input 'file_path)
                          (map-elt input 'notebook_path)
                          (map-elt (car (map-elt tool-call :locations)) 'path))))
      (expand-file-name file))))

(defun kdb-claude--show-artifact (file &optional select near)
  "Show FILE in the artifact pane beside window NEAR; with SELECT, go there.
The pane is reused only while it still shows the last document, so a
window you have since used for something else is left alone."
  (let* ((buf (find-file-noselect file t))
         (near (or near (selected-window)))
         (pane (car kdb-claude--artifact-pane)))
    (with-current-buffer buf
      (unless (or (buffer-modified-p) (verify-visited-file-modtime))
        (revert-buffer t t t))
      (auto-revert-mode 1))
    (let ((win (or (get-buffer-window buf (window-frame near))
                   (and (window-live-p pane)
                        (not (eq pane near))
                        (eq (window-frame pane) (window-frame near))
                        (eq (window-buffer pane) (cdr kdb-claude--artifact-pane))
                        (progn (set-window-buffer pane buf) pane))
                   (display-buffer buf `((display-buffer-in-direction)
                                         (direction . right)
                                         (window . ,near)
                                         (window-width . 0.45)
                                         (inhibit-same-window . t))))))
      (setq kdb-claude--artifact-pane (cons win buf))
      (when (and select win) (select-window win))
      win)))

;;;###autoload
(defun kdb-claude--display-in-pane (buffer _alist)
  "Show BUFFER, a file a chat link opened, in the artifact pane beside the chat.
A `display-buffer' action, for `agent-shell-file-display-action'."
  (if-let* ((file (buffer-file-name buffer)))
      (kdb-claude--show-artifact file)
    (display-buffer-pop-up-window buffer nil)))

(declare-function agent-shell-markdown--apply-link-properties "agent-shell-markdown")

(defun kdb-claude--link-documents ()
  "Link bare paths of documents that exist, in this (narrowed) reply.
agent-shell links a path only when it names a line; a document Claude
wrote is usually cited whole, as `notes/plan.org' or ~/report.md."
  (let ((case-fold-search t)
        (ext (concat "\\.\\(?:" (regexp-opt kdb-claude-artifact-extensions) "\\)\\b")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ext nil t)
        (let ((end (point)) start)
          (save-excursion
            ;; Bounded, so a long run of path chars costs little.
            (skip-chars-backward "[:alnum:]._~/@+-" (max (point-min) (- end 300)))
            (when (looking-back "\\b[A-Za-z]:" (max (point-min) (- (point) 2))) ; C:/...
              (backward-char 2))
            (setq start (point)))
          (when-let* (((< start end))
                      ((not (file-remote-p default-directory)))
                      ;; Not the tail of a URL (https://host/x.md) or a share.
                      ((not (string-prefix-p "//" (buffer-substring-no-properties
                                                   start (min end (+ start 2))))))
                      ((not (get-text-property start 'agent-shell-markdown-url)))
                      ((not (get-text-property start 'agent-shell-markdown-source-block-body)))
                      (file (expand-file-name (buffer-substring-no-properties start end)))
                      ((file-regular-p file)))
            (agent-shell-markdown--apply-link-properties :start start :end end :url file)))))))

(defun kdb-claude--note-artifact (buf id tool-call)
  "Record the file tool call ID wrote in session BUF, showing it if a document.
Only Write calls, which create files; Edit ones change files already there.
Files under a _temp/ folder are working files (the review skill writes
many) and are left alone."
  (when (and (buffer-live-p buf)
             (equal (map-elt tool-call :status) "completed")
             (equal (map-elt tool-call :kind) "edit"))
    (with-current-buffer buf
      (unless (member id kdb-claude--artifact-calls)
        (push id kdb-claude--artifact-calls)
        (when-let* (((map-contains-key (map-elt tool-call :raw-input) 'content))
                    (file (kdb-claude--tool-call-file tool-call))
                    ((not (string-match-p "/_temp/" file))))
          (setq kdb-claude--artifacts (cons file (delete file kdb-claude--artifacts)))
          (when (and kdb-claude-artifact-auto-show
                     (member (downcase (or (file-name-extension file) ""))
                             kdb-claude-artifact-extensions)
                     (file-exists-p file))
            ;; Only beside a session you're looking at: a session in
            ;; another tab mustn't open panes over your work.
            (if-let* ((win (get-buffer-window buf)))
                (kdb-claude--show-artifact file nil win)
              ;; Not over a prompt you're answering.
              (unless (active-minibuffer-window)
                (message "%s created %s (C-c l a to open)"
                         (buffer-name buf) (file-name-nondirectory file))))))))))

;;;###autoload
(defun kdb-claude--track-emacs-session ()
  "Remember this shell's session as an Emacs one, for the history's Source.
And name it after its title once it has one, to tell it from others."
  (let ((buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buf :event 'prompt-ready
     :on-event (lambda (_)
                 (when-let* (((buffer-live-p buf))
                             (id (agent-shell-session-id :shell-buffer buf)))
                   (kdb-claude--note-emacs-session id)
                   (kdb-claude--name-session buf))))))

(defvar comint-prompt-regexp)
(declare-function agent-shell--prompt-face-p "agent-shell-prompt")

(defvar kdb-claude-session-max-chars 1000000
  "A session buffer past this many characters loses its oldest turns.
About half is kept.  Millions of characters (long tool output adds up)
make every GC and redisplay slow, which freezes Emacs for all sessions.")

(defun kdb-claude--trim-session ()
  "Trim this shell's oldest turns whenever a turn leaves it too big.
Only the buffer: the session, and its archived transcript, keep all."
  (let ((buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buf :event 'turn-complete
     :on-event (lambda (_)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (with-demoted-errors "Trimming Claude session: %S"
                       (kdb-claude--trim-buffer))))))))

(defun kdb-claude--trim-buffer ()
  "Delete the turns before the prompt nearest half of the limit from the end.
Nothing when that would free little: one huge last turn stays as it is."
  (when (> (buffer-size) kdb-claude-session-max-chars)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (- (point-max) (/ kdb-claude-session-max-chars 2)))
        ;; A real prompt, live or restored (its face says so), not
        ;; "Claude> " in a reply.
        (while (and (re-search-backward comint-prompt-regexp nil 'move)
                    (not (agent-shell--prompt-face-p (get-text-property (point) 'font-lock-face)))))
        (when (> (- (point) (point-min)) (/ kdb-claude-session-max-chars 10))
          (let ((inhibit-read-only t))
            ;; Not recorded, and older entries' positions would be off by
            ;; what was deleted, so undo starts over from here.
            (let ((buffer-undo-list t))
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (insert (propertize "Earlier turns trimmed to keep Emacs responsive; the archive has them all.\n\n"
                                  'font-lock-face 'shadow)))
            (setq buffer-undo-list nil)))))))

(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--state "agent-shell")

(defvar kdb-claude-agent-logs)

(defvar kdb-claude--clients nil
  "Every ACP client started, for `kdb-claude-reap-agents'.")

(defun kdb-claude--watch-agent (&rest args)
  "After `acp--start-client' (ARGS), say so in the session if its agent dies.
Between turns nothing else would (acp only fails pending requests); mid-turn
this adds why it died and how to resume to agent-shell's request error."
  (let* ((client (plist-get args :client))
         (proc (map-elt client :process)))
    (when (processp proc)
      (push client kdb-claude--clients)
      (add-function :before (process-sentinel proc)
                    (lambda (p event) (kdb-claude--agent-died client p event))))))

(declare-function acp-shutdown "acp")

;;;###autoload
(defun kdb-claude-reap-agents ()
  "Stop agent processes no session owns any more; return how many.
Each holds 300-450 MB.  An agent is orphaned when its session buffer is
gone, or the session has since started another agent, without the old
one being shut down: 6 piled up over 11 days of one Emacs.  Clients of
a live buffer that isn't a session (agent-review) are left alone.  The
buffer each was started from is logged, to find what leaves them."
  (interactive)
  (let ((reaped 0))
    (setq kdb-claude--clients
          (seq-filter
           (lambda (client)
             (let ((proc (map-elt client :process))
                   (buf (map-elt client :context-buffer)))
               (cond ((not (process-live-p proc)) nil)        ; done: forget it
                     ((or (not (buffer-live-p buf))
                          (and (eq (buffer-local-value 'major-mode buf) 'agent-shell-mode)
                               (not (eq (map-elt (buffer-local-value 'agent-shell--state buf) :client)
                                        client))))
                      (message "Claude: stopping an orphaned agent (pid %s, was %s)"
                               (process-id proc) (if (buffer-live-p buf) (buffer-name buf) "a closed session"))
                      (acp-shutdown :client client)
                      (setq reaped (1+ reaped))
                      nil)
                     (t t))))
           kdb-claude--clients))
    (when (called-interactively-p 'any)
      (message "Stopped %d orphaned agent(s)" reaped))
    reaped))

(defvar kdb-claude-stall-minutes 15
  "Minutes without a word from a working agent before its session says so.
Claude's tool commands time out within 10, so longer is a hang.")

(defvar-local kdb-claude--stall-noted nil
  "The activity time a stall notice was given for, so it's given once.")

(declare-function agent-shell-last-activity-time "agent-shell")
(declare-function agent-shell--active-requests-p "agent-shell")

;;;###autoload
(defun kdb-claude-check-stalls ()
  "Tell sessions mid-turn whose agent has gone quiet that it may be stuck.
acp has no request timeout, so a hung connection leaves a session busy
forever, looking like it's still working.  Not one waiting on you."
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) 'agent-shell-mode)
      (with-current-buffer buf
        (when-let* (((agent-shell--active-requests-p (agent-shell--state)))
                    ((not (seq-some (lambda (e) (eq (nth 1 e) buf)) kdb-claude--permissions)))
                    (last (agent-shell-last-activity-time :shell-buffer buf))
                    ((> (float-time (time-subtract nil last)) (* 60 kdb-claude-stall-minutes)))
                    ((not (equal last kdb-claude--stall-noted))))
          (setq kdb-claude--stall-noted last)
          (agent-shell--update-fragment
           :state (agent-shell--state)
           :block-id (format "stalled-%s" (float-time last))
           :label-left (propertize "Agent quiet" 'font-lock-face 'warning)
           :body (format "No word from the agent for %d minutes; it may be stuck.\n\nC-c C-c interrupts the turn; M-x agent-shell-reload restarts the agent and resumes this conversation."
                         kdb-claude-stall-minutes)
           :create-new t :expanded t :above-last-prompt t)
          (kdb-claude--notify buf "Claude agent quiet" (buffer-name buf)))))))

(defun kdb-claude--agent-died (client proc event)
  "Tell CLIENT's session that PROC ended with EVENT, unless it was shut down.
`acp-shutdown' forgets the process before stopping it, so a process the
client still holds ended on its own."
  (when-let* (((memq (process-status proc) '(exit signal)))
              ((eq (map-elt client :process) proc))
              (buf (map-elt client :context-buffer))
              ((buffer-live-p buf)))
    (let* ((stderr (get-buffer (format "acp-client-stderr(%s)-%s"
                                       (map-elt client :command)
                                       (map-elt client :instance-count))))
           (tail (and stderr (with-current-buffer stderr
                               (string-trim (buffer-substring-no-properties
                                             (max (point-min) (- (point-max) 1500))
                                             (point-max))))))
           (body (format "The agent process %s.%s\n\nM-x agent-shell-reload resumes this conversation.  The agent's log is in %s."
                         (string-trim event)
                         (if (string-empty-p (or tail "")) "" (concat "\n\n" tail))
                         kdb-claude-agent-logs)))
      (with-current-buffer buf
        (agent-shell--update-fragment
         :state (agent-shell--state)
         :block-id (format "agent-died-%s" (process-id proc))
         :label-left (propertize "Agent stopped" 'font-lock-face 'error)
         :body body :create-new t :expanded t :above-last-prompt t))
      (message "Claude: %s's agent %s" (buffer-name buf) (string-trim event))
      (kdb-claude--notify buf "Claude agent stopped" (buffer-name buf)))))

(defun kdb-claude--rename-in-tab-states (old new)
  "Rename buffer OLD to NEW in the saved layouts of tabs you aren't in.
They name their buffers, so a session renamed while its tab is hidden
can't be found there, and showing it again opens a second tab."
  (cl-labels ((walk (tree)
                (when (consp tree)
                  (when (and (eq (car tree) 'buffer) (equal (car-safe (cdr tree)) old))
                    (setcar (cdr tree) new))
                  (walk (car tree))
                  (walk (cdr tree)))))
    (dolist (frame (frame-list))
      (dolist (tab (funcall tab-bar-tabs-function frame))
        (unless (eq (car tab) 'current-tab)
          (walk (alist-get 'ws tab)))))))

;;;###autoload
(defun kdb-claude--rename-keeps-tab (rename &rest args)
  "Around `rename-buffer': a renamed session keeps working, and its tab.
shell-maker finds a session's buffer by the name it was given, so that
is updated too.  Else a new session taking the old name gets the
renamed one's output, and the renamed one freezes."
  (let ((old (buffer-name)))
    (prog1 (apply rename args)
      (when (and (derived-mode-p 'agent-shell-mode)
                 (not (equal old (buffer-name))))
        (setq-local shell-maker--buffer-name-override (buffer-name))
        (when (bound-and-true-p tab-bar-mode)
          (kdb-claude--rename-in-tab-states old (buffer-name)))))))

(defvar-local kdb-claude--auto-name nil
  "(BASE . NAME): the name `kdb-claude--name-session' last gave this buffer.")
(put 'kdb-claude--auto-name 'permanent-local t)

(defvar agent-shell-viewport--suffix)
(defvar shell-maker--buffer-name-override)

(defun kdb-claude--name-session (buf)
  "Name session BUF, and its tab, after the title Claude gave it.
The title is agent-shell's, kept current, so /rename renames them too.
Only a default \"Claude Agent @ …\" name or one given here is replaced,
never one you chose; the same for the tab."
  (with-current-buffer buf
    (when-let* ((title (map-nested-elt agent-shell--state '(:session :title)))
                ((not (string-blank-p title)))
                (base (truncate-string-to-width (string-trim title) 40 nil nil "…"))
                (old (buffer-name))
                ((or (string-prefix-p "Claude Agent @ " old)
                     (equal old (cdr kdb-claude--auto-name))))
                ((not (equal base (car kdb-claude--auto-name)))))
      (rename-buffer (generate-new-buffer-name base))
      (setq kdb-claude--auto-name (cons base (buffer-name)))
      (when-let* ((kdb-claude--tab-auto)
                  ((bound-and-true-p tab-bar-mode))
                  (tab (tab-bar-get-buffer-tab buf))
                  ((equal (alist-get 'name tab) kdb-claude--tab-auto)))
        (tab-bar-rename-tab base (1+ (alist-get 'index tab)))
        (setq kdb-claude--tab-auto base))
      ;; Its viewport is found by the shell's name, so it follows.
      (when-let* (((boundp 'agent-shell-viewport--suffix))
                  (viewport (get-buffer (concat old agent-shell-viewport--suffix))))
        (with-current-buffer viewport
          (rename-buffer (concat (buffer-name buf) agent-shell-viewport--suffix) t))))))

;;;###autoload
(defun kdb-claude--track-artifacts ()
  "Note the files this shell's session writes; show documents as they land."
  (let ((buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buf :event 'tool-call-update
     :on-event (lambda (event)
                 (let ((data (map-elt event :data)))
                   (kdb-claude--note-artifact buf (map-elt data :tool-call-id)
                                              (map-elt data :tool-call)))))))

;;;###autoload
(defun kdb-reveal-in-file-manager (file)
  "Open FILE's folder in Explorer, Finder or the desktop's file manager.
FILE is selected there; a directory is opened itself.  Interactively,
this buffer's file, the Dired file at point, or the directory.  The
path goes on the kill ring, to paste in an attach dialog."
  (interactive (list (or buffer-file-name
                         (and (derived-mode-p 'dired-mode) (dired-get-filename nil t))
                         default-directory)))
  (let* ((file (expand-file-name file))
         (dir (file-directory-p file)))
    (pcase system-type
      ('windows-nt (w32-shell-execute "open" "explorer.exe"
                                      (format (if dir "\"%s\"" "/select,\"%s\"")
                                              (subst-char-in-string ?/ ?\\ (directory-file-name file)))))
      ('darwin (apply #'call-process "open" nil 0 nil (if dir (list file) (list "-R" file))))
      (_ (unless (ignore-errors
                   (require 'dbus)
                   (dbus-call-method :session "org.freedesktop.FileManager1" "/org/freedesktop/FileManager1"
                                     "org.freedesktop.FileManager1" (if dir "ShowFolders" "ShowItems")
                                     (list (concat "file://" file)) "")
                   t)
           (call-process "xdg-open" nil 0 nil (if dir file (file-name-directory file))))))
    (kill-new file)
    (message "Shown in file manager; path copied: %s" (abbreviate-file-name file))))

;;;###autoload
(defun kdb-claude-open-artifact (&optional reveal)
  "Open a file this session created, newest first, in the artifact pane.
REVEAL (`kdb-claude-reveal-artifact'): pick one of their folders to open
in the file manager instead, to share from."
  (interactive)
  (let* ((buf (if (derived-mode-p 'agent-shell-mode)
                  (current-buffer)
                (or (seq-find (lambda (b) (buffer-local-value 'kdb-claude--artifacts b))
                              (buffer-list))
                    (user-error "No Claude session has written files yet"))))
         (files (seq-filter #'file-exists-p (buffer-local-value 'kdb-claude--artifacts buf)))
         (names (delete-dups (mapcar (if reveal
                                         (lambda (f) (abbreviate-file-name (file-name-directory f)))
                                       #'abbreviate-file-name)
                                     files))))
    (unless files (user-error "This session hasn't created any files yet"))
    (funcall (if reveal #'kdb-reveal-in-file-manager (lambda (f) (kdb-claude--show-artifact f t)))
             (expand-file-name
              (completing-read (if reveal "Open folder: " "File Claude created: ")
                               (lambda (s p a)
                                 (if (eq a 'metadata) '(metadata (display-sort-function . identity) (cycle-sort-function . identity))
                                   (complete-with-action a names s p)))
                               nil t)))))

;;;###autoload
(defun kdb-claude-reveal-artifact ()
  "Open a folder this session created files in, in Explorer, Finder, etc."
  (interactive)
  (kdb-claude-open-artifact t))

;;; ============================================================
;;; Running code blocks from replies
;;; ============================================================

(defun kdb-claude--code-blocks ()
  "Code blocks in the buffer, as (LANG BODY-BEG BODY-END).
In agent-shell, the code blocks it rendered (fences replaced by a
\"LANG ⧉\" label); stray fences in tool output or thinking don't count.
Elsewhere, raw fenced blocks."
  (let ((prop 'agent-shell-markdown-source-block-body)
        (pos (point-min))
        blocks)
    (while-let ((beg (text-property-any pos (point-max) prop t)))
      (let ((end (or (next-single-property-change beg prop) (point-max))))
        (unless (invisible-p beg)               ; collapsed sections
          (push (list (save-excursion
                        (goto-char beg)
                        (if (re-search-backward "^\\(\\S-+\\) ⧉" (line-beginning-position -3) t)
                            (let ((l (downcase (match-string-no-properties 1))))
                              (if (equal l "snippet") "" l))
                          ""))
                      beg end)
                blocks))
        (setq pos end)))
    (unless blocks
      (let (open)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*```\\([^`\n]*\\)$" nil t)
            (if open
                (progn (push (list (car open) (cdr open) (match-beginning 0)) blocks)
                       (setq open nil))
              (setq open (cons (downcase (or (car (split-string (match-string-no-properties 1))) ""))
                               (1+ (line-end-position)))))))))
    ;; Bare org src blocks outside any fence, as ("org" BEG END) so the
    ;; whole block, header included, is copied as-is.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src\\_>" nil t)
        (let ((beg (line-beginning-position)))
          (when (and (not (invisible-p beg))
                     (not (seq-find (lambda (b) (<= (nth 1 b) beg (nth 2 b))) blocks))
                     (re-search-forward "^[ \t]*#\\+end_src[ \t]*$" nil t))
            (push (list "org" beg (min (1+ (point)) (point-max))) blocks)))))
    (sort blocks (lambda (a b) (< (nth 1 a) (nth 1 b))))))

;; Markdown fence languages -> org babel languages, where they differ.
;; Shell blocks never get here: they run in a project shell.
(defconst kdb-claude--babel-langs '(("elisp" . "emacs-lisp")))

(declare-function org-escape-code-in-string "org-src")
(defvar explicit-shell-file-name)
(defvar shell-fontify-input-enable)
(defvar comint-buffer-maximum-size)
(declare-function comint-truncate-buffer "comint")
(declare-function comint-send-input "comint")
(declare-function comint-check-proc "comint")

(defun kdb-claude--block-shell-kind (lang)
  "The shell, bash or pwsh, that runs a block in LANG, or nil if none does.
Blocks naming no shell run in bash (Git Bash on Windows, which works
better in Emacs than PowerShell), or pwsh where there's no bash."
  (pcase lang
    ((or "powershell" "pwsh" "ps1" "ps") 'pwsh)
    ((or "bash" "sh" "zsh") 'bash)
    ((or "" "shell" "console") (if (kdb-claude--bash) 'bash 'pwsh))))

(defvar-local kdb-claude--shell-for nil
  "(KIND . DIR) of the project shell this buffer is.")
(put 'kdb-claude--shell-for 'permanent-local t)

(defun kdb-claude--bash ()
  "Bash's path, or nil.  Not Windows' System32\\bash.exe: that one runs WSL."
  (when-let* ((bash (executable-find "bash")))
    (unless (string-match-p "/system32/" (downcase (subst-char-in-string ?\\ ?/ bash)))
      bash)))

(defun kdb-claude--block-shell (kind dir)
  "DIR's shell of KIND, started if need be and shown in the bottom panel."
  (let ((buf (or (seq-find (lambda (b) (equal (buffer-local-value 'kdb-claude--shell-for b)
                                              (cons kind dir)))
                           (buffer-list))
                 (generate-new-buffer
                  (format "*claude %s: %s*" kind (file-name-nondirectory (directory-file-name dir)))))))
    (unless (comint-check-proc buf)
      (with-current-buffer buf
        (setq-local kdb-claude--shell-for (cons kind dir))
        (setq default-directory dir))
      (let ((explicit-shell-file-name
             (or (pcase kind
                   ('pwsh (or (executable-find "pwsh") (executable-find "powershell")))
                   ('bash (kdb-claude--bash)))
                 (user-error "No %s on this machine" kind))))
        ;; No hidden highlighting copy of every line a dev server logs.
        (let ((shell-fontify-input-enable nil))
          (save-window-excursion (shell buf)))
        ;; Throwaway shells: killing one, or quitting Emacs, shouldn't ask.
        (set-process-query-on-exit-flag (get-buffer-process buf) nil)
        (with-current-buffer buf
          (add-hook 'comint-output-filter-functions #'comint-osc-process-output nil t)
          ;; Dev servers log without end: keep only the recent lines.
          (setq-local comint-buffer-maximum-size 5000)
          (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t))
        (let ((proc (get-buffer-process buf))
              (end (+ (float-time) 5)))
          ;; Until the prompt ("$", or pwsh's "? ") ends the output.
          (while (and (process-live-p proc) (< (float-time) end)
                      (not (with-current-buffer buf
                             (save-excursion
                               (goto-char (process-mark proc))
                               (looking-back "[#$%>?] " (pos-bol))))))
            (accept-process-output proc 0.05)))))
    (display-buffer buf)
    buf))

(defun kdb-claude--block-to-shell (kind code dir run)
  "Put CODE at the prompt of DIR's KIND shell; with RUN, run it too.
Output streams in as it comes, Emacs stays usable, and the shell is kept
for the next block, so cd and variables carry over."
  (let* ((buf (kdb-claude--block-shell kind dir))
         (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (goto-char (point-max))
      (delete-region (process-mark proc) (point-max))
      (insert code)
      (when run (comint-send-input)))
    buf))

(defun kdb-claude--block-start (block)
  "Where BLOCK (LANG BODY-BEG BODY-END) starts, its label or fence included.
In agent-shell, its \"LANG ⧉\" label and the blank line under it;
elsewhere its opening fence.  Point there means this block, not the
one before it."
  (if (equal (car block) "org")
      (nth 1 block)                             ; already its #+begin_src line
    (save-excursion
      (goto-char (nth 1 block))
      (forward-line (if (get-text-property (point) 'agent-shell-markdown-source-block-body) -2 -1))
      (point))))

;;;###autoload
(defun kdb-claude-run-block (&optional run)
  "Take the code block at point, or the last one before it, to run it.
Shell blocks go to the prompt of this project's shell in the bottom panel
\(bash, Git Bash on Windows, or as the block names), to edit, then
RET.  Others are appended to *claude blocks* as an org src block, to edit,
then \\[org-ctrl-c-ctrl-c].  With RUN, run it right away and stay here."
  (interactive)
  (pcase-let* ((pos (point))
               (blocks (kdb-claude--code-blocks))
               (`(,lang ,beg ,end)
                (or (seq-find (lambda (b) (<= (kdb-claude--block-start b) pos (nth 2 b))) blocks)
                    (car (last (seq-filter (lambda (b) (<= (nth 2 b) pos)) blocks)))
                    (user-error "No code block at or before point")))
               (code (string-trim-right (buffer-substring-no-properties beg end)))
               (dir default-directory))
    ;; Emacs keystrokes ("M-x load-library RET foo RET"), not shell:
    ;; queue them as input, typed once this command is done.  Not
    ;; `execute-kbd-macro': under a macro, commands aren't interactive,
    ;; so e.g. M-x emacs-version echoes nothing.
    (when (and (member lang '("" "console" "shell" "sh" "bash" "text"))
               (string-match-p "\\`[ \t]*[CMs]-[^ \t\n]+\\(?:[ \t]+[^ \t\n]+\\)*[ \t]*\\'" code))
      (let ((keys (string-trim code)))
        (setq lang "elisp"
              code (format "(progn (setq unread-command-events\n             (append unread-command-events (listify-key-sequence (kbd %S))))\n       \"Typed: %s\")"
                           keys keys))))
    (pulse-momentary-highlight-region beg end)
    (if-let* ((kind (kdb-claude--block-shell-kind lang)))
        (let ((shell (kdb-claude--block-to-shell kind code dir run)))
          ;; Yours now: no session's cleanup closes it.
          (with-current-buffer shell (setq kdb-claude--users nil))
          (if run
              (message "Running in %s" (buffer-name shell))
            (select-window (get-buffer-window shell))
            (goto-char (point-max))
            (message "Edit if needed, then RET to run")))
      (kdb-claude--block-to-org lang code dir run))))

(defun kdb-claude--block-to-org (lang code dir run)
  "Append CODE in LANG to *claude blocks* to run in DIR; with RUN, run it.
Run, its output is echoed and focus stays put; else point is left in the
block to edit, then \\[org-ctrl-c-ctrl-c]."
  (let ((from (selected-window)))
    (require 'org)
    (pop-to-buffer (get-buffer-create "*claude blocks*"))
    (unless (derived-mode-p 'org-mode) (org-mode))
    (goto-char (point-max))
    (insert (if (bobp) "" "\n"))
    (if (and (equal lang "org") (string-match-p "\\`[ \t\n]*#\\+begin_src" code))
        ;; Already a complete src block: insert it as-is (adding :dir if
        ;; missing), don't nest it.
        (progn
          (save-excursion (insert (string-trim-left code) "\n"))
          (unless (string-match-p ":dir\\_>" (buffer-substring (point) (pos-eol)))
            (save-excursion
              (end-of-line)
              (insert (format " :dir %S" (abbreviate-file-name dir))))))
      (insert (let ((lang (alist-get lang kdb-claude--babel-langs lang nil #'equal)))
                (format "#+begin_src %s :dir %S%s\n" lang (abbreviate-file-name dir)
                        (if (equal lang "emacs-lisp") "" " :results output"))))
      (save-excursion
        (insert (org-escape-code-in-string code) "\n#+end_src\n")))
    (if (not run)
        (message "Edit if needed, then C-c C-c to run")
      (let (result)
        (unwind-protect (setq result (org-babel-execute-src-block))
          (select-window from))
        (message "%s" (if (and result (not (equal result "")))
                          (string-trim-right (format "%s" result))
                        "Ran it; no output (see *claude blocks*)"))))))

(defun kdb-claude--block-at-label (run)
  "Take the code block under this \"LANG ⧉\" label to run; RUN it now if non-nil."
  (let ((buf (current-buffer))
        (pos (point)))
    (goto-char (or (next-single-property-change (point) 'agent-shell-markdown-source-block-body)
                   (point)))
    (unwind-protect (kdb-claude-run-block run)
      (with-current-buffer buf (goto-char pos)))))

(defun kdb-claude-edit-block-at-label ()
  "Open the code block under this label to edit, then run.
Shell blocks wait at the project shell's prompt; others in *claude blocks*."
  (interactive)
  (kdb-claude--block-at-label nil))

(defun kdb-claude-run-block-at-label ()
  "Run the code block under this label; focus stays here.
Shell blocks run in the project's shell in the bottom panel; others in
*claude blocks*, their output echoed.  e on the label opens it to edit
first instead."
  (interactive)
  (kdb-claude--block-at-label t))

(declare-function agent-shell-markdown-replace-markup "agent-shell-markdown")
(declare-function org-babel-execute-src-block "ob-core")

;;;###autoload
(defun kdb-claude--render-markdown (&rest args)
  "Render like `agent-shell-markdown-replace-markup', taking its ARGS.
Then code block labels also run their block, through org babel: x runs
it, e opens it to edit first.  The label's hint names both next to
RET's copy."
  (apply #'agent-shell-markdown-replace-markup args)
  (kdb-claude--link-documents)
  ;; Labels carry their own keymap (RET copies) and a hint sensor; the
  ;; ⧉ in each is tagged.  Streaming re-renders, so skip labels done.
  (let ((pos (point-min)))
    (while (setq pos (text-property-any pos (point-max) 'agent-shell-markdown-source-block-copy t))
      (let ((map (get-text-property pos 'keymap))
            (start (or (previous-single-property-change (1+ pos) 'keymap) (point-min)))
            (end (or (next-single-property-change pos 'keymap) (point-max))))
        (when (and (keymapp map) (not (eq (lookup-key map "x") #'kdb-claude-run-block-at-label)))
          (define-key map "x" #'kdb-claude-run-block-at-label)
          (define-key map "e" #'kdb-claude-edit-block-at-label)
          (put-text-property start end 'cursor-sensor-functions
                             (list (lambda (_window _old action)
                                     (when (eq action 'entered)
                                       (message "Press RET to copy, x to run, e to edit first"))))))
        (setq pos end)))))

(defun kdb-claude--last-reply (jsonl)
  "Markdown of the last reply in Claude Code transcript JSONL.
That is the text of every assistant message after the last prompt;
tool calls and results are left out."
  ;; ponytail: reads the whole transcript; seek from the end if big ones lag
  (let (texts)
    (with-temp-buffer
      (insert-file-contents jsonl)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when-let* ((obj (ignore-errors
                           (json-parse-string line :object-type 'alist :array-type 'list
                                              :null-object nil :false-object nil)))
                    ;; Not api-request-blob etc., which repeat messages
                    ((member (alist-get 'type obj) '("user" "assistant")))
                    ((not (or (alist-get 'isMeta obj) (alist-get 'isSidechain obj))))
                    (msg (alist-get 'message obj))
                    (content (alist-get 'content msg)))
          (let ((text (if (stringp content)
                          (list content)
                        (delq nil (mapcar (lambda (b) (and (equal (alist-get 'type b) "text")
                                                           (alist-get 'text b)))
                                          content)))))
            (pcase (alist-get 'role msg)
              ;; A real prompt starts a new reply; tool results and
              ;; <system-reminder>-style injections don't
              ("user" (when (seq-some (lambda (s) (not (string-prefix-p "<" (string-trim-left s)))) text)
                        (setq texts nil)))
              ("assistant" (setq texts (append texts text))))))))
    (string-join (seq-remove #'string-empty-p (mapcar #'string-trim texts)) "\n\n")))

(declare-function kdb/md-to-org-buffer "init-org")
(declare-function agent-shell-session-id "agent-shell")

;;;###autoload
(defun kdb-claude-reply-to-org ()
  "Open this session's last reply as org, from its raw markdown.
\\[org-export-dispatch] there exports it to HTML, markdown, ODT..."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this in a Claude session buffer"))
  (let* ((id (or (agent-shell-session-id) (user-error "No session yet")))
         (jsonl (or (car (file-expand-wildcards
                          (expand-file-name (format "*/%s.jsonl" id) (kdb-claude--projects-dir))))
                    (user-error "No transcript for session %s" id)))
         (md (kdb-claude--last-reply jsonl)))
    (when (string-empty-p md) (user-error "No reply yet"))
    (require 'org)                      ; loads init-org
    (kdb/md-to-org-buffer md (format "*claude reply: %s*" (buffer-name)))
    (message (substitute-command-keys "\\<org-mode-map>\\[org-export-dispatch] to export"))))

;;;###autoload
(defun kdb-claude-copy-session-id ()
  "Copy this session's Claude id, for claude --resume and the like."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this in a Claude session buffer"))
  (let ((id (or (agent-shell-session-id) (user-error "No session yet"))))
    (kill-new id)
    (message "Copied session id %s" id)
    id))

(defvar-local kdb-claude--remote-control nil
  "Non-nil while this session is shared by `kdb-claude-remote-control'.")

;;;###autoload
(defun kdb-claude-remote-control ()
  "Share this session live with claude.ai/code and the Claude apps, or stop.
Claude replies with its link; both sides can then send.  Run again to
stop sharing.  claude --desktop --resume can't take a session open
here, so this is how the apps see one."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this in a Claude session buffer"))
  (setq kdb-claude--remote-control (not kdb-claude--remote-control))
  (kdb-claude--queue-prompt (current-buffer)
                            (if kdb-claude--remote-control "/remote-control" "/remote-control off"))
  (message (if kdb-claude--remote-control
               "Sharing this session; its link comes in the reply"
             "Stopping sharing")))

;;;###autoload
(defun kdb-claude-usage ()
  "Show token usage and cost for recent weeks and months, via ccusage.
It reads the local transcripts, so it covers this machine only."
  (interactive)
  (unless (executable-find "ccusage")
    (user-error "ccusage not found; install it with: npm i -g ccusage"))
  ;; A pipe, not a pty, so the tables come out as plain text.  Through
  ;; the shell, so Windows finds npm's ccusage.cmd.
  (let ((process-connection-type nil))
    (with-current-buffer
        (compilation-start "ccusage weekly --last 4 --no-color && ccusage monthly --last 3 --no-color"
                           nil (lambda (_) "*claude usage*"))
      (setq truncate-lines t))))

;;; ============================================================
;;; Sessions talking to sessions
;;; ============================================================

(defvar agent-shell-anthropic-claude-environment)
(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--prompt-queue-enqueue "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-process-next "agent-shell-prompt-queue")

(defvar-local kdb-claude--id nil
  "This session's id, also in its environment as KDB_CLAUDE_SESSION.")
(put 'kdb-claude--id 'permanent-local t)

(defvar kdb-claude--next-id nil
  "Id for the session being started, when the caller picked it.")

(defun kdb-claude--session-id (buf)
  "BUF's session id, assigned on first use."
  (with-current-buffer buf
    (or kdb-claude--id
        (setq kdb-claude--id
              (or kdb-claude--next-id
                  (format "%s-%04x"
                          (file-name-nondirectory (directory-file-name default-directory))
                          (random #x10000)))))))

(defvar-local kdb-claude--provider-env nil
  "Environment pointing this session's Claude Code at another provider.")
(put 'kdb-claude--provider-env 'permanent-local t)

(defvar kdb-claude--next-env nil
  "Provider environment for the session being started.")

;;;###autoload
(defun kdb-claude--tag-client (make-client &rest args)
  "Around MAKE-CLIENT: give the session's process KDB_CLAUDE_SESSION, and
the environment of the model provider it was started with, if any.
ARGS has the session buffer under :buffer."
  (let* ((buf (plist-get args :buffer))
         (agent-shell-anthropic-claude-environment
          (append (list (concat "KDB_CLAUDE_SESSION=" (kdb-claude--session-id buf)))
                  (with-current-buffer buf
                    (or kdb-claude--provider-env
                        (setq kdb-claude--provider-env kdb-claude--next-env)))
                  agent-shell-anthropic-claude-environment)))
    (apply make-client args)))

;;; ============================================================
;;; Other models: Claude Code against any Anthropic-compatible API
;;; ============================================================

(defvar kdb-claude-model-providers
  '(("OpenRouter (fastest host per model)"
     :base "https://openrouter.ai/api" :key "OPENROUTER_API_KEY" :host "openrouter.ai"
     :models ("moonshotai/kimi-k2.6:nitro" "z-ai/glm-4.6:nitro"
              "qwen/qwen3-coder:nitro" "openai/gpt-oss-120b:nitro"))
    ("GLM (Z.ai)"
     :base "https://api.z.ai/api/anthropic" :key "ZAI_API_KEY" :host "api.z.ai"
     :models ("glm-4.6" "glm-5.1"))
    ("Kimi (Moonshot)"
     :base "https://api.moonshot.ai/anthropic" :key "MOONSHOT_API_KEY" :host "api.moonshot.ai"
     :models ("kimi-k2.6" "kimi-k3"))
    ("DeepSeek"
     :base "https://api.deepseek.com/anthropic" :key "DEEPSEEK_API_KEY" :host "api.deepseek.com"
     :models ("deepseek-chat" "deepseek-v4-pro"))
    ("Ollama (local)" :base "http://localhost:11434" :token "ollama" :models ollama))
  "Providers with an Anthropic-compatible API, for `kdb-claude-start-model'.
Each: NAME :base URL, then :token, or :key (an environment variable)
and :host (for ~/.authinfo.gpg: machine HOST password KEY); :models
lists suggestions (any name can be typed), or `ollama' for the
models pulled locally.  Model names change often; check the provider.")

(defun kdb-claude--provider-environment (base token model)
  "Environment running Claude Code on MODEL at BASE with TOKEN."
  (append (list (concat "ANTHROPIC_BASE_URL=" base)
                (concat "ANTHROPIC_AUTH_TOKEN=" token)
                (concat "ANTHROPIC_MODEL=" model)
                (concat "ANTHROPIC_SMALL_FAST_MODEL=" model)
                "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1")
          (mapcar (lambda (tier) (format "ANTHROPIC_DEFAULT_%s_MODEL=%s" tier model))
                  '("OPUS" "SONNET" "HAIKU"))))

;;;###autoload
(defun kdb-claude-start-model (provider model &optional dir)
  "Start a Claude Code session running on MODEL from PROVIDER, in DIR.
Same tools, prompts and Emacs integration as any session; only the
model differs.  Interactively, DIR is the project root; with a prefix
argument, or outside a project, pick it."
  (interactive
   (let* ((provider (completing-read "Provider: " kdb-claude-model-providers nil t))
          (models (plist-get (cdr (assoc provider kdb-claude-model-providers)) :models))
          (models (if (eq models 'ollama)
                      (or (mapcar (lambda (l) (car (split-string l)))
                                  (cdr (ignore-errors (process-lines "ollama" "list"))))
                          (user-error "No local models; try: ollama pull qwen3:4b"))
                    models)))
     (list provider
           (completing-read "Model: " models nil nil nil nil (car models))
           (when (or current-prefix-arg (not (project-current)))
             (kdb-claude-read-directory "Project: ")))))
  (let* ((spec (cdr (or (assoc provider kdb-claude-model-providers)
                        (user-error "Unknown provider %s" provider))))
         (token (or (plist-get spec :token)
                    (getenv (plist-get spec :key))
                    (progn (require 'auth-source)
                           (auth-source-pick-first-password :host (plist-get spec :host)))
                    (user-error "Set %s, or add to ~/.authinfo.gpg: machine %s password YOUR-KEY"
                                (plist-get spec :key) (plist-get spec :host))))
         (dir (or dir (if (project-current)
                          (project-root (project-current))
                        default-directory)))
         (kdb-claude--next-env (kdb-claude--provider-environment (plist-get spec :base) token model)))
    (prog1 (kdb-claude--start-with (file-name-as-directory (expand-file-name dir)))
      (message "Claude Code on %s (%s)" model provider))))

(defun kdb-claude--session-buffer (id)
  "The live Claude session with ID (or buffer name ID), or nil."
  (or (seq-find (lambda (b) (equal (buffer-local-value 'kdb-claude--id b) id))
                (buffer-list))
      (when-let* ((b (get-buffer id)))
        (and (buffer-local-value 'kdb-claude--id b) b))))

(defun kdb-claude--queue-prompt (buf text)
  "Queue TEXT in session BUF; agent-shell submits it when the turn is over.
Its queue keeps the user's own queued prompts in order, sets aside an
unsent draft, and asks before continuing after a cancelled turn."
  (with-current-buffer buf
    (agent-shell--prompt-queue-enqueue :prompt text)
    (unless (shell-maker-busy)
      (agent-shell--prompt-queue-process-next))))

;;;###autoload
(defun kdb-claude-message (to text &optional from)
  "Send TEXT to the Claude session TO (an id or buffer name), as from FROM.
Delivered when TO's current turn is over.  Meant for `emacsclient --eval'."
  (let ((buf (or (kdb-claude--session-buffer to)
                 (user-error "No Claude session %s" to))))
    (kdb-claude--queue-prompt
     buf (if from (format "[Message from Claude session %s]\n%s" from text) text))
    (format "Queued for %s" to)))

;;; What a session shows and runs, kept in its own tab

(defvar-local kdb-claude--users nil
  "Ids of the sessions using this buffer, which one of them opened.
Nil when it isn't Claude's to close: you opened it, or took it over.")
(put 'kdb-claude--users 'permanent-local t)

(defun kdb-claude--use (b id &optional opened)
  "Note that session ID uses buffer B; OPENED if ID opened it.
Only buffers a session opened are closed, and only once none uses them."
  (with-current-buffer b
    (when (or opened kdb-claude--users)
      (cl-pushnew id kdb-claude--users :test #'equal))))

(defun kdb-claude--in-session-tab (id fn &optional no-show)
  "Call FN with session ID's buffer, its window selected, in its tab.
Then come back to the tab and window you were in, so nothing moves
under you.  A session not shown anywhere is put in its project's tab
first, unless NO-SHOW.  Return FN's value."
  (let* ((buf (or (kdb-claude--session-buffer id) (user-error "No Claude session %s" id)))
         (from-tab (and (bound-and-true-p tab-bar-mode) (tab-bar--current-tab-index)))
         (from-win (selected-window)))
    (unless (or no-show (get-buffer-window buf t))
      (kdb-claude--show-in-background buf))
    (unwind-protect
        (progn
          (when-let* ((tab (and from-tab (tab-bar-get-buffer-tab buf))))
            (tab-bar-select-tab (1+ (alist-get 'index tab))))
          (if-let* ((win (get-buffer-window buf)))
              (with-selected-window win (funcall fn buf))
            (funcall fn buf)))
      (when (and from-tab (not (eql from-tab (tab-bar--current-tab-index))))
        (tab-bar-select-tab (1+ from-tab)))
      (when (window-live-p from-win) (select-window from-win)))))

(defun kdb-claude--session-window (buf id)
  "The window beside session BUF where session ID shows things.
The one it used before in this tab, else a new split, marked as its own."
  (or (seq-find (lambda (w) (equal (window-parameter w 'kdb-claude-session) id)) (window-list))
      (let* ((beside (or (get-buffer-window buf) (selected-window)))
             (win (or (ignore-errors
                        (split-window beside nil (if (>= (window-total-width beside) 160) 'right 'below)))
                      (user-error "No room in the session's tab to show it"))))
        (set-window-parameter win 'kdb-claude-session id)
        win)))

;;;###autoload
(defun kdb-claude-show (id what &optional line)
  "Show WHAT, a file or a buffer name, beside session ID in its tab, at LINE.
Your tab, window and cursor stay where they are.  Meant for
`emacsclient --eval'; `kdb-claude-cleanup' closes what this opened."
  (kdb-claude--in-session-tab
   id (lambda (buf)
        (let* ((shown (or (get-buffer what)
                          (let ((file (expand-file-name what (buffer-local-value 'default-directory buf))))
                            (unless (file-exists-p file) (user-error "No file or buffer %s" what))
                            (or (get-file-buffer file)
                                (let ((new (find-file-noselect file)))
                                  (kdb-claude--use new id t)
                                  new)))))
               (win (kdb-claude--session-window buf id)))
          (kdb-claude--use shown id)
          (set-window-buffer win shown)
          (when line
            (with-selected-window win
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter)))
          (format "Showing %s beside session %s" (buffer-name shown) id)))))

;;;###autoload
(defun kdb-claude-run (id command &optional dir shell)
  "Run COMMAND in the project shell of session ID, shown in its tab.
DIR defaults to the session's directory.  SHELL is \"bash\" or \"pwsh\";
by default bash (Git Bash on Windows, as Claude's own Bash tool uses),
else pwsh.  The shell is kept, so cd and variables carry over.  Return
the shell's buffer name, for `kdb-claude-output'.  Meant for
`emacsclient --eval'."
  (kdb-claude--in-session-tab
   id (lambda (buf)
        (let* ((dir (file-name-as-directory
                     (expand-file-name (or dir (buffer-local-value 'default-directory buf)))))
               (kind (pcase shell
                       ("pwsh" 'pwsh)
                       ("bash" 'bash)
                       (_ (if (kdb-claude--bash) 'bash 'pwsh))))
               (had (seq-find (lambda (b) (and (equal (buffer-local-value 'kdb-claude--shell-for b)
                                                      (cons kind dir))
                                               (get-buffer-process b)))
                              (buffer-list)))
               (shell (kdb-claude--block-to-shell kind command dir t)))
          (kdb-claude--use shell id (not had))
          (buffer-name shell)))))

;;;###autoload
(defun kdb-claude-output (buffer &optional lines)
  "The last LINES (default 40) lines of BUFFER, such as a `kdb-claude-run' shell."
  (with-current-buffer (or (get-buffer buffer) (user-error "No buffer %s" buffer))
    (save-excursion
      (goto-char (point-max))
      (forward-line (- (or lines 40)))
      (buffer-substring-no-properties (point) (point-max)))))

;;; What a session's commands are still running

(defvar kdb-claude-permanent-processes
  "mcp\\|language-server\\|langserver\\|lsp\\|csharp-ls\\|omnisharp\\|pyright\\|gopls\\|rust-analyzer\\|clangd\\|tsserver\\|conhost\\|chrome\\|chromium\\|msedge\\|firefox\\|playwright"
  "Regexp for what a session keeps running throughout, not work:
MCP servers, language servers, browsers, Windows' console hosts.")

(defun kdb-claude--process-table ()
  "PID -> attributes of every process."
  (let ((table (make-hash-table)))
    (dolist (pid (list-system-processes))
      (when-let* ((attrs (ignore-errors (process-attributes pid))))
        (puthash pid attrs table)))
    table))

(defvar kdb-claude--command-lines (make-hash-table)
  "PID -> (START . LINE): command lines read by PowerShell (Windows).
LINE is nil when it couldn't be read; START tells a reused PID apart.")

(defvar kdb-claude--command-lines-reading nil
  "The PowerShell process reading command lines, while it runs.")

(defun kdb-claude--args (pid attrs)
  "PID's command line: a string, `unreadable', or nil if not known yet.
Windows doesn't give Emacs command lines; those come from
`kdb-claude--read-command-lines'."
  (or (alist-get 'args attrs)
      (when-let* ((known (gethash pid kdb-claude--command-lines))
                  ((equal (car known) (alist-get 'start attrs))))
        (or (cdr known) 'unreadable))))

(defun kdb-claude--command-lines-query (pids)
  "PowerShell printing PID<TAB>command line for each of PIDS."
  (format "Get-CimInstance Win32_Process -Filter '%s' | %% { \"$($_.ProcessId)`t$($_.CommandLine)\" }"
          (mapconcat (lambda (p) (format "ProcessId=%d" p)) pids " or ")))

(defun kdb-claude--store-command-lines (pids output table)
  "Keep the command lines PowerShell printed in OUTPUT for PIDS.
PIDS it didn't print are kept as unreadable, so they aren't asked again."
  (let ((read (make-hash-table)))
    (dolist (line (split-string (or output "") "\n" t "\r"))
      (when (string-match "\\`\\([0-9]+\\)\t\\(.*\\)" line)
        (puthash (string-to-number (match-string 1 line)) (match-string 2 line) read)))
    (dolist (pid pids)
      (puthash pid (cons (alist-get 'start (gethash pid table))
                         (let ((l (gethash pid read))) (and l (not (string-empty-p l)) l)))
               kdb-claude--command-lines))))

(defun kdb-claude--read-command-lines (pids table &optional wait)
  "Read the command lines of PIDS (from process TABLE) with PowerShell.
In the background, so Emacs never stops for it; with WAIT, now."
  (when (and pids (eq system-type 'windows-nt))
    (if wait
        (kdb-claude--store-command-lines
         pids (ignore-errors
                (with-output-to-string
                  (call-process "powershell" nil standard-output nil
                                "-NoProfile" "-NonInteractive" "-Command"
                                (kdb-claude--command-lines-query pids))))
         table)
      (unless (process-live-p kdb-claude--command-lines-reading)
        (let ((out (generate-new-buffer " *claude command lines*")))
          (setq kdb-claude--command-lines-reading
                (ignore-errors
                  (make-process
                   :name "claude-command-lines" :buffer out :noquery t
                   :command (list "powershell" "-NoProfile" "-NonInteractive" "-Command"
                                  (kdb-claude--command-lines-query pids))
                   :sentinel (lambda (proc _)
                               (unless (process-live-p proc)
                                 (kdb-claude--store-command-lines
                                  pids (with-current-buffer out (buffer-string)) table)
                                 (kill-buffer out)))))))))))

(defun kdb-claude--session-work (buf &optional table wait)
  "What session BUF's commands are running, as (PID . ATTRIBUTES).
Everything under its Claude Code, except what it keeps throughout (see
`kdb-claude-permanent-processes').  On Windows, Claude Code sits under
cmd.exe and node.exe, and command lines are read by PowerShell, in the
background unless WAIT: until one is read, that process isn't counted.
One that can't be read is kept if Claude Code started it in its first 20
seconds, when it starts its MCP servers."
  (when-let* ((proc (map-nested-elt (buffer-local-value 'agent-shell--state buf) '(:client :process)))
              ((process-live-p proc))
              (root (process-id proc)))
    (let ((table (or table (kdb-claude--process-table)))
          (children (make-hash-table)))
      (maphash (lambda (pid attrs) (push pid (gethash (alist-get 'ppid attrs) children))) table)
      (cl-labels ((kept (pid attrs)
                    (let ((args (kdb-claude--args pid attrs)))
                      (string-match-p kdb-claude-permanent-processes
                                      (concat (alist-get 'comm attrs) " " (and (stringp args) args)))))
                  ;; Claude Code itself, however deep: on Windows cmd > node > claude.exe.
                  (claudes (pid)
                    (if (string-prefix-p "claude" (or (alist-get 'comm (gethash pid table)) ""))
                        (list pid)
                      (mapcan #'claudes (gethash pid children))))
                  (tops ()
                    (mapcan (lambda (claude)
                              (mapcar (lambda (top) (list top (alist-get 'start (gethash claude table))))
                                      (gethash claude children)))
                            (or (mapcan #'claudes (gethash root children)) (gethash root children)))))
        (let ((unknown (seq-filter (lambda (top) (null (kdb-claude--args (car top) (gethash (car top) table))))
                                   (tops))))
          (when unknown
            (kdb-claude--read-command-lines (mapcar #'car unknown) table wait)))
        (let (work)
          (cl-labels ((add (pid)
                        (let ((attrs (gethash pid table)))
                          (unless (kept pid attrs)
                            (push (cons pid attrs) work)
                            (mapc #'add (gethash pid children))))))
            (pcase-dolist (`(,top ,born) (tops))
              (let* ((attrs (gethash top table))
                     (args (kdb-claude--args top attrs))
                     (start (alist-get 'start attrs)))
                (unless (or (null args)          ; not read yet: not counted
                            (and (eq args 'unreadable) born start
                                 (< (float-time (time-subtract start born)) 20)))
                  (add top)))))
          (sort work (lambda (a b) (< (car a) (car b)))))))))

(defun kdb-claude--stop-process (pid)
  "Stop process PID and what it started.
On Windows, `taskkill /T /F': `signal-process' doesn't stop a console
program there, and children outlive their parent."
  (if (eq system-type 'windows-nt)
      (call-process "taskkill" nil nil nil "/PID" (number-to-string pid) "/T" "/F")
    (let ((table (kdb-claude--process-table)))
      (cl-labels ((below (p) (let (kids)
                               (maphash (lambda (k a) (when (eql (alist-get 'ppid a) p) (push k kids))) table)
                               (mapcan (lambda (k) (append (below k) (list k))) kids))))
        (dolist (p (append (below pid) (list pid)))
          (ignore-errors (signal-process p 'term)))))))

(defvar kdb-claude--background nil
  "(TIME . SESSIONS): sessions found running work while not busy, and when.")

(defun kdb-claude--working-in-background ()
  "Sessions not answering but still running something they started.
A background command outlives its turn, so agent-shell calls the session
ready.  From the last `kdb-claude--scan-background', so drawing the tab
bar never scans processes itself."
  (seq-filter #'buffer-live-p (cdr kdb-claude--background)))

(defun kdb-claude--scan-background ()
  "Find the sessions working in the background, at most every 10 seconds.
It reads every process, so only on the tab bar's own timer, and only
when some session is ready."
  (unless (and kdb-claude--background
               (< (float-time (time-subtract nil (car kdb-claude--background))) 10))
    (let* ((idle (seq-remove (lambda (b) (and (boundp 'agent-shell-attention--busy)
                                              (gethash b agent-shell-attention--busy)))
                             (and (fboundp 'agent-shell-buffers) (agent-shell-buffers))))
           (table (and idle (kdb-claude--process-table))))
      (setq kdb-claude--background
            (cons (current-time) (seq-filter (lambda (b) (kdb-claude--session-work b table)) idle))))))

(defun kdb-claude--task-outputs (buf)
  "Output files of session BUF's background tasks, newest first."
  (when-let* ((id (agent-shell-session-id :shell-buffer buf)))
    (sort (file-expand-wildcards
           (expand-file-name (format "claude*/*/%s/tasks/*.output" id) temporary-file-directory))
          #'file-newer-than-file-p)))

(defun kdb-claude--elapsed (attrs)
  "How long the process with ATTRS has run, as 1h05m or 42s."
  (if-let* ((start (alist-get 'start attrs)))
      (let ((s (truncate (float-time (time-subtract nil start)))))
        (cond ((>= s 3600) (format "%dh%02dm" (/ s 3600) (/ (% s 3600) 60)))
              ((>= s 60) (format "%dm%02ds" (/ s 60) (% s 60)))
              (t (format "%ds" s))))
    "?"))

(defvar-keymap kdb-claude-processes-mode-map
  :parent special-mode-map
  "g" #'kdb-claude-processes
  "k" #'kdb-claude-processes-kill
  "RET" #'kdb-claude-processes-visit)

(define-derived-mode kdb-claude-processes-mode special-mode "Claude-Processes"
  "What each Claude session is running, and its background output.
\\{kdb-claude-processes-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<kdb-claude-processes-mode-map>\\[kdb-claude-processes-visit] go to session / open output  \\[kdb-claude-processes-kill] stop process  \\[kdb-claude-processes] refresh")))

;;;###autoload
(defun kdb-claude-processes ()
  "Show what each Claude session's commands are running, and their output.
Busy or not: a background command (tests, a server) outlives its turn.
MCP servers, language servers and browsers a session keeps are left out."
  (interactive)
  (require 'agent-shell)
  (let* ((table (kdb-claude--process-table))
         (sessions (mapcar (lambda (b) (cons b (kdb-claude--session-work b table t)))
                           (agent-shell-buffers))))
    ;; Windows: the command lines of what's shown, read now.
    (kdb-claude--read-command-lines
     (seq-remove (lambda (pid) (kdb-claude--args pid (gethash pid table)))
                 (mapcar #'car (mapcan (lambda (s) (copy-sequence (cdr s))) sessions)))
     table t)
    (with-current-buffer (get-buffer-create "*Claude processes*")
      (let ((inhibit-read-only t) (pos (point)))
        (erase-buffer)
        (kdb-claude-processes-mode)
        (dolist (s sessions)
          (let* ((buf (car s)) (work (cdr s))
                 (state (or (cdr (assq buf (kdb-claude--waiting)))
                            (if work 'background 'ready))))
            (insert (propertize (buffer-name buf) 'face 'bold 'kdb-session buf)
                    (propertize (format "  %s\n" state) 'face 'shadow 'kdb-session buf))
            (if (null work)
                (insert (propertize "  nothing running\n" 'face 'shadow))
              (dolist (w work)
                (let ((attrs (cdr w)))
                  (insert (propertize
                           (format "  %7d  %8s  %s\n" (car w) (kdb-claude--elapsed attrs)
                                   (truncate-string-to-width
                                    (string-trim (let ((args (kdb-claude--args (car w) attrs)))
                                                   (if (stringp args) args
                                                     (or (alist-get 'comm attrs) ""))))
                                    (max 20 (- (window-width) 22)) nil nil "…"))
                           'kdb-pid (car w) 'kdb-session buf)))))
            (dolist (file (seq-take (kdb-claude--task-outputs buf) 3))
              (insert (propertize (format "  output: %s, %s ago\n" (file-name-nondirectory file)
                                          (kdb-claude--elapsed
                                           (list (cons 'start (file-attribute-modification-time
                                                               (file-attributes file))))))
                                  'face 'shadow 'kdb-file file))
              (let ((tail (with-temp-buffer
                            (insert-file-contents file)
                            (goto-char (point-max))
                            (forward-line -4)
                            (string-trim-right (buffer-substring-no-properties (point) (point-max))))))
                (unless (string-empty-p tail)
                  (insert (propertize (replace-regexp-in-string "^" "    " tail) 'kdb-file file) "\n"))))
            (insert "\n")))
        (when (null sessions) (insert "No Claude sessions.\n"))
        (goto-char (min pos (point-max))))
      (pop-to-buffer (current-buffer)))))

(defun kdb-claude-processes-kill ()
  "Stop the process on this line (asks first)."
  (interactive)
  (let ((pid (or (get-text-property (point) 'kdb-pid) (user-error "No process on this line"))))
    (when (y-or-n-p (format "Stop process %d and what it started? " pid))
      (kdb-claude--stop-process pid)
      (sit-for 0.5)
      (kdb-claude-processes))))

(defun kdb-claude-processes-visit ()
  "Open the background output on this line, or go to its session."
  (interactive)
  (cond ((get-text-property (point) 'kdb-file)
         (view-file-other-window (get-text-property (point) 'kdb-file)))
        ((get-text-property (point) 'kdb-session)
         (kdb-claude--goto-session (get-text-property (point) 'kdb-session)))
        (t (user-error "Nothing here"))))

;;;###autoload
(defun kdb-claude-cleanup (id)
  "Close what session ID opened: its windows, and buffers it opened or started.
A buffer another session still uses stays, as do unsaved changes; those
become yours.  Also run when the session ends."
  (let* ((buf (or (kdb-claude--session-buffer id) (user-error "No Claude session %s" id)))
         (gone nil) (kept nil) (windows 0))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (member id kdb-claude--users)
          (setq kdb-claude--users
                (seq-filter (lambda (u) (and (not (equal u id)) (kdb-claude--session-buffer u)))
                            kdb-claude--users))
          (unless kdb-claude--users              ; no other session uses it
            (if (and buffer-file-name (buffer-modified-p))
                (push b kept)
              (push b gone))))))
    ;; Windows first, in the session's tab: a buffer killed while its tab
    ;; is hidden leaves tab-bar's "Old buffer" placeholder behind.
    (kdb-claude--in-session-tab
     id (lambda (_)
          (dolist (w (window-list))
            (when (and (or (equal (window-parameter w 'kdb-claude-session) id)
                           (memq (window-buffer w) gone))
                       (ignore-errors (delete-window w) t))
              (cl-incf windows))))
     t)
    (let ((kill-buffer-query-functions nil))
      (mapc #'kill-buffer gone))
    ;; Said where you can see it: windows closing unannounced look like
    ;; something you did.
    (when (and (or (> windows 0) gone) (not (active-minibuffer-window)))
      (message "%s closed %d window(s), %d buffer(s) it had opened"
               (buffer-name buf) windows (length gone)))
    (format "Closed %d window(s), %d buffer(s)%s" windows (length gone)
            (if kept (format "; kept unsaved %s" (mapconcat #'buffer-name kept ", ")) ""))))

(defun kdb-claude--report-back (from id)
  "Instructions for session ID to report back to session FROM."
  (format "

---
You were started by Claude session %s; your session id is %s.  When you
finish, or need a decision, report back by running:

\"${EMACSCLIENT:-emacsclient}\" --eval \"$(cat <<'EOF'
(kdb-claude-message \"%s\" \"SUMMARY\" \"%s\")
EOF
)\"" from id from id))

;;;###autoload
(defun kdb-claude-spawn (dir prompt &optional from branch)
  "Start a Claude session in DIR and submit PROMPT; return its id.
With BRANCH, work in that branch's worktree (created if needed).  With
FROM, the id of the calling session, PROMPT ends with how to report back."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (dir (if branch (kdb-claude--worktree dir branch) dir))
         (id (format "%s-%04x" (file-name-nondirectory (directory-file-name dir))
                     (random #x10000)))
         (buf (let ((kdb-claude--next-id id))
                (kdb-claude--start-with
                 dir (if from (concat prompt (kdb-claude--report-back from id)) prompt)
                 nil 'background))))
    (kdb-claude--session-id buf)))

;;;###autoload
(defun kdb-claude-schedule (time dir prompt)
  "At TIME, start a Claude session in DIR and submit PROMPT.
TIME is anything `run-at-time' takes: \"11:30pm\", \"2 hours\", \"30 min\".
A clock time already past today means tomorrow.  Only runs while Emacs
does; for that, see Windows Task Scheduler."
  (interactive (list (read-string "When (e.g. 11:30pm, 2 hours): ")
                     (kdb-claude-read-directory "Project: ")
                     (read-string "Prompt: ")))
  (when (string-blank-p time)
    (user-error "No time given"))
  (let ((timer (run-at-time time nil #'kdb-claude--start-with dir prompt nil 'background)))
    (when (time-less-p (timer--time timer) nil)
      (cancel-timer timer)
      (timer-inc-time timer (* 24 60 60))
      (timer-activate timer))
    (message "Claude session scheduled for %s in %s"
             (format-time-string "%a %H:%M" (timer--time timer)) dir)
    timer))

;;; ============================================================
;;; PR / CI watcher
;;; ============================================================

(defvar kdb-claude-pr-poll-interval 150
  "Seconds between CI polls of watched PRs.")

(defvar kdb-claude-pr-log-lines 200
  "Failed-log lines sent per job.")

(defvar kdb-claude-pr-merged-functions nil
  "Abnormal hook run with (NUMBER BUFFER) when a watched PR is merged.")

(cl-defstruct (kdb-claude--watch (:constructor kdb-claude--watch-create))
  buffer dir number seen polling)

(defvar kdb-claude--watches nil "Active PR watches.")
(defvar kdb-claude--pr-lookups nil "Session buffers whose PR is being looked up.")
(defvar kdb-claude--pr-timer nil)

(defun kdb-claude--gh (dir args callback)
  "Run gh ARGS in DIR asynchronously.
Call CALLBACK with stdout, exit code and stderr; a failed spawn
reports exit code -1."
  (let ((buf (generate-new-buffer " *gh*"))
        (err (generate-new-buffer " *gh-stderr*"))
        (default-directory dir))
    (condition-case e
        (make-process
         :name "gh" :buffer buf :noquery t :connection-type 'pipe
         :coding 'utf-8 :command (cons "gh" args) :stderr err
         :sentinel (lambda (proc _)
                     (unless (process-live-p proc)
                       (let ((out (with-current-buffer buf (buffer-string)))
                             (msg (with-current-buffer err (buffer-string))))
                         (when-let* ((p (get-buffer-process err))) (delete-process p))
                         (kill-buffer buf)
                         (kill-buffer err)
                         (funcall callback out (process-exit-status proc) msg)))))
      (error (kill-buffer buf)
             (kill-buffer err)
             (funcall callback "" -1 (error-message-string e))))))

(defun kdb-claude--json (string)
  "Parse STRING as JSON (alists, lists), or nil."
  (ignore-errors (json-parse-string string :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil)))

(defun kdb-claude--watch-live-p (w)
  "Non-nil if W is still watched and its session is alive."
  (and (memq w kdb-claude--watches) (buffer-live-p (kdb-claude--watch-buffer w))))

(defun kdb-claude--watch-stop (w why)
  "Stop watching W and say WHY."
  (setq kdb-claude--watches (delq w kdb-claude--watches))
  (let ((buf (kdb-claude--watch-buffer w)))
    (when (buffer-live-p buf)
      (kdb-claude--notify buf (format "PR #%d" (kdb-claude--watch-number w)) why))
    (message "PR #%d: %s; stopped watching" (kdb-claude--watch-number w) why))
  (when (and (null kdb-claude--watches) kdb-claude--pr-timer)
    (cancel-timer kdb-claude--pr-timer)
    (setq kdb-claude--pr-timer nil)))

(defun kdb-claude--failed-log (output)
  "Last `kdb-claude-pr-log-lines' lines per job of `gh run --log-failed' OUTPUT."
  (let (jobs)
    (dolist (line (split-string output "\n" t))
      (let* ((tab (string-search "\t" line))
             (job (if tab (substring line 0 tab) ""))
             (cell (or (assoc job jobs) (car (push (list job) jobs)))))
        (push (if tab (substring line (1+ tab)) line) (cdr cell))))
    (mapconcat (lambda (cell)
                 (concat "### " (car cell) "\n"
                         (string-join (reverse (seq-take (cdr cell) kdb-claude-pr-log-lines)) "\n")))
               (nreverse jobs) "\n\n")))

(defun kdb-claude--watch-failure (w checks run-id)
  "Send W's session the failed CHECKS, with RUN-ID's log if it is an Actions run.
Each set of failed jobs is sent once; a re-run has new job links, so it
counts as new.  A log that can't be fetched yet is retried next poll."
  (let ((key (string-join (sort (mapcar (lambda (c) (concat (alist-get 'name c) " "
                                                            (alist-get 'link c)))
                                        checks)
                                #'string<)
                          "\n"))
        (n (kdb-claude--watch-number w))
        (name (mapconcat (lambda (c) (alist-get 'name c)) checks ", "))
        (link (alist-get 'link (car checks))))
    (unless (member key (kdb-claude--watch-seen w))
      (push key (kdb-claude--watch-seen w))
      (cl-flet ((send (log)
                  (when (kdb-claude--watch-live-p w)
                    (kdb-claude--queue-prompt
                     (kdb-claude--watch-buffer w) (format "CI check %s failed on PR #%d. Log below. Diagnose, fix, commit and push.\n%s\n\n```\n%s\n```"
                               name n link (if (string-blank-p log) "(no log available)" (string-trim log))))
                    (kdb-claude--notify (kdb-claude--watch-buffer w) (format "PR #%d CI failed" n) name))))
        (if (not run-id)
            (send "")
          (kdb-claude--gh (kdb-claude--watch-dir w) (list "run" "view" run-id "--log-failed")
                          (lambda (out status _)
                            (if (zerop status)
                                (send (kdb-claude--failed-log out))
                              (setf (kdb-claude--watch-seen w)
                                    (delete key (kdb-claude--watch-seen w)))))))))))

(defun kdb-claude--check-run (check)
  "The Actions run id of CHECK, or nil."
  (let ((link (or (alist-get 'link check) "")))
    (and (string-match "/actions/runs/\\([0-9]+\\)" link) (match-string 1 link))))

(defun kdb-claude--watch-checks (w checks)
  "Act on CHECKS (parsed `gh pr checks' JSON) for W."
  (let ((bucket (lambda (c) (alist-get 'bucket c))))
    (if (and checks (not (seq-find (lambda (c) (member (funcall bucket c) '("fail" "pending")))
                                   checks)))
        (kdb-claude--watch-stop w "all checks passed")
      ;; One prompt per finished run with failures: a run still going may
      ;; fail more jobs, and gh has no logs for it yet.
      (pcase-dolist (`(,run . ,group) (seq-group-by #'kdb-claude--check-run checks))
        (let ((failed (seq-filter (lambda (c) (equal (funcall bucket c) "fail")) group)))
          (cond ((null failed))
                ((null run)
                 (dolist (c failed) (kdb-claude--watch-failure w (list c) nil)))
                ((not (seq-find (lambda (c) (equal (funcall bucket c) "pending")) group))
                 (kdb-claude--watch-failure w failed run))))))))

(defun kdb-claude--watch-poll (w)
  "Check W's PR state, then its checks, without blocking."
  (unless (kdb-claude--watch-polling w)
    (setf (kdb-claude--watch-polling w) t)
    (let ((dir (kdb-claude--watch-dir w))
          (n (number-to-string (kdb-claude--watch-number w))))
      (kdb-claude--gh
       dir (list "pr" "view" n "--json" "state")
       (lambda (out &rest _)
         (if (not (kdb-claude--watch-live-p w))
             (setf (kdb-claude--watch-polling w) nil)
           (pcase (alist-get 'state (kdb-claude--json out))
             ("MERGED"
              (setf (kdb-claude--watch-polling w) nil)
              (kdb-claude--watch-stop w "merged")
              (run-hook-with-args 'kdb-claude-pr-merged-functions
                                  (kdb-claude--watch-number w) (kdb-claude--watch-buffer w)))
             ("CLOSED"
              (setf (kdb-claude--watch-polling w) nil)
              (kdb-claude--watch-stop w "closed"))
             (_
              ;; gh exits non-zero while checks fail or are pending; trust the JSON.
              (kdb-claude--gh
               dir (list "pr" "checks" n "--json" "name,state,bucket,link")
               (lambda (out &rest _)
                 (setf (kdb-claude--watch-polling w) nil)
                 (when (kdb-claude--watch-live-p w)
                   (when-let* ((checks (kdb-claude--json out)))
                     (kdb-claude--watch-checks w checks)))))))))))))

(defun kdb-claude--pr-poll ()
  "Poll every watched PR."
  (dolist (w kdb-claude--watches)
    (if (buffer-live-p (kdb-claude--watch-buffer w))
        (kdb-claude--watch-poll w)
      (kdb-claude--watch-stop w "session closed"))))

(defun kdb-claude--buffer-watch (buf)
  "The watch on session BUF, or nil."
  (seq-find (lambda (w) (eq (kdb-claude--watch-buffer w) buf)) kdb-claude--watches))

(defun kdb-claude--watch-start (buf dir number)
  "Watch PR NUMBER (in repo DIR) for session BUF."
  (let ((w (kdb-claude--watch-create :buffer buf :dir dir :number number)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook
                (lambda () (when (memq w kdb-claude--watches)
                             (kdb-claude--watch-stop w "session closed")))
                nil t))
    (push w kdb-claude--watches)
    (unless kdb-claude--pr-timer
      (setq kdb-claude--pr-timer (run-at-time kdb-claude-pr-poll-interval
                                              kdb-claude-pr-poll-interval
                                              #'kdb-claude--pr-poll)))
    (message "Watching PR #%d CI for %s" number (buffer-name buf))
    (kdb-claude--watch-poll w)
    w))

;;;###autoload
(defun kdb-claude-watch-pr ()
  "Watch CI on this session's PR and have the session fix failures.
Finds the PR for the session's branch, or asks for its number.  Each
failed run's log is sent once it finishes, through the session's prompt
queue.  Watching stops when all checks pass, the PR is merged or closed,
or the session ends.  Run again in a watched session to stop."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this in a Claude session buffer"))
  (let ((buf (current-buffer))
        (dir default-directory))
    (cond
     ((kdb-claude--buffer-watch buf)
      (let ((w (kdb-claude--buffer-watch buf)))
        (when (y-or-n-p (format "Stop watching PR #%d? " (kdb-claude--watch-number w)))
          (kdb-claude--watch-stop w "stopped"))))
     ((memq buf kdb-claude--pr-lookups)
      (message "Still looking up this session's PR..."))
     (t
      (push buf kdb-claude--pr-lookups)
      (message "Looking up the PR for this branch...")
      (kdb-claude--gh
       dir '("pr" "view" "--json" "number,url,headRefName,state")
       (lambda (out status err)
         (unwind-protect
             (let ((pr (kdb-claude--json out)))
               (when (and (buffer-live-p buf) (not (kdb-claude--buffer-watch buf)))
                 (kdb-claude--watch-start
                  buf dir
                  (if (equal (alist-get 'state pr) "OPEN")
                      (alist-get 'number pr)
                    (read-number
                     (cond (pr (format "PR #%s is %s; watch PR number: "
                                       (alist-get 'number pr) (downcase (alist-get 'state pr))))
                           ((and (/= status 0) (not (string-match-p "no pull requests found" err)))
                            (format "gh failed (%s); PR number: "
                                    (string-trim (or (car (split-string err "\n" t)) "no output"))))
                           (t "No open PR for this branch; PR number: ")))))))
           (setq kdb-claude--pr-lookups (delq buf kdb-claude--pr-lookups)))))))))

;;; ============================================================
;;; Menu
;;; ============================================================

(transient-define-prefix kdb-claude-menu ()
  "Claude"
  [["Start"
    ("n" "New here, on a branch (worktree)" kdb-claude-new)
    ("p" "New in a folder…" kdb-claude-new-in-folder)
    ("z" "Throwaway (one-off question)" kdb-claude-scratch)
    ("t" "Schedule a session" kdb-claude-schedule)
    ("m" "New with another model" kdb-claude-start-model)]
   ["Send & review"
    ("s" "Region/files/note/this file → session" kdb-claude-send)
    ("v" "Review my changes" kdb-claude-review)
    ("R" "Review a GitHub PR (URL)" kdb-claude-review-pr)
    ("w" "Watch PR CI, auto-fix (toggle)" kdb-claude-watch-pr)]
   ["Replies"
    ("x" "Run a code block" kdb-claude-run-block)
    ("o" "Last reply → org" kdb-claude-reply-to-org)
    ("a" "Open a file Claude created" kdb-claude-open-artifact)
    ("e" "Open its folder" kdb-claude-reveal-artifact)
    ("T" "Transcript, all of it" kdb-claude-transcript)]]
  [["Sessions (C-c ' answers)"
    ("c" "Switch / continue" kdb-claude-recent)
    ("P" "Status & processes" kdb-claude-processes)
    ("D" "Share live / stop" kdb-claude-remote-control)
    ("y" "Copy session id" kdb-claude-copy-session-id)]
   ["Queue"
    ("q" "Add a message" agent-shell-prompt-queue)
    ("E" "Edit a queued one" kdb-claude-queue-edit)
    ("k" "Remove queued" agent-shell-prompt-queue-remove)
    ("g" "Send queued now" agent-shell-prompt-queue-resume)]
   ["History"
    ("h" "All sessions" kdb-claude-history)
    ("/" "Search text" kdb-claude-search)
    ("A" "Ask past sessions" kdb-claude-ask-archive)]
   ["Setup"
    ("u" "Usage stats" kdb-claude-usage)
    ("S" "Sync & commit archive" kdb-claude-sync)
    ("i" "Import Desktop export" kdb-claude-import-export)
    ("!" "Set up this machine" kdb-claude-setup)]])

(defun kdb-claude ()
  "Claude menu."
  (interactive)
  (kdb-claude-menu))

(provide 'init-claude-workflow)
;;; init-claude-workflow.el ends here
