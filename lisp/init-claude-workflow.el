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
(declare-function agent-shell-start "agent-shell")
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

(defun kdb-claude--forget-permission (id)
  "Drop permission request ID from the queue."
  (remhash id kdb-claude--responders)
  (setq kdb-claude--permissions (seq-remove (lambda (e) (equal (car e) id))
                                            kdb-claude--permissions)))

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
                      (when-let* ((perm (gethash id kdb-claude--responders)))
                        (setq kdb-claude--permissions
                              (append kdb-claude--permissions (list (list id buf perm))))))
                     ('permission-response (kdb-claude--forget-permission id))
                     ('clean-up
                      (dolist (e kdb-claude--permissions)
                        (when (eq (nth 1 e) buf) (kdb-claude--forget-permission (car e)))))))))))

(defconst kdb-claude--permission-keys
  '(("allow_once" . ?y) ("allow_always" . ?!) ("reject_once" . ?n) ("reject_always" . ?N)))

;;;###autoload
(defun kdb-claude-answer ()
  "Answer the oldest permission request from any session, without leaving
the current buffer, then the next one.  With none pending, jump to the
next session waiting on you."
  (interactive)
  (setq kdb-claude--permissions (seq-filter (lambda (e) (buffer-live-p (nth 1 e)))
                                            kdb-claude--permissions))
  (pcase (car kdb-claude--permissions)
    ('nil (agent-shell-attention-jump))
    (`(,id ,buf ,perm)
     (let* ((options (seq-filter (lambda (o) (assoc (map-elt o :kind) kdb-claude--permission-keys))
                                 (map-elt perm :options)))
            (choices (append (mapcar (lambda (o)
                                       (list (cdr (assoc (map-elt o :kind) kdb-claude--permission-keys))
                                             (map-elt o :option) o))
                                     options)
                             '((?g "go to session") (?q "later"))))
            (more (1- (length kdb-claude--permissions)))
            (key (car (read-multiple-choice
                       (format "%s%s: %s" (buffer-name buf)
                               (if (> more 0) (format " (+%d waiting)" more) "")
                               (or (map-elt (map-elt perm :tool-call) :title) "tool"))
                       (mapcar (lambda (c) (seq-take c 2)) choices))))
            (opt (nth 2 (assq key choices))))
       (cond (opt
              (with-current-buffer buf
                (funcall (map-elt perm :respond) (map-elt opt :option-id))
                (when (equal (map-elt opt :kind) "reject_once")
                  (agent-shell-interrupt t)))
              (kdb-claude--forget-permission id)
              (when kdb-claude--permissions (kdb-claude-answer)))
             ((eq key ?g)
              (pop-to-buffer buf)
              (agent-shell-jump-to-latest-permission-button-row)))))))

(defvar kdb-claude--notification nil
  "Id of the last Windows notification, which must be closed before the next.")

;;;###autoload
(defun kdb-claude--notify (_buffer title body)
  "Show TITLE/BODY as a desktop notification, only when Emacs isn't focused."
  (unless (seq-some #'frame-focus-state (frame-list))
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

(defun kdb-claude--new-in (dir)
  "Start a new Claude shell in DIR (nil = the current project root).
The region, Dired files or org entry at point become its first input."
  (require 'agent-shell)
  (kdb-claude--mark-org-entry)
  (let ((agent-shell-cwd-function (if dir (lambda () dir) agent-shell-cwd-function)))
    (agent-shell-anthropic-start-claude-code)))

(defun kdb-claude--start-with (dir &optional prompt session-id)
  "Start a Claude shell in DIR, resuming SESSION-ID if non-nil.
Submit PROMPT once the shell is ready.  Return the shell buffer."
  (require 'agent-shell)
  (let* ((default-directory dir)
         (agent-shell-cwd-function (lambda () dir))
         (buf (agent-shell-start :config (agent-shell-anthropic-make-claude-code-config)
                                 :session-id session-id)))
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
(defun kdb-claude-new (&optional pick)
  "Start a new Claude session, carrying over the region, Dired files or
org entry at point.  Runs at the project root; with prefix arg PICK, or
outside any project, pick the directory."
  (interactive "P")
  (kdb-claude--new-in (when (or pick (not (project-current)))
                        (kdb-claude-read-directory "Project: "))))

;;;###autoload
(defun kdb-claude-send ()
  "Send the region, Dired files or org entry at point to a session you pick."
  (interactive)
  (require 'agent-shell)
  (kdb-claude--mark-org-entry)
  (agent-shell-send-dwim '(16)))

;;;###autoload
(defun kdb-claude-review ()
  "Have Claude review this repo's uncommitted changes (agent-review).
In the results, mark findings with m and send them to a session with S."
  (interactive)
  (require 'agent-shell)
  (agent-review (agent-shell-anthropic-make-claude-code-config)))

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

(defun kdb-claude--worktree (project branch)
  "Return a worktree of PROJECT on BRANCH, creating it beside PROJECT.
A BRANCH that doesn't exist yet is created from PROJECT's HEAD.
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
                                  (list "-b" branch dir))))
            (user-error "git worktree add failed for %s" branch)))
        (file-name-as-directory dir))))

(defun kdb-claude-start ()
  "Start a Claude session on a project and branch of your choice.
RET at the branch prompt keeps the current checkout; an existing branch
opens its worktree; a new name creates the branch in a new worktree."
  (interactive)
  (let* ((project (kdb-claude-read-directory "Project: "))
         (branches (kdb-claude--git-lines project "branch" "--format=%(refname:short)"))
         (branch (when branches
                   (let ((b (completing-read
                             "Branch (RET = current checkout, new name = new worktree): "
                             branches)))
                     (unless (string-empty-p b) b)))))
    (kdb-claude--new-in (if branch (kdb-claude--worktree project branch) project))))

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

;; Markdown fence languages -> org babel languages.
(defconst kdb-claude--babel-langs
  '(("" . "bash") ("sh" . "bash") ("shell" . "bash") ("console" . "bash") ("zsh" . "bash")
    ("elisp" . "emacs-lisp") ("pwsh" . "powershell") ("ps1" . "powershell") ("ps" . "powershell")))

(declare-function org-escape-code-in-string "org-src")

;;;###autoload
(defun kdb-claude-run-block ()
  "Copy the code block at point, or the last one before it, to org babel.
It is appended to *claude blocks* as a source block that runs in this
buffer's directory; edit it if needed, then \\[org-ctrl-c-ctrl-c] runs it and
the results land under it."
  (interactive)
  (pcase-let* ((pos (point))
               (blocks (kdb-claude--code-blocks))
               (`(,lang ,beg ,end)
                (or (seq-find (lambda (b) (<= (nth 1 b) pos (nth 2 b))) blocks)
                    (car (last (seq-filter (lambda (b) (<= (nth 2 b) pos)) blocks)))
                    (user-error "No code block at or before point")))
               (code (string-trim-right (buffer-substring-no-properties beg end)))
               (dir default-directory))
    (pulse-momentary-highlight-region beg end)
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
    (message "Edit if needed, then C-c C-c to run")))

;;; ============================================================
;;; Sessions talking to sessions
;;; ============================================================

(defvar agent-shell-anthropic-claude-environment)
(declare-function shell-maker-busy "shell-maker")
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

;;;###autoload
(defun kdb-claude--tag-client (make-client &rest args)
  "Around MAKE-CLIENT: give the session's process KDB_CLAUDE_SESSION.
ARGS has the session buffer under :buffer."
  (let ((agent-shell-anthropic-claude-environment
         (cons (concat "KDB_CLAUDE_SESSION="
                       (kdb-claude--session-id (plist-get args :buffer)))
               agent-shell-anthropic-claude-environment)))
    (apply make-client args)))

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
                 dir (if from (concat prompt (kdb-claude--report-back from id)) prompt)))))
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
  (let ((timer (run-at-time time nil #'kdb-claude--start-with dir prompt)))
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
    ("n" "New here (C-u pick project)" kdb-claude-new)
    ("N" "New on project + branch" kdb-claude-start)
    ("t" "Schedule a session" kdb-claude-schedule)]
   ["Send"
    ("s" "Region/note/files → session" kdb-claude-send)
    ("f" "File → session" agent-shell-send-file-to)
    ("v" "Review my changes" kdb-claude-review)
    ("x" "Code block → org babel (C-c C-c runs)" kdb-claude-run-block)]
   ["Sessions"
    ("'" "Answer / next waiting" kdb-claude-answer)
    ("d" "Dashboard" agent-shell-attention-dashboard)
    ("l" "Peek / switch (by project)" agent-shell-hq-peek)
    ("r" "Name this session" agent-shell-hq-label)
    ("w" "Watch PR CI, auto-fix (toggle)" kdb-claude-watch-pr)]
   ["History"
    ("h" "All sessions" kdb-claude-history)
    ("/" "Search text" kdb-claude-search)
    ("A" "Ask past sessions" kdb-claude-ask-archive)
    ("S" "Sync & commit" kdb-claude-sync)
    ("i" "Import Desktop export" kdb-claude-import-export)
    ("!" "Set up this machine" kdb-claude-setup)]])

(defun kdb-claude ()
  "Claude menu."
  (interactive)
  (kdb-claude-menu))

(provide 'init-claude-workflow)
;;; init-claude-workflow.el ends here
