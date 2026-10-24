;;; init-claude-workflow.el --- Claude workflow on agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; One transient menu over agent-shell (ACP) sessions: start them per
;; project, branch or note, send them context, jump to whichever needs
;; you (agent-shell-attention), and search or resume past sessions.

;;; Code:

(require 'init-claude)
(require 'transient)

(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-send-dwim "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell-attention-jump "agent-shell-attention")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function notifications-notify "notifications")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(declare-function org-back-to-heading "org")
(declare-function org-mark-subtree "org")
(defvar agent-shell-cwd-function)

;;; ============================================================
;;; Sessions
;;; ============================================================

(defun kdb-claude--project-branch (dir)
  "Return (PROJECT . BRANCH) for DIR; worktrees report their main repo."
  (pcase (kdb-claude--git-lines dir "rev-parse" "--path-format=absolute"
                                "--git-common-dir" "--abbrev-ref" "HEAD")
    (`(,common ,branch)
     (cons (file-name-nondirectory
            (directory-file-name (file-name-directory (directory-file-name common))))
           branch))
    (_ (cons (abbreviate-file-name dir) nil))))

(defun kdb-claude--read-session (prompt)
  "Read a live session with PROMPT, grouped by project, and return its buffer.
Sessions in the current project are listed first."
  (require 'agent-shell)
  (let* ((here (car (kdb-claude--project-branch default-directory)))
         (cands (mapcar
                 (lambda (buf)
                   (let ((pb (kdb-claude--project-branch
                              (buffer-local-value 'default-directory buf))))
                     (cons (propertize (format "%-8s %-24s %s"
                                               (agent-shell-status :shell-buffer buf)
                                               (or (cdr pb) "-") (buffer-name buf))
                                       'kdb-project (car pb))
                           buf)))
                 (or (agent-shell-buffers)
                     (user-error "No active sessions.  Start one with C-c l n"))))
         (cands (seq-sort-by (lambda (c)
                               (let ((p (get-text-property 0 'kdb-project (car c))))
                                 (if (equal p here) "" p)))
                             #'string< cands))
         (choice (completing-read
                  prompt
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        `(metadata (display-sort-function . identity)
                                   (group-function
                                    . ,(lambda (c transform)
                                         (if transform c (get-text-property 0 'kdb-project c)))))
                      (complete-with-action action cands str pred)))
                  nil t)))
    (cdr (assoc choice cands))))

(defun kdb-claude-sessions ()
  "Switch to a live Claude session, grouped by project."
  (interactive)
  (switch-to-buffer (kdb-claude--read-session "Session: ")))

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
;;; Menu
;;; ============================================================

(transient-define-prefix kdb-claude-menu ()
  "Claude"
  [["Start"
    ("n" "New here (C-u pick project)" kdb-claude-new)
    ("N" "New on project + branch" kdb-claude-start)]
   ["Send"
    ("s" "Region/note/files → session" kdb-claude-send)
    ("f" "File → session" agent-shell-send-file-to)]
   ["Sessions"
    ("'" "Answer / next waiting" kdb-claude-answer)
    ("d" "Dashboard" agent-shell-attention-dashboard)
    ("l" "Switch (by project)" kdb-claude-sessions)]
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
