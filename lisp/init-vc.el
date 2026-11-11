;;; init-vc.el --- Version Control configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete version control setup with enhanced VC and vc-dir features
;; Provides Magit-like functionality using built-in VC

;;; Code:

;; Version Control =============================================== ;;

(use-package vc
  :ensure nil
  :config
  (setq vc-follow-symlinks t
        vc-git-diff-switches '("--histogram" "--color=never")
        vc-git-print-log-follow t
        vc-git-log-edit-summary-target-len 50
        vc-git-log-edit-summary-max-len 70
        vc-suppress-confirm t
        vc-command-messages t)
  ;; No custom `vc-git-log-switches': a --pretty format leaves log-view no
  ;; "commit HASH" lines, so n/p, diff, annotate and find-revision in a log
  ;; find nothing.  The branch log (L) is one line per commit already.

  (add-hook 'vc-diff-finish-functions
            (lambda ()
              (when (string-match-p "\\*vc-diff\\*" (buffer-name))
                (setq-local truncate-lines t)
                (when diff-refine
                  (diff-refine-hunk))))))

;; Log-view configuration
(use-package log-view
  :ensure nil
  :after vc
  :config
  (add-hook 'log-view-mode-hook (lambda () (setq-local truncate-lines t) (hl-line-mode 1)))

  ;; Quick diff for commit at point
  (defun kdb-log-view-show-commit-diff ()
    "Show diff for the commit at point in log view."
    (interactive)
    (let ((commit-hash (or (log-view-current-tag)
                           (save-excursion
                             (beginning-of-line)
                             (when (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" 
                                                     (line-end-position) t)
                               (match-string 1))))))
      (if commit-hash
          (kdb-vc-commit-show-diff-in-buffer commit-hash)
        (message "No commit hash found at point"))))
  
  (defun kdb-log-view-show-commit-stat ()
    "Show commit stats without the full diff."
    (interactive)
    (let ((commit-hash (or (log-view-current-tag)
                           (save-excursion
                             (beginning-of-line)
                             (when (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" 
                                                     (line-end-position) t)
                               (match-string 1))))))
      (if commit-hash
          (kdb-vc-commit-show-stat-in-buffer commit-hash)
        (message "No commit hash found at point"))))
  
  (defun kdb-vc-commit-show-diff-in-buffer (hash)
    "Show diff for commit HASH in a side buffer."
    (let ((buf (get-buffer-create "*Commit Diff*"))
          (root (kdb-vc-root)))
      (with-current-buffer buf
        (setq default-directory root)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (diff-mode)
          (kdb-commit-diff-mode 1)  ; Enable navigation mode
          ;; Store commit hash for navigation
          (setq-local kdb-current-commit-hash hash)
          ;; Add header with navigation hints
          (insert (propertize (format "Commit: %s | [N]ext [P]revious [q]uit\n" hash)
                             'face 'font-lock-comment-face))
          (insert (make-string 70 ?─) "\n")
          ;; Show the commit message and diff
          (insert (kdb-vc-git-output "show" "--stat" "--patch" hash))
          (goto-char (point-min))))
      ;; Display in side window
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 0.5)))))
  
  (defun kdb-vc-commit-show-stat-in-buffer (hash)
    "Show commit stats for HASH in a side buffer."
    (let ((buf (get-buffer-create "*Commit Info*"))
          (root (kdb-vc-root)))
      (with-current-buffer buf
        (setq default-directory root)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (kdb-vc-git-output "show" "--stat" "--format=fuller" hash))
          (goto-char (point-min))
          (diff-mode)))
      ;; Display in side window
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 0.5)))))
  
  ;; Navigation in commit diff buffer
  (defun kdb-commit-diff-show-next ()
    "Show diff for next (newer) commit."
    (interactive)
    (when (bound-and-true-p kdb-current-commit-hash)
      (let ((next-hash (car (ignore-errors
                              (process-lines "git" "log" "--reverse" "--ancestry-path" "--format=%h"
                                             (concat kdb-current-commit-hash "..HEAD"))))))
        (if next-hash
            (kdb-vc-commit-show-diff-in-buffer next-hash)
          (message "No next commit")))))
  
  (defun kdb-commit-diff-show-previous ()
    "Show diff for previous commit."
    (interactive)
    (when (bound-and-true-p kdb-current-commit-hash)
      (let ((prev-hash (car (ignore-errors
                              (process-lines "git" "rev-parse" "--short"
                                             (concat kdb-current-commit-hash "^"))))))
        (if prev-hash
            (kdb-vc-commit-show-diff-in-buffer prev-hash)
          (message "No previous commit")))))
  
  ;; Add keybindings to log-view-mode
  (define-key log-view-mode-map (kbd "d") 'kdb-log-view-show-commit-diff)
  (define-key log-view-mode-map (kbd "D") 'kdb-log-view-show-commit-stat)
  (define-key log-view-mode-map (kbd "RET") 'kdb-log-view-show-commit-diff)
  (define-key log-view-mode-map (kbd "=") 'kdb-log-view-show-commit-diff)
  (define-key log-view-mode-map (kbd "s") 'kdb-log-view-show-commit-stat)
  
  ;; Add keybindings to commit diff buffer only
  (defvar kdb-commit-diff-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "N") 'kdb-commit-diff-show-next)
      (define-key map (kbd "P") 'kdb-commit-diff-show-previous)
      (define-key map (kbd "q") 'quit-window)
      map)
    "Keymap for commit diff buffers.")
  
  (define-minor-mode kdb-commit-diff-mode
    "Minor mode for commit diff navigation."
    :lighter " CommitDiff"
    :keymap kdb-commit-diff-mode-map))


;; VC-msg - Show commit messages for current line
(use-package vc-msg
  :ensure t
  :config
  (setq vc-msg-git-show-commit-function #'kdb-vc-commit-show))

;; Diff-hl - changed-line indicators in the fringe + hunk-level staging
(use-package diff-hl
  :ensure t
  :demand t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (if (eq system-type 'windows-nt)
      ;; flydiff runs git on every idle pause; process startup is slow there
      (setq diff-hl-update-async t)
    (diff-hl-flydiff-mode 1))   ; update indicators without waiting for save
  ;; Hunk-level workflow on the vc prefix:
  ;;   C-x v S  stage hunk at point   C-x v K  discard hunk at point
  ;;   C-x v *  peek hunk popup       C-x v [ / ]  previous/next hunk
  (define-key vc-prefix-map (kbd "S") #'diff-hl-stage-current-hunk)
  (define-key vc-prefix-map (kbd "K") #'diff-hl-revert-hunk)
  (define-key vc-prefix-map (kbd "*") #'diff-hl-show-hunk)
  (define-key vc-prefix-map (kbd "[") #'diff-hl-previous-hunk)
  (define-key vc-prefix-map (kbd "]") #'diff-hl-next-hunk))

(defun kdb-vc-root ()
  "This repository's root, or an error outside one.
Found from `default-directory' too, so it works from buffers visiting no
file, such as a Claude session, where `vc-root-dir' gives nil."
  (or (vc-root-dir)
      (vc-find-root default-directory ".git")
      (user-error "Not in a git repository")))

(defun kdb-vc-git-output (&rest args)
  "Output of git ARGS at the repository root.
Arguments go to git as they are, never through a shell, so file and
branch names need no quoting (and none goes wrong on Windows' cmd)."
  (let ((default-directory (kdb-vc-root)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file "git" nil t nil args)))))

(defun kdb-vc-git-lines (&rest args)
  "Lines git ARGS prints at the repository root."
  (split-string (apply #'kdb-vc-git-output args) "\n" t))

(defun kdb-vc-git-run (&rest args)
  "Run git ARGS at the repository root; error with git's own words on failure."
  (let ((default-directory (kdb-vc-root)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (user-error "git %s: %s" (car args) (string-trim (buffer-string)))))))

(defvar vc-ewoc)

(defun kdb-vc-files ()
  "Files to act on, relative to the root: vc-dir's marked or current ones,
else this buffer's file, else nil."
  (let ((root (kdb-vc-root)))
    (mapcar (lambda (f) (file-relative-name (expand-file-name f) root))
            (cond ((derived-mode-p 'vc-dir-mode)
                   (or (vc-dir-marked-files)
                       ;; On the header vc-dir-current-file gives the first
                       ;; file, which you didn't pick.
                       (when-let* ((first (ewoc-nth vc-ewoc 0)))
                         (and (>= (point) (ewoc-location first))
                              (list (vc-dir-current-file))))))
                  (buffer-file-name (list buffer-file-name))))))

(defun kdb-vc-refresh ()
  "Refresh vc-dir and the fringe marks after git changed things."
  (when (derived-mode-p 'vc-dir-mode) (vc-dir-refresh))
  (when (and buffer-file-name (fboundp 'diff-hl-update)) (diff-hl-update)))

(defun kdb-vc-git-stage ()
  "Stage the file(s) here, tracked or not (git add), like magit's s."
  (interactive)
  (let ((files (or (kdb-vc-files) (user-error "No file here to stage"))))
    (apply #'kdb-vc-git-run "add" "--" files)
    (kdb-vc-refresh)
    (message "Staged: %s" (string-join files ", "))))

(defun kdb-vc-git-unstage ()
  "Unstage the file(s) here, or everything staged when there's no file."
  (interactive)
  (if-let* ((files (kdb-vc-files)))
      (progn (apply #'kdb-vc-git-run "reset" "-q" "HEAD" "--" files)
             (message "Unstaged: %s" (string-join files ", ")))
    (when (yes-or-no-p "Unstage all staged changes? ")
      (kdb-vc-git-run "reset" "-q" "HEAD")
      (message "All changes unstaged")))
  (kdb-vc-refresh))

(defun kdb-vc-fetch ()
  "Fetch from all remotes, pruning, without touching your branch (magit's f)."
  (interactive)
  (let ((default-directory (kdb-vc-root)))
    (message "Fetching…")
    (set-process-sentinel
     (start-file-process "git-fetch" " *git-fetch*" "git" "fetch" "--all" "--prune")
     (lambda (proc _)
       (unless (process-live-p proc)
         (message (if (zerop (process-exit-status proc)) "Fetched"
                    "Fetch failed; see buffer \" *git-fetch*\"")))))))

(defun kdb-vc-push-specific ()
  "Push, editing the git push command first (remote, branch, flags)."
  (interactive)
  (vc-push t))

;; VC Transient Menu - provides Magit-like interface for VC
(require 'transient)
(transient-define-prefix kdb-vc-transient ()
  "Custom VC transient menu with Magit-like keybindings."
  [["Status & Diff"
    ("g" "Status (Root)" project-vc-dir)
    ("G" "Lazygit" kdb-lazygit)
    ("s" "Status" vc-dir)
    ("d" "Diff (unstaged)" vc-diff)
    ("D" "Diff vs Branch" vc-root-diff)
    ("=" "Basic Diff" vc-diff)
    ("e" "Ediff" vc-ediff)
    ("E" "Ediff Revision" vc-version-ediff)]
   
   ["Log & History"
    ("l" "Log (current file)" vc-print-log)
    ("L" "Log (branch)" vc-print-root-log)
    ("h s" "Show Commit" kdb-vc-commit-show)
    ("h d" "Commit Diff" kdb-vc-commit-diff)
    ("h f" "Find Revision" vc-revision-other-window)
    ("/" "Search Log" vc-log-search)
    ("B" "Blame" vc-annotate)
    ("M" "Line Commit" vc-msg-show)]
   
   ["Branch"
    ("b b" "Switch" vc-switch-branch)
    ("b n" "New" vc-create-branch)
    ("b d" "Delete" kdb-vc-delete-branch)
    ("b r" "Rename" kdb-vc-rename-branch)
    ("?" "File at Branch" kdb-vc-find-branch-file)
    ""
    "-Merge-"
    ("m" "Merge" vc-merge)]
   
   ["Remote"
    ("f" "Pull" vc-update)
    ("F" "Fetch" kdb-vc-fetch)
    ("p" "Push" vc-push)
    ("P" "Push Specific" kdb-vc-push-specific)
    ""
    "-Management-"
    ("r l" "List" kdb-vc-remote-list)
    ("r a" "Add" kdb-vc-remote-add)
    ("r u" "Set URL" kdb-vc-remote-set-url)
    ("C" "Clone" kdb-vc-clone)]]
  
  [["Changes"
    ("a" "Add/Stage" kdb-vc-git-stage)
    ("x" "Unstage" kdb-vc-git-unstage)
    ("c" "Commit" vc-next-action)
    ("C-c" "Amend" kdb-vc-git-amend)
    ("u" "Revert" vc-revert)
    ("U" "Discard changes" kdb-vc-revert-file)
    ("k" "Delete" vc-delete-file)
    ("R" "Rename" vc-rename-file)]
   
   ["Stash"
    ("z z" "Snapshot" vc-git-stash-snapshot)
    ("z s" "Stash..." vc-git-stash)
    ("z a" "Apply..." vc-git-stash-apply)
    ("z p" "Pop..." vc-git-stash-pop)
    ("z v" "Show..." vc-git-stash-show)
    ("z k" "Delete..." vc-git-stash-delete)]
   
   ["Tags & Info"
    ("t" "Create Tag" vc-create-tag)
    ("T" "List Tags" kdb-vc-git-list-tags)
    ("i" "Init Repo" vc-create-repo)
    ("!" "Git Command" kdb-vc-git-command)
    ""
    ("q" "Quit" transient-quit-one)]])

;; Custom VC helper functions
(defun kdb-vc-remote-set-url ()
  "Set remote URL for current repository."
  (interactive)
  (let* ((remote (read-string "Remote name (default: origin): " "origin"))
         (url (read-string "New URL: ")))
    (when (and remote url (not (string-empty-p url)))
      (shell-command (format "git remote set-url %s %s"
                             (shell-quote-argument remote)
                             (shell-quote-argument url)))
      (message "Remote '%s' URL updated to: %s" remote url))))

(defun kdb-vc-remote-add ()
  "Add a new remote to the current repository."
  (interactive)
  (let* ((remote (read-string "Remote name: "))
         (url (read-string "Remote URL: ")))
    (when (and remote url (not (string-empty-p url)) (not (string-empty-p remote)))
      (shell-command (format "git remote add %s %s"
                             (shell-quote-argument remote)
                             (shell-quote-argument url)))
      (message "Remote '%s' added with URL: %s" remote url))))

(defun kdb-vc-git-amend ()
  "Amend the last commit via log-edit, pre-filled with its message.
Needs at least one modified/registered file in the fileset (like any
vc check-in).  \\`C-c C-e' in log-edit toggles amend on any commit."
  (interactive)
  (vc-next-action nil)
  (when (derived-mode-p 'log-edit-mode)
    (vc-git-log-edit-toggle-amend)))

(defun kdb-vc-revert-file ()
  "Discard the changes to the file(s) here, back to the last commit."
  (interactive)
  (let ((files (or (kdb-vc-files) (user-error "No file here to revert"))))
    (when (yes-or-no-p (format "Discard all changes to %s? " (string-join files ", ")))
      (apply #'kdb-vc-git-run "checkout" "HEAD" "--" files)
      ;; Every buffer on a discarded file, or under a discarded directory,
      ;; else a later save puts the changes back.  By truename and ignoring
      ;; case: git and Emacs spell Windows paths differently (C: and c:,
      ;; KEVINB~1), which `vc-resynch-buffers-in-directory' misses.
      (let ((paths (mapcar (lambda (f)
                             (let ((p (file-truename (expand-file-name f (kdb-vc-root)))))
                               (if (file-directory-p p) (file-name-as-directory p) p)))
                           files))
            (fold (memq system-type '(windows-nt cygwin darwin))))
        (dolist (b (buffer-list))
          (when-let* ((name (buffer-file-name b))
                      (name (file-truename name))
                      ((seq-some (lambda (p)
                                   (if (directory-name-p p)
                                       (string-prefix-p p name fold)
                                     (eq t (compare-strings p nil nil name nil nil fold))))
                                 paths)))
            (with-current-buffer b (revert-buffer t t t)))))
      (kdb-vc-refresh)
      (message "Reverted %s to the last commit" (string-join files ", ")))))

(defun kdb-vc-delete-branch ()
  "Delete a git branch."
  (interactive)
  (let ((branch (completing-read "Delete branch: "
                                 (kdb-vc-git-lines "branch" "--format=%(refname:short)") nil t)))
    (when (yes-or-no-p (format "Delete branch '%s'? " branch))
      (kdb-vc-git-run "branch" "-d" branch)
      (message "Branch '%s' deleted" branch))))

(defun kdb-vc-rename-branch ()
  "Rename the current git branch."
  (interactive)
  (let* ((current-branch (car (kdb-vc-git-lines "branch" "--show-current")))
         (new-name (read-string (format "Rename branch '%s' to: " current-branch))))
    (when (and new-name (not (string-empty-p new-name)))
      (kdb-vc-git-run "branch" "-m" new-name)
      (message "Branch renamed from '%s' to '%s'" current-branch new-name))))

;; Stash operations use the vc-git built-ins — see the transient.  vc-dir's
;; header line also lists stashes with a clickable menu.  vc-git.el loads
;; lazily, so make its stash commands callable from the transient anytime.
(dolist (fn '(vc-git-stash vc-git-stash-snapshot vc-git-stash-pop
              vc-git-stash-apply vc-git-stash-show vc-git-stash-delete
              vc-git-log-edit-toggle-amend))
  (autoload fn "vc-git" nil t))

(declare-function w32-shell-execute "w32fns.c")

(defun kdb-lazygit ()
  "Open lazygit in an eat terminal at the repository root.
For history surgery vc doesn't cover: interactive rebase, cherry-pick."
  (interactive)
  (let ((lazygit (or (executable-find "lazygit")
                     (user-error "lazygit is not installed")))
        (root (expand-file-name (or (vc-root-dir) default-directory))))
    (if (eq system-type 'windows-nt)
        ;; eat can't run on Windows, and lazygit needs a real console
        (w32-shell-execute "open" lazygit (format "-p \"%s\"" root))
      (require 'eat)
      (let ((default-directory root))
        (eat "lazygit" t)))))

(defun kdb-vc-git-list-tags ()
  "List all tags in the repository."
  (interactive)
  (let ((tags (shell-command-to-string "git tag -l --sort=-version:refname")))
    (if (string-empty-p (string-trim tags))
        (message "No tags found")
      (with-current-buffer (get-buffer-create "*Git Tags*")
        (erase-buffer)
        (insert "Git Tags (sorted by version):\n\n")
        (insert tags)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun kdb-vc-find-branch-file ()
  "Find and open a file from another branch."
  (interactive)
  (let* ((branch (completing-read "Branch: " (kdb-vc-git-lines "branch" "-a" "--format=%(refname:short)") nil t))
         (file (completing-read "File: " (kdb-vc-git-lines "ls-tree" "-r" "--name-only" branch) nil t)))
    (when file
      (let ((content (kdb-vc-git-output "show" (concat branch ":" file))))
        (with-current-buffer (get-buffer-create (format "*%s:%s*" branch file))
          (erase-buffer)
          (insert content)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))))))

(defun kdb-vc-git-command (command)
  "Run git COMMAND, its output streaming into *Git Output*."
  (interactive "sGit command (without 'git'): ")
  ;; Async: git may open an editor (emacsclient) that this Emacs must serve.
  (async-shell-command (concat "git " command) "*Git Output*"))

;; Commit hash lookup functions
(defun kdb-vc-commit-show (hash)
  "Show detailed information about a commit hash."
  (interactive "sCommit hash: ")
  (let ((output (kdb-vc-git-output "show" "--stat" "--pretty=fuller" hash))
        (root (kdb-vc-root)))
    (with-current-buffer (get-buffer-create "*Commit Details*")
      (setq default-directory root)
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun kdb-vc-commit-diff (hash)
  "Show diff for a specific commit HASH."
  (interactive "sCommit hash: ")
  (let ((output (kdb-vc-git-output "show" "--format=" hash))
        (root (kdb-vc-root)))
    (with-current-buffer (get-buffer-create "*Commit Diff*")
      (setq default-directory root)
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defalias 'kdb-vc-clone 'vc-clone)

(defun kdb-vc-remote-list ()
  "List all remotes with their URLs."
  (interactive)
  (let ((output (shell-command-to-string "git remote -v")))
    (if (string-empty-p (string-trim output))
        (message "No remotes configured")
      (with-current-buffer (get-buffer-create "*VC Remotes*")
        (erase-buffer)
        (insert "Git Remotes:\n\n")
        (insert output)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;; VC-dir Enhancements
(use-package vc-dir
  :ensure nil
  :after vc
  :config
  ;; Better defaults for cleaner UI
  (setq vc-dir-auto-hide-up-to-date t)     ; hide unchanged files after a refresh
  ;; Added to, not replaced: init.el's project settings put "elpa" etc. here.
  (dolist (dir '(".git" ".hg" ".svn" "node_modules" ".venv"))
    (add-to-list 'vc-directory-exclusion-list dir))

  ;; === MERGE CONFLICT RESOLUTION === ;;
  
  ;; Smerge mode for handling merge conflicts
  (use-package smerge-mode
    :ensure nil
    :bind (:map smerge-mode-map
           ("C-c m n" . smerge-next)
           ("C-c m p" . smerge-prev)
           ("C-c m a" . smerge-keep-upper)
           ("C-c m b" . smerge-keep-lower)
           ("C-c m m" . smerge-keep-all)
           ("C-c m c" . smerge-keep-current)
           ("C-c m e" . smerge-ediff)
           ("C-c m r" . smerge-resolve)
           ("C-c m R" . smerge-refine)))

  ;; Useful VC helper functions from emacs-solo
  (defun kdb-vc-browse-remote ()
    "Browse the remote repository URL in browser."
    (interactive)
    (let ((remote-url (string-trim 
                      (shell-command-to-string 
                       "git config --get remote.origin.url"))))
      (when (string-match "git@\\(.+\\):\\(.+\\)" remote-url)
        (setq remote-url (format "https://%s/%s" 
                                (match-string 1 remote-url)
                                (match-string 2 remote-url))))
      (when (string-match "\\.git$" remote-url)
        (setq remote-url (substring remote-url 0 -4)))
      (browse-url remote-url)
      (message "Opening %s" remote-url)))

  ;; Function to open all conflict files
  (defun kdb-vc-resolve-conflicts ()
    "Open all files with merge conflicts for resolution."
    (interactive)
    (let* ((root (kdb-vc-root))
           (conflicts (kdb-vc-git-lines "diff" "--name-only" "--diff-filter=U"))
           (conflict-count (length conflicts)))
      (if (= conflict-count 0)
          (message "No merge conflicts found!")
        (message "Opening %d conflict files..." conflict-count)
        (dolist (file conflicts)
          (find-file (expand-file-name file root)))
        (message "Resolve with: C-c m n/p (navigate) C-c m a (mine) C-c m b (theirs) C-c m e (ediff)"))))
  
  (defun kdb-vc-dir-mark-same-state ()
    "Mark all files with same state as current file."
    (interactive)
    (let ((file (vc-dir-current-file)))
      (when file
        (let ((state (vc-state file)))
          (cond
           ((eq state 'unregistered)
            (vc-dir-mark-unregistered-files)
            (message "Marked all unregistered files"))
           ((memq state '(edited added removed))
            (vc-dir-mark-registered-files) 
            (message "Marked all registered files"))
           (t
            (vc-dir-mark-all-files t)
            (message "Marked all files")))))))

  ;; Show only modified files
  (defun kdb-vc-dir-show-only-modified ()
    "Show only modified files in vc-dir."
    (interactive)
    (vc-dir-hide-state 'up-to-date)
    (vc-dir-hide-state 'ignored)
    (vc-dir-hide-state 'unregistered))
  
  ;; Toggle showing all files
  (defun kdb-vc-dir-toggle-all ()
    "Toggle between showing all files and only modified files."
    (interactive)
    (if (get 'kdb-vc-dir-toggle-all 'showing-all)
        (progn
          (kdb-vc-dir-show-only-modified)
          (put 'kdb-vc-dir-toggle-all 'showing-all nil)
          (message "Showing only modified files"))
      (progn
        (vc-dir-unmark-all-files t)
        (revert-buffer)
        (put 'kdb-vc-dir-toggle-all 'showing-all t)
        (message "Showing all files"))))
  
  ;; Quick commit with message
  (defun kdb-vc-dir-register-unregistered ()
    "Register any unregistered files among the marked files (or file at point).
Makes a mixed fileset (untracked + edited) committable in one step."
    (let* ((files (or (vc-dir-marked-files)
                      (when (vc-dir-current-file)
                        (list (vc-dir-current-file)))))
           (unreg (seq-filter (lambda (f) (eq (vc-state f) 'unregistered)) files)))
      (when unreg
        (vc-register (list (vc-responsible-backend default-directory) unreg))
        (message "Registered %d untracked file(s)" (length unreg)))))

  (defun kdb-vc-dir-quick-commit ()
    "Quick commit with a simple message prompt.
Marked untracked files are registered automatically first."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (kdb-vc-dir-register-unregistered)
      (vc-next-action nil)
      (insert msg)
      (log-edit-done)))
  
  

  ;; Show summary of changes
  (defun kdb-vc-dir-summary ()
    "Show a summary of repository status."
    (interactive)
    (let ((modified 0) (added 0) (removed 0) (unregistered 0))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "^[ *][ *]\\s-+\\(\\S-+\\)\\s-+")
            (let ((state (match-string 1)))
              (cond ((string= state "edited") (cl-incf modified))
                    ((string= state "added") (cl-incf added))
                    ((string= state "removed") (cl-incf removed))
                    ((string= state "unregistered") (cl-incf unregistered)))))
          (forward-line 1)))
      (message "Modified: %d | Added: %d | Removed: %d | Untracked: %d"
               modified added removed unregistered)))
  
  ;; Help function
  (defun kdb-vc-dir-help ()
    "Show vc-dir keybinding help."
    (interactive)
    (message "VC-Dir: [d]iff [D]iff-all [SPC]mark [a]mark-same-type [c]ommit [C]onflicts [h/x]hide [r]evert [s]ummary | log-edit: C-c C-d diff, C-c C-e amend"))

  ;; Set up keybindings using define-key instead of :bind
  (with-eval-after-load 'vc-dir
    (define-key vc-dir-mode-map (kbd "h") 'vc-dir-hide-up-to-date)
    (define-key vc-dir-mode-map (kbd "H") 'kdb-vc-dir-toggle-all)
    (define-key vc-dir-mode-map (kbd "M") 'kdb-vc-dir-show-only-modified)
    (define-key vc-dir-mode-map (kbd "c") 'kdb-vc-dir-quick-commit)
    (define-key vc-dir-mode-map (kbd "a") 'kdb-vc-dir-mark-same-state)
    (define-key vc-dir-mode-map (kbd "s") 'kdb-vc-dir-summary)
    (define-key vc-dir-mode-map (kbd "P") 'vc-push)
    (define-key vc-dir-mode-map (kbd "F") 'vc-update)
    (define-key vc-dir-mode-map (kbd "b") 'vc-switch-branch)
    (define-key vc-dir-mode-map (kbd "l") 'vc-print-log)
    (define-key vc-dir-mode-map (kbd "=") 'vc-diff)
    (define-key vc-dir-mode-map (kbd "g") 'revert-buffer)
    (define-key vc-dir-mode-map (kbd "r") 'kdb-vc-revert-file)
    (define-key vc-dir-mode-map (kbd "k") 'vc-dir-delete-file)
    (define-key vc-dir-mode-map (kbd "!") 'vc-dir-ignore)
    ;; New diff-related keybindings
    (define-key vc-dir-mode-map (kbd "d") 'vc-diff)
    (define-key vc-dir-mode-map (kbd "D") 'vc-root-diff)
    (define-key vc-dir-mode-map (kbd "SPC") 'vc-dir-mark)
    (define-key vc-dir-mode-map (kbd "?") 'kdb-vc-dir-help)
    (define-key vc-dir-mode-map (kbd "C") 'kdb-vc-resolve-conflicts)
    (define-key vc-dir-mode-map (kbd "B") 'kdb-vc-browse-remote))
  
  ;; Auto-refresh and enhance display
  ;; g keeps vc-dir's own refresh; s gives the summary once the status is in.
  (add-hook 'vc-dir-mode-hook
            (lambda ()
              (hl-line-mode 1)
              (display-line-numbers-mode -1))))  ; Cleaner without line numbers

;; Better commit message interface
(setq log-edit-confirm 'changed
      log-edit-keep-buffer nil
      log-edit-require-final-newline t
      log-edit-setup-add-author nil)

;; Keybinding is set in init.el via autoload
;; (global-set-key (kbd "C-c g") 'kdb-vc-transient)

(provide 'init-vc)
;;; init-vc.el ends here
