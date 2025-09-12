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
        vc-handled-backends '(Git)
        vc-git-diff-switches '("--histogram" "--patience" "--no-prefix")
        vc-git-print-log-follow t
        vc-git-log-edit-summary-target-len 50
        vc-git-log-edit-summary-max-len 70
        vc-git-log-switches '("--graph" 
                              "--pretty=format:%h %s" 
                              "--abbrev-commit"
                              "--date=short"))
  
  (defun kdb-vc-diff-enhanced (&optional historic not-urgent)
    "Enhanced vc-diff with better visual presentation."
    (interactive "P")
    (call-interactively 'vc-diff)
    (when (get-buffer "*vc-diff*")
      (with-current-buffer "*vc-diff*"
        (diff-mode)
        (setq-local truncate-lines t)
        (when diff-refine
          (diff-refine-hunk))
        (goto-char (point-min))
        (when (re-search-forward "^@@" nil t)
          (beginning-of-line)))))
  
  (add-hook 'vc-diff-finish-functions
            (lambda ()
              (when (string-match-p "\\*vc-diff\\*" (buffer-name))
                (setq-local truncate-lines t)
                (when diff-refine
                  (diff-refine-hunk)))))
  
  (defun kdb-vc-log-next-commit ()
    "Navigate to next commit in log."
    (interactive)
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at "^\\*\\s-+[0-9a-f]\\{7,\\}")))
      (forward-line 1))
    (beginning-of-line))
  
  (defun kdb-vc-log-prev-commit ()
    "Navigate to previous commit in log."
    (interactive)
    (forward-line -1)
    (while (and (not (bobp))
                (not (looking-at "^\\*\\s-+[0-9a-f]\\{7,\\}")))
      (forward-line -1))
    (beginning-of-line))
  
  (defun kdb-vc-log-enhanced ()
    "Enhanced vc-log with better colors and navigation."
    (interactive)
    (call-interactively 'vc-print-log)
    (when (get-buffer "*vc-change-log*")
      (with-current-buffer "*vc-change-log*"
        (local-set-key (kbd "n") 'kdb-vc-log-next-commit)
        (local-set-key (kbd "p") 'kdb-vc-log-prev-commit)
        (local-set-key (kbd "TAB") 'kdb-vc-log-next-commit)
        (local-set-key (kbd "<backtab>") 'kdb-vc-log-prev-commit)
        (font-lock-add-keywords
         nil
         '(;; Branch merge/split indicators (|/ or |\)
           ("^\\(|[/\\\\]\\)\\s-*$" (0 'font-lock-warning-face t))
           ;; Branch lines with commits (| * hash)
           ("^\\(|\\)\\s-\\*" (1 'font-lock-warning-face t))
           ;; Vertical lines alone
           ("^\\(|\\)\\s-*$" (1 'font-lock-comment-face t))
           ;; Commit hash
           ("\\b\\([0-9a-f]\\{7,\\}\\)\\b" (1 'font-lock-type-face t))
           ;; Commit message after hash
           ("[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face t)))
         t)
        (font-lock-fontify-buffer))))
  
  )

;; Log-view configuration
(use-package log-view
  :ensure nil
  :after vc
  :config
  ;; Match git log --graph --oneline format
  (setq log-view-message-re "^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)\\s-+\\(.*\\)$"
        log-view-file-re "^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)"
        log-view-font-lock-keywords
        '(("^\\*\\s-+\\([0-9a-f]\\{7,\\}\\)" (1 'log-view-message))
          ("^\\*\\s-+[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face))))
  
  ;; Make commit hashes clickable and copyable
  (defun kdb-make-hashes-clickable ()
    "Make commit hashes clickable and copyable."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" nil t)
        (let ((hash (match-string 1))
              (start (match-beginning 1))
              (end (match-end 1)))
          (put-text-property start end 'mouse-face 'highlight)
          (put-text-property start end 'help-echo "Click: show commit details | Right-click: copy hash")
          (put-text-property start end 'keymap 
                             (let ((map (make-sparse-keymap)))
                               (define-key map [mouse-1] 
                                 (lambda (e) (interactive "e") 
                                   (kdb-vc-commit-show hash)))
                               (define-key map [mouse-3] 
                                 (lambda (e) (interactive "e") 
                                   (kill-new hash) 
                                   (message "Copied hash: %s" hash)))
                               (define-key map (kbd "RET") 
                                 (lambda () (interactive) 
                                   (kdb-vc-commit-show hash)))
                               (define-key map (kbd "c") 
                                 (lambda () (interactive) 
                                   (kill-new hash) 
                                   (message "Copied hash: %s" hash)))
                               map))))))
  
  ;; Add branch styling font-lock patterns
  (defun kdb-add-log-styling ()
    "Add font-lock patterns for branch indicators and styling."
    (font-lock-add-keywords
     nil
     '(;; Branch merge/split indicators on their own line
       ("^\\(|[/\\\\]\\|[/\\\\]|\\)\\s-*$" . 'font-lock-warning-face)
       ;; Branch lines with commits (| * hash message)
       ("^\\(|\\)\\s-\\*" (1 'font-lock-warning-face))
       ;; Vertical branch lines alone
       ("^\\(|\\)\\s-*$" . 'font-lock-comment-face)
       ;; Commit hash (7+ hex characters)
       ("\\b\\([0-9a-f]\\{7,\\}\\)\\b" (1 'log-view-message))
       ;; Everything after the hash is the commit message
       ("[0-9a-f]\\{7,\\}\\s-+\\(.*\\)$" (1 'font-lock-string-face))
       ;; Branch/tag names in parentheses
       ("(\\([^)]+\\))" (1 'font-lock-keyword-face))
       ;; Merge indicators
       ("\\<Merge:\\s-" . 'font-lock-builtin-face)
       ;; Issue numbers
       ("#[0-9]+" . 'font-lock-constant-face))
     t))
  
  ;; Enhanced display settings
  (add-hook 'log-view-mode-hook
            (lambda ()
              (setq-local truncate-lines t)
              (hl-line-mode 1)
              ;; Ensure proper mode setup
              (setq-local log-view-per-file-logs nil)
              (setq-local log-view-message-face 'log-view-message)
              ;; Apply styling and make hashes clickable
              (kdb-add-log-styling)
              (font-lock-fontify-buffer)
              (kdb-make-hashes-clickable)))
  
  ;; Also apply to vc-git-log-view-mode
  (add-hook 'vc-git-log-view-mode-hook
            (lambda ()
              (kdb-add-log-styling)
              (font-lock-fontify-buffer)
              (kdb-make-hashes-clickable)))
  
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
    (let ((buf (get-buffer-create "*Commit Diff*")))
      (with-current-buffer buf
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
          (insert (shell-command-to-string 
                   (format "git show --stat --patch %s" 
                          (shell-quote-argument hash))))
          (goto-char (point-min))))
      ;; Display in side window
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 0.5)))))
  
  (defun kdb-vc-commit-show-stat-in-buffer (hash)
    "Show commit stats for HASH in a side buffer."
    (let ((buf (get-buffer-create "*Commit Info*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (shell-command-to-string 
                   (format "git show --stat --format=fuller %s" 
                          (shell-quote-argument hash))))
          (goto-char (point-min))
          (diff-mode)))
      ;; Display in side window
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 0.5)))))
  
  ;; Navigation in commit diff buffer
  (defun kdb-commit-diff-show-next ()
    "Show diff for next commit."
    (interactive)
    (when (bound-and-true-p kdb-current-commit-hash)
      (let ((next-hash (string-trim 
                       (shell-command-to-string 
                        (format "git rev-list --reverse HEAD..HEAD~100 | grep -A1 %s | tail -1"
                               kdb-current-commit-hash)))))
        (if (and next-hash (not (string-empty-p next-hash)))
            (kdb-vc-commit-show-diff-in-buffer next-hash)
          (message "No next commit")))))
  
  (defun kdb-commit-diff-show-previous ()
    "Show diff for previous commit."
    (interactive)
    (when (bound-and-true-p kdb-current-commit-hash)
      (let ((prev-hash (string-trim 
                       (shell-command-to-string 
                        (format "git rev-parse %s^" kdb-current-commit-hash)))))
        (if (and prev-hash (not (string-empty-p prev-hash)))
            (kdb-vc-commit-show-diff-in-buffer prev-hash)
          (message "No previous commit")))))
  
  ;; Add keybindings to log-view-mode
  (with-eval-after-load 'log-view
    (define-key log-view-mode-map (kbd "d") 'kdb-log-view-show-commit-diff)
    (define-key log-view-mode-map (kbd "D") 'kdb-log-view-show-commit-stat)
    (define-key log-view-mode-map (kbd "RET") 'kdb-log-view-show-commit-diff)
    (define-key log-view-mode-map (kbd "=") 'kdb-log-view-show-commit-diff)
    (define-key log-view-mode-map (kbd "s") 'kdb-log-view-show-commit-stat))
  
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

(use-package ssh-agency
  :if (eq system-type 'windows-nt)
  :vc (:url "https://github.com/magit/ssh-agency" :rev :newest))


;; VC-msg - Show commit messages for current line
(use-package vc-msg
  :ensure t
  :config
  ;; Customize popup appearance - cleaner style
  (setq vc-msg-show-author t
        vc-msg-time-format "%Y-%m-%d %H:%M"
        vc-msg-persist-popup t)
  
  ;; Use built-in vc backend
  (setq vc-msg-git-show-commit-function 'vc-msg-git-show-commit-internal)
  
  ;; Remove box/border from popup
  (add-hook 'vc-msg-mode-hook
            (lambda ()
              (when (facep 'vc-msg-face)
                (set-face-attribute 'vc-msg-face nil
                                    :box nil
                                    :background "#232A2C"  ; uwu-bright-black
                                    :foreground "#C5C8C9")))) ; uwu-fg
  
  ;; Available in VC transient menu as 'm'
  )

;; VC Transient Menu - provides Magit-like interface for VC
(defun kdb-vc-transient ()
  "Custom VC transient menu with Magit-like keybindings."
  (interactive)
  (require 'transient)
  (transient-define-prefix kdb-vc-menu ()
    "VC operations menu"
    [["Status & Diff"
      ("s" "Status" vc-dir)
      ("d" "Diff (unstaged)" kdb-vc-diff-enhanced)
      ("D" "Diff vs Branch" vc-root-diff)
      ("=" "Basic Diff" vc-diff)
      ("e" "Ediff" vc-ediff)
      ("E" "Ediff Revision" vc-version-ediff)]
     
     ["Log & History"
      ("l" "Log (current file)" kdb-vc-log-enhanced)
      ("L" "Log (branch)" vc-print-root-log)
      ("h h" "File History" vc-print-root-log)
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
      ("F" "Fetch" vc-update)
      ("p" "Push" vc-push)
      ("P" "Push Specific" vc-push)
      ""
      "-Management-"
      ("r l" "List" kdb-vc-remote-list)
      ("r a" "Add" kdb-vc-remote-add)
      ("r u" "Set URL" kdb-vc-remote-set-url)
      ("C" "Clone" kdb-vc-clone)]]
    
    [["Changes"
      ("a" "Add/Stage" vc-register)
      ("c" "Commit" vc-next-action)
      ("C-c" "Amend" kdb-vc-git-amend-commit)
      ("u" "Revert" vc-revert)
      ("U" "Checkout" vc-revert-file)
      ("k" "Delete" vc-delete-file)
      ("R" "Rename" vc-rename-file)]
     
     ["Stash"
      ("z z" "Stash" kdb-vc-git-stash)
      ("z s" "Stash Message" kdb-vc-git-stash-push)
      ("z a" "Apply" kdb-vc-git-stash-apply)
      ("z p" "Pop" kdb-vc-git-stash-pop)
      ("z l" "List" kdb-vc-git-stash-list)]
     
     ["Tags & Info"
      ("t" "Create Tag" vc-create-tag)
      ("T" "List Tags" kdb-vc-git-list-tags)
      ("i" "Init Repo" vc-create-repo)
      ("!" "Git Command" kdb-vc-git-command)
      ""
      ("q" "Quit" transient-quit-one)]])
  (kdb-vc-menu))

;; Custom VC helper functions
(defun kdb-vc-remote-set-url ()
  "Set remote URL for current repository."
  (interactive)
  (let* ((remote (read-string "Remote name (default: origin): " "origin"))
         (url (read-string "New URL: ")))
    (when (and remote url (not (string-empty-p url)))
      (shell-command (format "git remote set-url %s %s" remote url))
      (message "Remote '%s' URL updated to: %s" remote url))))

(defun kdb-vc-remote-add ()
  "Add a new remote to the current repository."
  (interactive)
  (let* ((remote (read-string "Remote name: "))
         (url (read-string "Remote URL: ")))
    (when (and remote url (not (string-empty-p url)) (not (string-empty-p remote)))
      (shell-command (format "git remote add %s %s" remote url))
      (message "Remote '%s' added with URL: %s" remote url))))

(defun kdb-vc-git-amend-commit ()
  "Amend the last commit."
  (interactive)
  (if (vc-git-conflicted-files default-directory)
      (error "Cannot amend while there are unresolved conflicts"))
  (let ((last-msg (shell-command-to-string "git log -1 --pretty=%B")))
    (vc-git-checkin nil nil t last-msg t)))

(defun vc-revert-file ()
  "Revert the current file to the last committed version."
  (interactive)
  (when (vc-backend buffer-file-name)
    (if (yes-or-no-p (format "Revert %s to last committed version? " 
                             (file-name-nondirectory buffer-file-name)))
        (progn
          (vc-revert buffer-file-name)
          (revert-buffer t t)
          (message "File reverted to last committed version"))
      (message "Revert cancelled"))))

(defun kdb-vc-delete-branch ()
  "Delete a git branch."
  (interactive)
  (let* ((branches (split-string 
                    (shell-command-to-string "git branch --format='%(refname:short)'") 
                    "\n" t))
         (branch (completing-read "Delete branch: " branches)))
    (when (yes-or-no-p (format "Delete branch '%s'? " branch))
      (shell-command (format "git branch -d %s" branch))
      (message "Branch '%s' deleted" branch))))

(defun kdb-vc-rename-branch ()
  "Rename the current git branch."
  (interactive)
  (let* ((current-branch (string-trim (shell-command-to-string "git branch --show-current")))
         (new-name (read-string (format "Rename branch '%s' to: " current-branch))))
    (when (and new-name (not (string-empty-p new-name)))
      (shell-command (format "git branch -m %s" new-name))
      (message "Branch renamed from '%s' to '%s'" current-branch new-name))))

(defun kdb-vc-git-stash ()
  "Stash current changes."
  (interactive)
  (shell-command "git stash")
  (message "Changes stashed")
  (when (derived-mode-p 'vc-dir-mode)
    (vc-dir-refresh)))

(defun kdb-vc-git-stash-push ()
  "Create a new stash with a custom message."
  (interactive)
  (let ((message (read-string "Stash message: ")))
    (if (string-empty-p message)
        (shell-command "git stash push")
      (shell-command (format "git stash push -m %s" (shell-quote-argument message))))
    (message "Changes stashed")
    (when (derived-mode-p 'vc-dir-mode)
      (vc-dir-refresh))))

(defun kdb-vc-git-stash-apply ()
  "Apply a stash from the list."
  (interactive)
  (let* ((stashes (shell-command-to-string "git stash list"))
         (stash-list (split-string stashes "\n" t))
         (stash (when stash-list
                  (completing-read "Apply stash: " stash-list nil t))))
    (if stash
        (let ((stash-num (car (split-string stash ":"))))
          (shell-command (format "git stash apply %s" stash-num))
          (message "Stash applied: %s" stash-num)
          (when (derived-mode-p 'vc-dir-mode)
            (vc-dir-refresh)))
      (message "No stashes available"))))

(defun kdb-vc-git-stash-pop ()
  "Pop a stash from the list."
  (interactive)
  (let* ((stashes (shell-command-to-string "git stash list"))
         (stash-list (split-string stashes "\n" t))
         (stash (when stash-list
                  (completing-read "Pop stash: " stash-list nil t))))
    (if stash
        (let ((stash-num (car (split-string stash ":"))))
          (shell-command (format "git stash pop %s" stash-num))
          (message "Stash popped: %s" stash-num)
          (when (derived-mode-p 'vc-dir-mode)
            (vc-dir-refresh)))
      (message "No stashes available"))))

(defun kdb-vc-git-stash-list ()
  "List all stashes."
  (interactive)
  (let ((stashes (shell-command-to-string "git stash list")))
    (if (string-empty-p (string-trim stashes))
        (message "No stashes found")
      (with-current-buffer (get-buffer-create "*Git Stashes*")
        (erase-buffer)
        (insert "Git Stashes:\n\n")
        (insert stashes)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

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
  (let* ((branches (split-string 
                    (shell-command-to-string "git branch -a --format='%(refname:short)'") 
                    "\n" t))
         (branch (completing-read "Branch: " branches))
         (files (split-string 
                 (shell-command-to-string (format "git ls-tree -r --name-only %s" branch))
                 "\n" t))
         (file (completing-read "File: " files)))
    (when file
      (let ((content (shell-command-to-string 
                      (format "git show %s:%s" branch file))))
        (with-current-buffer (get-buffer-create (format "*%s:%s*" branch file))
          (erase-buffer)
          (insert content)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))))))

(defun kdb-vc-git-command ()
  "Run an arbitrary git command."
  (interactive)
  (let* ((command (read-string "Git command (without 'git'): "))
         (full-command (format "git %s" command))
         (output (shell-command-to-string full-command)))
    (if (string-empty-p (string-trim output))
        (message "Git command completed: %s" command)
      (with-current-buffer (get-buffer-create "*Git Output*")
        (erase-buffer)
        (insert (format "$ %s\n\n" full-command))
        (insert output)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;; Commit hash lookup functions
(defun kdb-vc-commit-show (hash)
  "Show detailed information about a commit hash."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --stat --pretty=fuller %s" (shell-quote-argument hash)))))
    (with-current-buffer (get-buffer-create "*Commit Details*")
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun kdb-vc-commit-diff (hash)
  "Show diff for a specific commit HASH."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --format= %s" (shell-quote-argument hash)))))
    (with-current-buffer (get-buffer-create "*Commit Diff*")
      (erase-buffer)
      (insert output)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun kdb-vc-commit-stats (hash)
  "Show stats for a specific commit HASH."
  (interactive "sCommit hash: ")
  (let ((output (shell-command-to-string 
                 (format "git show --stat --format='%%h %%s%%n%%aD %%an' %s" (shell-quote-argument hash)))))
    (message "%s" (string-trim output))))

(defun kdb-vc-clone ()
  "Clone a repository. Requires Emacs 29.1 or later."
  (interactive)
  (if (>= emacs-major-version 31)
      (if (fboundp 'vc-clone)
          (call-interactively 'vc-clone)
        (message "vc-clone not available in this Emacs build"))
    (message "vc-clone requires Emacs 31.1 or later. Current version: %s" emacs-version)))

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
  (setq vc-dir-hide-up-to-date t           ; Hide unchanged files by default
        vc-dir-hide-unregistered nil       ; Show untracked files
        vc-stay-local t                    ; Faster for remote repos
        vc-directory-exclusion-list '(".git" ".hg" ".svn" "node_modules" ".venv"))

  ;; === MERGE CONFLICT RESOLUTION === ;;
  
  ;; Smerge mode for handling merge conflicts
  (use-package smerge-mode
    :ensure nil  ; Built-in
    :hook (find-file . smerge-mode)
    :bind (:map smerge-mode-map
           ("C-c m n" . smerge-next)         ; Next conflict
           ("C-c m p" . smerge-prev)         ; Previous conflict
           ("C-c m a" . smerge-keep-mine)    ; Accept mine (ours)
           ("C-c m b" . smerge-keep-other)   ; Accept theirs  
           ("C-c m m" . smerge-keep-all)     ; Merge both
           ("C-c m c" . smerge-keep-current) ; Keep current
           ("C-c m e" . smerge-ediff)        ; Launch ediff
           ("C-c m r" . smerge-resolve)      ; Auto-resolve or mark resolved
           ("C-c m R" . smerge-refine))      ; Refine highlighting
    :config
    ;; Automatically enable smerge-mode for files with conflict markers
    (defun enable-smerge-maybe ()
      "Auto-enable smerge-mode when merge conflict markers are detected."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< \\|^=======$\\|^>>>>>>> " nil t)
          (smerge-mode 1))))
    (add-hook 'find-file-hook 'enable-smerge-maybe)
    
    ;; Better conflict highlighting
    (set-face-background 'smerge-refined-added "#22aa22")
    (set-face-background 'smerge-refined-removed "#aa2222"))

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

  (defun kdb-vc-copy-current-branch ()
    "Copy the current branch name to clipboard."
    (interactive)
    (let ((branch (string-trim (shell-command-to-string "git branch --show-current"))))
      (kill-new branch)
      (message "Copied branch: %s" branch)))

  (defun kdb-vc-copy-file-path ()
    "Copy the repository-relative path of current file."
    (interactive)
    (let ((filepath (file-relative-name buffer-file-name (vc-root-dir))))
      (kill-new filepath)
      (message "Copied: %s" filepath)))

  ;; Function to open all conflict files
  (defun kdb-vc-resolve-conflicts ()
    "Open all files with merge conflicts for resolution."
    (interactive)
    (let* ((conflicts (split-string 
                       (shell-command-to-string 
                        "git diff --name-only --diff-filter=U") "\n" t))
           (conflict-count (length conflicts)))
      (if (= conflict-count 0)
          (message "No merge conflicts found!")
        (message "Opening %d conflict files..." conflict-count)
        (dolist (file conflicts)
          (find-file file))
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
            (vc-dir-mark-all-files)
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
  (defun kdb-vc-dir-quick-commit ()
    "Quick commit with a simple message prompt."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
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
      (message "📊 Modified: %d | Added: %d | Removed: %d | Untracked: %d" 
               modified added removed unregistered)))
  
  ;; Show diff for file at point or all marked files
  (defun kdb-vc-dir-show-diff ()
    "Show diff for the file at point or all marked files in a side window."
    (interactive)
    (let* ((files (or (vc-dir-marked-files)
                      (when (vc-dir-current-file)
                        (list (vc-dir-current-file)))))
           (buf (get-buffer-create "*vc-dir-diff*")))
      (if (not files)
          (message "No file at point or marked files")
        ;; Simpler approach: just call vc-diff and redirect output
        (let* ((root (vc-dir-root))
               (full-files (mapcar (lambda (f) 
                                    (expand-file-name f root)) 
                                  files)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (diff-mode)
              ;; Run git diff directly to avoid VC overhead
              (insert (shell-command-to-string 
                       (format "cd %s && git diff HEAD -- %s" 
                               (shell-quote-argument root)
                               (mapconcat 'shell-quote-argument files " "))))
              (goto-char (point-min))))
          ;; Display in side window
          (display-buffer buf
                          '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.5)))))))
  
  (defun kdb-vc-dir-show-diff-quietly ()
    "Show diff without triggering refreshes."
    (let* ((files (or (vc-dir-marked-files)
                      (when (vc-dir-current-file)
                        (list (vc-dir-current-file)))))
           (buf (get-buffer-create "*vc-dir-diff*")))
      (when files
        (let ((inhibit-message t)  ; Suppress messages
              (root (vc-dir-root)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (diff-mode)
              ;; Run git diff directly - much faster and no VC overhead
              (insert (shell-command-to-string 
                       (format "cd %s && git diff HEAD -- %s 2>/dev/null" 
                               (shell-quote-argument root)
                               (mapconcat 'shell-quote-argument files " "))))
              (goto-char (point-min))))
          ;; Display in side window quietly
          (when (> (buffer-size buf) 0)
            (display-buffer buf
                            '((display-buffer-in-side-window)
                              (side . right)
                              (window-width . 0.5))))))))
  
  ;; Show all diffs at once (like git diff)
  (defun kdb-vc-dir-show-all-diffs ()
    "Show all uncommitted changes in a single diff buffer."
    (interactive)
    (let ((buf (get-buffer-create "*vc-all-diffs*"))
          (backend (vc-backend default-directory)))
      (if (not backend)
          (message "No version control backend found")
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (vc-diff-internal backend nil nil nil t buf)
        (pop-to-buffer buf))))
  
  ;; Quick stage/unstage with diff preview
  
  ;; Enhanced display with icons
  (defun kdb-vc-dir-prettify ()
    "Add icons and better formatting to vc-dir."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(edited\\)" nil t)
        (replace-match "modified" nil nil nil 1))
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(added\\)" nil t)
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face '(:foreground "#6BB05D" :weight bold)))
      (goto-char (point-min))
      (while (re-search-forward "^[ *][ *]\\s-+\\(removed\\)" nil t)
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face '(:foreground "#F65B5B" :weight bold)))))
  
  ;; Help function
  (defun kdb-vc-dir-help ()
    "Show vc-dir keybinding help."
    (interactive)
    (message "VC-Dir: [d]iff [D]iff-all [SPC]mark [a]mark-same-type [c]ommit [C]onflicts [h/x]hide [s]ummary"))
  
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
  (add-hook 'vc-dir-mode-hook
            (lambda ()
              (auto-revert-mode 1)
              (hl-line-mode 1)
              (display-line-numbers-mode -1)  ; Cleaner without line numbers
              (kdb-vc-dir-prettify)             ; Apply prettification
              (setq-local revert-buffer-function
                          (lambda (_ignore-auto _noconfirm)
                            (vc-dir-refresh)
                            (kdb-vc-dir-prettify)))
              ;; Show summary on startup
              (run-at-time 0.1 nil 'kdb-vc-dir-summary))))

;; Additional VC customizations for better experience
(with-eval-after-load 'vc
  ;; Faster git operations
  (setq vc-git-diff-switches '("--histogram" "--color=never")
        vc-suppress-confirm t  ; Don't ask for confirmation on every operation
        vc-command-messages t)  ; Show VC command messages
  
  ;; Better commit message interface
  (setq log-edit-confirm 'changed  ; Only confirm if buffer changed
        log-edit-keep-buffer nil  ; Don't keep log buffer after commit
        log-edit-require-final-newline t
        log-edit-setup-add-author nil))

(global-set-key (kbd "C-c g") 'kdb-vc-transient)

(provide 'init-vc)
;;; init-vc.el ends here
