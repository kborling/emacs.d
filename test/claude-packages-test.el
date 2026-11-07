;;; claude-packages-test.el --- Claude is wired up at startup -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-packages-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)

(ert-deftest claude-wired-up-at-startup ()
  ;; What lisp/init-claude-packages.el sets up, without loading the commands.
  (should (featurep 'init-claude-packages))
  (should (eq (key-binding (kbd "C-c l")) 'kdb-claude))
  (should (eq (key-binding (kbd "C-c '")) 'kdb-claude-answer))
  (should (fboundp 'kdb-claude-review-pr))
  (should (fboundp 'kdb-claude-queue-edit))
  (should (fboundp 'agent-shell-prompt-queue-remove))         ; on the menu before a session
  (should (featurep 'init-local-models))
  (should window-combination-resize)
  (should (eq (key-binding (kbd "C-c a s")) 'kdb-local-models-setup))
  (should (assoc "\\*\\(claude blocks\\|claude \\(bash\\|pwsh\\): .*\\|Org-Babel Error Output\\)\\*"
                 display-buffer-alist))
  (require 'agent-shell)
  (should (equal agent-shell-display-action
                 '((kdb-claude--display-in-own-tab display-buffer-same-window))))
  (should (equal agent-shell-anthropic-default-session-mode-id "auto"))
  (should (memq 'kdb-claude--tab-name-waiting tab-bar-tab-name-format-functions))
  (require 'tabspaces)
  (should (assq 'claude tabspaces--buffer-kind-handlers)))

(ert-deftest claude-archive-out-of-recent-files ()
  (require 'recentf)
  (let ((excluded (lambda (f) (seq-some (lambda (p) (and (functionp p) (funcall p f))) recentf-exclude)))
        (kdb-claude-archive-dir "~/.claude-archive"))
    (should (funcall excluded "~/.claude-archive/code-sessions/x.org"))
    (should-not (funcall excluded "~/"))                 ; a parent of the archive
    (should-not (funcall excluded "~/.claude-archive-notes.org"))))

(ert-deftest claude-menu-has-no-duplicates ()
  ;; Each command one key: none twice in C-c l, none also bound globally.
  (let* ((src (with-temp-buffer
                (insert-file-contents (expand-file-name "lisp/init-claude-workflow.el" user-emacs-directory))
                (goto-char (point-min))
                (search-forward "(transient-define-prefix kdb-claude-menu")
                (buffer-substring (point) (progn (backward-up-list) (forward-sexp) (point)))))
         (entries (let (found (pos 0))
                    (while (string-match "(\"\\([^\"]+\\)\" \"[^\"]*\" \\([^ )]+\\))" src pos)
                      (push (cons (match-string 1 src) (intern (match-string 2 src))) found)
                      (setq pos (match-end 0)))
                    found))
         (keys (mapcar #'car entries))
         (commands (mapcar #'cdr entries)))
    (should (> (length entries) 20))
    (should (equal (length keys) (length (delete-dups (copy-sequence keys)))))
    (should (equal (length commands) (length (delete-dups (copy-sequence commands)))))
    (should-not (memq 'kdb-claude-answer commands))            ; it's on C-c '
    (should (eq (key-binding (kbd "C-c '")) 'kdb-claude-answer))
    (should-not (key-binding (kbd "C-c ;")))))

(ert-deftest send-gives-this-file-when-nothing-is-selected ()
  (require 'agent-shell)
  (let ((file (make-temp-file "notes" nil ".txt" "line one\nline two\n")))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (let ((agent-shell-context-sources kdb-claude--context-sources))
        (let ((context (agent-shell--context)))
          (should (string-match-p (regexp-quote (file-name-nondirectory file)) context))
          (should-not (string-match-p "line one" context))))
      (kill-buffer))
    (delete-file file)))

;;; claude-packages-test.el ends here
