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

;;; claude-packages-test.el ends here
