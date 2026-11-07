;;; init-claude-packages.el --- Claude in Emacs: packages, keys, background -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded at startup by init.el, so it stays small: agent-shell and its
;; companions, configured to use the commands in init-claude-workflow.el,
;; init-claude.el and init-claude-print.el, which load on first use.

;;; Code:

;; agent-shell runs Claude Code over ACP using the `claude' login (Max).
;; Needs node: npm install -g @agentclientprotocol/claude-agent-acp

;; Before any use-package below: installing a :vc package compiles it,
;; which loads agent-shell and runs its :config mid-install.
(autoload 'kdb-claude-agent-shell-subdir "init-claude")
(dolist (fn '(kdb-claude--capture-permission kdb-claude--track-permissions kdb-claude--notify
              kdb-claude--start-with kdb-claude-spawn kdb-claude-message
              kdb-claude-show kdb-claude-run kdb-claude-output kdb-claude-cleanup
              kdb-claude--rename-keeps-tab kdb-claude--permission-message
              kdb-claude--permission-title
              kdb-claude--tag-client kdb-claude--render-markdown
              kdb-claude--track-artifacts kdb-claude--track-emacs-session
              kdb-claude--header-symbol-fonts kdb-claude--tab-name-waiting
              kdb-claude--redraw-tabs kdb-claude--tab-save kdb-claude--tab-restore
              kdb-claude--busy-frames kdb-claude--logo-icon kdb-claude--logo-header
              kdb-claude--steady-busy-frame kdb-claude--heartbeat-rate kdb-claude--display-in-own-tab
              kdb-claude--close-session-window))
  (autoload fn "init-claude-workflow"))
(dolist (spec '(("init-claude-workflow" kdb-claude kdb-claude-new
                 kdb-claude-send kdb-claude-start kdb-claude-answer kdb-claude-review
                 kdb-claude-watch-pr kdb-claude-schedule kdb-claude-run-block
                 kdb-claude-reply-to-org kdb-claude-usage kdb-claude-open-artifact
                 kdb-claude-copy-session-id kdb-claude-remote-control kdb-claude-queue-edit
                 kdb-claude-set-busy-indicator kdb-claude-start-model
                 kdb-claude-reveal-artifact kdb-reveal-in-file-manager kdb-claude-scratch kdb-claude-new-in-folder
                 kdb-claude-review-pr)
                ("init-claude-print" kdb-claude-print)
                ("init-claude" kdb-claude-sync kdb-claude-sync-code-sessions
                 kdb-claude-search kdb-claude-import-export
                 kdb-claude-ask-archive kdb-claude-resume-archived kdb-claude-history kdb-claude-recent kdb-claude-transcript
                 kdb-claude-setup)))
  (dolist (fn (cdr spec))
    (autoload fn (car spec) nil t)))
;; agent-shell's queue commands, on the C-c l menu before any session runs.
(dolist (fn '(agent-shell-prompt-queue agent-shell-prompt-queue-remove agent-shell-prompt-queue-resume))
  (autoload fn "agent-shell" nil t))

(use-package agent-shell
  :ensure t
  :commands (agent-shell-anthropic-start-claude-code agent-shell-send-file-to)
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        ;; M-x agent-shell asks which agent; C-c l starts Claude directly.
        agent-shell-preferred-agent-config nil
        ;; Claude decides what needs asking; C-c C-m switches mode
        ;; (e.g. bypass) per session.
        agent-shell-anthropic-default-session-mode-id "auto"
        ;; Code block labels: RET copies (built in), x runs the block (shell
        ;; blocks in the project's shell, others in org babel), e edits first.
        agent-shell-markdown-render-function #'kdb-claude--render-markdown
        ;; Resume from the history list (C-c l h), not a picker on every start.
        agent-shell-session-strategy 'new
        ;; Each session in a tab of its own, named after it.
        agent-shell-display-action '((kdb-claude--display-in-own-tab display-buffer-same-window))
        ;; Keep conversation data out of projects and this repo: the private
        ;; archive already has every transcript, so agent-shell's copy is off.
        agent-shell-transcript-file-path-function nil
        ;; ...and don't offer to save one when a session is killed.
        shell-maker-prompt-before-killing-buffer nil
        agent-shell-dot-subdir-function #'kdb-claude-agent-shell-subdir
        shell-maker-root-path (kdb-claude-agent-shell-subdir "")
        ;; Queue permission requests so C-c ' can answer them from anywhere.
        agent-shell-permission-responder-function #'kdb-claude--capture-permission
        ;; The adapter logs to stderr unless given a log dir, and acp.el
        ;; reports any stderr as an error, which aborts agent-review.
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "CLAUDE_AGENT_LOGS" (expand-file-name "claude-agent-logs" temporary-file-directory)))
  ;; The default, ImageMagick's import, is often missing on Linux; maim
  ;; (-s: drag a region) where installed.  macOS/Windows keep the default.
  (when (and (eq system-type 'gnu/linux) (executable-find "maim"))
    (setq agent-shell-screenshot-command '("maim" "-s")))
  (add-hook 'agent-shell-mode-hook #'kdb-claude--track-permissions)
  (add-hook 'agent-shell-mode-hook #'kdb-claude--track-artifacts)
  (add-hook 'agent-shell-mode-hook #'kdb-claude--track-emacs-session)
  (add-hook 'agent-shell-mode-hook #'kdb-claude--close-session-window)
  (setq agent-shell-busy-indicator-frames #'kdb-claude--busy-frames)
  (advice-add 'agent-shell--busy-indicator-frame :filter-return #'kdb-claude--steady-busy-frame)
  (advice-add 'agent-shell-heartbeat-make :filter-args #'kdb-claude--heartbeat-rate)
  (advice-add 'rename-buffer :around #'kdb-claude--rename-keeps-tab)
  (advice-add 'agent-shell--permission-title :around #'kdb-claude--permission-title)
  (advice-add 'agent-shell--fetch-agent-icon :before-until #'kdb-claude--logo-icon)
  (dolist (fn '(agent-shell--make-header-model agent-shell--header-model-refresh-frame))
    (advice-add fn :filter-return #'kdb-claude--logo-header))
  (when (eq system-type 'windows-nt)
    (advice-add 'agent-shell--make-header-model :filter-return #'kdb-claude--header-symbol-fonts))
  ;; Each session knows its id (KDB_CLAUDE_SESSION) for kdb-claude-message.
  (advice-add 'agent-shell-anthropic-make-claude-client :around #'kdb-claude--tag-client))

;; Peek: sessions grouped by project with live status; RET switches.
;; Label: Claude names the session.  (hq's HUD needs persp-mode; unused.)
(use-package agent-shell-hq
  :vc (:url "https://github.com/SreenivasVRao/agent-shell-hq" :rev :newest)
  :commands (agent-shell-hq-peek agent-shell-hq-label))

;; Second-opinion review of uncommitted changes; send findings back with S.
(use-package agent-review
  :vc (:url "https://github.com/nineluj/agent-review" :rev :newest)
  :commands agent-review)

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el" :rev :newest)
  :after agent-shell
  :demand t
  :config
  (setq agent-shell-attention-notify-function #'kdb-claude--notify)
  ;; A marker on each tab with a session waiting for you.
  (add-to-list 'tab-bar-tab-name-format-functions #'kdb-claude--tab-name-waiting t)
  ;; A marked tab still counts as a tab, so it's sized like the others
  ;; (Emacs 30 goes by the face its name starts with).
  (dolist (face '(kdb-claude-tab-input kdb-claude-tab-done kdb-claude-tab-busy))
    (add-to-list 'tab-bar-auto-width-faces face t))
  (advice-add 'agent-shell-attention--update-indicator :after #'kdb-claude--redraw-tabs)
  (advice-add 'agent-shell-attention--message :around #'kdb-claude--permission-message)
  (agent-shell-attention-mode 1))

(global-set-key (kbd "C-c l") #'kdb-claude)
(global-set-key (kbd "C-c ;") #'agent-shell-hq-peek)
(global-set-key (kbd "C-c '") #'kdb-claude-answer)

;; Session transcripts stay out of recent files (and so out of var/recentf).
;; By name: on Windows here `file-in-directory-p' takes a parent of the
;; archive, such as ~/ opened in Dired, to be inside it.
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (lambda (f)
                 (string-prefix-p (file-name-as-directory
                                   (expand-file-name (or (bound-and-true-p kdb-claude-archive-dir)
                                                         "~/.claude-archive")))
                                  (expand-file-name f)
                                  (memq system-type '(windows-nt cygwin darwin))))))

;; Keep the archive current so search/ask/resume see recent sessions.
;; The conversion runs in a background Emacs, so idle time never freezes.
(run-with-idle-timer 300 t (lambda ()
                             (require 'init-claude)
                             (when (kdb-claude--archive-repo-p)
                               (with-demoted-errors "Claude archive sync: %S"
                                 (kdb-claude-sync-code-sessions-async)))))

;; Code blocks run from Claude's replies (C-c l x), and babel's errors.
(add-to-list 'display-buffer-alist
             '("\\*\\(claude blocks\\|claude \\(bash\\|pwsh\\): .*\\|Org-Babel Error Output\\)\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)
               (slot . 0)))

;; Claude sessions come back with their tabs after a restart.
(with-eval-after-load 'tabspaces
  (tabspaces-register-buffer-kind 'claude #'kdb-claude--tab-save #'kdb-claude--tab-restore))

(provide 'init-claude-packages)
;;; init-claude-packages.el ends here
