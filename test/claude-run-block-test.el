;;; claude-run-block-test.el --- Code blocks from replies to org babel -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-run-block-test.el \
;;   -f ert-run-tests-batch-and-exit

;; These open, close and reset tabs and windows, and kill buffers: loaded
;; into a working Emacs they wipe its tabs.
(unless noninteractive
  (user-error "Run %s in batch Emacs (emacs --batch), not in a live one"
              (file-name-nondirectory (or load-file-name buffer-file-name "these tests"))))
(require 'ert)
(require 'init-claude-workflow)
(require 'org)

(defconst test-reply
  "Try this:
```bash
echo \"it's $((1 + 2))\"
pwd
```
Or in elisp:
```elisp
(* 2 21)
```
and a plain one:
```
echo plain
cat <<'X'
* not an org heading
X
```
Done.")

(defun test-buffer ()
  (with-current-buffer (generate-new-buffer "*reply*")
    (insert test-reply)
    (setq default-directory (file-name-as-directory (make-temp-file "blockdir" t)))
    (current-buffer)))

(defun test-open (buf where)
  "Send the block chosen from WHERE in BUF to org; return the org buffer."
  (ignore-errors (kill-buffer "*claude blocks*"))
  (with-current-buffer buf
    (goto-char (point-min))
    (goto-char (funcall where))
    (kdb-claude-run-block))
  (get-buffer "*claude blocks*"))

(defun test-execute ()
  "C-c C-c the block at point in *claude blocks*; return its results."
  (with-current-buffer "*claude blocks*"
    (org-babel-execute-src-block)
    (org-babel-where-is-src-block-result)
    (save-excursion
      (goto-char (org-babel-where-is-src-block-result))
      (forward-line 1)
      (buffer-substring-no-properties (point) (point-max)))))

(defun test-shell-buffer (kind dir)
  (get-buffer (format "*claude %s: %s*" kind (file-name-nondirectory (directory-file-name dir)))))

(defun test-shell-output (buf regexp)
  "Wait until shell BUF shows REGEXP; return its text."
  (let ((end (+ (float-time) 10)))
    (while (and (not (with-current-buffer buf (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t))))
                (< (float-time) end))
      (accept-process-output nil 0.05)))
  (with-current-buffer buf (buffer-string)))

(defun test-kill-shells ()
  "Kill the block shells, and wait for them to end: Windows won't delete
a folder a shell is still in."
  (let (procs)
    (dolist (b (buffer-list))
      (when (string-match-p "\\`\\*claude \\(bash\\|pwsh\\): " (buffer-name b))
        (when-let* ((p (get-buffer-process b))) (push p procs))
        (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
    (let ((end (+ (float-time) 5)))
      (while (and (seq-some #'process-live-p procs) (< (float-time) end))
        (accept-process-output nil 0.05)))))

(ert-deftest run-block-finds-blocks ()
  (with-current-buffer (test-buffer)
    (should (equal (mapcar #'car (kdb-claude--code-blocks)) '("bash" "elisp" "")))
    (let ((b (car (kdb-claude--code-blocks))))
      (should (equal (buffer-substring-no-properties (nth 1 b) (nth 2 b))
                     "echo \"it's $((1 + 2))\"\npwd\n")))))

(ert-deftest run-block-shell-goes-to-project-shell ()
  ;; C-c l x on a bash block: at the prompt of the project's bash, in the
  ;; bottom panel, to edit; RET runs it there, in the session's directory.
  (test-kill-shells)
  (delete-other-windows)
  (let* ((buf (test-buffer))
         (dir (buffer-local-value 'default-directory buf)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (search-forward "pwd")
    (kdb-claude-run-block)
    (let ((shell (test-shell-buffer 'bash dir)))
      (should shell)
      (should (eq (window-buffer (selected-window)) shell))
      (should (window-parameter (selected-window) 'window-side))   ; the bottom panel
      (should (string-suffix-p "echo \"it's $((1 + 2))\"\npwd" (with-current-buffer shell (buffer-string))))
      (should-not (string-match-p "it's 3" (with-current-buffer shell (buffer-string))))   ; not run yet
      (with-current-buffer shell (comint-send-input))
      (let ((out (test-shell-output shell "it's 3")))
        (should (string-match-p "it's 3" out))
        ;; Git Bash prints /c/... for c:/..., so match the directory's own name.
        (should (string-match-p (regexp-quote (file-name-nondirectory (directory-file-name dir))) out)))))
  (test-kill-shells))

(ert-deftest run-block-elisp-and-history ()
  (let ((buf (test-buffer)))
    (test-open buf (lambda () (search-forward "(* 2 21)")))
    (should (string-match-p ": 42" (test-execute)))
    ;; A second block is appended after the first and its results.
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "(* 2 21)")
      (kdb-claude-run-block))
    (with-current-buffer "*claude blocks*"
      (should (= 2 (how-many "^#\\+begin_src" (point-min) (point-max))))
      (should (string-match-p ": 42" (buffer-string))))))

(ert-deftest run-block-shell-per-directory ()
  ;; Two projects both named "app" get a shell each, in their own folder.
  (test-kill-shells)
  (let* ((root (make-temp-file "twins" t))
         (a (file-name-as-directory (expand-file-name "x/app" root)))
         (b (file-name-as-directory (expand-file-name "y/app" root))))
    (make-directory a t)
    (make-directory b t)
    (let ((sa (kdb-claude--block-shell 'bash a))
          (sb (kdb-claude--block-shell 'bash b)))
      (should-not (eq sa sb))
      (should (eq sa (kdb-claude--block-shell 'bash a)))
      (kdb-claude--block-to-shell 'bash "pwd" b t)
      (should (string-match-p "y/app$" (test-shell-output sb "y/app$"))))
    (test-kill-shells)
    (delete-directory root t)))

(ert-deftest run-block-busy-shell-gets-one-beside-it ()
  ;; A command sent to a shell running a server would wait for it to end,
  ;; which it never does: a new shell instead.  An idle one is reused.
  (test-kill-shells)
  (let ((dir (file-name-as-directory (file-truename (make-temp-file "busy" t)))))
    ;; A builtin, so no child keeps the folder in use once bash is killed.
    (let ((first (kdb-claude--block-to-shell 'bash "read -t 30 _" dir t)))
      (test-shell-output first "read -t 30")
      (should-not (kdb-claude--shell-at-prompt-p first))
      (let ((second (kdb-claude--block-to-shell 'bash "echo beside" dir t)))
        (should-not (eq first second))
        (should (string-match-p "^beside" (test-shell-output second "^beside")))
        ;; Back at its prompt: reused.
        (let ((end (+ (float-time) 5)))
          (while (and (not (kdb-claude--shell-at-prompt-p second)) (< (float-time) end))
            (accept-process-output nil 0.05)))
        (should (eq second (kdb-claude--block-shell 'bash dir)))))
    (test-kill-shells)
    (delete-directory dir t)))

(ert-deftest claude-run-notes-each-step ()
  (let ((kdb-claude-run-log (make-temp-file "run-log")))
    (cl-letf (((symbol-function 'kdb-claude--in-session-tab) (lambda (_id fn &rest _) (funcall fn (current-buffer))))
              ((symbol-function 'kdb-claude--block-to-shell) (lambda (&rest _) (get-buffer-create " *test shell*")))
              ((symbol-function 'kdb-claude--use) #'ignore))
      (unwind-protect
          (progn
            (should (equal (kdb-claude-run "s1" "dotnet run --project Api" default-directory "pwsh")
                           " *test shell*"))
            (let ((log (with-temp-buffer (insert-file-contents kdb-claude-run-log) (buffer-string))))
              (should (string-match-p "s1: start dotnet run --project Api\n.*s1: in its tab\n.*s1: sent to  \\*test shell\\*\n.*s1: done after" log)))
            ;; A step that never comes back is the last line.
            (cl-letf (((symbol-function 'kdb-claude--block-to-shell) (lambda (&rest _) (error "Stuck"))))
              (should-error (kdb-claude-run "s2" "x" default-directory "pwsh")))
            (should (string-match-p "s2: in its tab\n.*s2: FAILED or quit after"
                                    (with-temp-buffer (insert-file-contents kdb-claude-run-log) (buffer-string)))))
        (kill-buffer " *test shell*")
        (delete-file kdb-claude-run-log)))))

(ert-deftest shell-prompt-told-from-output ()
  ;; At a prompt: bash's "$ ", pwsh's "PS C:\x> ".  Busy: a log line, a
  ;; progress line ending "45% ", a password prompt.
  (let* ((buf (generate-new-buffer " *test prompt*"))
         (proc (make-pipe-process :name "test-prompt" :buffer buf :noquery t)))
    (unwind-protect
        (dolist (case '(("user@host MINGW64 ~/x\n$ " . t) ("PS C:\\x> " . t)
                        ("info: Now listening on: http://127.0.0.1:5088\n" . nil)
                        ("Restoring packages 45% " . nil) ("Password: " . nil)))
          (with-current-buffer buf
            (erase-buffer)
            (insert (car case))
            (set-marker (process-mark proc) (point-max)))
          (should (eq (and (kdb-claude--shell-at-prompt-p buf) t) (cdr case))))
      (delete-process proc)
      (kill-buffer buf))))

(ert-deftest claude-run-claims-only-shells-it-started ()
  ;; A shell started by this run (new, or in a buffer whose shell had
  ;; ended) is the session's to close; one already running isn't.
  (let* ((buf (generate-new-buffer " *test claim*"))
         (running (make-pipe-process :name "test-running" :buffer buf :noquery t))
         opened)
    (cl-letf (((symbol-function 'kdb-claude--in-session-tab) (lambda (_id fn &rest _) (funcall fn (current-buffer))))
              ((symbol-function 'kdb-claude--use) (lambda (_b _id o) (setq opened o)))
              ((symbol-function 'kdb-claude--run-log) #'ignore))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'kdb-claude--block-to-shell) (lambda (&rest _) buf)))
              (kdb-claude-run "s" "x" default-directory "bash")
              (should-not opened))
            (delete-process running)
            (cl-letf (((symbol-function 'kdb-claude--block-to-shell)
                       (lambda (&rest _) (make-pipe-process :name "test-restarted" :buffer buf :noquery t) buf)))
              (kdb-claude-run "s" "x" default-directory "bash")
              (should opened)))
        (when (get-buffer-process buf) (delete-process (get-buffer-process buf)))
        (kill-buffer buf)))))

(ert-deftest claude-run-log-is-one-line-per-step-and-utf-8 ()
  (let ((kdb-claude-run-log (make-temp-file "run-log")))
    (cl-letf (((symbol-function 'kdb-claude--in-session-tab) (lambda (_id fn &rest _) (funcall fn (current-buffer))))
              ((symbol-function 'kdb-claude--block-to-shell) (lambda (&rest _) (get-buffer-create " *test shell*")))
              ((symbol-function 'kdb-claude--use) #'ignore))
      (unwind-protect
          (with-temp-buffer
            ;; A Latin-1 buffer, and a command with a … : no coding prompt.
            (setq buffer-file-coding-system 'iso-latin-1-dos)
            (kdb-claude-run "s3" (concat "git commit -F - <<'EOF'\nfix … done\nEOF") default-directory "bash")
            (let ((lines (with-temp-buffer
                           (let ((coding-system-for-read 'utf-8))
                             (insert-file-contents kdb-claude-run-log))
                           (split-string (buffer-string) "\n" t))))
              (should (= 4 (length lines)))
              (should (string-match-p "s3: start git commit -F - <<'EOF' ⏎ fix … done ⏎ EOF" (car lines)))))
        (kill-buffer " *test shell*")
        (delete-file kdb-claude-run-log)))))

(ert-deftest run-block-plain-block-is-machine-shell ()
  ;; A block naming no language runs in bash (Git Bash on Windows).
  (should (eq (kdb-claude--block-shell-kind "") (if (kdb-claude--bash) 'bash 'pwsh)))
  (when (kdb-claude--bash)                        ; the body is a bash heredoc
    (test-kill-shells)
    (let* ((buf (test-buffer))
           (dir (buffer-local-value 'default-directory buf)))
      (with-current-buffer buf
        (goto-char (point-max))
        (kdb-claude-run-block t))                 ; run it now
      (let ((out (test-shell-output (test-shell-buffer 'bash dir) "not an org heading")))
        (should (string-match-p "plain" out))
        (should (string-match-p "\\* not an org heading" out))))
    (test-kill-shells)))

(ert-deftest run-block-keystrokes-are-typed ()
  ;; A block of Emacs keys, unlabeled or labeled as shell, is queued as
  ;; input (typed once the running command ends), not run in bash.
  (dolist (fence '("" "bash"))
    (with-current-buffer (generate-new-buffer "*keys*")
      (insert (format "```%s\nM-x emacs-version RET\n```\n" fence))
      (test-open (current-buffer) #'point-max))
    (with-current-buffer "*claude blocks*"
      (should (string-prefix-p "#+begin_src emacs-lisp " (buffer-string)))
      (let ((unread-command-events nil))
        (should (equal (org-babel-execute-src-block) "Typed: M-x emacs-version RET"))
        (should (equal unread-command-events (listify-key-sequence (kbd "M-x emacs-version RET")))))))
  ;; Shell commands that merely contain C- or M- stay shell
  (test-kill-shells)
  (dolist (cmd '("make -C src" "git commit -m M-x" "echo C-c"))
    (with-current-buffer (generate-new-buffer "*keys*")
      (setq default-directory (file-name-as-directory (make-temp-file "blockdir" t)))
      (insert (format "```bash\n%s\n```\n" cmd))
      (goto-char (point-max))
      (kdb-claude-run-block)
      (should (string-suffix-p cmd (with-current-buffer (test-shell-buffer 'bash default-directory)
                                     (buffer-string))))))
  (test-kill-shells))

(ert-deftest run-block-shell-names-map-per-machine ()
  (let ((sh (if (kdb-claude--bash) 'bash 'pwsh)))
    (dolist (case `(("" . ,sh) ("console" . ,sh) ("shell" . ,sh) ("sh" . bash) ("bash" . bash)
                    ("zsh" . bash) ("pwsh" . pwsh) ("powershell" . pwsh) ("ps1" . pwsh)
                    ("elisp" . nil) ("python" . nil) ("org" . nil)))
      (should (eq (kdb-claude--block-shell-kind (car case)) (cdr case))))))

(ert-deftest run-block-powershell-language ()
  (with-current-buffer (generate-new-buffer "*ps*")
    (insert "```pwsh\nGet-Date\n```\n")
    (goto-char (point-max))
    (if (or (executable-find "pwsh") (executable-find "powershell"))
        (progn (kdb-claude-run-block)
               (should (test-shell-buffer 'pwsh default-directory))
               (test-kill-shells))
      ;; Without PowerShell: a clear error, not a broken shell.
      (should (string-match-p "No pwsh on this machine"
                              (cadr (should-error (kdb-claude-run-block) :type 'user-error)))))))

(ert-deftest run-block-uses-agent-shell-rendering ()
  ;; As in a session: a tool call with an unpaired raw fence, then a reply
  ;; rendered by agent-shell, which replaces fences with "LANG ⧉" labels.
  (require 'agent-shell-markdown)
  (with-current-buffer (generate-new-buffer "*rendered*")
    (insert "Tool output:\n```console\nstray, never closed\n\n")
    (insert (agent-shell-markdown-convert
             "Run:\n\n```bash\necho one\n```\n\nthen\n\n```\nplain\n```\n\nand\n\n```elisp\n(+ 1 2)\n```\n\nDone.\n"))
    (let ((blocks (kdb-claude--code-blocks)))
      (should (equal (mapcar #'car blocks) '("bash" "" "elisp")))
      (should (equal (mapcar (lambda (b) (string-trim (buffer-substring-no-properties (nth 1 b) (nth 2 b))))
                             blocks)
                     '("echo one" "plain" "(+ 1 2)"))))
    (test-open (current-buffer) #'point-max)     ; newest: the elisp block
    (should (string-match-p ": 3" (test-execute)))))

(ert-deftest run-block-org-src-block-inserted-as-is ()
  ;; An org fence holding a src block, with a leading blank line: not nested,
  ;; and :dir is added to its header.
  (with-current-buffer (generate-new-buffer "*org-fence*")
    (setq default-directory (file-name-as-directory (make-temp-file "blockdir" t)))
    (insert "```org\n\n#+begin_src sh\necho nested\n#+end_src\n```\n")
    (let ((dir default-directory))
      (test-open (current-buffer) #'point-max)
      (with-current-buffer "*claude blocks*"
        (should (equal (buffer-string)
                       (format "#+begin_src sh :dir %S\necho nested\n#+end_src\n" (abbreviate-file-name dir))))
        (should (string-match-p "nested" (test-execute))))))
  ;; Its own :dir is kept.
  (with-current-buffer (generate-new-buffer "*org-fence*")
    (insert "```org\n#+begin_src sh :dir /sudo::\nid\n#+end_src\n```\n")
    (test-open (current-buffer) #'point-max))
  (with-current-buffer "*claude blocks*"
    (should (equal (buffer-string) "#+begin_src sh :dir /sudo::\nid\n#+end_src\n"))))

(ert-deftest run-block-bare-org-src-block ()
  ;; A #+begin_src block with no fence, raw and as agent-shell renders it.
  (require 'agent-shell-markdown)
  (let ((md "Run:\n\n#+begin_src sh :dir /sudo::\nid\n#+end_src\n\nthen\n\n```elisp\n(+ 1 2)\n```\n"))
    (dolist (text (list md (agent-shell-markdown-convert md)))
      (with-current-buffer (generate-new-buffer "*bare*")
        (insert text)
        (should (equal (mapcar #'car (kdb-claude--code-blocks)) '("org" "elisp")))
        (test-open (current-buffer) (lambda () (search-forward "id")))
        (with-current-buffer "*claude blocks*"
          (should (equal (buffer-string) "#+begin_src sh :dir /sudo::\nid\n#+end_src\n")))))))

;;; Last reply → org (kdb-claude-reply-to-org)

(defconst test-transcript
  (mapconcat
   #'json-encode
   '(((type . "user") (message (role . "user") (content . "first prompt")))
     ((type . "assistant") (message (role . "assistant") (content . [((type . "text") (text . "old reply"))])))
     ((type . "user") (message (role . "user") (content . [((type . "text") (text . "second prompt"))])))
     ((type . "assistant") (message (role . "assistant")
                                    (content . [((type . "text") (text . "Checking [x.el](https://g.com/x.el#L84)."))
                                                ((type . "tool_use") (name . "Bash") (input (command . "ls")))])))
     ((type . "user") (message (role . "user") (content . [((type . "tool_result") (content . "out"))])))
     ((type . "user") (isMeta . t) (message (role . "user") (content . "meta prompt")))
     ((type . "user") (message (role . "user") (content . "<system-reminder>x</system-reminder>")))
     ((type . "api-request-blob") (message (role . "assistant") (content . [((type . "text") (text . "Checking"))])))
     ((type . "assistant") (isSidechain . t) (message (role . "assistant") (content . [((type . "text") (text . "subagent"))])))
     ((type . "assistant") (message (role . "assistant") (content . [((type . "text") (text . "## Done\n\n**All** good."))]))))
   "\n"))

(ert-deftest reply-last-reply-from-transcript ()
  (let ((jsonl (make-temp-file "transcript" nil ".jsonl" (concat test-transcript "\nnot json\n"))))
    (should (equal (kdb-claude--last-reply jsonl)
                   "Checking [x.el](https://g.com/x.el#L84).\n\n## Done\n\n**All** good."))
    (delete-file jsonl)))

(ert-deftest archive-skips-repeated-messages ()
  ;; The archive converter, on the same transcript: api-request-blob
  ;; copies don't become sections.
  (let ((jsonl (make-temp-file "transcript" nil ".jsonl" test-transcript))
        (org (make-temp-file "archive" nil ".org")))
    (kdb-claude--jsonl-to-org jsonl org)
    (with-temp-buffer
      (insert-file-contents org)
      (should (= 1 (how-many "^Checking" (point-min) (point-max))))
      (should (= 8 (how-many "^\\* \\(You\\|Claude\\)$" (point-min) (point-max)))))
    (delete-file jsonl)
    (delete-file org)))

(ert-deftest reply-to-org-opens-converted-reply ()
  (let* ((config (make-temp-file "claude-config" t))
         (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config) process-environment)))
    (make-directory (expand-file-name "projects/-proj" config) t)
    (with-temp-file (expand-file-name "projects/-proj/sid.jsonl" config) (insert test-transcript))
    (with-current-buffer (generate-new-buffer "*session*")
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'agent-shell-session-id) (lambda (&rest _) "sid")))
        (kdb-claude-reply-to-org)))
    (with-current-buffer "*claude reply: *session**"
      (should (derived-mode-p 'org-mode))
      (should (equal (buffer-string)
                     "Checking [[https://g.com/x.el#L84][x.el]].\n\n** Done\n\n*All* good."))
      (should (string-match-p "<a href=\"https://g.com/x.el#L84\">x.el</a>"
                              (org-export-as 'html nil nil t)))
      (should (string-match-p "\\[x.el\\](https://g.com/x.el#L84)" (org-export-as 'md nil nil t))))
    (delete-directory config t)))

(ert-deftest usage-shows-weekly-and-monthly-tables ()
  ;; Real ccusage on this machine's transcripts; plain text, no escapes.
  (skip-unless (executable-find "ccusage"))
  (ignore-errors (kill-buffer "*claude usage*"))
  (kdb-claude-usage)
  (with-current-buffer "*claude usage*"
    (let ((p (get-buffer-process (current-buffer))))
      (while (process-live-p p) (accept-process-output p 1)))
    (should truncate-lines)
    (should (string-match-p "Usage Report - Weekly" (buffer-string)))
    (should (string-match-p "Usage Report - Monthly" (buffer-string)))
    (should-not (string-match-p "\e" (buffer-string)))))

(ert-deftest run-block-from-code-block-label ()
  ;; Rendered as agent-shell does: each block's "LANG ⧉" label keeps RET
  ;; (copy) and gains x (run) and e (open to edit first); its hint
  ;; names all three.
  (require 'agent-shell-markdown)
  (switch-to-buffer (generate-new-buffer "*labels*"))
  (delete-other-windows)
  (setq default-directory (file-name-as-directory (make-temp-file "blockdir" t)))
  (insert "First:\n\n```bash\necho first\n```\n\nSecond:\n\n```elisp\n(+ 40 2)\n```\n")
  (kdb-claude--render-markdown :complete t)
  (kdb-claude--render-markdown :complete t)     ; streaming re-renders are harmless
  (goto-char (point-min))
  (search-forward "elisp ⧉")
  (let* ((session (selected-window))
         (label (match-beginning 0))
         (map (get-text-property label 'keymap))
         messages)
    (should (eq (lookup-key map "x") #'kdb-claude-run-block-at-label))
    (should (eq (lookup-key map "e") #'kdb-claude-edit-block-at-label))
    (should (lookup-key map (kbd "RET")))
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (funcall (car (get-text-property label 'cursor-sensor-functions)) nil nil 'entered))
    (should (equal messages '("Press RET to copy, x to run, e to edit first")))
    ;; x: the second block (not the first) runs without asking, its
    ;; result lands in *claude blocks*, and focus stays in the session
    (ignore-errors (kill-buffer "*claude blocks*"))
    (goto-char label)
    (let (last)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq last (and fmt (apply #'format fmt args))))))
        (let ((org-confirm-babel-evaluate nil))
          (kdb-claude-run-block-at-label)))
      (should (equal last "42")))                 ; output echoed, not just "complete"
    (should (eq (selected-window) session))
    (should (= (point) label))
    (with-current-buffer "*claude blocks*"
      (should (string-match-p "\\`#\\+begin_src emacs-lisp " (buffer-string)))
      (should (string-match-p "^#\\+RESULTS:\n: 42" (buffer-string))))
    ;; e: opened in *claude blocks*, not run
    (select-window session)
    (ignore-errors (kill-buffer "*claude blocks*"))
    (goto-char label)
    (kdb-claude-edit-block-at-label)
    (should (equal (buffer-name (window-buffer (selected-window))) "*claude blocks*"))
    (should (org-in-src-block-p))
    (should-not (string-match-p "RESULTS" (buffer-string)))
    ;; A bash block: x runs it in the project's shell, focus stays here,
    ;; and the shell is reused, so cd carries over to the next block.
    (select-window session)
    (test-kill-shells)
    (goto-char (point-min))
    (search-forward "bash ⧉")
    (let ((bash-label (match-beginning 0))
          (dir default-directory))
      (goto-char bash-label)
      (kdb-claude-run-block-at-label)
      (should (eq (selected-window) session))
      (should (= (point) bash-label))
      (let ((shell (test-shell-buffer 'bash dir)))
        ;; Sent before bash's first prompt, so the output may follow it.
        (should (string-match-p "\\(^\\|\\$ \\)first$" (test-shell-output shell "\\(^\\|\\$ \\)first$")))
        (with-current-buffer shell
          (goto-char (point-max))
          (insert "cd /tmp && echo moved")
          (comint-send-input))
        (test-shell-output shell "^moved$")
        (kdb-claude--block-to-shell 'bash "pwd" dir t)
        (should (string-match-p "^/tmp$" (test-shell-output shell "^/tmp$"))))
      ;; e on it: at the prompt, not run, and the shell window selected.
      (select-window session)
      (goto-char bash-label)
      (kdb-claude-edit-block-at-label)
      (should (eq (window-buffer (selected-window)) (test-shell-buffer 'bash dir)))
      (should (string-suffix-p "echo first" (buffer-string))))
    (test-kill-shells)))

(ert-deftest run-block-none-before-point ()
  (with-current-buffer (test-buffer)
    (goto-char (point-min))
    (should-error (kdb-claude-run-block) :type 'user-error)))

(ert-deftest run-block-plain-goes-to-pwsh-only-without-bash ()
  (cl-letf (((symbol-function 'kdb-claude--bash) (lambda () nil)))
    (should (eq (kdb-claude--block-shell-kind "") 'pwsh)))
  (cl-letf (((symbol-function 'kdb-claude--bash) (lambda () "C:/Program Files/Git/bin/bash.exe")))
    (should (eq (kdb-claude--block-shell-kind "console") 'bash))
    (should (eq (kdb-claude--block-shell-kind "powershell") 'pwsh))))   ; named, it's PowerShell

(ert-deftest run-block-picks-the-block-you-are-on ()
  ;; Anywhere on a block, its label included, takes that block, not the
  ;; one before it.
  (require 'agent-shell-markdown)
  (with-current-buffer (generate-new-buffer "*rendered*")
    (insert (agent-shell-markdown-convert
             "First:\n\n```elisp\n(+ 1 1)\n```\n\nSecond:\n\n```elisp\n(+ 2 2)\n(+ 3 3)\n```\n\nDone.\n"))
    (let (picked)
      (cl-letf (((symbol-function 'kdb-claude--block-to-org)
                 (lambda (_lang code &rest _) (setq picked code))))
        (cl-flet ((at (move) (goto-char (point-min)) (funcall move) (kdb-claude-run-block) picked))
          (should (equal (at (lambda () (search-forward "elisp ⧉" nil t 2) (beginning-of-line)))
                         "(+ 2 2)\n(+ 3 3)"))                     ; on its label
          (should (equal (at (lambda () (search-forward "elisp ⧉" nil t 2) (forward-line 1)))
                         "(+ 2 2)\n(+ 3 3)"))                     ; the blank line under it
          (should (equal (at (lambda () (search-forward "(+ 2")))
                         "(+ 2 2)\n(+ 3 3)"))                     ; in it
          (should (equal (at (lambda () (search-forward "(+ 3 3)")))
                         "(+ 2 2)\n(+ 3 3)"))                     ; its end
          (should (equal (at (lambda () (search-forward "(+ 1")))
                         "(+ 1 1)"))                               ; the first one
          (should (equal (at (lambda () (search-forward "Done")))
                         "(+ 2 2)\n(+ 3 3)")))))                  ; after: the last before
    (kill-buffer))
  ;; Raw fences: the opening fence is the block's.
  (with-temp-buffer
    (insert "```elisp\n(a)\n```\n```elisp\n(b)\n```\n")
    (let (picked)
      (cl-letf (((symbol-function 'kdb-claude--block-to-org)
                 (lambda (_lang code &rest _) (setq picked code))))
        (goto-char (point-min)) (forward-line 3)        ; second opening fence
        (kdb-claude-run-block)
        (should (equal picked "(b)"))
        (goto-char (point-min)) (forward-line 2)        ; first closing fence
        (kdb-claude-run-block)
        (should (equal picked "(a)"))))))

;; These are the lines Git Bash prints first in an Emacs shell.
(ert-deftest shell-drops-bash-job-control-warnings ()
  (should (equal (kdb-shell--drop-bash-job-warnings
                  (concat "bash: cannot set terminal process group (1234): Inappropriate ioctl for device\r\n"
                          "bash: no job control in this shell\n"
                          "hi\n$ "))
                 "hi\n$ ")))

;;; claude-run-block-test.el ends here
