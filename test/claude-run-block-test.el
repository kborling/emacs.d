;;; claude-run-block-test.el --- Code blocks from replies to org babel -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-run-block-test.el \
;;   -f ert-run-tests-batch-and-exit

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

(ert-deftest run-block-finds-blocks ()
  (with-current-buffer (test-buffer)
    (should (equal (mapcar #'car (kdb-claude--code-blocks)) '("bash" "elisp" "")))
    (let ((b (car (kdb-claude--code-blocks))))
      (should (equal (buffer-substring-no-properties (nth 1 b) (nth 2 b))
                     "echo \"it's $((1 + 2))\"\npwd\n")))))

(ert-deftest run-block-shell-runs-in-session-dir ()
  (let* ((buf (test-buffer))
         (dir (buffer-local-value 'default-directory buf))
         (org (test-open buf (lambda () (search-forward "pwd")))))
    (with-current-buffer org
      (should (derived-mode-p 'org-mode))
      (should (string-prefix-p (format "#+begin_src bash :dir %S :results output\n" (abbreviate-file-name dir))
                               (buffer-string)))
      (should (org-in-src-block-p)))             ; point is in the block, ready to edit
    (let ((out (test-execute)))
      (should (string-match-p "it's 3" out))
      ;; Git Bash prints /c/... for c:/..., so match the directory's own name.
      (should (string-match-p (regexp-quote (file-name-nondirectory (directory-file-name dir))) out)))))

(ert-deftest run-block-elisp-and-history ()
  (let ((buf (test-buffer)))
    (test-open buf (lambda () (search-forward "(* 2 21)")))
    (should (string-match-p ": 42" (test-execute)))
    ;; A second block is appended after the first and its results.
    (with-current-buffer buf
      (goto-char (point-max))
      (kdb-claude-run-block))
    (with-current-buffer "*claude blocks*"
      (should (= 2 (how-many "^#\\+begin_src" (point-min) (point-max))))
      (should (string-match-p ": 42" (buffer-string))))))

(ert-deftest run-block-plain-block-is-machine-shell-and-escaped ()
  ;; A block naming no language runs in this machine's shell.
  (let ((org (test-open (test-buffer) (lambda () (point-max)))))   ; newest block
    (with-current-buffer org
      (should (string-match-p (if (eq system-type 'windows-nt) "^#\\+begin_src powershell " "^#\\+begin_src bash ")
                              (buffer-string)))
      (should (string-match-p "^,\\* not an org heading" (buffer-string)))
      (should (= 0 (how-many "^\\* " (point-min) (point-max)))))
    (unless (eq system-type 'windows-nt)          ; the body is a bash heredoc
      (let ((out (test-execute)))
        (should (string-match-p "plain" out))
        (should (string-match-p "\\* not an org heading" out))))))

(defvar test-ping nil)
(defun test-ping (arg)
  "Command the keystroke test runs through M-x."
  (interactive "sArg: ")
  (setq test-ping arg))

(ert-deftest run-block-keystrokes-replay-as-keys ()
  ;; A block of Emacs keys, unlabeled or labeled as shell, is replayed
  ;; with `execute-kbd-macro' rather than run in bash.
  (dolist (fence '("" "bash"))
    (setq test-ping nil)
    (with-current-buffer (generate-new-buffer "*keys*")
      (insert (format "```%s\nM-x test-ping RET hello RET\n```\n" fence))
      (test-open (current-buffer) #'point-max))
    (with-current-buffer "*claude blocks*"
      (should (string-match-p "\\`#\\+begin_src emacs-lisp .*\n(execute-kbd-macro (kbd \"M-x test-ping RET hello RET\"))\n"
                              (buffer-string)))
      (org-babel-execute-src-block))
    (should (equal test-ping "hello")))
  ;; Shell commands that merely contain C- or M- stay shell
  (dolist (cmd '("make -C src" "git commit -m M-x" "echo C-c"))
    (with-current-buffer (generate-new-buffer "*keys*")
      (insert (format "```bash\n%s\n```\n" cmd))
      (test-open (current-buffer) #'point-max))
    (with-current-buffer "*claude blocks*"
      (should (string-prefix-p "#+begin_src bash " (buffer-string))))))

(ert-deftest run-block-shell-names-map-per-machine ()
  (let ((sh (if (eq system-type 'windows-nt) "powershell" "bash")))
    (dolist (case `(("" . ,sh) ("console" . ,sh) ("shell" . ,sh) ("sh" . "bash") ("bash" . "bash")
                    ("zsh" . "bash") ("pwsh" . "powershell") ("powershell" . "powershell")))
      (with-current-buffer (generate-new-buffer "*lang*")
        (insert (format "```%s\necho hi\n```\n" (car case)))
        (test-open (current-buffer) #'point-max))
      (with-current-buffer "*claude blocks*"
        (should (string-prefix-p (format "#+begin_src %s " (cdr case)) (buffer-string)))))))

(ert-deftest run-block-powershell-language ()
  (with-current-buffer (generate-new-buffer "*ps*")
    (insert "```pwsh\nGet-Date\n```\n")
    (test-open (current-buffer) #'point-max))
  (with-current-buffer "*claude blocks*"
    (should (string-prefix-p "#+begin_src powershell " (buffer-string))))
  (should (fboundp 'org-babel-execute:powershell)))

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
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
      (let ((org-confirm-babel-evaluate nil))
        (kdb-claude-run-block-at-label)))
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
    (should-not (string-match-p "RESULTS" (buffer-string)))))

(ert-deftest run-block-none-before-point ()
  (with-current-buffer (test-buffer)
    (goto-char (point-min))
    (should-error (kdb-claude-run-block) :type 'user-error)))

;;; claude-run-block-test.el ends here
