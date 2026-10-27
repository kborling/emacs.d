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

(ert-deftest run-block-plain-block-is-bash-and-escaped ()
  (let ((org (test-open (test-buffer) (lambda () (point-max)))))   ; newest block
    (with-current-buffer org
      (should (string-match-p "^#\\+begin_src bash " (buffer-string)))
      (should (string-match-p "^,\\* not an org heading" (buffer-string)))
      (should (= 0 (how-many "^\\* " (point-min) (point-max)))))
    (let ((out (test-execute)))
      (should (string-match-p "plain" out))
      (should (string-match-p "\\* not an org heading" out)))))

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

(ert-deftest run-block-none-before-point ()
  (with-current-buffer (test-buffer)
    (goto-char (point-min))
    (should-error (kdb-claude-run-block) :type 'user-error)))

;;; claude-run-block-test.el ends here
