;;; claude-sync-test.el --- Archive sync skips unreadable transcripts -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-sync-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'init-claude)

(defun test-jsonl (dir name text)
  (make-directory dir t)
  (with-temp-file (expand-file-name (concat name ".jsonl") dir)
    (insert (json-serialize `((type . "user")
                              (message . ((role . "user") (content . ,text)))
                              (cwd . "/tmp/x")))
            "\n")))

(ert-deftest sync-skips-unreadable-transcript ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (archive (expand-file-name "archive/" root))
         (convert (symbol-function 'kdb-claude--jsonl-to-org))
         msgs)
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "first session")
    (test-jsonl (expand-file-name "-proj-a/" projects) "s2" "unreadable one")
    (test-jsonl (expand-file-name "-proj-b/" projects) "s3" "third session")
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive))
              ((symbol-function 'kdb-claude--jsonl-to-org)
               (lambda (jsonl org &rest more)
                 (if (string-match-p "/s2\\.jsonl\\'" jsonl)
                     (signal 'file-missing (list "Opening input file" "No such file or directory" jsonl))
                   (apply convert jsonl org more))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (car (push (apply #'format fmt args) msgs)))))
      (should (equal (kdb-claude-sync-code-sessions)
                     "Synced 2 Claude Code sessions (1 unreadable, skipped)"))
      (should (seq-find (lambda (m) (string-match-p "\\`Skipped .*/s2\\.jsonl: " m)) msgs))
      (should (file-exists-p (expand-file-name "code-sessions/-proj-a/s1.org" archive)))
      (should (file-exists-p (expand-file-name "code-sessions/-proj-b/s3.org" archive)))
      (should-not (file-exists-p (expand-file-name "code-sessions/-proj-a/s2.org" archive)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "code-sessions/-proj-b/s3.org" archive))
        (should (string-match-p "third session" (buffer-string))))
      ;; Converted ones are up to date; the unreadable one is retried each sync.
      (should (equal (kdb-claude-sync-code-sessions)
                     "Synced 0 Claude Code sessions (1 unreadable, skipped)"))
      ;; Nothing skipped: the plain message.
      (delete-file (expand-file-name "-proj-a/s2.jsonl" projects))
      (should (equal (kdb-claude-sync-code-sessions) "Synced 0 Claude Code sessions")))
    (delete-directory root t)))

(ert-deftest sync-skips-unlistable-project-dir ()
  (skip-unless (not (eq system-type 'windows-nt)))
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (archive (expand-file-name "archive/" root))
         (locked (expand-file-name "-proj-locked/" projects)))
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "readable")
    (test-jsonl locked "s2" "can't even list this dir")
    (set-file-modes locked #o000)
    (unwind-protect
        (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
                  ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive)))
          (should (equal (kdb-claude-sync-code-sessions) "Synced 1 Claude Code sessions"))
          (should (file-exists-p (expand-file-name "code-sessions/-proj-a/s1.org" archive))))
      (set-file-modes locked #o755)
      (delete-directory root t))))

(ert-deftest sync-other-errors-still-surface ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root)))
    (test-jsonl (expand-file-name "-proj-a/" projects) "s1" "x")
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () (expand-file-name "archive/" root)))
              ((symbol-function 'kdb-claude--jsonl-to-org) (lambda (&rest _) (error "Converter bug"))))
      (should-error (kdb-claude-sync-code-sessions)))
    (delete-directory root t)))

;;; Paths too long for Windows Emacs

(defvar test-ps-sources nil "Source paths the fake PowerShell was asked to copy.")

(defun test-fake-powershell (start end program &rest _)
  "Stand-in for PowerShell: run the generated copy script with `copy-file'."
  (should (equal program "powershell"))
  (let ((script (buffer-substring-no-properties start end)))
    (should (string-match-p "\\`[[:ascii:]]*\\'" script))   ; ASCII whatever the code page
    (dolist (line (split-string script "\n" t))
      (when (string-match "Copy((d '\\([^']*\\)'), (d '\\([^']*\\)')" line)
        (let ((src (decode-coding-string (base64-decode-string (match-string 1 line)) 'utf-8))
              (dst (decode-coding-string (base64-decode-string (match-string 2 line)) 'utf-8)))
          (push src test-ps-sources)
          (should (string-prefix-p "\\\\?\\" src))
          (copy-file (subst-char-in-string ?\\ ?/ (string-remove-prefix "\\\\?\\" src)) dst t t)))))
  0)

(defmacro test-with-long-paths (&rest body)
  "Run BODY with ROOT, PROJECTS, ARCHIVE and a 100-char project name LONG,
under a path limit that makes LONG's transcripts too long to open."
  `(let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
          (projects (expand-file-name "projects/" root))
          (archive (expand-file-name "archive/" root))
          (long (concat "-C--Users-k-OneDrive---Cardinal-Health-ñ-"
                        (make-string 59 ?x)))
          (kdb-claude--max-path (+ (length root) 100))
          (test-ps-sources nil)
          (temps-before (directory-files temporary-file-directory nil "\\`claude-long")))
     (unwind-protect
         (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
                   ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive)))
           ,@body
           (should (equal (directory-files temporary-file-directory nil "\\`claude-long")
                          temps-before)))
       (delete-directory root t))))

(ert-deftest sync-copies-long-path-transcripts ()
  (test-with-long-paths
   (test-jsonl (expand-file-name long projects) "s1" "long path session")
   (test-jsonl (expand-file-name "-a/" projects) "s2" "short path session")
   (should (kdb-claude--path-too-long-p (expand-file-name (concat long "/s1.jsonl") projects)))
   (should-not (kdb-claude--path-too-long-p (expand-file-name "-a/s2.jsonl" projects)))
   (cl-letf (((symbol-function 'call-process-region) #'test-fake-powershell))
     (should (equal (kdb-claude-sync-code-sessions) "Synced 2 Claude Code sessions"))
     ;; Only the long one went through PowerShell, by its \\?\ name.
     (should (equal (length test-ps-sources) 1))
     (should (string-suffix-p "\\s1.jsonl" (car test-ps-sources)))
     (let ((org (car (directory-files-recursively (expand-file-name "code-sessions/" archive)
                                                  "\\`s1\\.org\\'"))))
       ;; Shortened directory (40 chars + hash), same file name, so history
       ;; and resume (by session id) still work; SOURCE is the original.
       (should (string-match-p "/-C--Users-k-OneDrive---Cardinal-Health-ñ-[0-9a-f]\\{8\\}/s1\\.org\\'" org))
       (should-not (kdb-claude--path-too-long-p org))
       (should (equal (kdb-claude--org-keyword org "SOURCE")
                      (expand-file-name (concat long "/s1.jsonl") projects)))
       (with-temp-buffer
         (insert-file-contents org)
         (should (string-match-p "long path session" (buffer-string)))))
     ;; The copy kept the mtime, so an unchanged transcript isn't reconverted.
     (should (equal (kdb-claude-sync-code-sessions) "Synced 0 Claude Code sessions")))))

(ert-deftest sync-long-path-copy-failure-is-skipped ()
  (test-with-long-paths
   (test-jsonl (expand-file-name long projects) "s1" "long")
   (test-jsonl (expand-file-name "-a/" projects) "s2" "short")
   (dolist (fail (list (lambda (&rest _) 1)                                   ; copy failed
                       (lambda (&rest _) (signal 'file-missing '("Searching for program" "powershell")))))
     (cl-letf (((symbol-function 'call-process-region) fail))
       (should (string-match-p "(1 unreadable, skipped)\\'" (kdb-claude-sync-code-sessions)))))
   (should (file-exists-p (expand-file-name "code-sessions/-a/s2.org" archive)))))

(ert-deftest sync-long-path-real-powershell ()
  "On Windows: copy a transcript whose path really is over MAX_PATH."
  (skip-unless (eq system-type 'windows-nt))
  (let* ((root (file-name-as-directory (make-temp-file "claude-long-test" t)))
         (dir (expand-file-name (make-string 240 ?d) root))
         (file (expand-file-name "s1.jsonl" dir))
         (out (file-name-as-directory (make-temp-file "claude-long-out" t)))
         (win (lambda (f) (concat "\\\\?\\" (subst-char-in-string ?/ ?\\ f)))))
    (unwind-protect
        (progn
          (should (> (length file) 260))
          (should (zerop (call-process
                          "powershell" nil nil nil "-NoProfile" "-NonInteractive" "-Command"
                          (format "[IO.Directory]::CreateDirectory('%s') | Out-Null; [IO.File]::WriteAllText('%s', 'hello ñ', [Text.UTF8Encoding]::new($false)); [IO.File]::SetLastWriteTime('%s', [DateTime]'2020-01-02')"
                                  (funcall win dir) (funcall win file) (funcall win file)))))
          (let ((copy (cdr (assoc file (kdb-claude--copy-long-files (list file) out)))))
            (should copy)
            (should (equal (with-temp-buffer (insert-file-contents copy) (buffer-string)) "hello ñ"))
            (should (equal (format-time-string "%F" (file-attribute-modification-time
                                                     (file-attributes copy)))
                           "2020-01-02"))))
      (call-process "powershell" nil nil nil "-NoProfile" "-NonInteractive" "-Command"
                    (format "Remove-Item -LiteralPath '%s' -Recurse -Force" (funcall win root)))
      (delete-directory out t))))

;;; Session names and projects

(defun test-jsonl-lines (file &rest objs)
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (dolist (o objs) (insert (json-serialize o) "\n"))))

(defun test-convert (&rest objs)
  "Convert OBJS as a transcript; return the org header alist."
  (let* ((dir (file-name-as-directory (make-temp-file "conv" t)))
         (jsonl (expand-file-name "s.jsonl" dir))
         (org (expand-file-name "s.org" dir)))
    (apply #'test-jsonl-lines jsonl objs)
    (kdb-claude--jsonl-to-org jsonl org)
    (prog1 (kdb-claude--read-org-header org)
      (delete-directory dir t))))

(defun test-user (text &optional cwd branch client)
  `((type . "user") (message . ((role . "user") (content . ,text)))
    ,@(when cwd `((cwd . ,cwd))) ,@(when branch `((gitBranch . ,branch)))
    ,@(when client `((entrypoint . ,client)))))

(ert-deftest convert-uses-desktop-session-name ()
  ;; Claude Code rewrites the AI title as the session goes; the last wins.
  (let ((h (test-convert (test-user "first prompt")
                         '((type . "ai-title") (aiTitle . "Early title"))
                         '((type . "ai-title") (aiTitle . "Fix login redirect loop")))))
    (should (equal (cdr (assoc "TITLE" h)) "Fix login redirect loop")))
  ;; A name you gave it beats the AI title.
  (let ((h (test-convert (test-user "first prompt")
                         '((type . "ai-title") (aiTitle . "AI title"))
                         '((type . "custom-title") (customTitle . "My name for it")))))
    (should (equal (cdr (assoc "TITLE" h)) "My name for it")))
  ;; No title entries: the first prompt, as before.
  (should (equal (cdr (assoc "TITLE" (test-convert (test-user "just this prompt"))))
                 "just this prompt")))

(ert-deftest convert-title-is-one-line ()
  (should (equal (cdr (assoc "TITLE" (test-convert (test-user "x")
                                                   '((type . "custom-title") (customTitle . "Two\nlines\r\n")))))
                 "Two lines")))

(ert-deftest history-refresh-drops-stale-headers ()
  (puthash "/some/file.org" '(stale . (("TITLE" . "old"))) kdb-claude--header-cache)
  (cl-letf (((symbol-function 'kdb-claude-sync-code-sessions-async) (lambda (cb &rest _) (funcall cb))))
    (with-temp-buffer (kdb-claude--history-refresh)))
  (should (= 0 (hash-table-count kdb-claude--header-cache))))

(ert-deftest convert-records-branch ()
  (let ((h (test-convert (test-user "a" "/tmp/x" "HEAD")
                         (test-user "b" "/tmp/x" "fix/login")
                         (test-user "c" "/tmp/x" "HEAD"))))
    (should (equal (cdr (assoc "BRANCH" h)) "fix/login")))
  (should-not (assoc "BRANCH" (test-convert (test-user "a" "/tmp/x" "HEAD")))))

(ert-deftest project-name-of-worktrees ()
  (let* ((root (file-name-as-directory (make-temp-file "proj" t)))
         (repo (expand-file-name "sns-technology/" root))
         (wt (expand-file-name "anywhere/brave-tesla-2a3f/" root)))
    (make-directory wt t)
    (make-directory (expand-file-name ".git/worktrees/brave-tesla-2a3f" repo) t)
    (with-temp-file (expand-file-name ".git" wt)
      (insert "gitdir: " repo ".git/worktrees/brave-tesla-2a3f\n"))
    ;; A live worktree: its main repository, wherever it lives.
    (should (equal (kdb-claude--project-name wt) "sns-technology"))
    ;; Gone worktrees, by layout; Windows paths too.
    (should (equal (kdb-claude--project-name "/src/sns-technology/.claude/worktrees/brave-tesla") "sns-technology"))
    (should (equal (kdb-claude--project-name "C:\\Users\\k\\src\\sns-technology\\.claude\\worktrees\\brave-tesla")
                   "sns-technology"))
    (should (equal (kdb-claude--project-name "C:\\Users\\k\\.claude-worktrees\\sns-technology\\brave-tesla")
                   "sns-technology"))
    ;; Not a worktree: its own name.
    (should (equal (kdb-claude--project-name "/home/k/src/emacs.d/") "emacs.d"))
    (should (equal (cdr (assoc "PROJECT" (test-convert (test-user "x" (directory-file-name wt)))))
                   "sns-technology"))
    (delete-directory root t)))

(ert-deftest sync-upgrades-old-archives-and-history-shows-project ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (archive (expand-file-name "archive/" root))
         (org (expand-file-name "code-sessions/-p/s1.org" archive)))
    (test-jsonl-lines (expand-file-name "-p/s1.jsonl" projects)
                      (test-user "hello" "/src/sns-technology/.claude/worktrees/brave-tesla")
                      '((type . "ai-title") (aiTitle . "Fix login redirect loop")))
    ;; An archive written before #+PROJECT existed, newer than the transcript.
    (make-directory (file-name-directory org) t)
    (with-temp-file org (insert "#+TITLE: hello\n#+CWD: /src/sns-technology/.claude/worktrees/brave-tesla\n\n"))
    (set-file-times org (time-add nil 3600))
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive)))
      (should (equal (kdb-claude-sync-code-sessions) "Synced 1 Claude Code sessions"))
      (should (equal (kdb-claude--org-keyword org "PROJECT") "sns-technology"))
      (should (equal (kdb-claude-sync-code-sessions) "Synced 0 Claude Code sessions"))
      (let ((entry (car (kdb-claude--history-entries))))
        (should (equal (aref (cadr entry) 3) "sns-technology"))
        (should (equal (aref (cadr entry) 4) "Fix login redirect loop"))))
    (delete-directory root t)))

(ert-deftest symbol-fonts-apply ()
  (kdb-apply-symbol-fonts))

;;; Telling sessions apart in history

(ert-deftest convert-records-client-and-size ()
  (let ((h (test-convert (test-user "first" "/tmp/x" nil "cli")
                         (test-user "second" "/tmp/x" nil "cli"))))
    (should (equal (cdr (assoc "CLIENT" h)) "cli"))
    (should (equal (cdr (assoc "MESSAGES" h)) "2")))
  (should (equal (cdr (assoc "CLIENT" (test-convert (test-user "no client")))) "")))

(ert-deftest history-shows-where-sessions-came-from ()
  (let* ((archive (file-name-as-directory (make-temp-file "archive" t)))
         (write (lambda (rel text)
                  (let ((f (expand-file-name rel archive)))
                    (make-directory (file-name-directory f) t)
                    (with-temp-file f (insert text))
                    f))))
    (funcall write "code-sessions/-p/a.org" "#+TITLE: In the terminal\n#+CLIENT: cli\n#+PROJECT: app\n\n")
    (funcall write "code-sessions/-p/b.org" "#+TITLE: In Emacs\n#+CLIENT: emacs\n#+PROJECT: app\n\n")
    (funcall write "code-sessions/-p/f.org" "#+TITLE: A script\n#+CLIENT: sdk-ts\n#+PROJECT: app\n\n")
    (funcall write "code-sessions/-p/c.org" "#+TITLE: In Desktop\n#+CLIENT: claude-desktop\n#+PROJECT: app\n\n")
    (funcall write "code-sessions/-p/d.org" "#+TITLE: Something new\n#+CLIENT: future-client\n#+PROJECT: app\n\n")
    (funcall write "desktop/e.org" "#+TITLE: A chat\n#+DATE: 2026-04-19 02:32\n\n* You\n\nhi\n")
    (cl-letf (((symbol-function 'kdb-claude--ensure-archive) (lambda () archive))
              ((symbol-function 'kdb-claude--live-sessions) #'ignore))
      (let ((kdb-claude--history-filter nil)
            (rows (lambda () (mapcar (lambda (e) (list (aref (cadr e) 2) (aref (cadr e) 3) (aref (cadr e) 4)))
                                     (kdb-claude--history-entries)))))
        (should (seq-set-equal-p (funcall rows)
                                 '(("terminal" "app" "In the terminal") ("emacs" "app" "In Emacs")
                                   ("sdk" "app" "A script")
                                   ("desktop" "app" "In Desktop") ("future-client" "app" "Something new")
                                   ("chat" "" "A chat"))))
        ;; / narrows by source too.
        (let ((kdb-claude--history-filter "chat"))
          (should (equal (funcall rows) '(("chat" "" "A chat")))))
        (let ((kdb-claude--history-filter "desktop"))
          (should (equal (mapcar #'car (funcall rows)) '("desktop"))))))
    (delete-directory archive t)))

(ert-deftest history-preview-says-how-a-session-ended ()
  (let* ((header "#+TITLE: t\n#+CWD: /home/k/src/app\n#+BRANCH: fix/login\n#+MESSAGES: 14\n\n")
         (f (make-temp-file "session" nil ".org"
                            (concat header
                                    "* You\n\nWhy does the login page redirect forever?\n\n"
                                    "* Claude\n\nFixed it:   the reset token\nwasn't cleared.\n")))
         (asked (make-temp-file "session" nil ".org"
                                (concat header "* Claude\n\nDone.\n\n* You\n\nNow add a test for it\n")))
         (long (make-temp-file "session" nil ".org"
                               (concat "#+TITLE: t\n\n* You\n\nhi\n\n* Claude\n\n" (make-string 300 ?x) "\n")))
         ;; The last message is longer than the tail read first.
         (huge (make-temp-file "session" nil ".org"
                               (concat "#+TITLE: t\n\n* You\n\nwrite it all\n\n* Claude\n\nAll of it: "
                                       (make-string 30000 ?y) "\n"))))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () f)))
      (should (equal (kdb-claude--history-preview)
                     "Claude: Fixed it: the reset token wasn't cleared.  ·  14 messages  ·  fix/login  ·  /home/k/src/app")))
    ;; Ending on your prompt shows that.
    (should (equal (kdb-claude--last-message asked) "You: Now add a test for it"))
    ;; A long last message is cut to one short line.
    (should (= 140 (string-width (kdb-claude--last-message long))))
    (should (string-suffix-p "…" (kdb-claude--last-message long)))
    (should (string-prefix-p "Claude: All of it: yyy" (kdb-claude--last-message huge)))
    (mapc #'delete-file (list f asked long huge))))

(ert-deftest sync-labels-sessions-run-in-emacs ()
  (let* ((root (file-name-as-directory (make-temp-file "claude-sync" t)))
         (projects (expand-file-name "projects/" root))
         (kdb-claude-archive-dir (expand-file-name "archive/" root))
         (archive kdb-claude-archive-dir)
         (org (lambda (id) (expand-file-name (format "code-sessions/-p/%s.org" id) archive))))
    (test-jsonl-lines (expand-file-name "-p/e1.jsonl" projects) (test-user "from emacs" "/tmp/x" nil "sdk-ts"))
    (test-jsonl-lines (expand-file-name "-p/s1.jsonl" projects) (test-user "a script" "/tmp/x" nil "sdk-ts"))
    (cl-letf (((symbol-function 'kdb-claude--projects-dir) (lambda () projects))
              ((symbol-function 'kdb-claude--ensure-archive) (lambda () archive)))
      ;; Synced before Emacs noted e1: both look like any SDK program.
      (kdb-claude-sync-code-sessions)
      (should (equal (kdb-claude--org-keyword (funcall org "e1") "CLIENT") "sdk-ts"))
      ;; Noted once, however often its prompt comes back.
      (dotimes (_ 3) (kdb-claude--note-emacs-session "e1"))
      (should (equal (kdb-claude--emacs-session-ids) '("e1")))
      ;; The next sync relabels it, though its transcript didn't change.
      (should (equal (kdb-claude-sync-code-sessions) "Synced 1 Claude Code sessions"))
      (should (equal (kdb-claude--org-keyword (funcall org "e1") "CLIENT") "emacs"))
      (should (equal (kdb-claude--org-keyword (funcall org "s1") "CLIENT") "sdk-ts"))
      (should (equal (kdb-claude-sync-code-sessions) "Synced 0 Claude Code sessions")))
    (delete-directory root t)))

;;; claude-sync-test.el ends here
