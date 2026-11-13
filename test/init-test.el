;;; init-test.el --- init.el's own helpers do what they say -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/init-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

(ert-deftest messages-window-goes-to-its-own-end ()
  ;; Shown from a short buffer: *Messages*' end, not the caller's.
  (with-current-buffer (messages-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (make-string 500 ?x) "\n")))
  (let ((win (display-buffer "*Messages*")))
    (set-window-point win (point-min))
    (with-temp-buffer
      (kdb-messages-buffer-advice "*Messages*"))
    (should (= (window-point win)
               (with-current-buffer "*Messages*" (point-max))))))

(ert-deftest sql-connect-builds-exact-command-lines ()
  (require 'sql)
  (let (seen)
    (cl-letf (((symbol-function 'sql-ms)
               (lambda (buffer) (push (list 'ms buffer kdb-sql--args
                                            (getenv "SQLCMDPASSWORD"))
                                      seen)))
              ((symbol-function 'sql-postgres)
               (lambda (buffer) (push (list 'pg buffer kdb-sql--args
                                            (getenv "PGPASSWORD")
                                            (getenv "PGSSLMODE")
                                            (getenv "PGCONNECT_TIMEOUT"))
                                      seen)))
              ((symbol-function 'kdb-sql--vscode-connections) #'ignore)
              ((symbol-function 'kdb-sql--az-token) (lambda () "tok"))
              ((symbol-function 'read-passwd) (lambda (&rest _) "pw"))
              ;; No database saved and none listed: asked, RET keeps the default.
              ((symbol-function 'kdb-sql--databases) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (let ((kdb-sql-connections
             '(("ms" :product ms :server "s" :database "d")
               ("ms-nodb" :product ms :server "s" :database "")
               ("ms-login" :product ms :server "s" :database "d" :user "u")
               ("pg" :product postgres :server "h" :database "postgres" :user "admins"))))
        (dolist (c kdb-sql-connections) (kdb-sql-connect (car c))))
      ;; Entra ID through az, long options only: never -E, -G or -d, which
      ;; sql.el's own builder adds and the Go sqlcmd then signs in with.
      (should (member '(ms "*SQL: ms*" ("--server=s" "--authentication-method=ActiveDirectoryAzCli"
                                        "--database-name=d" "--login-timeOut=10" "-W")
                           nil)
                      seen))
      (should (member '(ms "*SQL: ms-nodb*" ("--server=s" "--authentication-method=ActiveDirectoryAzCli"
                                             "--login-timeOut=10" "-W")
                           nil)
                      seen))
      ;; A SQL login: password through the environment, not the command line.
      (should (member '(ms "*SQL: ms-login*" ("--server=s" "--user-name=u" "--database-name=d"
                                              "--login-timeOut=10" "-W")
                           "pw")
                      seen))
      (should (member '(pg "*SQL: pg*" ("-X" "-P" "pager=off" "-h" "h" "-p" "5432" "-U" "admins"
                                        "-d" "postgres")
                           "tok" "require" "10")
                      seen)))))

(ert-deftest sql-connect-replaces-a-live-session ()
  ;; sql.el only shows a live session of the same name, so reconnecting
  ;; (an expired token, another database) would keep the old one.
  (require 'sql)
  (let* ((kdb-sql-connections '(("ms" :product ms :server "s" :database "d")))
         (buf (get-buffer-create "*SQL: ms*"))
         (old (make-pipe-process :name "old-sqlcmd" :buffer buf :noquery t)))
    (cl-letf (((symbol-function 'kdb-sql--vscode-connections) #'ignore)
              ((symbol-function 'sql-ms) #'ignore))
      (unwind-protect
          (progn (kdb-sql-connect "ms")
                 (should-not (process-live-p old)))
        (when (process-live-p old) (delete-process old))
        (kill-buffer buf)))))

(ert-deftest sql-connect-rejects-an-entry-without-product-before-asking ()
  (require 'sql)
  (let ((kdb-sql-connections '(("old" :server "s" :database "")))
        asked)
    (cl-letf (((symbol-function 'kdb-sql--vscode-connections) #'ignore)
              ((symbol-function 'kdb-sql--pick-database) (lambda (&rest _) (setq asked t) "d")))
      (should-error (kdb-sql-connect "old") :type 'user-error)
      (should-not asked))))

(ert-deftest sql-connect-from-a-script-stays-in-the-script ()
  (require 'sql)
  (let ((kdb-sql-connections '(("ms" :product ms :server "s" :database "d")))
        (script (generate-new-buffer "script.sql")))
    (cl-letf (((symbol-function 'kdb-sql--vscode-connections) #'ignore)
              ((symbol-function 'sql-ms)
               ;; As sql-product-interactive does: the SQLi buffer popped and current.
               (lambda (name) (pop-to-buffer (get-buffer-create name)))))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer script)
            (sql-mode)
            (kdb-sql-connect "ms")
            (should (eq (window-buffer (selected-window)) script))
            (should (eq (buffer-local-value 'sql-product script) 'ms))
            (should (get-buffer-window "*SQL: ms*")))
        (kill-buffer script)
        (when (get-buffer "*SQL: ms*") (kill-buffer "*SQL: ms*"))))))

(ert-deftest sql-database-picked-from-the-server-and-switched ()
  (require 'sql)
  (let ((kdb-sql-connections '(("ms" :product ms :server "s" :database "")
                               ("pg" :product postgres :server "h" :database "postgres" :user "u")))
        listed sent)
    (cl-letf (((symbol-function 'kdb-sql--vscode-connections) #'ignore)
              ((symbol-function 'kdb-sql--az-token) (lambda () "tok"))
              ((symbol-function 'kdb-sql--databases)
               (lambda (product server user) (push (list product server user) listed) '("A" "B]x")))
              ((symbol-function 'completing-read) (lambda (_prompt coll &rest _) (car (last coll))))
              ;; The switch goes to the process as is; sql-send-string would add
              ;; psql's \g, which runs the previous query again on the new database.
              ((symbol-function 'sql-send-string) (lambda (s) (push (list 'via-sql-send s) sent)))
              ((symbol-function 'comint-send-string) (lambda (_proc s) (push s sent)))
              ((symbol-function 'sql-ms) (lambda (name) (set-buffer (get-buffer-create name)) (sql-interactive-mode)))
              ((symbol-function 'sql-postgres) (lambda (name) (set-buffer (get-buffer-create name)) (sql-interactive-mode))))
      (unwind-protect
          (progn
            ;; No database saved: picked from the server's list.
            (kdb-sql-connect "ms")
            (should (equal listed '((ms "s" nil))))
            (with-current-buffer "*SQL: ms*"
              (should (equal (plist-get kdb-sql--conn :database) "B]x"))
              (call-interactively #'kdb-sql-use-database)
              (should (equal (car sent) "USE [B]]x];\ngo\n")))
            (kdb-sql-connect "pg")
            (with-current-buffer "*SQL: pg*"
              (kdb-sql-use-database "my \"db\"")
              (should (equal (car sent) "\\connect \"my \"\"db\"\"\"\n"))
              (should (equal (plist-get kdb-sql--conn :database) "my \"db\"")))
            (should-not (assq 'via-sql-send sent)))
        (dolist (b '("*SQL: ms*" "*SQL: pg*")) (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest sql-run-sends-the-region-or-the-whole-script ()
  (require 'sql)
  (let (sent)
    (cl-letf (((symbol-function 'sql-send-buffer) (lambda () (setq sent 'buffer)))
              ((symbol-function 'sql-send-region) (lambda (&rest _) (setq sent 'region))))
      (with-temp-buffer
        (sql-mode)
        (insert "DECLARE @x INT = 1;\n\nSELECT @x;\n")
        (goto-char (point-min))
        (kdb-sql-send-dwim)
        (should (eq sent 'buffer))
        (transient-mark-mode 1)
        (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
        (kdb-sql-send-dwim)
        (should (eq sent 'region))))))

(ert-deftest sql-comint-uses-the-exact-arguments ()
  (require 'sql)
  (let (args)
    (cl-letf (((symbol-function 'sql-comint) (lambda (_product params &rest _) (setq args params))))
      ;; Left over from an earlier connection: sql-comint-ms would make these
      ;; -U/-S and add -E, so the Go sqlcmd signs in with Windows.
      (let ((kdb-sql--args '("--server=s")) (sql-user "") (sql-password "") (sql-server "old"))
        (kdb-sql--comint 'ms '("-w" "300") "*SQL*"))
      (should (equal args '("--server=s")))
      (let ((kdb-sql--args nil))
        (should-error (kdb-sql--comint 'ms nil "*SQL*") :type 'user-error)))))

(ert-deftest sql-lines-leave-out-stderr ()
  ;; az prints update notices and warnings on stderr; they aren't the token.
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (should (equal (kdb-sql--lines emacs "-Q" "--batch" "--eval"
                                   "(progn (message \"WARNING: newer az\") (princ \"tok\\n\"))")
                   '("tok")))
    (should-error (kdb-sql--lines emacs "-Q" "--batch" "--eval" "(kill-emacs 3)"))
    (should-error (kdb-sql--lines "no-such-program-here") :type 'user-error)))

(ert-deftest vscode-connections-read-despite-comments ()
  (let* ((dir (make-temp-file "vscode" t))
         (file (expand-file-name "Code/User/settings.json" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "{\n  // a comment\n  \"editor.fontSize\": 14, // inline\n"
                    "  /* a block\n     comment */\n"
                    "  \"http.proxy\": \"http://proxy/x\", \"odd\": \"a,]b\",\n"
                    "  \"mssql.connections\": [\n"
                    "    {\"profileName\": \"Dev\", \"server\": \"s\", \"database\": \"d\","
                    " \"authenticationType\": \"AzureMFA\"},\n"
                    "    {\"server\": \"t\", \"database\": null, \"authenticationType\": \"SqlLogin\","
                    " \"user\": \"u\"},\n  ],\n}\n"))
          (cl-letf (((symbol-function 'getenv) (lambda (v &rest _) (and (equal v "APPDATA") dir))))
            (let ((system-type 'windows-nt))
              (should (equal (kdb-sql--vscode-connections)
                             '(("Dev" :product ms :server "s" :database "d" :user nil)
                               ("t" :product ms :server "t" :database nil :user "u")))))))
      (delete-directory dir t))))

(ert-deftest sql-results-parsed-from-sqlcmd-and-psql ()
  (let ((us (string #x1f)) (rs (string #x1e)))
    ;; sqlcmd -s US -W: blocks between blank lines, a table's second line dashes.
    (should (equal (kdb-sql--parse
                    'ms (concat "name" us "n\n----" us "-\nab" us "NULL\nc" us "\n\n"
                                "hello\nMsg 208, Level 16\n"))
                   `((table ("name" "n") (("ab" "NULL") ("c" "")))
                     (text "hello\nMsg 208, Level 16"))))
    ;; psql -A -F US -R RS: header RS rows RS (N rows), one set after another;
    ;; a value may hold a newline, and an empty set is a header alone.
    (should (equal (kdb-sql--parse
                    'postgres (concat "a" us "b" rs "1" us "x,y" rs "(1 row)\n"
                                      "multi" rs "l1\nl2" rs "(1 row)\n"
                                      "none" rs "(0 rows)\n"))
                   '((table ("a" "b") (("1" "x,y")))
                     (table ("multi") (("l1\nl2")))
                     (table ("none") nil))))))

(ert-deftest sql-results-shown-as-org-tables ()
  (let ((buf (kdb-sql--show-results
              " *test sql results*"
              '((table ("a" "b|c") (("1" "two\nlines")))
                (text "Msg 1"))
              "test")))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'org-mode))
          (should (string-match-p "^\\* Result 1 (1 row)$" (buffer-string)))
          ;; | and newlines in values don't break the table.
          (goto-char (point-min))
          (re-search-forward "^| a")
          (should (org-at-table-p))
          (should (equal (org-table-to-lisp) '(("a" "b¦c") hline ("1" "two lines"))))
          (should (string-match-p "#\\+begin_example\nMsg 1\n#\\+end_example" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest sql-names-quoted-for-each-product ()
  (should (equal (kdb-sql--qualified 'ms '("dbo" . "a]b")) "[dbo].[a]]b]"))
  (should (equal (kdb-sql--qualified 'postgres '("public" . "a\"b")) "\"public\".\"a\"\"b\""))
  (should (equal (kdb-sql--literal "O'Brien") "'O''Brien'")))

(ert-deftest sql-results-parsed-edge-cases ()
  (let ((us (string #x1f)) (rs (string #x1e)))
    ;; SELECT COUNT(*): a blank header, which the block trimming takes.
    (should (equal (kdb-sql--parse 'ms "  \n-\n11484\n")
                   '((table ("") (("11484"))))))
    ;; SELECT COUNT(*), 7: two unnamed columns keep a header, as sqlcmd
    ;; prints it (with spaces) and without; the dashes aren't a row.
    (should (equal (kdb-sql--parse 'ms (concat " " us " \n-" us "-\n11484" us "7\n"))
                   '((table ("" " ") (("11484" "7"))))))
    (should (equal (kdb-sql--parse 'ms (concat us "\n-" us "-\n1" us "2\n"))
                   '((table ("" "") (("1" "2"))))))
    ;; CRLF, as sqlcmd writes on Windows.
    (should (equal (kdb-sql--parse 'ms (concat "a" us "b\r\n-" us "-\r\n1" us "2\r\n"))
                   '((table ("a" "b") (("1" "2"))))))
    ;; psql: a value that looks like the footer doesn't end the table.
    (should (equal (kdb-sql--parse 'postgres (concat "v" rs "(1 row) x" rs "(1 row)" rs "(2 rows)\n"))
                   '((table ("v") (("(1 row) x") ("(1 row)"))))))))

(defun test-sql-dead-process ()
  (let ((p (make-pipe-process :name "test-dead" :noquery t)))
    (delete-process p)
    p))

(ert-deftest sql-query-command-lines-and-cleanup ()
  (let ((us (string #x1f)) plist got)
    (cl-letf (((symbol-function 'make-process) (lambda (&rest args) (setq plist args) 'started))
              ((symbol-function 'kdb-sql--pg-environment) (lambda () process-environment)))
      ;; sqlcmd: a batch of its own for NOCOUNT, whole values, messages on
      ;; stderr, no $(name) substitution.
      (let ((conn '(:product ms :server "s" :database "d")))
        (kdb-sql--query conn "CREATE VIEW v AS SELECT 1 AS a;" (lambda (r) (setq got r)))
        (let* ((cmd (plist-get plist :command))
               (file (car (last cmd)))
               (err-pipe (plist-get plist :stderr)))
          (should (equal (cdr cmd) (append (kdb-sql--ms-args conn)
                                           (list "-s" us "-W" "-y" "8000" "-r" "1" "-x" "-i" file))))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "SET NOCOUNT ON;\nGO\nCREATE VIEW v AS SELECT 1 AS a;\n"))
          ;; It finishes: the results, stderr last, nothing left behind.
          (with-current-buffer (plist-get plist :buffer) (insert "a\n-\n1\n"))
          (with-current-buffer (process-buffer err-pipe) (insert "Msg 1\n"))
          (funcall (plist-get plist :sentinel) (test-sql-dead-process) "finished\n")
          (should (equal got '((table ("a") (("1"))) (text "Msg 1"))))
          (should-not (file-exists-p file))
          (should-not (buffer-live-p (plist-get plist :buffer)))
          (should-not (buffer-live-p (process-buffer err-pipe)))
          (should-not (process-live-p err-pipe))))
      ;; psql: the script as is.
      (kdb-sql--query '(:product postgres :server "h" :database "db" :user "u") "SELECT 1;" #'ignore)
      (let* ((cmd (plist-get plist :command))
             (file (car (last cmd))))
        (should (equal (car cmd) "psql"))
        (should (equal (car (last cmd 2)) "-f"))
        (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "SELECT 1;\n"))
        (funcall (plist-get plist :sentinel) (test-sql-dead-process) "finished\n")))))

(ert-deftest sql-query-quit-at-the-password-leaves-nothing ()
  (let ((buffers (buffer-list))
        (processes (process-list))
        (files (directory-files temporary-file-directory nil "\\`kdb-sql.*\\.sql\\'")))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (signal 'quit nil))))
      ;; C-g isn't an error, so not should-error.
      (should (eq (condition-case nil
                      (kdb-sql--query '(:product ms :server "s" :database "d" :user "u")
                                      "SELECT 1;" #'ignore)
                    (quit 'quit))
                  'quit)))
    (should-not (seq-difference (buffer-list) buffers))
    (should-not (seq-difference (process-list) processes))
    (should (equal (directory-files temporary-file-directory nil "\\`kdb-sql.*\\.sql\\'") files))))

(ert-deftest sql-session-conn-is-a-copy ()
  ;; C-c C-u changes the session's own in place; a table list keeps its database.
  (with-temp-buffer
    (sql-interactive-mode)
    (setq kdb-sql--conn (list :product 'ms :server "s" :database "a"))
    (let ((conn (kdb-sql--session-conn)))
      (should (equal conn kdb-sql--conn))
      (plist-put kdb-sql--conn :database "b")
      (should (equal (plist-get conn :database) "a")))))

(ert-deftest sql-table-list-and-its-commands ()
  (let (listed ran
        (case-fold-search nil))         ; upper case is the point
    (cl-letf (((symbol-function 'kdb-sql--session-conn) (lambda () (list :product 'ms :server "s" :database "d")))
              ((symbol-function 'kdb-sql--query)
               (lambda (_conn sql callback)
                 (setq listed sql)
                 (funcall callback '((table ("TABLE_SCHEMA" "TABLE_NAME" "TABLE_TYPE")
                                            (("dbo" "a" "BASE TABLE") ("x y" "it's" "VIEW")))))))
              ((symbol-function 'kdb-sql--run-and-show) (lambda (_conn sql &rest _) (push sql ran)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (with-temp-buffer (kdb-sql-tables))
            (should (string-match-p "FROM INFORMATION_SCHEMA\\.TABLES" listed))
            (with-current-buffer "*SQL tables: d*"
              (should (equal (mapcar #'car tabulated-list-entries) '(("dbo" . "a") ("x y" . "it's"))))
              (should (equal (aref (cadr (car tabulated-list-entries)) 2) "TABLE"))
              ;; Odd names, quoted in the SQL that RET and s build.
              (goto-char (point-min))
              (forward-line 1)
              (kdb-sql-tables-columns)
              (kdb-sql-tables-select)
              (should (member "SELECT TOP 100 * FROM [x y].[it's];" ran))
              (should (seq-find (lambda (q) (string-match-p
                                             "FROM INFORMATION_SCHEMA\\.COLUMNS WHERE TABLE_SCHEMA = 'x y' AND TABLE_NAME = 'it''s'"
                                             q))
                                ran))
              ;; i and g once the script is gone.
              (setq kdb-sql--script (generate-new-buffer "gone.sql"))
              (kill-buffer kdb-sql--script)
              (should-error (kdb-sql-tables-insert) :type 'user-error)
              (should-error (kdb-sql-tables-refresh) :type 'user-error)))
        (when (get-buffer "*SQL tables: d*") (kill-buffer "*SQL tables: d*"))))))

(ert-deftest vscode-connections-survive-a-broken-settings-file ()
  (let* ((dir (make-temp-file "vscode" t))
         (file (expand-file-name "Code/User/settings.json" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          ;; A long string, and a file that isn't JSON at all.
          (with-temp-file file
            (insert "{\"x\": \"" (make-string 200000 ?a) "\", /* " (make-string 50000 ?b) " */\n"
                    "\"mssql.connections\": [{\"server\": \"s\"}]}"))
          (cl-letf (((symbol-function 'getenv) (lambda (v &rest _) (and (equal v "APPDATA") dir))))
            (let ((system-type 'windows-nt))
              (should (equal (kdb-sql--vscode-connections)
                             '(("s" :product ms :server "s" :database nil :user nil))))
              (with-temp-file file (insert "{ not json"))
              (should-not (kdb-sql--vscode-connections)))))
      (delete-directory dir t))))

(ert-deftest sql-menu-from-any-buffer ()
  (require 'transient)
  (should (eq (key-binding (kbd "C-c d")) #'kdb-sql))
  (should (commandp 'kdb-sql-menu))
  ;; Every command the menu offers exists.
  (dolist (cmd '(kdb-sql-connect kdb-sql-new-script kdb-sql-open-script kdb-sql-send-dwim
                 kdb-sql-run-to-org kdb/export-deliverable kdb-sql-tables kdb-sql-use-database
                 kdb-sql-switch-session kdb-sql-disconnect))
    (should (commandp cmd)))
  (with-temp-buffer
    (should (string-match-p "\\`SQL: not connected" (kdb-sql--menu-status)))
    ;; Run commands outside a script would run that buffer's text.
    (insert "not SQL")
    (should-error (kdb-sql-send-dwim) :type 'user-error)
    (should-error (kdb-sql-run-to-org) :type 'user-error)))

(ert-deftest sql-new-script-opens-a-connected-buffer ()
  (let (connected-from)
    (cl-letf (((symbol-function 'kdb-sql-connect)
               (lambda (name) (setq connected-from (list name (current-buffer) major-mode)))))
      (save-window-excursion
        (kdb-sql-new-script "dev x")
        (unwind-protect
            (progn
              (should (equal (car connected-from) "dev x"))
              (should (eq (nth 2 connected-from) 'sql-mode))
              (should (equal (buffer-name (nth 1 connected-from)) "dev x.sql")))
          (kill-buffer (nth 1 connected-from)))))))

(ert-deftest sql-sessions-listed-switched-and-closed ()
  (let* ((a (get-buffer-create "*SQL: a*"))
         (pa (make-pipe-process :name "test-a" :buffer a :noquery t))
         (dead (get-buffer-create "*SQL: dead*")))
    (unwind-protect
        (progn
          (with-current-buffer a (sql-interactive-mode)
                               (setq kdb-sql--conn (list :product 'ms :server "s" :database "A")))
          (with-current-buffer dead (sql-interactive-mode)
                               (setq kdb-sql--conn (list :product 'ms :server "s" :database "D")))
          ;; Only live sessions, and one with no process isn't one.
          (should (equal (kdb-sql--sessions) (list a)))
          (with-current-buffer a
            (should (string-match-p "\\`SQL: \\*SQL: a\\*, on A\\'" (kdb-sql--menu-status))))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "*SQL: a*")))
            (save-window-excursion
              (kdb-sql-switch-session)
              (should (eq (window-buffer) a)))
            (with-temp-buffer (kdb-sql-disconnect)))
          (should-not (process-live-p pa))
          (should-not (kdb-sql--sessions))
          (should-error (kdb-sql-switch-session) :type 'user-error))
      (when (process-live-p pa) (delete-process pa))
      (kill-buffer a)
      (kill-buffer dead))))

(ert-deftest sql-new-script-cancelled-leaves-nothing ()
  (let ((buffers (buffer-list)))
    (cl-letf (((symbol-function 'kdb-sql-connect) (lambda (_) (user-error "No token"))))
      (save-window-excursion
        (should-error (kdb-sql-new-script "dev y") :type 'user-error)))
    (should-not (seq-difference (buffer-list) buffers))))

(ert-deftest sql-menu-has-no-autoloaded-entries ()
  ;; transient loads autoloaded suffixes as the menu opens: an autoload
  ;; from org there would load org on the first C-c d.
  (require 'transient)
  (let (cmds)
    (cl-labels ((walk (x) (cond ((and (symbolp x) (commandp x)) (push x cmds))
                                ((consp x) (walk (car x)) (walk (cdr x)))
                                ((vectorp x) (mapc #'walk x)))))
      (walk (get 'kdb-sql-menu 'transient--layout)))
    (should (memq 'kdb-sql-connect cmds))
    (should-not (seq-filter (lambda (c) (autoloadp (symbol-function c))) cmds))))

(ert-deftest sql-script-menu-status-and-closing-its-own-session ()
  (let* ((sqli (get-buffer-create "*SQL: b*"))
         (proc (make-pipe-process :name "test-b" :buffer sqli :noquery t))
         (script (generate-new-buffer "b.sql")))
    (unwind-protect
        (progn
          (with-current-buffer sqli
            (sql-interactive-mode)
            (setq kdb-sql--conn (list :product 'ms :server "s" :database "B")))
          (with-current-buffer script
            (sql-mode)
            (setq-local sql-buffer (buffer-name sqli))
            (should (string-match-p "\\`SQL: \\*SQL: b\\*, on B\\'" (kdb-sql--menu-status)))
            ;; From the script: its own session, no asking.
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Asked"))))
              (kdb-sql-disconnect)))
          (should-not (process-live-p proc)))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer script)
      (kill-buffer sqli))))

(ert-deftest term-buffer-closes-after-terms-own-sentinel ()
  (let* ((buf (generate-new-buffer " *term test*"))
         (proc (make-pipe-process :name "term-test" :buffer buf :noquery t))
         (live-in-term-sentinel nil))
    (set-process-sentinel proc (lambda (&rest _)
                                 (push (buffer-live-p buf) live-in-term-sentinel)))
    (with-current-buffer buf (run-hooks 'term-exec-hook))
    (funcall (process-sentinel proc) proc "finished\n")
    (should (car (last live-in-term-sentinel)))   ; term's ran, buffer still there
    (should-not (buffer-live-p buf))))

(ert-deftest fido-keeps-orderless-and-scrolling ()
  (require 'icomplete)
  (require 'orderless)
  (with-temp-buffer
    (let ((icomplete-mode t))
      ;; As fido's setup leaves the minibuffer.
      (setq-local completion-styles '(flex)
                  completion-category-defaults '((buffer (styles flex basic)))
                  icomplete-scroll nil)
      (kdb-orderless-override-fido)
      (should (equal completion-styles '(orderless basic)))
      (should-not completion-category-defaults)
      (should icomplete-scroll))))

;;; init-test.el ends here
