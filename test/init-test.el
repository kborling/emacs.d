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
                                            (getenv "PGSSLMODE"))
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
      ;; Entra ID through az, long options only: never -E, -G or -d.
      (should (member '(ms "*SQL: ms*" ("--server=s" "--authentication-method=ActiveDirectoryAzCli"
                                        "--database-name=d" "-W")
                           nil)
                      seen))
      (should (member '(ms "*SQL: ms-nodb*" ("--server=s" "--authentication-method=ActiveDirectoryAzCli" "-W") nil)
                      seen))
      ;; A SQL login: password through the environment, not the command line.
      (should (member '(ms "*SQL: ms-login*" ("--server=s" "--user-name=u" "--database-name=d" "-W") "pw")
                      seen))
      (should (member '(pg "*SQL: pg*" ("-X" "-P" "pager=off" "-h" "h" "-p" "5432" "-U" "admins"
                                        "-d" "postgres")
                           "tok" "require")
                      seen)))))

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
        (kill-buffer "*SQL: ms*")))))

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
              ((symbol-function 'sql-send-string) (lambda (s) (push s sent)))
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
              (should (equal (car sent) "USE [B]]x];")))
            (kdb-sql-connect "pg")
            (with-current-buffer "*SQL: pg*"
              (kdb-sql-use-database "other")
              (should (equal (car sent) "\\connect other"))
              (should (equal (plist-get kdb-sql--conn :database) "other"))))
        (dolist (b '("*SQL: ms*" "*SQL: pg*")) (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest sql-run-sends-the-region-or-the-whole-script ()
  (require 'sql)
  (let (sent)
    (cl-letf (((symbol-function 'sql-send-buffer) (lambda () (setq sent 'buffer)))
              ((symbol-function 'sql-send-region) (lambda (&rest _) (setq sent 'region))))
      (with-temp-buffer
        (insert "DECLARE @x INT = 1;

SELECT @x;
")
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
      (let ((kdb-sql--args '("--server=s")) (sql-user "") (sql-password "") (sql-server "old"))
        (kdb-sql--comint 'ms '("-w" "300") "*SQL*"))
      (should (equal args '("--server=s"))))))

(ert-deftest vscode-connections-read-despite-comments ()
  (let* ((dir (make-temp-file "vscode" t))
         (file (expand-file-name "Code/User/settings.json" dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "{\n  // a comment\n  \"mssql.connections\": [\n"
              "    {\"profileName\": \"Dev\", \"server\": \"s\", \"database\": \"d\","
              " \"authenticationType\": \"AzureMFA\"},\n"
              "    {\"server\": \"t\", \"database\": \"\", \"authenticationType\": \"SqlLogin\","
              " \"user\": \"u\"},\n  ],\n}\n"))
    (cl-letf (((symbol-function 'getenv) (lambda (v &rest _) (and (equal v "APPDATA") dir))))
      (let ((system-type 'windows-nt))
        (should (equal (kdb-sql--vscode-connections)
                       '(("Dev (VS Code)" :product ms :server "s" :database "d" :user nil)
                         ("t (VS Code)" :product ms :server "t" :database "" :user "u"))))))
    (delete-directory dir t)))

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
