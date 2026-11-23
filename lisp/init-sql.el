;;; init-sql.el --- SQL: Azure SQL and PostgreSQL sessions -*- lexical-binding: t; -*-

;;; Code:

;; Azure SQL and Azure Database for PostgreSQL, signed in with Entra ID
;; through the Azure CLI (az login), the way VS Code's mssql extension and
;; `psql' with an az token do.  C-c C-a connects, C-c C-c runs the region
;; or the whole script, C-c C-u switches database, C-c C-o runs it into
;; org tables (C-c o x there exports CSV or Excel), C-c C-t lists tables.
;; C-c d, from any buffer, has all of it in a menu.

(defvar kdb-sql-connections nil
  "Saved connections for `kdb-sql-connect', as (NAME . PLIST); set in personal.el.
PLIST has :product, `ms' (Azure SQL) or `postgres', :server, :database,
and for postgres :user, the Entra user or group the server knows (and
:port, default 5432).  VS Code's saved mssql connections are offered too:
  (setq kdb-sql-connections
        \\='((\"dev\" :product ms :server \"dev.database.windows.net\" :database \"app\")
          (\"pg-dev\" :product postgres :server \"pg-dev.postgres.database.azure.com\"
           :database \"postgres\" :user \"db-admins\")))")

(defvar kdb-sql--args nil
  "The exact program arguments for the connection being started.")

(defun kdb-sql--comint (product _options &optional buf-name)
  "Start PRODUCT's interpreter with exactly `kdb-sql--args'.
sql.el's own builders add -E (Windows sign-in) to sqlcmd when there is no
user or password, which overrides Entra ID, and fill in -S/-d/-U from the
previous connection's values: what made connecting work only sometimes."
  (unless kdb-sql--args
    (user-error "Connect with C-c C-a (kdb-sql-connect)"))
  (sql-comint product kdb-sql--args buf-name))

(defun kdb-sql--strip-jsonc ()
  "Turn the JSON with comments and trailing commas VS Code writes into JSON.
Strings are matched first and kept, so // in a URL stays.  The patterns
are unrolled, so a long string or comment can't overflow the matcher."
  (dolist (junk '("//[^\n]*\\|/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" ",\\([ \t\r\n]*[]}]\\)"))
    (goto-char (point-min))
    (while (re-search-forward (concat "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\"\\|\\(" junk "\\)") nil t)
      (when (match-beginning 1)
        (replace-match (or (match-string 2) "") t t)))))

(defun kdb-sql--vscode-connections ()
  "VS Code's saved mssql connections, as in `kdb-sql-connections'."
  (let ((file (expand-file-name
               "Code/User/settings.json"
               (pcase system-type
                 ('windows-nt (getenv "APPDATA"))
                 ('darwin "~/Library/Application Support")
                 (_ "~/.config")))))
    ;; Whatever is wrong with the file, saved connections still work.
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (kdb-sql--strip-jsonc)
          (goto-char (point-min))
          (mapcar (lambda (c)
                    (let-alist c
                      (list (or .profileName .server)
                            :product 'ms :server .server :database .database
                            :user (and (equal .authenticationType "SqlLogin") .user))))
                  (alist-get 'mssql.connections
                             (json-parse-buffer :object-type 'alist :array-type 'list
                                                :null-object nil))))))))

(defun kdb-sql--lines (program &rest args)
  "PROGRAM's output lines, without its stderr (az's warnings, for one).
Signals an error naming PROGRAM when it fails or isn't installed."
  (with-temp-buffer
    (let ((status (condition-case nil
                      (apply #'call-process program nil '(t nil) nil args)
                    (file-missing (user-error "%s isn't installed or isn't on PATH" program)))))
      (unless (eq status 0)
        (error "%s exited with %s" program status))
      (split-string (buffer-string) "[\r\n]+" t "[ \t]+"))))

(defvar kdb-sql--token nil
  "(TIME . TOKEN): the last Azure CLI token, reused for a few minutes.")

(defun kdb-sql--az-token ()
  "An Entra ID token for Azure Database for PostgreSQL, from the Azure CLI.
Kept 5 minutes, so listing databases and then connecting asks az once."
  (if (and kdb-sql--token (< (float-time (time-since (car kdb-sql--token))) 300))
      (cdr kdb-sql--token)
    (let ((token (condition-case nil
                     (car (kdb-sql--lines "az" "account" "get-access-token" "--resource-type"
                                          "oss-rdbms" "--query" "accessToken" "-o" "tsv"))
                   (error nil))))
      (unless token
        (user-error "No token from the Azure CLI: run az login, then connect again"))
      (setq kdb-sql--token (cons (current-time) token))
      token)))

(defun kdb-sql--pg-environment ()
  "psql's environment: the az token as password, SSL, a short connect timeout."
  (append (list (concat "PGPASSWORD=" (kdb-sql--az-token)) "PGSSLMODE=require"
                "PGCONNECT_TIMEOUT=10")
          process-environment))

(defvar-local kdb-sql--conn nil
  "In a SQLi buffer, the connection it was started with, database included.")

(defun kdb-sql--databases (product server user)
  "The databases on SERVER that USER can open, or nil if they can't be listed.
A SQL login (ms with USER) isn't listed: it would ask for the password twice."
  (ignore-errors
    (pcase product
      ('ms (unless user
             (kdb-sql--lines (or (bound-and-true-p sql-ms-program) "sqlcmd")
                             (concat "--server=" server)
                             "--authentication-method=ActiveDirectoryAzCli" "--login-timeOut=5"
                             (concat "--query=SET NOCOUNT ON; SELECT name FROM sys.databases"
                                     " WHERE state_desc = 'ONLINE' ORDER BY name")
                             "-h" "-1" "-W")))
      ('postgres
       (let ((process-environment (kdb-sql--pg-environment)))
         (kdb-sql--lines "psql" "-X" "-At" "-h" server "-U" user "-d" "postgres" "-c"
                         (concat "SELECT datname FROM pg_database"
                                 " WHERE datallowconn AND NOT datistemplate ORDER BY 1")))))))

(defun kdb-sql--pick-database (product server user)
  "Ask for a database on SERVER, from its list when it can be read."
  (if-let* ((dbs (kdb-sql--databases product server user)))
      (completing-read (format "Database on %s: " server) dbs)
    (read-string (format "Database on %s (RET for its default): " server))))

(defun kdb-sql-connect (name)
  "Connect to the saved database NAME, or a new one typed in.
A session already open under NAME is replaced, so this also reconnects:
after an expired token, or to start over on another database."
  (interactive
   (list (completing-read "Connection: "
                          ;; A saved one of the same name comes first and wins.
                          (delete-dups (mapcar #'car (append kdb-sql-connections
                                                             (kdb-sql--vscode-connections)))))))
  (require 'sql)
  (let* ((from (current-buffer))         ; sql-ms/sql-postgres make the SQLi buffer current
         (conn (or (cdr (assoc name (append kdb-sql-connections (kdb-sql--vscode-connections))))
                   (let ((product (intern (completing-read "Product: " '("ms" "postgres") nil t))))
                     (list :product product
                           :server (read-string "Server: ")
                           :database (read-string "Database (RET to pick from the server): ")
                           :user (and (eq product 'postgres) (read-string "Entra user or group: "))))))
         (product (plist-get conn :product))
         (server (plist-get conn :server))
         (user (plist-get conn :user))
         (buf-name (format "*SQL: %s*" name)))
    (unless (memq product '(ms postgres))
      (user-error "%s needs :product ms or postgres, not %S" name product))
    ;; VS Code leaves it to a dropdown; without one the server's default
    ;; database is used, and the script's tables aren't there.
    (let ((database (let ((d (plist-get conn :database)))
                      (if (seq-empty-p d) (kdb-sql--pick-database product server user) d))))
      ;; sql.el only shows a live session of the same name, it doesn't reconnect.
      (when-let* ((proc (get-buffer-process buf-name)))
        (delete-process proc))
      (if (eq product 'ms)
          ;; Long options with =: the Go sqlcmd quietly falls back to Windows
          ;; sign-in when they're mixed with -G or follow -d.
          (let* ((kdb-sql--args
                  ;; -W: columns as wide as their values, not their types
                  (append (kdb-sql--ms-args (plist-put (copy-sequence conn) :database database))
                          (list "-W")))
                 (process-environment
                  (if user
                      (cons (concat "SQLCMDPASSWORD=" (read-passwd (format "Password for %s: " user)))
                            process-environment)
                    process-environment)))
            (sql-ms buf-name))
        (let ((kdb-sql--args (append (list "-X" "-P" "pager=off")
                                     (kdb-sql--pg-args
                                      (plist-put (copy-sequence conn) :database database))))
              ;; Read at sign-in only, so an expired token doesn't end the session.
              (process-environment (kdb-sql--pg-environment)))
          (sql-postgres buf-name)))
      (when-let* ((sqli (get-buffer buf-name)))
        (with-current-buffer sqli
          (setq kdb-sql--conn (plist-put (copy-sequence conn) :database database)))))
    ;; The buffer it was started from runs on it, whatever its mode: sql.el
    ;; links only sql-mode ones.  Back there, the results showing beside it:
    ;; connecting moves point to the empty SQLi buffer, which looks like a
    ;; query that failed.
    (unless (or (eq from (get-buffer buf-name))
                (with-current-buffer from (derived-mode-p 'sql-interactive-mode)))
      (with-current-buffer from
        (setq-local sql-buffer buf-name)
        (when (derived-mode-p 'sql-mode) (sql-set-product product)))
      (when-let* ((win (get-buffer-window from)))
        (select-window win)))))

(defun kdb-sql--conninfo-value (value)
  "VALUE quoted for a libpq connection string."
  (format "'%s'" (replace-regexp-in-string "['\\\\]" "\\\\\\&" value)))

(defun kdb-sql--use-command (conn database)
  "The command that switches session CONN to DATABASE, newline included.
PostgreSQL gets a whole connection string with a fresh az token: psql
would reuse the one it started with, which lasts about an hour."
  (if (eq (plist-get conn :product) 'postgres)
      (format "\\connect \"%s\"\n"
              (string-replace
               "\"" "\"\""
               (mapconcat (lambda (kv) (concat (car kv) "=" (kdb-sql--conninfo-value (cdr kv))))
                          `(("host" . ,(plist-get conn :server))
                            ("port" . ,(number-to-string (or (plist-get conn :port) 5432)))
                            ("user" . ,(plist-get conn :user))
                            ("dbname" . ,database)
                            ("sslmode" . "require")
                            ("password" . ,(kdb-sql--az-token)))
                          " ")))
    (format "USE [%s];\ngo\n" (string-replace "]" "]]" database))))

(defun kdb-sql-use-database (database)
  "Switch the connected session to DATABASE, picked from the server's list.
From a script, its session; from a SQLi buffer, that one."
  (interactive
   (let* ((sqli (kdb-sql--session))
          (conn (or (buffer-local-value 'kdb-sql--conn sqli)
                    (user-error "%s wasn't started by kdb-sql-connect" (buffer-name sqli)))))
     (list (kdb-sql--pick-database (plist-get conn :product) (plist-get conn :server)
                                   (plist-get conn :user)))))
  (let ((sqli (kdb-sql--session)))
    (with-current-buffer sqli
      (setq kdb-sql--conn (plist-put kdb-sql--conn :database database))
      ;; Straight to the process, not sql-send-string: that adds psql's \g,
      ;; which would run the previous query again on the new database.
      (comint-send-string (get-buffer-process sqli) (kdb-sql--use-command kdb-sql--conn database)))
    (message "%s: now on %s" (buffer-name sqli) database)))

(defun kdb-sql--linked ()
  "The live session this buffer is linked to (a SQLi buffer is its own), or nil."
  (cond ((derived-mode-p 'sql-interactive-mode)
         (and (sql-buffer-live-p (current-buffer)) (current-buffer)))
        ((and (local-variable-p 'sql-buffer) (sql-buffer-live-p sql-buffer))
         (get-buffer sql-buffer))))

(defun kdb-sql--session ()
  "The session for the current buffer, any buffer: the one it's linked to,
else the only one open, else one picked, else a new connection.  Whichever,
the buffer is linked to it, so the next run goes there without asking."
  (or (kdb-sql--linked)
      ;; Linked to one that has ended (an expired az sign-in, say): say so,
      ;; with its last words, rather than quietly connecting elsewhere.
      ;; (sql-mode makes `sql-buffer' local in every script, nil until a
      ;; first session: that's no ended one.)
      (when-let* ((name (and (local-variable-p 'sql-buffer) sql-buffer))
                  (gone (get-buffer name)))
        (user-error "%s has ended: %s  (C-c d c connects again)" name
                    (with-current-buffer gone
                      (truncate-string-to-width
                       (string-trim (buffer-substring-no-properties
                                     (max (point-min) (- (point-max) 300)) (point-max)))
                       160 nil nil "…"))))
      (let ((sessions (kdb-sql--sessions)))
        (if (null sessions)
            (progn (call-interactively #'kdb-sql-connect)
                   (or (kdb-sql--linked) (user-error "Not connected")))
          (kdb-sql-use-session
           (if (cdr sessions)
               (completing-read "Run on session: " (mapcar #'buffer-name sessions) nil t)
             (buffer-name (car sessions))))))))

(defun kdb-sql-use-session (session)
  "Run this buffer's SQL on SESSION from now on; return its buffer.
Any buffer: a .sql file, an org or markdown note, a scratch buffer."
  (interactive
   (list (let ((sessions (mapcar #'buffer-name (kdb-sql--sessions))))
           (unless sessions (user-error "No SQL session open: C-c d c connects"))
           (completing-read "Run this buffer on: " sessions nil t))))
  (let ((sqli (get-buffer session)))
    (setq-local sql-buffer (buffer-name sqli))
    (when (derived-mode-p 'sql-mode)
      (sql-set-product (plist-get (buffer-local-value 'kdb-sql--conn sqli) :product)))
    (when (called-interactively-p 'any)
      (message "%s runs on %s" (buffer-name) session))
    sqli))

(defun kdb-sql--named-database (text)
  "The database TEXT names in a \"-- database: NAME\" line, else the
buffer's first such line (near the top), else nil.
Claude sessions start the SQL they share with one; a selection out of
the middle of a script still has the buffer's."
  (let ((case-fold-search t)
        (line "^[ \t]*--[ \t]*database:[ \t]*\\([^ \t\r\n]+\\)"))
    (cond ((string-match line text) (match-string 1 text))
          ((save-excursion
             (goto-char (point-min))
             (re-search-forward line (min (point-max) 4000) t))
           (match-string-no-properties 1)))))

(defun kdb-sql--to-named-database (text)
  "Switch this buffer's session to the database TEXT names, if another."
  (when-let* ((database (kdb-sql--named-database text))
              (sqli (kdb-sql--session))
              (conn (buffer-local-value 'kdb-sql--conn sqli))
              ((not (string-equal-ignore-case database (or (plist-get conn :database) "")))))
    (kdb-sql-use-database database)))

(defun kdb-sql-send-dwim ()
  "Run the region, or else the whole buffer, as VS Code does; any buffer.
Not the paragraph: a script's DECLAREs and the statements using them,
often with blank lines between, have to go as one batch.  On the
database a \"-- database: NAME\" line names, switching to it first."
  (interactive)
  (kdb-sql--session)
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (kdb-sql--to-named-database (buffer-substring-no-properties beg end))
    (sql-send-region beg end)))

(defun kdb-sql-send-statement ()
  "Run the statement at point: its paragraph, between blank lines; any buffer.
On the database a \"-- database: NAME\" line names, as `kdb-sql-send-dwim'."
  (interactive)
  (kdb-sql--session)
  (pcase-let ((`(,beg . ,end) (save-excursion
                                (cons (progn (backward-paragraph) (point))
                                      (progn (forward-paragraph) (point))))))
    (kdb-sql--to-named-database (buffer-substring-no-properties beg end))
    (sql-send-region beg end)))

;; Results as org tables, and a list of tables.  Both run apart from the
;; session, in the background, on its server and database: what the
;; session set up itself (USE, temp tables) isn't there.

(defconst kdb-sql--us (string #x1f) "Column separator in output read by machine.")
(defconst kdb-sql--rs (string #x1e) "Row separator in psql's output read by machine.")

(defun kdb-sql--ms-args (conn)
  "sqlcmd's sign-in arguments for CONN: long options only (see `kdb-sql-connect')."
  (append (list (concat "--server=" (plist-get conn :server)))
          (if (plist-get conn :user)
              (list (concat "--user-name=" (plist-get conn :user)))
            (list "--authentication-method=ActiveDirectoryAzCli"))
          (unless (seq-empty-p (plist-get conn :database))
            (list (concat "--database-name=" (plist-get conn :database))))
          (list "--login-timeOut=10")))

(defun kdb-sql--pg-args (conn)
  "psql's connection arguments for CONN."
  (list "-h" (plist-get conn :server)
        "-p" (number-to-string (or (plist-get conn :port) 5432))
        "-U" (plist-get conn :user) "-d" (plist-get conn :database)))

(defun kdb-sql--parse (product output)
  "PRODUCT's machine-read OUTPUT as a list of (table HEADER ROWS) and (text STRING)."
  (let ((split (lambda (rec) (split-string rec kdb-sql--us)))
        (output (string-replace "\r\n" "\n" output)))
    (if (eq product 'postgres)
        ;; header RS row RS ... RS (N rows) LF: one result set after another.
        ;; The LF, which psql always ends the footer with, tells it from a
        ;; value starting "(1 row)".  A value holding RS or US still splits.
        (let ((footer (concat kdb-sql--rs "([0-9]+ rows?)\n"))
              (pos 0) results)
          (while (string-match footer output pos)
            ;; Positions first: split-string and string-trim reset the match data.
            (let* ((end (match-end 0))
                   (recs (split-string (substring output pos (match-beginning 0)) kdb-sql--rs)))
              (push (list 'table (funcall split (string-trim-left (car recs)))
                          (mapcar split (cdr recs)))
                    results)
              (setq pos end)))
          (let ((rest (string-trim (substring output pos))))
            (unless (string-empty-p rest) (push (list 'text rest) results)))
          (nreverse results))
      ;; sqlcmd: blocks between blank lines; a table's second line is dashes.
      ;; A lone unnamed column (SELECT COUNT(*)) has a blank header, which
      ;; the trimming takes: the block then starts with plain dashes.  Not
      ;; two or more: their header keeps its separators, so it stays.
      (mapcar (lambda (block)
                (let ((lines (split-string block "\n")))
                  (when (string-match-p "\\`-+\\'" (car lines))
                    (push "" lines))
                  (if (and (cdr lines)
                           (string-match-p (concat "\\`[-" kdb-sql--us "]+\\'") (cadr lines)))
                      (list 'table (funcall split (car lines)) (mapcar split (cddr lines)))
                    (list 'text block))))
              (split-string output "\n[ \t]*\n" t "[ \t\n]+")))))

(defun kdb-sql--query (conn sql callback)
  "Run SQL on CONN in the background, then call CALLBACK with its results.
They're as `kdb-sql--parse' gives them, with anything on stderr (errors,
notices) last, as text."
  (let* ((product (plist-get conn :product))
         ;; First: the password prompt and the az token can quit or fail,
         ;; before there's a file, buffer or pipe to leave behind.
         (process-environment
          (cond ((eq product 'postgres)
                 (cons "PGCLIENTENCODING=UTF8" (kdb-sql--pg-environment)))
                ((plist-get conn :user)
                 (cons (concat "SQLCMDPASSWORD="
                               (read-passwd (format "Password for %s: " (plist-get conn :user))))
                       process-environment))
                (t process-environment)))
         (file (make-temp-file "kdb-sql" nil ".sql"))
         (out (generate-new-buffer " *kdb-sql out*"))
         (err (generate-new-buffer " *kdb-sql err*"))
         ;; Its own pipe, quiet: the default sentinel would write
         ;; "Process ... finished" into the errors shown with the results.
         (err-pipe (make-pipe-process :name "kdb-sql-query stderr" :buffer err
                                      :noquery t :sentinel #'ignore))
         (coding-system-for-write 'utf-8))
    (with-temp-file file
      ;; In a batch of its own: CREATE VIEW, PROCEDURE and the like must
      ;; come first in theirs.  It's a session setting, so it carries over.
      (insert (if (eq product 'postgres) sql (concat "SET NOCOUNT ON;\nGO\n" sql)) "\n"))
    (condition-case err-data
        (make-process
         :name "kdb-sql-query" :buffer out :stderr err-pipe :noquery t
         :coding 'utf-8-dos               ; sqlcmd ends lines with CRLF on Windows
         :command (if (eq product 'postgres)
                      (append (list "psql" "-X" "-q" "-A" "-F" kdb-sql--us "-R" kdb-sql--rs
                                    "-P" "footer=on")
                              (kdb-sql--pg-args conn) (list "-f" file))
                    ;; -s before -i: -i takes the rest as more input files.
                    ;; -y 8000: (max) and xml values whole, not cut at 256.
                    ;; -r 1: PRINT and errors to stderr, not between tables.
                    ;; -x: no $(name) substitution in the script.
                    (append (list (or (bound-and-true-p sql-ms-program) "sqlcmd"))
                            (kdb-sql--ms-args conn)
                            (list "-s" kdb-sql--us "-W" "-y" "8000" "-r" "1" "-x" "-i" file)))
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             ;; stderr comes through its own pipe, which may still be draining:
             ;; until it's quiet for 0.2 s.
             (while (accept-process-output err-pipe 0.2))
             (delete-process err-pipe)
             (let ((results (kdb-sql--parse product (with-current-buffer out (buffer-string))))
                   (errors (string-trim (with-current-buffer err (buffer-string)))))
               (kill-buffer out)
               (kill-buffer err)
               (delete-file file)
               (funcall callback (if (string-empty-p errors) results
                                   (append results (list (list 'text errors)))))))))
      ;; sqlcmd or psql not found: nothing to leave behind.
      (error (delete-process err-pipe)
             (kill-buffer out)
             (kill-buffer err)
             (delete-file file)
             (signal (car err-data) (cdr err-data))))))

(defun kdb-sql--org-cell (value)
  "VALUE as an org table cell: | and newlines would break the table."
  (replace-regexp-in-string "[\r\n]+" " " (string-replace "|" "¦" value)))

(defun kdb-sql--show-results (name results title)
  "Show RESULTS in org buffer NAME, under TITLE; return the buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t) (n 0))
      (erase-buffer)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (insert "#+TITLE: " title "\n"
              "# " (format-time-string "%F %T")
              ".  In a table, C-c o x exports it to CSV or Excel.\n")
      (dolist (r results)
        (pcase r
          (`(table ,header ,rows)
           (insert (format "\n* Result %d (%d row%s)\n" (setq n (1+ n)) (length rows)
                           (if (= 1 (length rows)) "" "s")))
           (let ((start (point)))
             (dolist (row (cons header rows))
               (insert "| " (mapconcat #'kdb-sql--org-cell row " | ") " |\n")
               (when (eq row header) (insert "|-\n")))
             ;; Aligning is slow on big results; unaligned, it's still a table.
             (when (< (length rows) 2000)
               (save-excursion (goto-char start) (org-table-align)))))
          (`(text ,text)
           (insert "\n#+begin_example\n" text "\n#+end_example\n"))))
      ;; The Go sqlcmd prints nothing at all for a result set without rows.
      (unless results
        (insert "\nNo rows.\n"))
      (goto-char (point-min))
      (re-search-forward "^|" nil t)
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun kdb-sql--session-conn ()
  "A copy of the connection of the current script's session (or SQLi buffer's).
A copy: switching the session's database (C-c C-u) changes its own in place,
and a table list opened before must keep the database it lists."
  (let ((sqli (kdb-sql--session)))
    (copy-sequence
     (or (buffer-local-value 'kdb-sql--conn sqli)
         (user-error "%s wasn't started by kdb-sql-connect" (buffer-name sqli))))))

(defun kdb-sql--run-and-show (conn sql name title)
  "Run SQL on CONN in the background, then show its results in NAME under TITLE."
  (message "Running on %s..." (or (plist-get conn :database) (plist-get conn :server)))
  (kdb-sql--query conn sql
                  (lambda (results)
                    (display-buffer (kdb-sql--show-results name results title))
                    (message "%s: %d result set(s)" title
                             (seq-count (lambda (r) (eq (car r) 'table)) results)))))

(defun kdb-sql-run-to-org ()
  "Run the region, or else the whole script, with the results as org tables.
On the database of the buffer's session, in the background; any buffer."
  (interactive)
  (let* ((conn (kdb-sql--session-conn))
         (sql (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Its own connection: on the named database, the session left as is.
    (when-let* ((database (kdb-sql--named-database sql)))
      (setq conn (plist-put conn :database database)))
    (kdb-sql--run-and-show conn sql (format "*SQL results: %s*" (buffer-name))
                           (format "%s on %s" (buffer-name)
                                   (or (plist-get conn :database) (plist-get conn :server))))))

(defun kdb-sql--ident (product name)
  "NAME quoted as a PRODUCT identifier."
  (if (eq product 'postgres)
      (format "\"%s\"" (string-replace "\"" "\"\"" name))
    (format "[%s]" (string-replace "]" "]]" name))))

(defun kdb-sql--literal (value)
  "VALUE as an SQL string literal."
  (format "'%s'" (string-replace "'" "''" value)))

(defvar-local kdb-sql--script nil
  "In a table list, the script it was opened from, for `kdb-sql-tables-insert'.")

(defvar-keymap kdb-sql-tables-mode-map
  :doc "Keys in a list of tables."
  "RET" #'kdb-sql-tables-columns
  "s" #'kdb-sql-tables-select
  "w" #'kdb-sql-tables-copy
  "i" #'kdb-sql-tables-insert
  "g" #'kdb-sql-tables-refresh)

(define-derived-mode kdb-sql-tables-mode tabulated-list-mode "SQL Tables"
  "The tables and views of a database.
\\<kdb-sql-tables-mode-map>\\[kdb-sql-tables-columns] its columns, \\[kdb-sql-tables-select] its first 100 rows, \\[kdb-sql-tables-insert] its name into the script, \\[kdb-sql-tables-copy] copy the name, \\[kdb-sql-tables-refresh] refresh."
  (setq tabulated-list-format [("Schema" 24 t) ("Name" 56 t) ("Type" 12 t)]
        tabulated-list-sort-key '("Schema" . nil))
  (tabulated-list-init-header))

(defun kdb-sql-tables ()
  "List the tables and views of the script's session database, dired style."
  (interactive)
  (let ((conn (kdb-sql--session-conn))
        (script (current-buffer)))
    (message "Listing tables on %s..." (plist-get conn :database))
    (kdb-sql--query
     ;; Upper case for SQL Server databases with a case-sensitive collation;
     ;; PostgreSQL folds unquoted names to lower case.
     conn (concat "SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES"
                  " WHERE TABLE_SCHEMA NOT IN ('pg_catalog', 'information_schema', 'sys')"
                  " ORDER BY 1, 2;")
     (lambda (results)
       (let ((table (seq-find (lambda (r) (eq (car r) 'table)) results)))
         (unless table
           (user-error "No tables listed: %s" (or (cadr (assq 'text results)) "no output")))
         (with-current-buffer (get-buffer-create
                               (format "*SQL tables: %s*" (or (plist-get conn :database)
                                                              (plist-get conn :server))))
           (kdb-sql-tables-mode)
           (setq kdb-sql--conn conn
                 kdb-sql--script script
                 tabulated-list-entries
                 (mapcar (lambda (row)
                           (list (cons (nth 0 row) (nth 1 row))
                                 (vector (nth 0 row) (nth 1 row)
                                         (string-remove-prefix "BASE " (nth 2 row)))))
                         (nth 2 table)))
           (tabulated-list-print)
           (pop-to-buffer (current-buffer))))))))

(defun kdb-sql--tables-at-point ()
  "The (SCHEMA . TABLE) on this line."
  (or (tabulated-list-get-id) (user-error "No table on this line")))

(defun kdb-sql--qualified (product id)
  "ID, (SCHEMA . TABLE), as a quoted PRODUCT name."
  (concat (kdb-sql--ident product (car id)) "." (kdb-sql--ident product (cdr id))))

(defun kdb-sql-tables-columns ()
  "Show the columns of the table on this line."
  (interactive)
  (let ((id (kdb-sql--tables-at-point)))
    (kdb-sql--run-and-show
     kdb-sql--conn
     (format (concat "SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, IS_NULLABLE,"
                     " COLUMN_DEFAULT FROM INFORMATION_SCHEMA.COLUMNS"
                     " WHERE TABLE_SCHEMA = %s AND TABLE_NAME = %s ORDER BY ORDINAL_POSITION;")
             (kdb-sql--literal (car id)) (kdb-sql--literal (cdr id)))
     (format "*SQL columns: %s.%s*" (car id) (cdr id))
     (format "Columns of %s.%s" (car id) (cdr id)))))

(defun kdb-sql-tables-select ()
  "Show the first 100 rows of the table on this line."
  (interactive)
  (let* ((id (kdb-sql--tables-at-point))
         (product (plist-get kdb-sql--conn :product))
         (name (kdb-sql--qualified product id)))
    (kdb-sql--run-and-show
     kdb-sql--conn
     (if (eq product 'postgres)
         (format "SELECT * FROM %s LIMIT 100;" name)
       (format "SELECT TOP 100 * FROM %s;" name))
     (format "*SQL rows: %s.%s*" (car id) (cdr id))
     (format "First 100 rows of %s.%s" (car id) (cdr id)))))

(defun kdb-sql-tables-copy ()
  "Copy the quoted name of the table on this line."
  (interactive)
  (let ((name (kdb-sql--qualified (plist-get kdb-sql--conn :product) (kdb-sql--tables-at-point))))
    (kill-new name)
    (message "Copied %s" name)))

(defun kdb-sql-tables-insert ()
  "Insert the quoted name of the table on this line into the script."
  (interactive)
  (let ((name (kdb-sql--qualified (plist-get kdb-sql--conn :product) (kdb-sql--tables-at-point))))
    (unless (buffer-live-p kdb-sql--script) (user-error "The script is gone"))
    (with-current-buffer kdb-sql--script (insert name))
    (if-let* ((win (get-buffer-window kdb-sql--script)))
        (select-window win)
      (pop-to-buffer kdb-sql--script))))

(defun kdb-sql-tables-refresh ()
  "List the tables again."
  (interactive)
  (if (buffer-live-p kdb-sql--script)
      (with-current-buffer kdb-sql--script (kdb-sql-tables))
    (user-error "The script is gone; list tables from another with C-c C-t")))

;; The C-c d menu: everything SQL, from any buffer.

(defvar kdb-sql-scripts-directory "~/"
  "Where `kdb-sql-open-script' starts looking; set in personal.el.")

(defun kdb-sql--sessions ()
  "Live SQL sessions started by `kdb-sql-connect', most recent first."
  (seq-filter (lambda (b) (and (buffer-local-value 'kdb-sql--conn b)
                               (get-buffer-process b)
                               (process-live-p (get-buffer-process b))))
              (buffer-list)))

(defun kdb-sql-new-script (name)
  "A new SQL script on connection NAME: a sql-mode buffer, connected."
  (interactive
   (list (completing-read "New script on: "
                          (delete-dups (mapcar #'car (append kdb-sql-connections
                                                             (kdb-sql--vscode-connections)))))))
  (let ((script (generate-new-buffer (format "%s.sql" name)))
        connected)
    ;; Quit at a prompt, or no az token: no empty script left behind.
    (unwind-protect
        (progn
          (pop-to-buffer-same-window script)
          (sql-mode)
          (kdb-sql-connect name)
          (setq connected t))
      (unless connected (kill-buffer script)))))

(defun kdb-sql-open-script ()
  "Open a SQL script file, from `kdb-sql-scripts-directory'."
  (interactive)
  (find-file (read-file-name "SQL script: " (file-name-as-directory kdb-sql-scripts-directory))))

(defun kdb-sql-switch-session ()
  "Go to an open SQL session's results buffer."
  (interactive)
  (let ((sessions (mapcar #'buffer-name (kdb-sql--sessions))))
    (unless sessions (user-error "No SQL session open: C-c d c connects"))
    (pop-to-buffer (completing-read "Session: " sessions nil t))))

(defun kdb-sql-disconnect ()
  "Close a SQL session: this buffer's, or one picked from the open ones."
  (interactive)
  (let* ((sqli (or (kdb-sql--linked)
                   (let ((sessions (mapcar #'buffer-name (kdb-sql--sessions))))
                     (unless sessions (user-error "No SQL session open"))
                     (get-buffer (completing-read "Close session: " sessions nil t))))))
    (delete-process (get-buffer-process sqli))
    (message "Closed %s" (buffer-name sqli))))

(defun kdb-sql--menu-status ()
  "The menu's heading: the session this buffer runs on, or what Run will do."
  (if-let* ((sqli (kdb-sql--linked))
            (conn (buffer-local-value 'kdb-sql--conn sqli)))
      (format "SQL: this buffer runs on %s, %s" (buffer-name sqli)
              (or (plist-get conn :database) (plist-get conn :server)))
    (pcase (length (kdb-sql--sessions))
      (0 "SQL: not connected; Run connects first")
      (1 "SQL: this buffer isn't linked; Run uses the open session")
      (n (format "SQL: this buffer isn't linked; Run asks which of %d sessions" n)))))

(with-eval-after-load 'transient
  (transient-define-prefix kdb-sql-menu ()
    "Everything SQL: connections, scripts, results and tables."
    [:description kdb-sql--menu-status
     ["Start"
      ("c" "Connect…" kdb-sql-connect)
      ("n" "New script on…" kdb-sql-new-script)
      ("f" "Open a script…" kdb-sql-open-script)]
     ["Run (this buffer, any mode)"
      ("r" "Selection, else whole buffer" kdb-sql-send-dwim)
      ("p" "Statement at point" kdb-sql-send-statement)
      ("o" "Selection or buffer → org tables" kdb-sql-run-to-org)
      ("l" "Run this buffer on…" kdb-sql-use-session)
      ;; A lambda, not the autoloaded command: transient loads autoloads as
      ;; the menu opens, which would load org on every first C-c d.
      ("x" "Export this org table (CSV/Excel)" (lambda () (interactive) (kdb/export-deliverable))
       :inapt-if-not (lambda () (and (derived-mode-p 'org-mode) (org-at-table-p))))]
     ["Browse"
      ("t" "Tables and views" kdb-sql-tables)
      ("u" "Switch database" kdb-sql-use-database)]
     ["Sessions"
      ("s" "Go to a session" kdb-sql-switch-session)
      ("k" "Close a session" kdb-sql-disconnect)]]))

(defun kdb-sql ()
  "The SQL menu."
  (interactive)
  (require 'sql)
  (require 'transient)
  ;; As if the menu were the command, else transient draws it twice.
  (setq this-command 'kdb-sql-menu)
  (kdb-sql-menu))

(global-set-key (kbd "C-c d") #'kdb-sql)
;; From org, as the C-c o commands are: org loads init-org.
(autoload 'kdb/export-deliverable "org" nil t)

(use-package sql
  :ensure nil
  :defer t
  :config
  (setq sql-ms-program (or (executable-find "sqlcmd") "sqlcmd")
        ;; sqlcmd runs nothing until GO, psql nothing until ;
        sql-send-terminator t
        ;; No login prompts: kdb-sql-connect has everything
        sql-ms-login-params nil
        sql-postgres-login-params nil)
  (dolist (product '(ms postgres))
    (sql-set-product-feature product :sqli-comint-func #'kdb-sql--comint))
  :bind (:map sql-mode-map
              ("C-c C-c" . kdb-sql-send-dwim)
              ("C-c C-a" . kdb-sql-connect)
              ("C-c C-u" . kdb-sql-use-database)
              ("C-c C-o" . kdb-sql-run-to-org)
              ("C-c C-t" . kdb-sql-tables)))

(provide 'init-sql)
;;; init-sql.el ends here
