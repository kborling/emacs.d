;;; claude-artifacts-test.el --- Showing files Claude writes -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/claude-artifacts-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'init-claude-workflow)

(defvar test-dir nil)

(defun test-session ()
  "A stand-in session buffer, alone in the frame, in a fresh directory."
  (setq test-dir (file-name-as-directory (make-temp-file "artifacts" t)))
  (delete-other-windows)
  (setq kdb-claude--artifact-pane nil)
  (let ((buf (generate-new-buffer "*Claude test*")))
    (with-current-buffer buf (setq default-directory test-dir))
    (switch-to-buffer buf)
    buf))

(defun test-write (buf id file &rest tool-call)
  "Create FILE, then report tool call ID for it to BUF, as the adapter would."
  (with-temp-file (expand-file-name file test-dir) (insert "# " file "\n"))
  (kdb-claude--note-artifact
   buf id (append tool-call
                  `((:status . "completed") (:kind . "edit")
                    (:raw-input . ((file_path . ,(expand-file-name file test-dir))))
                    (:locations . (((path . ,(expand-file-name file test-dir)))))))))

(defun test-shown ()
  "Files shown in windows other than the selected one."
  (delq nil (mapcar (lambda (w) (unless (eq w (selected-window))
                                  (buffer-file-name (window-buffer w))))
                    (window-list))))

(ert-deftest artifact-document-shows-in-side-pane ()
  (let ((buf (test-session)))
    (test-write buf "t1" "report.md")
    (should (equal (test-shown) (list (expand-file-name "report.md" test-dir))))
    (should (eq (selected-window) (get-buffer-window buf)))   ; typing stays in the session
    (should (equal (window-buffer (window-in-direction 'right (get-buffer-window buf)))
                   (get-file-buffer (expand-file-name "report.md" test-dir))))
    ;; The next document replaces it in the same pane.
    (let ((pane (car kdb-claude--artifact-pane)))
      (test-write buf "t2" "notes.org")
      (should (eq (car kdb-claude--artifact-pane) pane))
      (should (equal (test-shown) (list (expand-file-name "notes.org" test-dir))))
      (should (= 2 (length (window-list)))))))

(ert-deftest artifact-code-is-listed-not-shown ()
  (let ((buf (test-session)))
    (test-write buf "t1" "main.el")
    (should-not (test-shown))
    (should (equal (buffer-local-value 'kdb-claude--artifacts buf)
                   (list (expand-file-name "main.el" test-dir))))))

(ert-deftest artifact-only-completed-edits-once ()
  (let ((buf (test-session)))
    (with-temp-file (expand-file-name "a.md" test-dir) (insert "x"))
    (kdb-claude--note-artifact buf "t1" `((:status . "in_progress") (:kind . "edit")
                                          (:locations . (((path . ,(expand-file-name "a.md" test-dir)))))))
    (kdb-claude--note-artifact buf "t2" `((:status . "completed") (:kind . "read")
                                          (:locations . (((path . ,(expand-file-name "a.md" test-dir)))))))
    (should-not (buffer-local-value 'kdb-claude--artifacts buf))
    (should-not (test-shown))
    ;; Completed, from locations alone (no raw input); a repeated update is ignored.
    (dotimes (_ 2)
      (kdb-claude--note-artifact buf "t1" `((:status . "completed") (:kind . "edit")
                                            (:locations . (((path . ,(expand-file-name "a.md" test-dir))))))))
    (should (equal (buffer-local-value 'kdb-claude--artifacts buf)
                   (list (expand-file-name "a.md" test-dir))))
    (should (equal (test-shown) (list (expand-file-name "a.md" test-dir))))))

(ert-deftest artifact-rewrite-refreshes-pane ()
  (let ((buf (test-session))
        (file nil))
    (test-write buf "t1" "plan.md")
    (setq file (expand-file-name "plan.md" test-dir))
    (sleep-for 1.1)                              ; a newer mtime
    (with-temp-file file (insert "# plan v2\n"))
    (kdb-claude--note-artifact buf "t2" `((:status . "completed") (:kind . "edit")
                                          (:locations . (((path . ,file))))))
    (should (equal (with-current-buffer (get-file-buffer file) (buffer-string)) "# plan v2\n"))
    (should (equal (buffer-local-value 'kdb-claude--artifacts buf) (list file)))))

(ert-deftest artifact-auto-show-off ()
  (let ((buf (test-session))
        (kdb-claude-artifact-auto-show nil))
    (test-write buf "t1" "report.md")
    (should-not (test-shown))
    (should (buffer-local-value 'kdb-claude--artifacts buf))))

(ert-deftest artifact-open-picks-newest-first ()
  (let ((buf (test-session))
        offered)
    (let ((kdb-claude-artifact-auto-show nil))
      (test-write buf "t1" "old.md")
      (test-write buf "t2" "new.el"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (should (eq (cdr (assq 'display-sort-function
                                        (cdr (funcall table "" nil 'metadata))))
                             'identity))
                 (setq offered (all-completions "" table))
                 (car offered))))
      (with-current-buffer buf (kdb-claude-open-artifact)))
    (should (equal offered (mapcar #'abbreviate-file-name
                                   (list (expand-file-name "new.el" test-dir)
                                         (expand-file-name "old.md" test-dir)))))
    ;; Explicitly opened: shown and selected, code included.
    (should (equal (buffer-file-name (window-buffer (selected-window)))
                   (expand-file-name "new.el" test-dir)))))

(ert-deftest artifact-track-subscribes-to-tool-calls ()
  (let ((buf (test-session)) sub)
    (cl-letf (((symbol-function 'agent-shell-subscribe-to)
               (lambda (&rest args) (setq sub args) 'token)))
      (with-current-buffer buf (kdb-claude--track-artifacts)))
    (should (eq (plist-get sub :event) 'tool-call-update))
    (should (eq (plist-get sub :shell-buffer) buf))
    (with-temp-file (expand-file-name "via-event.md" test-dir) (insert "x"))
    (funcall (plist-get sub :on-event)
             `((:event . tool-call-update)
               (:data . ((:tool-call-id . "t9")
                         (:tool-call . ((:status . "completed") (:kind . "edit")
                                        (:raw-input . ((file_path . ,(expand-file-name "via-event.md" test-dir))))))))))
    (should (equal (test-shown) (list (expand-file-name "via-event.md" test-dir))))))

(ert-deftest artifact-hidden-session-leaves-layout-alone ()
  ;; A session in another tab (not visible) writes a document.
  (let ((buf (test-session)) msg)
    (switch-to-buffer (get-buffer-create "*my work*"))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (test-write buf "t1" "report.md"))
    (should (= 1 (length (window-list))))
    (should (eq (window-buffer (selected-window)) (get-buffer "*my work*")))
    (should (equal msg (format "%s wrote report.md (C-c l a to open)" (buffer-name buf))))
    (should (buffer-local-value 'kdb-claude--artifacts buf))))

(ert-deftest artifact-reused-pane-is-not-taken-over ()
  (let ((buf (test-session)))
    (test-write buf "t1" "report.md")
    (let ((pane (car kdb-claude--artifact-pane)))
      ;; You use the pane for your own work...
      (set-window-buffer pane (get-buffer-create "*my work*"))
      (test-write buf "t2" "notes.md")
      ;; ...so the next document gets a new pane beside the session.
      (should (eq (window-buffer pane) (get-buffer "*my work*")))
      (should (member (expand-file-name "notes.md" test-dir) (test-shown)))
      (should-not (eq (car kdb-claude--artifact-pane) pane)))))

(ert-deftest artifact-pane-opens-beside-the-session ()
  ;; The session is visible, but you're in another window.
  (let* ((buf (test-session))
         (other (split-window (selected-window) nil 'below)))
    (set-window-buffer other (get-buffer-create "*my work*"))
    (select-window other)
    (test-write buf "t1" "report.md")
    (should (eq (selected-window) other))
    (should (equal (window-buffer (window-in-direction 'right (get-buffer-window buf)))
                   (get-file-buffer (expand-file-name "report.md" test-dir))))))

(ert-deftest header-gets-symbol-font-fallbacks ()
  (let* ((model (list (cons :buffer-name "x") (cons :font-family "Comic Code") (cons :width 800)))
         (out (kdb-claude--header-symbol-fonts model)))
    (should (equal (map-elt out :font-family) "Comic Code, Segoe UI Symbol, Segoe UI Emoji"))
    (should (equal (map-elt out :width) 800))
    (should (= 1 (seq-count (lambda (c) (eq (car c) :font-family)) out)))
    (should (equal (map-elt model :font-family) "Comic Code")))  ; original untouched
  (let ((model (list (cons :font-family 'unspecified))))
    (should (eq (kdb-claude--header-symbol-fonts model) model))))

;;; claude-artifacts-test.el ends here
