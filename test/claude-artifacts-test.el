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


(ert-deftest busy-animations ()
  (require 'agent-shell)
  ;; The bar slides out and back; every frame differs from the next, so
  ;; each tick redraws and the hopping icon never skips a step.
  (should (seq-every-p (lambda (f) (= (string-width f) 8)) kdb-claude--busy-bar))
  (should (member "░░░░░░░░" kdb-claude--busy-bar))
  (should (cl-loop for (a b) on (append kdb-claude--busy-bar (list (car kdb-claude--busy-bar)))
                   while b always (not (equal a b))))
  ;; Through agent-shell's own frame picker, as the header uses it.
  (let ((agent-shell-busy-indicator-frames #'kdb-claude--busy-frames)
        (agent-shell-show-busy-indicator t)
        (tick 8))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () `((:heartbeat . ((:status . busy) (:value . ,tick)))))))
      (let ((kdb-claude-busy-indicator 'claude))
        (should (equal (agent-shell--busy-indicator-frame) (concat " " (nth 8 kdb-claude--busy-bar)))))
      (let ((kdb-claude-busy-indicator 'wave))
        (should (equal (agent-shell--busy-indicator-frame) " ▇"))))))


(ert-deftest logo-hops ()
  (require 'agent-shell)
  (let* ((icon (make-temp-file "claude-icon" nil ".png" "PNGDATA"))
         (fetched nil))
    (cl-letf (((symbol-function 'agent-shell--fetch-agent-icon)
               (lambda (name) (setq fetched name) (and (equal name "claude-color.png") icon))))
      ;; A frame embeds the icon itself, moved for its step of the hop.
      (let ((file (kdb-claude--logo-icon "kdb-logo-4|claude-color.png")))
        (should (equal fetched "claude-color.png"))
        (should (eq (image-supported-file-p file) 'svg))
        (let ((svg (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (should (string-match-p (regexp-quote (base64-encode-string "PNGDATA" t)) svg))
          (should (with-temp-buffer (insert svg) (libxml-parse-xml-region (point-min) (point-max))))))
      (let ((svg (lambda (k) (kdb-claude--logo-svg icon k))))
        ;; Squashed on the ground, a fifth of its height up at the peak,
        ;; leaning left on the first hop and right on the second.
        (should (string-match-p "translate(50 100) rotate(0) scale(0.93[0-9]* 0.68" (funcall svg 0)))
        (should (string-match-p "translate(50 79) rotate(-6) " (funcall svg 4)))
        (should (string-match-p "translate(50 79) rotate(6) " (funcall svg 16)))
        (should (string-match-p "translate(50 100) rotate(0) scale(0.82 0.82)" (funcall svg 11)))
        ;; Most steps differ, so it visibly moves every tick.
        (should (< 15 (length (seq-uniq (mapcar svg (number-sequence 0 23)))))))
      ;; Other icons are agent-shell's business.
      (should-not (kdb-claude--logo-icon "claude-color.png")))
    (delete-file icon))
  (let ((model '((:icon-name . "claude-color.png") (:busy-indicator-frame . " ░   "))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () '((:heartbeat . ((:status . busy) (:value . 7)))))))
      ;; Busy: a step every tick; tick 7 is step 7.
      (let ((kdb-claude-busy-indicator 'claude))
        (should (equal (map-elt (kdb-claude--logo-header model) :icon-name)
                       "kdb-logo-7|claude-color.png")))
      ;; Another animation style leaves the icon alone.
      (let ((kdb-claude-busy-indicator 'wave))
        (should (eq (kdb-claude--logo-header model) model))))
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:heartbeat . ((:status . ended)))))))
      (let ((kdb-claude-busy-indicator 'claude))
        (should (eq (kdb-claude--logo-header model) model))))))

(ert-deftest busy-frame-keeps-its-width-when-idle ()
  (require 'agent-shell)
  (let ((agent-shell-busy-indicator-frames #'kdb-claude--busy-frames)
        (agent-shell-show-busy-indicator t))
    (dolist (status '(busy ended))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () `((:heartbeat . ((:status . ,status) (:value . ,(and (eq status 'busy) 5))))))))
        (let ((kdb-claude-busy-indicator 'claude))
          (should (= 9 (string-width (agent-shell--busy-indicator-frame)))))))
    ;; Idle is blank.
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:heartbeat . ((:status . ended)))))))
      (should (string-blank-p (agent-shell--busy-indicator-frame)))))
  ;; Someone else's animation is left alone.
  (let ((agent-shell-busy-indicator-frames 'wide)
        (agent-shell-show-busy-indicator t))
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:heartbeat . ((:status . ended)))))))
      (should-not (agent-shell--busy-indicator-frame)))))

;;; claude-artifacts-test.el ends here
