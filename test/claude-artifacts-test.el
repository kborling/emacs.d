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
                    (:raw-input . ((file_path . ,(expand-file-name file test-dir))
                                   (content . "...")))
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
    ;; An Edit of a file already there isn't one Claude made.
    (kdb-claude--note-artifact buf "t3" `((:status . "completed") (:kind . "edit")
                                          (:raw-input . ((file_path . ,(expand-file-name "a.md" test-dir))
                                                         (old_string . "x") (new_string . "y")))))
    (should-not (buffer-local-value 'kdb-claude--artifacts buf))
    ;; A completed Write; a repeated update is ignored.
    (dotimes (_ 2)
      (kdb-claude--note-artifact buf "t1" `((:status . "completed") (:kind . "edit")
                                            (:raw-input . ((file_path . ,(expand-file-name "a.md" test-dir))
                                                           (content . "x"))))))
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
                                          (:raw-input . ((file_path . ,file) (content . "# plan v2\n")))))
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
                                        (:raw-input . ((file_path . ,(expand-file-name "via-event.md" test-dir))
                                                       (content . "x")))))))))
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
    (should (equal msg (format "%s created report.md (C-c l a to open)" (buffer-name buf))))
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
      ;; Busy ticks reuse the last full header, which may hold a frame
      ;; already: the hop carries on from it rather than freezing.
      (let ((kdb-claude-busy-indicator 'claude)
            (cached '((:icon-name . "kdb-logo-5|claude-color.png") (:busy-indicator-frame . " ░"))))
        (should (equal (map-elt (kdb-claude--logo-header cached) :icon-name)
                       "kdb-logo-7|claude-color.png")))
      ;; Another animation style leaves the icon alone.
      (let ((kdb-claude-busy-indicator 'wave))
        (should (eq (kdb-claude--logo-header model) model))))
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:heartbeat . ((:status . ended)))))))
      (let ((kdb-claude-busy-indicator 'claude))
        (should (eq (kdb-claude--logo-header model) model))
        ;; Done: a left-over frame goes back to the still icon.
        (should (equal (map-elt (kdb-claude--logo-header '((:icon-name . "kdb-logo-5|claude-color.png")))
                                :icon-name)
                       "claude-color.png"))))))

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

(ert-deftest emacs-sessions-are-noted-when-ready ()
  (let ((buf (generate-new-buffer "*claude test*")) sub noted)
    (cl-letf (((symbol-function 'agent-shell-subscribe-to) (lambda (&rest args) (setq sub args) 'token))
              ((symbol-function 'agent-shell-session-id) (lambda (&rest _) "sess-42"))
              ((symbol-function 'kdb-claude--note-emacs-session) (lambda (id) (push id noted))))
      (with-current-buffer buf (kdb-claude--track-emacs-session))
      (should (eq (plist-get sub :event) 'prompt-ready))
      (funcall (plist-get sub :on-event) '((:event . prompt-ready)))
      (should (equal noted '("sess-42"))))
    (kill-buffer buf)))

(ert-deftest tabs-mark-sessions-waiting-for-you ()
  (require 'agent-shell-attention)
  (let ((a (generate-new-buffer "*claude a*"))
        (b (generate-new-buffer "*claude b*"))
        (agent-shell-attention--pending (make-hash-table :test #'eq))
        (tab-bar-tab-hints t)
        (tabspaces-mode nil))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (delete-other-windows)
          (switch-to-buffer a)
          (tab-bar-new-tab)
          (switch-to-buffer b)                     ; tab 1 shows a, tab 2 shows b
          (cl-flet ((names () (mapcar (lambda (tab)
                                        (substring-no-properties
                                         (kdb-claude--tab-name-waiting
                                          "x" tab (1+ (seq-position (frame-parameter nil 'tabs) tab)))))
                                      (frame-parameter nil 'tabs))))
            (should (equal (names) '("x" "x")))     ; nobody waiting
            ;; Entries as agent-shell-attention stores them.
            (puthash a (cons "Turn complete" 1.0) agent-shell-attention--pending)
            (should (equal (names) '("✓ x" "x")))   ; the hidden tab with a: a reply
            (remhash a agent-shell-attention--pending)
            (puthash b (list :label "Permission: Bash ls" :timestamp 1.0 :request-id 7)
                     agent-shell-attention--pending)
            (should (equal (names) '("x" "● x")))   ; now the current tab: wants input
            ;; Each its own color.
            (should (eq (get-text-property 0 'face (kdb-claude--tab-name-waiting
                                                     "x" (nth 1 (frame-parameter nil 'tabs)) 2))
                        'kdb-claude-tab-input))
            ;; Input outranks a reply in the same tab.
            (puthash a (cons "Turn complete" 1.0) agent-shell-attention--pending)
            (should (equal (names) '("✓ x" "● x")))
            (split-window)
            (switch-to-buffer a)                    ; tab 2 now shows a and b
            (should (equal (names) '("✓ x" "● x")))
            ;; Marks follow what a tab shows, not what tabspaces says was
            ;; once visited there.
            (let ((tabspaces-mode t))
              (cl-letf (((symbol-function 'tabspaces--buffer-list) (lambda (&rest _) (list a b))))
                (should (equal (names) '("✓ x" "● x")))))))
      (ignore-errors (tab-bar-close-other-tabs))
      (tab-bar-mode -1)
      (kill-buffer a) (kill-buffer b))))

(ert-deftest reveal-in-file-manager-per-system ()
  (let (calls)
  ;; Paths as this system expands them: /tmp is c:/tmp on Windows.
  (let ((plan (expand-file-name "/tmp/plan.docx"))
        (tmp (file-name-as-directory (expand-file-name "/tmp/")))
        (interprogram-cut-function nil)
        (interprogram-paste-function nil))
    (cl-letf (((symbol-function 'w32-shell-execute) (lambda (&rest a) (push (cons 'w32 a) calls)))
              ((symbol-function 'call-process) (lambda (&rest a) (push a calls) 0))
              ((symbol-function 'dbus-call-method) (lambda (&rest _) (error "No file manager"))))
      (let ((system-type 'windows-nt))
        (kdb-reveal-in-file-manager "/Users/me/My Docs/plan.docx"))
      ;; Backslashes, and quoted for the space.
      (should (equal (car calls) `(w32 "open" "explorer.exe"
                                       ,(format "/select,\"%s\"" (subst-char-in-string
                                                                  ?/ ?\\ (expand-file-name "/Users/me/My Docs/plan.docx"))))))
      (should (string-match-p "\\\\me\\\\My Docs\\\\plan" (nth 3 (car calls))))
      (let ((system-type 'darwin))
        (kdb-reveal-in-file-manager "/tmp/plan.docx"))
      (should (equal (car calls) (list "open" nil 0 nil "-R" plan)))
      ;; Linux with no FileManager1 service: open the folder.
      (let ((system-type 'gnu/linux))
        (kdb-reveal-in-file-manager "/tmp/plan.docx"))
      (should (equal (car calls) (list "xdg-open" nil 0 nil tmp)))
      (should (equal (current-kill 0) plan))
      ;; A folder is opened, not selected in its parent.
      (let ((system-type 'darwin))
        (kdb-reveal-in-file-manager "/tmp/"))
      (should (equal (car calls) (list "open" nil 0 nil tmp)))
      (let ((system-type 'windows-nt))
        (kdb-reveal-in-file-manager "/tmp/"))
      (should (string-prefix-p "\"" (nth 3 (car calls))))))))

(ert-deftest artifact-reveal-offers-folders ()
  (let ((buf (test-session))
        offered opened)
    (let ((kdb-claude-artifact-auto-show nil))
      (make-directory (expand-file-name "out" test-dir))
      (test-write buf "t1" "a.md")
      (test-write buf "t2" "out/b.docx")
      (test-write buf "t3" "c.md"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq offered (all-completions "" table))
                 (cadr offered)))
              ((symbol-function 'kdb-reveal-in-file-manager) (lambda (f) (setq opened f))))
      (with-current-buffer buf (kdb-claude-reveal-artifact)))
    (should (equal offered (mapcar #'abbreviate-file-name
                                   (list test-dir (expand-file-name "out/" test-dir)))))
    (should (equal opened (expand-file-name "out/" test-dir)))))

(ert-deftest busy-header-has-few-frames ()
  ;; Bar and hop repeat together: 24 headers to draw, each frame made once.
  (should (zerop (mod (length kdb-claude--logo-hop) (length kdb-claude--busy-bar))))
  (let ((kdb-claude--logo-files (make-hash-table :test #'equal))
        (made 0))
    (cl-letf (((symbol-function 'kdb-claude--logo-file) (lambda (_) (cl-incf made) "/tmp/frame.svg")))
      (dotimes (_ 3) (kdb-claude--logo-icon "kdb-logo-3|claude"))
      (should (equal (kdb-claude--logo-icon "kdb-logo-3|claude") "/tmp/frame.svg"))
      (should-not (kdb-claude--logo-icon "claude"))           ; not a hop frame
      (should (= made 1)))))

(ert-deftest tabs-redrawn-only-when-waiting-changes ()
  (require 'agent-shell-attention)
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (kdb-claude--waiting-drawn nil)
        (a (generate-new-buffer "a")) (b (generate-new-buffer "b"))
        (redraws 0))
    (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&rest _) (cl-incf redraws))))
      (kdb-claude--redraw-tabs)                    ; nothing waits, nothing changed
      (should (= redraws 0))
      (puthash a t agent-shell-attention--pending)
      (dotimes (_ 3) (kdb-claude--redraw-tabs))    ; the 1 s timer, a waiting
      (should (= redraws 1))
      (remhash a agent-shell-attention--pending)   ; same count, another session
      (puthash b t agent-shell-attention--pending)
      (kdb-claude--redraw-tabs)
      (should (= redraws 2))
      ;; The same session, from a reply to wanting input: its mark changes.
      (puthash b (list :label "Permission: Edit" :timestamp 2.0 :request-id 3)
               agent-shell-attention--pending)
      (kdb-claude--redraw-tabs)
      (should (= redraws 3)))
    (kill-buffer a) (kill-buffer b)))

(ert-deftest heartbeat-rate-is-set ()
  (let ((kdb-claude-busy-beats-per-second 5))
    (should (= 5 (plist-get (kdb-claude--heartbeat-rate '(:on-heartbeat ignore)) :beats-per-second)))))

(ert-deftest artifact-temp-working-files-ignored ()
  ;; The review skill writes many files under _temp/: not listed, shown or announced.
  (let ((buf (test-session)) msgs)
    (make-directory (expand-file-name "_temp" test-dir))
    (cl-letf (((symbol-function 'message) (lambda (&rest args) (push args msgs))))
      (test-write buf "t1" "_temp/notes.md"))
    (should-not (buffer-local-value 'kdb-claude--artifacts buf))
    (should-not (test-shown))
    (should-not msgs)
    ;; A real document still is.
    (test-write buf "t2" "report.md")
    (should (equal (buffer-local-value 'kdb-claude--artifacts buf)
                   (list (expand-file-name "report.md" test-dir))))))

(ert-deftest artifact-announcement-waits-for-no-prompt ()
  ;; A session you're not looking at doesn't write over a prompt you're answering.
  (let ((buf (test-session)) msgs)
    (switch-to-buffer (get-buffer-create "*my work*"))
    (cl-letf (((symbol-function 'message) (lambda (&rest args) (push args msgs)))
              ((symbol-function 'active-minibuffer-window) (lambda () (selected-window))))
      (test-write buf "t1" "report.md"))
    (should (buffer-local-value 'kdb-claude--artifacts buf))   ; still listed
    (should-not msgs)))

(ert-deftest session-id-copied-and-shared-live ()
  (let ((buf (test-session)) queued
        (interprogram-cut-function nil)          ; the kill ring only, no clipboard
        (interprogram-paste-function nil))
    (with-current-buffer buf
      (setq major-mode 'agent-shell-mode)
      (cl-letf (((symbol-function 'agent-shell-session-id) (lambda (&rest _) "sess-42"))
                ((symbol-function 'kdb-claude--queue-prompt)
                 (lambda (b text) (setq queued (list b text)))))
        (should (equal (kdb-claude-copy-session-id) "sess-42"))
        (should (equal (current-kill 0) "sess-42"))
        ;; Shared live: queued, so a turn in progress isn't cut into.
        (kdb-claude-remote-control)
        (should (equal queued (list buf "/remote-control")))
        ;; Again: stop sharing; and again: share.
        (kdb-claude-remote-control)
        (should (equal queued (list buf "/remote-control off")))
        (kdb-claude-remote-control)
        (should (equal queued (list buf "/remote-control")))))
    (with-temp-buffer
      (should-error (kdb-claude-copy-session-id) :type 'user-error)
      (should-error (kdb-claude-remote-control) :type 'user-error))))

(ert-deftest marked-tabs-sized-like-the-others ()
  ;; Tab width goes by the face a tab's name starts with (Emacs 30 always,
  ;; 31 once the face list is extended): a marked tab must still count.
  (require 'agent-shell-attention)
  (dolist (face '(kdb-claude-tab-input kdb-claude-tab-done kdb-claude-tab-busy))
    (should (memq face tab-bar-auto-width-faces))
    (let ((item (list 'tab-2 'menu-item (concat (propertize "● " 'face face)
                                                (propertize "2 Fix the parser" 'face 'tab-bar-tab-inactive)))))
      (should (tab-bar-auto-width-predicate-default item))))
  ;; Plain tabs still count too.
  (should (tab-bar-auto-width-predicate-default
           (list 'tab-1 'menu-item (propertize "1 api" 'face 'tab-bar-tab-inactive)))))

(ert-deftest tabs-mark-sessions-still-working ()
  (require 'agent-shell-attention)
  (let ((a (generate-new-buffer "*claude a*"))
        (b (generate-new-buffer "*claude b*"))
        (agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (kdb-claude--waiting-drawn nil)
        (redraws 0))
    (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&rest _) (cl-incf redraws))))
      (delete-other-windows)
      (switch-to-buffer a)                          ; the current tab shows a, and b for index 1
      (let ((tabspaces-mode nil))
        (cl-flet ((mark (index) (when (= index 1) (split-window) (switch-to-buffer b))
                    (prog1 (substring-no-properties
                            (kdb-claude--tab-name-waiting "x" '(current-tab) (1+ index)))
                      (when (= index 1) (delete-other-windows) (switch-to-buffer a)))))
          (should (equal (mark 0) "x"))
          ;; Working: dimmed ◌, and the tab bar redraws for it.
          (puthash a 1 agent-shell-attention--busy)
          (kdb-claude--redraw-tabs)
          (should (= redraws 1))
          (should (equal (mark 0) "◌ x"))
          (should (eq (get-text-property 0 'face (kdb-claude--tab-name-waiting "x" '(current-tab) 1))
                      'kdb-claude-tab-busy))
          ;; In a tab with a finished reply too, the reply shows; input above all.
          (puthash b (cons "Turn complete" 1.0) agent-shell-attention--pending)
          (should (equal (mark 1) "✓ x"))
          (puthash a (list :label "Permission: ls" :timestamp 1.0 :request-id 3)
                   agent-shell-attention--pending)
          (should (equal (mark 1) "● x"))
          ;; Done working: the mark goes, and the bar redraws.
          (clrhash agent-shell-attention--pending)
          (remhash a agent-shell-attention--busy)
          (kdb-claude--redraw-tabs)
          (should (equal (mark 0) "x"))
          (should (= redraws 2)))))
    (kill-buffer a) (kill-buffer b)))

;;; claude-artifacts-test.el ends here
