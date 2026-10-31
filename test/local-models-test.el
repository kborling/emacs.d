;;; local-models-test.el --- One-step setup of local Ollama models -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/local-models-test.el \
;;   -f ert-run-tests-batch-and-exit
;; Ollama itself is stubbed; nothing is downloaded or started.

(require 'ert)

(defvar test-startup-untouched
  (and (eq kdb-ollama-available 'unknown)          ; not even looked for
       (not (featurep 'minuet)) (not (featurep 'gptel))))

(defvar test-enabled nil "Buffers completion was turned on in.")
(defvar test-browsed nil)
(defvar test-pulled nil)

(defmacro test-with-ollama (installed models &rest body)
  "Run BODY as if Ollama is INSTALLED with MODELS pulled."
  (declare (indent 2))
  `(let ((kdb-ollama-available 'unknown)
         (test-enabled nil) (test-browsed nil) (test-pulled nil)
         (lists 0))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (p &rest _) (and ,installed (equal p "ollama") "/usr/bin/ollama")))
               ((symbol-function 'kdb-ollama-ensure) #'ignore)
               ((symbol-function 'process-lines)
                (lambda (&rest _) (cl-incf lists)
                  (cons "NAME ID SIZE MODIFIED" (mapcar (lambda (m) (concat m " abc 1 GB now")) ,models))))
               ((symbol-function 'minuet-auto-suggestion-mode)
                (lambda (&rest _) (push (current-buffer) test-enabled)))
               ((symbol-function 'browse-url) (lambda (url &rest _) (setq test-browsed url)))
               ((symbol-function 'async-shell-command)
                (lambda (cmd buf) (setq test-pulled cmd)
                  (start-process "fake-pull" (get-buffer-create buf) "true"))))
       ,@body)))

(defun test-wait-for (pred)
  (let ((end (+ (float-time) 5)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))))

(ert-deftest local-models-startup-is-free ()
  (should test-startup-untouched))

(ert-deftest local-models-no-ollama ()
  (test-with-ollama nil nil
    (with-temp-buffer (emacs-lisp-mode))
    (should-not test-enabled)                      ; code buffers stay as they were
    (should-error (kdb-local-models-setup) :type 'user-error)
    (should (equal test-browsed "https://ollama.com/download"))))

(ert-deftest local-models-everything-present ()
  (test-with-ollama t kdb-local-models
    (with-temp-buffer
      (emacs-lisp-mode)                            ; hook turns completion on
      (should (memq (current-buffer) test-enabled))
      (setq test-enabled nil)
      (kdb-local-models-setup)
      (should-not test-pulled)                     ; nothing to download
      (should (memq (current-buffer) test-enabled)))))

(ert-deftest local-models-pulls-only-missing ()
  (test-with-ollama t (list (car kdb-local-models))
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq test-enabled nil)
      (kdb-local-models-setup)
      (should (equal test-pulled (concat "ollama pull " (cadr kdb-local-models))))
      ;; Completion comes on in open code buffers once the pull finishes.
      (test-wait-for (lambda () test-enabled))
      (should (memq (current-buffer) test-enabled))))
  (ignore-errors (kill-buffer "*Ollama setup*")))

(ert-deftest local-models-waits-for-server ()
  (let ((calls 0))
    (test-with-ollama t kdb-local-models
      (cl-letf* ((list-fn (symbol-function 'process-lines))
                 ((symbol-function 'process-lines)
                  (lambda (&rest args)
                    (if (< (cl-incf calls) 3) (error "could not connect to ollama server")
                      (apply list-fn args)))))
        (kdb-local-models-setup)
        (should (= calls 3))
        (should-not test-pulled)))))

;;; local-models-test.el ends here
