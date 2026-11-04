;;; init-local-models.el --- Local coding models: Ollama, gptel, minuet -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded at startup by init.el; gptel and minuet themselves load on first
;; use of their keys (C-c a).

;;; Code:

;; Coding models that fit a 4 GB GPU, run locally: completion when asked,
;; C-c a c (minuet), and chat/rewrite (gptel).  Setup: C-c a s.

(defvar kdb-local-models '("qwen2.5-coder:3b" "qwen2.5-coder:7b")
  "Ollama models: the first completes code, the last chats.")

(defvar kdb-ollama-available 'unknown)

(defun kdb-ollama-available-p ()
  "Non-nil if Ollama is installed; looked up once."
  (when (eq kdb-ollama-available 'unknown)
    (setq kdb-ollama-available (and (executable-find "ollama") t)))
  kdb-ollama-available)

(defun kdb-ollama-ensure ()
  "Start the Ollama server unless it is already listening."
  (unless (ignore-errors
            (delete-process (make-network-process :name "ollama-probe" :host "localhost"
                                                  :service 11434 :nowait nil))
            t)
    (when (kdb-ollama-available-p)
      (let ((process-environment (cons "OLLAMA_KEEP_ALIVE=30m" process-environment)))
        (make-process :name "ollama" :buffer " *ollama*" :noquery t
                      :command '("ollama" "serve"))))))

(defun kdb-local-models--ready ()
  "Say how to use the local models.
Completion is on demand only; as-you-type is C-c a C, per buffer."
  (message "Local models ready: C-c a c suggests a completion (TAB accepts), C-c a a to chat"))

(defun kdb-local-models-setup ()
  "Get local coding models working: Ollama running, `kdb-local-models' pulled.
Safe to re-run; only missing models are downloaded."
  (interactive)
  (setq kdb-ollama-available 'unknown)
  (unless (kdb-ollama-available-p)
    (browse-url "https://ollama.com/download")
    (user-error "Install Ollama from the page just opened, then run C-c a s again"))
  (kdb-ollama-ensure)
  (let ((have :down))
    (dotimes (_ 20)
      (when (eq have :down)
        (setq have (condition-case nil
                       (mapcar (lambda (l) (car (split-string l)))
                               (cdr (process-lines "ollama" "list")))
                     (error (sleep-for 0.5) :down)))))
    (when (eq have :down)
      (user-error "Ollama didn't start; see the \" *ollama*\" buffer"))
    (if-let* ((missing (seq-difference kdb-local-models have)))
        (let ((buf "*Ollama setup*"))
          (async-shell-command (mapconcat (lambda (m) (concat "ollama pull " m)) missing " && ")
                               buf)
          (set-process-sentinel
           (get-buffer-process buf)
           (lambda (p _)
             (unless (process-live-p p)
               (if (zerop (process-exit-status p))
                   (kdb-local-models--ready)
                 (message "Pulling models failed; see %s" buf))))))
      (kdb-local-models--ready))))

(global-set-key (kbd "C-c a s") #'kdb-local-models-setup)

(use-package gptel
  :ensure t
  :bind (("C-c a a" . gptel)
         ("C-c a RET" . gptel-send)
         ("C-c a r" . gptel-rewrite)
         ("C-c a m" . gptel-menu))
  :config
  (kdb-ollama-ensure)
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434" :stream t
                        :models (mapcar #'intern (reverse kdb-local-models)))
        gptel-model (intern (car (last kdb-local-models)))))

(use-package minuet
  :ensure t
  :bind (("C-c a c" . minuet-show-suggestion)
         ("C-c a C" . minuet-auto-suggestion-mode)
         :map minuet-active-mode-map
         ("TAB" . minuet-accept-suggestion)
         ("M-a" . minuet-accept-suggestion-line)
         ("M-n" . minuet-next-suggestion)
         ("M-p" . minuet-previous-suggestion)
         ("M-e" . minuet-dismiss-suggestion))
  :config
  (kdb-ollama-ensure)
  (setq minuet-provider 'openai-fim-compatible
        minuet-n-completions 1
        minuet-context-window 1024
        ;; Room to load or swap in the model (one fits the GPU at a time).
        minuet-request-timeout 8)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key (lambda () "ollama"))
  (plist-put minuet-openai-fim-compatible-options :model (car kdb-local-models))
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(provide 'init-local-models)
;;; init-local-models.el ends here
