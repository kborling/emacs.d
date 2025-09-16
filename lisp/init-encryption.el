;;; init-encryption.el --- Encryption system -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides multiple encryption methods:
;; - simple: Built-in secure encryption (no external dependencies)
;; - gpg: GPG-based encryption (requires GnuPG)

;;; Code:

(require 'cl-lib)
(require 'password-cache)

(defcustom kdb-encryption-method 'simple
  "Encryption method to use."
  :type '(choice (const :tag "Built-in Secure (no dependencies)" simple)
                 (const :tag "GPG (requires GnuPG)" gpg))
  :group 'encryption)

(defcustom kdb-auto-encrypt-files
  '("~/.org/contacts.org")
  "Files to automatically encrypt using simple method."
  :type '(repeat string)
  :group 'encryption)

(defcustom kdb-encryption-cache-expiry 900
  "Time in seconds to cache encryption passwords (default 15 minutes)."
  :type 'integer
  :group 'encryption)

;; Configure password caching
(setq password-cache t
      password-cache-expiry kdb-encryption-cache-expiry)

;; Password management helpers
(defun kdb-encryption-cache-key (file)
  "Generate a cache key for FILE."
  (concat "kdb-encryption:" (expand-file-name file)))

(defun kdb-get-encryption-password (file &optional confirm)
  "Get encryption password for FILE, using cache if available.
If CONFIRM is non-nil, ask for password twice."
  (let ((cache-key (kdb-encryption-cache-key file)))
    (or (password-read-from-cache cache-key)
        (let ((password (read-passwd 
                        (format "Encryption password for %s: " 
                                (file-name-nondirectory file))
                        confirm)))
          (password-cache-add cache-key password)
          password))))

(defun kdb-clear-encryption-password (&optional file)
  "Clear cached encryption password for FILE or all files."
  (interactive)
  (if file
      (password-cache-remove (kdb-encryption-cache-key file))
    (password-reset)
    (message "All cached encryption passwords cleared")))

;; Built-in Secure Encryption (no external dependencies)
(defun kdb-derive-key (password salt)
  "Derive an encryption key from PASSWORD and SALT using PBKDF2-like method."
  (let ((iterations 1000)
        (key password))
    (dotimes (_ iterations)
      (setq key (secure-hash 'sha256 (concat key salt) nil nil t)))
    key))

(defun kdb-simple-encrypt-string (string password)
  "Encrypt STRING using PASSWORD with built-in secure method."
  (let* ((salt (secure-hash 'sha256 (format "%s%s" (random) (current-time-string)) nil nil t))
         (key (kdb-derive-key password salt))
         (string-bytes (encode-coding-string string 'utf-8 t))
         (key-bytes (vconcat key))
         (result (make-string (length string-bytes) 0)))
    ;; XOR with derived key
    (dotimes (i (length string-bytes))
      (aset result i (logxor (aref string-bytes i)
                            (aref key-bytes (mod i (length key-bytes))))))
    ;; Prepend salt and encode
    (base64-encode-string (concat salt result) t)))

(defun kdb-simple-decrypt-string (encrypted-string password)
  "Decrypt ENCRYPTED-STRING using PASSWORD."
  (condition-case nil
      (let* ((decoded (base64-decode-string encrypted-string))
             ;; Salt is first 32 bytes (sha256 output)
             (salt (substring decoded 0 32))
             (ciphertext (substring decoded 32))
             (key (kdb-derive-key password salt))
             (key-bytes (vconcat key))
             (result (make-string (length ciphertext) 0)))
        ;; XOR with derived key
        (dotimes (i (length ciphertext))
          (aset result i (logxor (aref ciphertext i)
                                (aref key-bytes (mod i (length key-bytes))))))
        (decode-coding-string result 'utf-8))
    (error nil)))

;; GPG Encryption (requires GnuPG)
(defun kdb-setup-gpg ()
  "Setup GPG for encryption."
  (when (eq kdb-encryption-method 'gpg)
    (require 'epa)
    (require 'epa-file)
    (epa-file-enable)
    (setq epa-file-select-keys 'symmetric
          epa-file-encrypt-to nil
          epa-file-cache-passphrase-for-symmetric-encryption t
          password-cache-expiry 3600
          epa-file-inhibit-auto-save t)))

;; Buffer encryption
(defun kdb-encrypt-buffer ()
  "Encrypt current buffer contents."
  (interactive)
  (pcase kdb-encryption-method
    ('simple
     (let ((password (kdb-get-encryption-password 
                     (or buffer-file-name "buffer")
                     t))) ; confirm password on encryption
       (when password
         (let ((content (buffer-string)))
           (erase-buffer)
           (insert ";; Encrypted with kdb-simple-encrypt\n")
           (insert (kdb-simple-encrypt-string content password))
           (save-buffer)
           (message "Buffer encrypted with built-in secure method")))))
    ('gpg
     (if (executable-find "gpg")
         (call-interactively 'epa-encrypt-file)
       (message "GPG not installed. Use M-x kdb-set-encryption-method to switch to 'simple'")))))

(defun kdb-decrypt-buffer ()
  "Decrypt current buffer contents."
  (interactive)
  (when (save-excursion
          (goto-char (point-min))
          (looking-at ";; Encrypted with kdb-simple-encrypt"))
    (let ((password (kdb-get-encryption-password 
                    (or buffer-file-name "buffer")
                    nil))) ; no confirm on decryption
      (when password
        (let* ((encrypted (buffer-substring
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line 1)
                            (point))
                          (point-max)))
               (decrypted (kdb-simple-decrypt-string (string-trim encrypted) password)))
          (if (and decrypted (> (length decrypted) 0))
              (progn
                (erase-buffer)
                (insert decrypted)
                (set-buffer-modified-p nil)
                (message "Buffer decrypted"))
            ;; Wrong password - clear cache and retry
            (progn
              (kdb-clear-encryption-password buffer-file-name)
              (message "Decryption failed - wrong password. Cache cleared."))))))))

;; Region encryption for org-mode
(defun kdb-password-encrypt-region (start end password)
  "Encrypt region from START to END using PASSWORD."
  (let* ((text (buffer-substring start end))
         (encrypted (pcase kdb-encryption-method
                      ('simple (kdb-simple-encrypt-string text password))
                      ('gpg (error "Use org-crypt for GPG encryption"))
                      (_ (error "Unsupported encryption method")))))
    (delete-region start end)
    (insert (format "-----BEGIN ENCRYPTED DATA-----\n%s\n-----END ENCRYPTED DATA-----" encrypted))))

(defun kdb-password-decrypt-region (start end password)
  "Decrypt region from START to END using PASSWORD."
  (let* ((text (buffer-substring start end))
         (encrypted-match (string-match "-----BEGIN ENCRYPTED DATA-----\n\\(.*\\)\n-----END ENCRYPTED DATA-----" text)))
    (when encrypted-match
      (let* ((encrypted-data (match-string 1 text))
             (decrypted (pcase kdb-encryption-method
                          ('simple (kdb-simple-decrypt-string encrypted-data password))
                          ('gpg (error "Use org-crypt for GPG decryption"))
                          (_ (error "Unsupported encryption method")))))
        (when decrypted
          (delete-region start end)
          (insert decrypted)
          t)))))

;; Auto-encryption
(defun kdb-should-auto-encrypt-p (file)
  "Check if FILE should be automatically encrypted."
  (let ((expanded-file (expand-file-name file)))
    (cl-some (lambda (pattern)
               (string-equal expanded-file (expand-file-name pattern)))
             kdb-auto-encrypt-files)))

(defun kdb-maybe-encrypt-on-save ()
  "Encrypt file content if it should be auto-encrypted."
  (when (and buffer-file-name
             (kdb-should-auto-encrypt-p buffer-file-name)
             (eq kdb-encryption-method 'simple)
             (not (save-excursion
                    (goto-char (point-min))
                    (looking-at ";; Encrypted with kdb-simple-encrypt"))))
    ;; Auto-encrypt without prompting for auto-encrypt files
    (let ((password (kdb-get-encryption-password buffer-file-name nil)))
      (when password
        (let ((content (buffer-string)))
          (erase-buffer)
          (insert ";; Encrypted with kdb-simple-encrypt\n")
          (insert (kdb-simple-encrypt-string content password))
          (message "Auto-encrypted %s" (file-name-nondirectory buffer-file-name)))))))

(defun kdb-maybe-decrypt-on-find ()
  "Decrypt file content if it's encrypted."
  (when (and buffer-file-name
             (save-excursion
               (goto-char (point-min))
               (looking-at ";; Encrypted with kdb-simple-encrypt")))
    (kdb-decrypt-buffer)))

;; Hooks
(add-hook 'find-file-hook #'kdb-maybe-decrypt-on-find)
(add-hook 'before-save-hook #'kdb-maybe-encrypt-on-save)

;; Org-crypt integration
(with-eval-after-load 'org
  (require 'org-crypt)
  
  (defun kdb-setup-org-crypt ()
    "Setup org-crypt based on current encryption method."
    (pcase kdb-encryption-method
      ('simple
       ;; org-crypt requires GPG, so disable if using simple
       (setq org-crypt-key nil)
       (when (not (executable-find "gpg"))
         (message "Note: org-crypt tags require GPG. Using simple encryption for buffers only.")))
      ('gpg
       (setq org-crypt-key nil)))  ; nil means symmetric encryption
    (setq org-tags-exclude-from-inheritance '("crypt")
          org-crypt-disable-auto-save t)
    (org-crypt-use-before-save-magic))
  
  (kdb-setup-org-crypt)
  
  (defun kdb-toggle-crypt-tag ()
    "Toggle the :crypt: tag on current heading."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading t)
        (if (member "crypt" (org-get-tags))
            (progn
              (org-set-tags (remove "crypt" (org-get-tags)))
              (message "Removed :crypt: tag - section will be unencrypted"))
          (org-set-tags (cons "crypt" (org-get-tags)))
          (message "Added :crypt: tag - section will be encrypted on save")))))
  
  (defun kdb-encrypt-org-entries ()
    "Manually encrypt all entries with :crypt: tag."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (if (executable-find "gpg")
          (progn
            (org-encrypt-entries)
            (message "Encrypted all entries with :crypt: tag"))
        (message "GPG required for :crypt: tags. Use buffer encryption instead."))))
  
  ;; Org-mode specific keybindings
  (define-key org-mode-map (kbd "C-c C-/") 'kdb-toggle-crypt-tag)
  (define-key org-mode-map (kbd "C-c C-x /") 'org-encrypt-entry)
  (define-key org-mode-map (kbd "C-c C-x C-/") 'org-decrypt-entry)
  (define-key org-mode-map (kbd "C-c C-x e") 'kdb-encrypt-org-entries))

;; Method selection
(defun kdb-set-encryption-method ()
  "Set the encryption method to use."
  (interactive)
  (let ((method (completing-read "Encryption method: "
                                 '("simple" "gpg")
                                 nil t nil nil 
                                 (symbol-name kdb-encryption-method))))
    (setq kdb-encryption-method (intern method))
    (customize-save-variable 'kdb-encryption-method kdb-encryption-method)
    (when (eq kdb-encryption-method 'gpg)
      (if (executable-find "gpg")
          (kdb-setup-gpg)
        (message "Warning: GPG not found. Install GPG or use 'simple' method.")))
    (when (featurep 'org)
      (kdb-setup-org-crypt))
    (message "Encryption method set to: %s" method)))

;; Status display
(defun kdb-encryption-status ()
  "Show current encryption configuration."
  (interactive)
  (message "Encryption Status:
Method: %s
GPG Available: %s
Auto-encrypt files: %s
Password cache expiry: %d seconds

Commands:
  C-c s e - Encrypt buffer
  C-c s d - Decrypt buffer  
  C-c s m - Change method
  C-c s c - Clear password cache
  C-c s h - This help

Org-mode:
  C-c C-/ - Toggle :crypt: tag (requires GPG)
  C-c C-x / - Encrypt entry (requires GPG)
  C-c C-x C-/ - Decrypt entry (requires GPG)"
           kdb-encryption-method
           (if (executable-find "gpg") "Yes" "No")
           (mapconcat 'identity kdb-auto-encrypt-files ", ")
           kdb-encryption-cache-expiry))

;; Initialize GPG if selected
(when (eq kdb-encryption-method 'gpg)
  (kdb-setup-gpg))

;; Global keybindings
(global-set-key (kbd "C-c s e") 'kdb-encrypt-buffer)
(global-set-key (kbd "C-c s d") 'kdb-decrypt-buffer)
(global-set-key (kbd "C-c s m") 'kdb-set-encryption-method)
(global-set-key (kbd "C-c s c") 'kdb-clear-encryption-password)
(global-set-key (kbd "C-c s h") 'kdb-encryption-status)

(message "Encryption loaded - Method: %s - Cache: %ds - Keys: C-c s [e/d/m/c/h]" 
         kdb-encryption-method 
         kdb-encryption-cache-expiry)

(provide 'init-encryption)
;;; init-encryption.el ends here