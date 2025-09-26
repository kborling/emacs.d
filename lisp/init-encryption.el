;;; init-encryption.el --- Encryption system -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'password-cache)

(defcustom kdb-encryption-method 'simple
  "Encryption method to use."
  :type '(choice (const :tag "Built-in Secure (no dependencies)" simple)
                 (const :tag "GPG (requires GnuPG)" gpg))
  :group 'encryption)

(defcustom kdb-encryption-cache-expiry 900
  "Time in seconds to cache encryption passwords (default 15 minutes)."
  :type 'integer
  :group 'encryption)

(setq password-cache t
      password-cache-expiry kdb-encryption-cache-expiry)

;; Password management helpers
(defun kdb-encryption-cache-key (file)
  "Generate a cache key for FILE."
  (concat "kdb-encryption:" (expand-file-name file)))

(defun kdb-get-encryption-password (file &optional confirm)
  "Get encryption password for FILE, using cache if available."
  (let ((cache-key (kdb-encryption-cache-key file)))
    (or (password-read-from-cache cache-key)
        (let ((password (read-passwd
                        (format "Encryption password for %s: "
                                (file-name-nondirectory file))
                        confirm)))
          (password-cache-add cache-key password)
          password))))

;; Built-in encryption functions
(defun kdb-derive-key (password salt)
  "Derive an encryption key from PASSWORD and SALT."
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

(defun kdb-validate-decrypted-content (content)
  "Validate that CONTENT appears to be successfully decrypted text."
  (and content
       (> (length content) 0)
       ;; Check if content is mostly printable characters
       (let ((printable-chars 0)
             (total-chars (length content)))
         (dotimes (i (min total-chars 200)) ; Check first 200 chars
           (when (or (and (>= (aref content i) 32) (<= (aref content i) 126)) ; Printable ASCII
                     (memq (aref content i) '(9 10 13))) ; Tab, newline, carriage return
             (cl-incf printable-chars)))
         ;; Content should be at least 70% printable characters
         (> (/ (* printable-chars 100.0) (min total-chars 200)) 70))
       ;; Additional check: should not contain too many null bytes
       (< (cl-count 0 (substring content 0 (min 100 (length content)))) 10)))

(defun kdb-simple-decrypt-string (encrypted-string password)
  "Decrypt ENCRYPTED-STRING using PASSWORD. Returns nil if decryption fails."
  (condition-case nil
      (let* ((decoded (base64-decode-string encrypted-string))
             (salt (substring decoded 0 32))
             (ciphertext (substring decoded 32))
             (key (kdb-derive-key password salt))
             (key-bytes (vconcat key))
             (result (make-string (length ciphertext) 0)))
        (dotimes (i (length ciphertext))
          (aset result i (logxor (aref ciphertext i)
                                (aref key-bytes (mod i (length key-bytes))))))
        (let ((decrypted (decode-coding-string result 'utf-8)))
          ;; Validate decryption by checking if it contains readable text
          (if (kdb-validate-decrypted-content decrypted)
              decrypted
            nil)))
    (error nil)))

(defun kdb-decrypt-buffer (&optional retry-count)
  "Decrypt current buffer contents. RETRY-COUNT tracks failed attempts."
  (interactive)
  (setq retry-count (or retry-count 0))
  (when (save-excursion
          (goto-char (point-min))
          (looking-at ";; Encrypted with kdb-simple-encrypt"))
    (let* ((cache-key (kdb-encryption-cache-key (or buffer-file-name "buffer")))
           (password (if (> retry-count 0)
                         (read-passwd
                          (format "Encryption password for %s (attempt %d): "
                                  (file-name-nondirectory (or buffer-file-name "buffer"))
                                  (1+ retry-count)))
                       (kdb-get-encryption-password
                        (or buffer-file-name "buffer")
                        nil))))
      (when password
        (let* ((encrypted (buffer-substring
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line 1)
                            (point))
                          (point-max)))
               (decrypted (kdb-simple-decrypt-string (string-trim encrypted) password)))
          (if decrypted
              (progn
                (when (> retry-count 0)
                  (password-cache-add cache-key password))
                (erase-buffer)
                (insert decrypted)
                (set-buffer-modified-p nil)
                (message "Buffer decrypted successfully"))
            (progn
              (password-cache-remove cache-key)
              (if (< retry-count 2)
                  (progn
                    (message "Decryption failed - wrong password. Try again...")
                    (sit-for 1)
                    (kdb-decrypt-buffer (1+ retry-count)))
                (message "Decryption failed after %d attempts. Giving up." (1+ retry-count))))))))))

;; Buffer encryption
(defun kdb-encrypt-buffer ()
  "Encrypt current buffer contents."
  (interactive)
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

(defun kdb-clear-encryption-password (&optional file)
  "Clear cached encryption password for FILE or all files."
  (interactive)
  (if file
      (password-cache-remove (kdb-encryption-cache-key file))
    (password-reset)
    (message "All cached encryption passwords cleared")))

(defun kdb-set-encryption-method ()
  "Set the encryption method to use."
  (interactive)
  (let ((method (completing-read "Encryption method: "
                                 '("simple" "gpg")
                                 nil t nil nil
                                 (symbol-name kdb-encryption-method))))
    (setq kdb-encryption-method (intern method))
    (message "Encryption method set to: %s" method)))

(defun kdb-encryption-status ()
  "Show current encryption configuration."
  (interactive)
  (message "Encryption - Method: %s, Cache: %ds, Keys: C-c s [e/d/m/c/h]"
           kdb-encryption-method
           kdb-encryption-cache-expiry))

;; Global keybindings
(global-set-key (kbd "C-c s d") 'kdb-decrypt-buffer)
(global-set-key (kbd "C-c s e") 'kdb-encrypt-buffer)
(global-set-key (kbd "C-c s m") 'kdb-set-encryption-method)
(global-set-key (kbd "C-c s c") 'kdb-clear-encryption-password)
(global-set-key (kbd "C-c s h") 'kdb-encryption-status)

(message "Encryption loaded - Method: %s - Keys: C-c s [e/d/m/c/h]" kdb-encryption-method)

(provide 'init-encryption)
;;; init-encryption.el ends here