;;; init-encryption.el --- Automatic file encryption configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for automatic encryption of sensitive files using EasyPG

;;; Code:

(require 'epa)
(require 'epa-file)
(require 'epg-config)

;; Enable automatic encryption/decryption
(epa-file-enable)

;; List of files that should be automatically encrypted
;; These files will be transparently encrypted when saved
(defcustom kdb-encrypted-files
  '("~/.org/contacts.org")
  "List of files that should be automatically encrypted.
Files should be specified with their full paths or using ~ for home directory."
  :type '(repeat string)
  :group 'encryption)

;; GPG configuration
(setq epg-gpg-program (or (executable-find "gpg2")
                          (executable-find "gpg")
                          "gpg"))

;; Use loopback for pinentry to avoid GUI popups
;; Comment out for GUI pinentry-mac
;; (setq epg-pinentry-mode 'loopback)

;; Encryption method configuration
(defcustom kdb-encryption-method 'symmetric
  "Encryption method to use: 'symmetric, 'asymmetric, or 'ask.
Symmetric is recommended for multi-machine use."
  :type '(choice (const :tag "Symmetric (password-only)" symmetric)
                 (const :tag "Asymmetric (GPG key)" asymmetric)  
                 (const :tag "Ask each time" ask))
  :group 'encryption)

;; Default to symmetric for better portability
(setq epa-file-select-keys 'symmetric)  ; Use symmetric by default
(setq epa-file-encrypt-to nil)  ; Don't use public key by default

;; Cache passphrases for convenience (adjust timeout as needed)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq password-cache-expiry 3600)  ; Cache for 1 hour

;; Allow multiple password attempts
(setq epa-file-inhibit-auto-save t)  ; Don't auto-save encrypted files
(setq epg-passphrase-coding-system 'utf-8)

;; Function to check if a file should be encrypted
(defun kdb-should-encrypt-file-p (file)
  "Check if FILE should be automatically encrypted."
  (let ((expanded-file (expand-file-name file)))
    (cl-some (lambda (pattern)
               (string-equal expanded-file (expand-file-name pattern)))
             kdb-encrypted-files)))

;; Setup encryption method for specific files
(defun kdb-setup-file-encryption-method ()
  "Setup encryption method based on file and configuration."
  (when (string-match "\\.gpg\\'" buffer-file-name)
    (pcase kdb-encryption-method
      ('symmetric 
       (setq-local epa-file-encrypt-to nil
                   epa-file-select-keys 'symmetric))
      ('asymmetric
       (setq-local epa-file-select-keys nil))
      ('ask
       (if (y-or-n-p "Use symmetric encryption (password-only)? ")
           (setq-local epa-file-encrypt-to nil
                       epa-file-select-keys 'symmetric)
         (setq-local epa-file-select-keys nil))))))

;; Hook to setup encryption when finding .gpg files
(add-hook 'find-file-hook #'kdb-setup-file-encryption-method)

;; Function to setup encryption for a file
(defun kdb-setup-file-encryption ()
  "Setup encryption for the current file if it matches encryption patterns."
  (when (and buffer-file-name
             (kdb-should-encrypt-file-p buffer-file-name)
             (not (string-suffix-p ".gpg" buffer-file-name)))
    ;; Add .gpg extension to the file name
    (let ((new-name (concat buffer-file-name ".gpg")))
      (message "Setting up encryption for %s" buffer-file-name)
      ;; Rename the file
      (rename-file buffer-file-name new-name t)
      ;; Update buffer file name
      (set-visited-file-name new-name)
      ;; Save to trigger encryption
      (save-buffer)
      (message "File encrypted and saved as %s" new-name))))

;; Function to optionally redirect to encrypted file
(defun kdb-prompt-for-encrypted-file ()
  "Prompt user to open encrypted version if it exists."
  (when (and buffer-file-name
             (kdb-should-encrypt-file-p buffer-file-name)
             (not (string-suffix-p ".gpg" buffer-file-name))
             (file-exists-p (concat buffer-file-name ".gpg")))
    ;; Only prompt, don't automatically redirect
    (when (y-or-n-p (format "Encrypted version exists (%s.gpg). Open that instead? " 
                            (file-name-nondirectory buffer-file-name)))
      (let ((gpg-file (concat buffer-file-name ".gpg")))
        (find-alternate-file gpg-file)))))

;; Removed automatic hook - user must explicitly choose

;; Function to encrypt an existing file
(defun kdb-encrypt-file (file)
  "Encrypt FILE using GPG."
  (interactive "fFile to encrypt: ")
  (let* ((expanded-file (expand-file-name file))
         (gpg-file (concat expanded-file ".gpg")))
    (if (file-exists-p expanded-file)
        (progn
          (with-temp-buffer
            (insert-file-contents expanded-file)
            (write-region (point-min) (point-max) gpg-file))
          (delete-file expanded-file)
          (message "File encrypted: %s -> %s" expanded-file gpg-file))
      (error "File does not exist: %s" expanded-file))))

;; Function to decrypt a file
(defun kdb-decrypt-file (file)
  "Decrypt FILE (must end with .gpg)."
  (interactive "fGPG file to decrypt: ")
  (let* ((expanded-file (expand-file-name file))
         (plain-file (substring expanded-file 0 -4)))
    (if (and (file-exists-p expanded-file)
             (string-suffix-p ".gpg" expanded-file))
        (progn
          (with-temp-buffer
            (insert-file-contents expanded-file)
            (write-region (point-min) (point-max) plain-file))
          (message "File decrypted: %s -> %s" expanded-file plain-file))
      (error "File must exist and end with .gpg: %s" expanded-file))))

;; Function to add a file to the encryption list
(defun kdb-add-file-to-encryption (file)
  "Add FILE to the list of automatically encrypted files."
  (interactive "fFile to add to encryption list: ")
  (let ((expanded-file (expand-file-name file)))
    (add-to-list 'kdb-encrypted-files expanded-file)
    (customize-save-variable 'kdb-encrypted-files kdb-encrypted-files)
    (message "Added %s to encryption list" expanded-file)))

;; Function to remove a file from the encryption list
(defun kdb-remove-file-from-encryption (file)
  "Remove FILE from the list of automatically encrypted files."
  (interactive
   (list (completing-read "Remove from encryption list: "
                         kdb-encrypted-files nil t)))
  (setq kdb-encrypted-files (delete file kdb-encrypted-files))
  (customize-save-variable 'kdb-encrypted-files kdb-encrypted-files)
  (message "Removed %s from encryption list" file))

;; Function to open encrypted version of current file
(defun kdb-open-encrypted-version ()
  "Open the encrypted (.gpg) version of the current file if it exists."
  (interactive)
  (if buffer-file-name
      (let ((gpg-file (concat buffer-file-name ".gpg")))
        (if (file-exists-p gpg-file)
            (find-alternate-file gpg-file)
          (if (y-or-n-p "No encrypted version exists. Create one? ")
              (kdb-setup-file-encryption)
            (message "No encrypted version found"))))
    (message "No file associated with current buffer")))

;; Function to switch encryption method
(defun kdb-switch-encryption-method ()
  "Switch between symmetric and asymmetric encryption."
  (interactive)
  (let ((method (completing-read "Encryption method: " 
                                 '("symmetric" "asymmetric" "ask") 
                                 nil t nil nil 
                                 (symbol-name kdb-encryption-method))))
    (setq kdb-encryption-method (intern method))
    (customize-save-variable 'kdb-encryption-method kdb-encryption-method)
    (message "Encryption method set to: %s" method)
    (when (string-match "\\.gpg\\'" (or buffer-file-name ""))
      (kdb-setup-file-encryption-method))))

;; Create menu for encryption functions
(define-prefix-command 'kdb-encryption-map)
(define-key global-map (kbd "C-c E") 'kdb-encryption-map)
(define-key kdb-encryption-map (kbd "e") #'kdb-encrypt-file)
(define-key kdb-encryption-map (kbd "d") #'kdb-decrypt-file)
(define-key kdb-encryption-map (kbd "a") #'kdb-add-file-to-encryption)
(define-key kdb-encryption-map (kbd "r") #'kdb-remove-file-from-encryption)
(define-key kdb-encryption-map (kbd "o") #'kdb-open-encrypted-version)
(define-key kdb-encryption-map (kbd "m") #'kdb-switch-encryption-method)
(define-key kdb-encryption-map (kbd "l") 
  (lambda () 
    (interactive) 
    (message "Encrypted files: %s | Method: %s" 
             (mapconcat 'identity kdb-encrypted-files ", ")
             kdb-encryption-method)))

;; Advice for org-mode files
(defun kdb-org-check-encryption (&rest _)
  "Check if the current org file should be encrypted."
  (when (and (derived-mode-p 'org-mode)
             buffer-file-name
             (kdb-should-encrypt-file-p buffer-file-name)
             (not (string-suffix-p ".gpg" buffer-file-name)))
    (when (y-or-n-p (format "Encrypt %s? " buffer-file-name))
      (kdb-setup-file-encryption))))

;; Check org files after saving
(advice-add 'org-save-all-org-buffers :after #'kdb-org-check-encryption)

;; Wrapper for safe file operations with retry
(defun kdb-with-encrypted-file (file operation &rest args)
  "Execute OPERATION on FILE with decryption retry support.
If decryption fails, offer to retry or skip."
  (condition-case err
      (apply operation file args)
    (file-error
     (if (and (string-match-p "\\.gpg\\'" file)
              (yes-or-no-p (format "Failed to decrypt %s. Retry? " 
                                   (file-name-nondirectory file))))
         ;; Clear password cache and retry
         (progn
           (password-cache-remove file)
           (apply operation file args))
       (signal (car err) (cdr err))))
    (epg-error
     (if (yes-or-no-p (format "Decryption error for %s. Retry? " 
                              (file-name-nondirectory file)))
         (progn
           (password-cache-remove file)
           (apply operation file args))
       (message "Skipping encrypted file operation")))))

;; Helper to open encrypted files with retry
(defun kdb-find-encrypted-file (file)
  "Open FILE with automatic retry on decryption failure."
  (interactive "fFile: ")
  (kdb-with-encrypted-file file #'find-file))

(provide 'init-encryption)
;;; init-encryption.el ends here