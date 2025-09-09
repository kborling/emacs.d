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

;; Automatically select keys for encryption
(setq epa-file-select-keys nil)  ; Use default key

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

;; Function to transparently handle encrypted files
(defun kdb-handle-encrypted-file ()
  "Handle opening of files that should be encrypted."
  (when (and buffer-file-name
             (kdb-should-encrypt-file-p buffer-file-name)
             (not (string-suffix-p ".gpg" buffer-file-name))
             (file-exists-p (concat buffer-file-name ".gpg")))
    ;; If the .gpg version exists, open that instead
    (let ((gpg-file (concat buffer-file-name ".gpg")))
      (find-alternate-file gpg-file))))

;; Hook to check files when opening
(add-hook 'find-file-hook #'kdb-handle-encrypted-file)

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

;; Create menu for encryption functions
(define-prefix-command 'kdb-encryption-map)
(define-key global-map (kbd "C-c E") 'kdb-encryption-map)
(define-key kdb-encryption-map (kbd "e") #'kdb-encrypt-file)
(define-key kdb-encryption-map (kbd "d") #'kdb-decrypt-file)
(define-key kdb-encryption-map (kbd "a") #'kdb-add-file-to-encryption)
(define-key kdb-encryption-map (kbd "r") #'kdb-remove-file-from-encryption)
(define-key kdb-encryption-map (kbd "l") 
  (lambda () 
    (interactive) 
    (message "Encrypted files: %s" (mapconcat 'identity kdb-encrypted-files ", "))))

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