;;; theme-test.el --- Theme switching keeps the font and loads once -*- lexical-binding: t -*-

;; emacs --batch -l early-init.el -l init.el -l test/theme-test.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'init-theme)

(ert-deftest theme-set-keeps-weight-and-loads-once ()
  ;; Theme machinery stubbed: on a batch (tty) frame enabling any theme
  ;; resets the default weight itself, which a GUI frame doesn't.
  (let (calls kdb-current-theme kdb-last-dark-theme acme-theme-black-fg)
    (cl-letf (((symbol-function 'kdb--sync-desktop-theme) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'custom-theme-p) (lambda (theme) (eq theme 'fleury)))
              ((symbol-function 'enable-theme) (lambda (theme) (push (list 'enable theme) calls)))
              ((symbol-function 'load-theme) (lambda (theme &rest _) (push (list 'load theme) calls)))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest args) (push (cons face args) calls))))
      (kdb/theme-set 'fleury)
      (kdb/theme-set 'acme))
    ;; A loaded theme is only enabled; the font is left alone.
    (should (equal (nreverse calls) '((enable fleury) (load acme))))))

;;; theme-test.el ends here
