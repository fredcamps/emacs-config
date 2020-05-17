(require 'eglot)
(require 'flycheck)

(defvar-local
  flycheck-eglot-current-errors
  nil)

(defun flycheck-eglot-report-fn (diags &rest _)
  (setq flycheck-eglot-current-errors (mapcar (lambda (diag)
                                                (save-excursion (goto-char (flymake--diag-beg diag))
                                                                (flycheck-error-new-at
                                                                 (line-number-at-pos)
                                                                 (1+ (- (point)
                                                                        (line-beginning-position)))
                                                                 (pcase (flymake--diag-type diag)
                                                                   ('eglot-error 'error)
                                                                   ('eglot-warning 'warning)
                                                                   ('eglot-note 'info)
                                                                   (_
                                                                    (error
                                                                     "Unknown diag type, %S"
                                                                     diag)))
                                                                 (flymake--diag-text diag)
                                                                 :checker 'eglot))) diags))
  (flycheck-buffer))

(defun flycheck-eglot--start (checker callback)
  (funcall callback 'finished flycheck-eglot-current-errors))

(defun flycheck-eglot--available-p ()
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker 'eglot "Report `eglot' diagnostics using `flycheck'."
                                 :start #'flycheck-eglot--start
                                 :predicate #'flycheck-eglot--available-p
                                 :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

(defun sanityinc/eglot-prefer-flycheck ()
  (when eglot--managed-mode (flycheck-add-mode 'eglot major-mode)
        (flycheck-select-checker 'eglot)
        (flycheck-mode)
        (flymake-mode -1)
        (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))
(add-hook 'eglot--managed-mode-hook 'sanityinc/eglot-prefer-flycheck)

(provide 'eglot-flycheck)
