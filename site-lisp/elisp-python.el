;;; elisp-python.el --- Package containing helpers for python mode
;;
;;; Commentary:
;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; M-x eval-buffer for reload config
;;
;;; Code:
;;
(require 'utils)

(defvar after-bootstrap-hook nil
  "Hook called after the bootstrap was ran.")

(defvar before-bootstrap-hook nil
  "Hook called before the bootstrap run.")

(defvar after-hack-local-variables-hook nil
  "Hook called after hack local variables.")

(defvar before-hack-local-variables-hook nil
  "Hook called before hack local variables.")

(defun python:--replace-template-variables (dir-locals-file)
  "Replace variables from .dir-locals.el file.  DIR-LOCALS-FILE."
  (with-eval-after-load "virtualenvwrapper"
    (call-interactively #'venv-workon)
    (utils:replace-string-in-file dir-locals-file "{{ VENV-NAME }}" venv-current-name)))

(defun python:setup ()
  "Auto-configure environment."
  (interactive)
  (let ((dir-locals-file) (project-root))
    (with-eval-after-load "projectile"
      (setq project-root (projectile-project-root)))
    (setq dir-locals-file (concat project-root ".dir-locals.el"))
    (unless (file-exists-p dir-locals-file)
      (utils:generate-project-files "python")
      (python:--replace-template-variables dir-locals-file)
      (python:bootstrap))))

;;;###autoload
(defun python:bootstrap ()
  "Bootstrap elisp-python mode."
  (interactive)
  (run-hooks 'before-bootstrap-hook)
  (run-hooks 'before-hack-local-variables-hook)
  (hack-local-variables)
  (run-hooks 'after-hack-local-variables-hook)
  (with-eval-after-load "flycheck"
    (add-to-list 'flycheck-disabled-checkers '(python-mypy))
    (add-to-list 'flycheck-enabled-checkers '(python-pycompile
                                              python-pylint
                                              python-flake8))
    (setq-local flycheck-checker 'python-flake8)
    (when (executable-find "flake8")
      (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
      (setq flycheck-flake8rc (file-name-directory (expand-file-name ".flake8")))))
  (run-hooks 'after-bootstrap-hook))

(provide 'elisp-python)
;;; elisp-python ends here
