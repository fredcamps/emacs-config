;;; elisp-python.el --- Package containing helpers for python mode
;;
;;; Commentary:
;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; M-x eval-buffer for reload config
;;
;;; Code:
;;
(require 'utils)

(defun python:--replace-template-variables (dir-locals-file)
  "Replace variables from .dir-locals.el file.  DIR-LOCALS-FILE."
  (with-eval-after-load "virtualenvwrapper"
    (call-interactively #'venv-workon)
    (utils:replace-string-in-file dir-locals-file "{{ VENV-NAME }}" venv-current-name)))

;;;###autoload
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
      (hack-local-variables))))

(provide 'elisp-python)
;;; elisp-python ends here
