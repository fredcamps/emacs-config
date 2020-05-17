(require 'importmagic)
(require 'projectile)
(require 'virtualenvwrapper)
(require 'utils)

(defcustom python:virtualenv-name nil)

(when (executable-find "ipython")
  (setq-local python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt"))

(defun python:replace-template-variables (dir-locals-file)
  "Replace variables from .dir-locals.el file.  DIR-LOCALS-FILE."
  (call-interactively #'venv-workon)
  (utils:replace-string-in-file dir-locals-file "{{ VENV-NAME }}" venv-current-name))

(defun python:setup ()
  "Auto-configure environment."
  (interactive)
  (let ((dir-locals-file) (project-root))
    (setq project-root (projectile-project-root))
    (setq dir-locals-file (concat project-root ".dir-locals.el"))
    (unless (file-exists-p dir-locals-file)
      (utils:generate-project-files "python")
      (python:replace-template-variables dir-locals-file)
      (python:bootstrap))))

(defun python:bootstrap ()
  "Bootstrap elisp-python mode."
  (interactive)
  (hack-local-variables)
  (when python:virtualenv-name
    (venv-workon python:virtualenv-name))
  (importmagic-mode +1))

(add-hook 'python-mode-hook #'python:bootstrap)

(provide 'elisp-python)
