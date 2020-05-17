;; init.el --- Emacs initialization
;;; Commentary:
;;
;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; Emacs initialization script
;;
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file (concat user-emacs-directory "local-init.el"))


(provide 'init)
;;; init.el ends here
