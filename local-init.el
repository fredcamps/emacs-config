;;; local-init.el --- Package that setup Emacs
;;
;;; Commentary:
;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; M-x eval-buffer for reload config
;;
;;; Code:
;;
(defconst packages-to-install
  '(use-package)
  "Packages to install.")

(defun init:setup-repository ()
  ;; Repository channels
  (setq load-prefer-newer t)
  (setq package-enable-at-startup nil)
  (setq package-check-signature nil)
  (setq package-archives
        '(("melpa" .
           "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
          ("org" .
           "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
          ("gnu" .
           "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
  (package-refresh-contents)

  ;; install missing packages
  (dolist (pkg packages-to-install)
    (package-install pkg)))

(eval-when-compile
  (unless (require 'use-package nil 'noerror)
    (init:setup-repository)
    (require 'use-package))
  (setq use-package-always-ensure nil))

;; --- Emacs settings --- ;;
;;; error and logging
;(setq debug-on-error t)
;(setq debug-ignored-errors t)
;(setq message-log-max 1000)
;;;

;;; Encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;

;;; ANSI Colors
(ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;;

;;; Behaviour settings
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 10000)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq temporary-file-directory (concat user-emacs-directory "tmp"))
(delete-selection-mode t)
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t) ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t) ; show completion list when ambiguous
 '(comint-input-ignoredups t) ; no duplicates in command history
 '(comint-completion-addsuffix t) ; insert space/slash after file completion
 )
(icomplete-mode +1)
(setq completion-auto-help t)
;;;

;;; Misc settings
(setq browse-url-generic-program "firefox")
;;;

;;; Indentation Settings
(electric-pair-mode +1)
(electric-indent-mode +1)
;;;

;;; Appearance settings
(transient-mark-mode t)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(display-time-mode -1)
(display-battery-mode -1)
(line-number-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(which-function-mode +1)

(load-theme 'wombat t)
(custom-set-faces
 `(region ((t (:background, "#919199"))))
 `(hl-line ((t (:background, "#424242"))))
 `(show-paren-match ((t (:background, "#6a6a6b"))))
 `(show-paren-match ((t (:background, "#6a6a6b"))))
 `(which-func ((t (:foreground, "gray")))))
;;;

;;; Keybindings settings
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-x t") #'ansi-term)
(define-key global-map (kbd "M-.") #'xref-find-definitions)
(define-key global-map (kbd "M-,") #'xref-pop-marker-stack)
(define-key global-map (kbd "C-x 4 ,") #'xref-find-definitions-other-window)
(define-key global-map (kbd "<backtab>") #'indent-for-tab-command)
;;;

;;; Spellchecking
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode t)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode -1))))
;;;

;;; Hooks
(add-hook 'completion-setup-hook
          (lambda ()
            (run-at-time 3 nil (lambda ()
                                 (delete-windows-on "*Completions*")))))
(add-hook 'dirtrack-mode-hook
          (function (lambda ()
                      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(add-hook 'before-save-hook 'delete-trailing-whitespace t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode t)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode t)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode t)
;;;
;; --- ;;

;; --- Extensions settings --- ;;
(use-package auto-compile
  :hook elisp-mode-hook
  :demand t
  :ensure t
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t)
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package redo+
  :load-path "3rd-party"
  :bind (("C-x u" . undo-only)
         ("C-x U" . redo)))

(use-package move-lines
  :load-path "3rd-party"
  :commands move-lines-mode
  :init
  (add-hook 'after-init-hook 'move-lines-mode))

(use-package ido
  :commands (ido-everywhere)
  :config
  (setq ido-enable-flex-matching nil)
  (setq ido-create-new-buffer 'prompt)
  (setq ido-use-faces nil)
  (setq ido-completion-buffer nil)
  (setq ido-completion-buffer-all-completions nil)
  (ido-everywhere t)
  (ido-mode +1))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :init
  (flymake-mode -1)
  :config
  ;; (setq flycheck-check-syntax-automatically '(mode-enabled
  ;;                                             save
  ;;                                             idle-change
  ;;                                             new-line
  ;;                                             idle-buffer-switch))
  (setq flycheck-indication-mode nil)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-idle-change-delay 0.5)
  (set-face-attribute 'flycheck-info nil
                      :background nil
                      :foreground "#fff"
                      :underline t)
  (set-face-attribute 'flycheck-warning nil
                      :background nil
                      :foreground "#b58900"
                      :underline t)
  (set-face-attribute 'flycheck-error nil
                      :background nil
                      :foreground "#dc322f"
                      :underline t))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (bind-key "C-c c" 'company-capf)
  (bind-key "C-c d" 'company-show-doc-buffer)
  (bind-key "C-c v" 'company-show-location)
  (when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode -1))
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 3))

(use-package company-dict
  :ensure t
  :init
  (setq company-dict-dir (concat user-emacs-directory "dict"))
  (setq company-dict-enable-fuzzy nil)
  (setq company-dict-enable-yasnippet +1)
  (add-to-list 'company-backends 'company-dict))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file)
  (setq projectile-require-project-root 't)
  (setq projectile-completion-system 'ido))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (let ((snippets-dir)
        (yasnippet-snippets-dir))
    (setq snippets-dir (concat user-emacs-directory "snippets"))
    (setq yas-snippet-dirs (list snippets-dir))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-c b" . eglot-format)
              ("C-c h" . eglot-help-at-point)
              ("M-." . eglot-find-definitions))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-9"))
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode))
               "typescript-language-server" "stdio")
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'js2-mode-hook 'eglot-ensure)
  (add-hook 'js2-jsx-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure))

(use-package eglot-flycheck
  :after (eglot flycheck)
  :load-path "site-lisp")

(use-package eglot-mspyls
  :after eglot
  :load-path "site-lisp")

(use-package dumb-jump
  :ensure t
  :hook (prog-mode . dumb-jump-mode)
  :bind (:map dumb-jump-mode-map
              ("C-c ." . dumb-jump-back)
              ("C-c ;" . dumb-jump-go)
              ("C-x 4 ;" . dumb-jump-go-other-window)))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  :ensure t)

(use-package magit
  :ensure t)

(use-package magit-gitflow
  :ensure t)

(use-package loccur
  :ensure t
  :bind (:map prog-mode-map
              ("C-c o" . loccur)))

(use-package neotree
  :ensure t
  :bind (("C-c e" . neotree-toggle)))

;;; Rest Client
(use-package company-restclient
  :hook restclient-mode
  :config
  (add-to-list 'company-backends 'company-restclient)
  :ensure t)

(use-package restclient
  :mode "\\.rest$'"
  :ensure t)

(use-package skewer-mode
  :ensure t
  :requires (simple-httpd)
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (html-mode . skewer-html-mode))
  :config
  (setq httpd-port 54322))

(use-package
    ag
  :ensure t)

(use-package column-enforce-mode
  :ensure t
  :hook (prog-mode . column-enforce-mode)
  :config
  (setq column-enforce-column 100))

(use-package indent-guide
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/shorten-modes t)
  (setq sml/mode-width 'full)
  (setq sml/shorten-directory nil)
  (setq sml/show-client t)
  (setq sml/theme 'smart-mode-line-dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))
;; --- ;;

;; --- My Custom utilities --- ;;
(use-package utils
  :load-path "site-lisp"
  :bind (("C-c SPC" . utils:toggle-invisibles)
         ("C-c k" . utils:kill-all-buffers)
         ("C-c n" . utils:new-buffer-frame)))
;; --- ;;

;; --- Programming/Markdown/Serialization Languages Settings --- ;;
;;; JSON/Yaml
(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
              ("C-c b" . json-mode-beautify))
  :init
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-always-indent 'complete)
  (setq-local js-indent-level 2))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-always-indent 'complete)
  (setq-local tab-width 2))
;;;

;;; Elisp
(use-package package-lint
  :ensure t)

(use-package elisp-format
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c b" . elisp-format-buffer)))

(use-package elisp-mode
  :requires (projectile)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (add-to-list 'company-backends '(company-elisp)))

;;; Python
(use-package virtualenvwrapper
  :ensure t)

(use-package importmagic
  :ensure t
  :requires (python)
  :commands (importmagic-fix-imports)
  :bind (:map python-mode-map
              ("C-c C-l" . importmagic-fix-imports)))

(use-package python-black
  :ensure t
  :requires (python)
  :bind (:map python-mode-map
              ("C-c b" . python-black-buffer)))

(use-package pytest
  :requires (python)
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-t" . pytest-pdb-one))
  :config
  (setq-local pytest-project-root-files '(".dir-locals.el" "pyproject.toml"))
  (setq-local pytest-cmd-format-string "cd '%s' ; and %s %s '%s'"))

(use-package elisp-python
  :load-path "site-lisp"
  :after (importmagic projectile utils virtualenvwrapper))

(use-package python
  :bind (:map python-mode-map
              ("<tab>" . 'python-indent-shift-right)
              ("<backtab>" . 'python-indent-shift-left)
              ("C-c C-e" . 'run-python)
              ("C-c C-d" . 'pdb))
  :config
  (defalias 'run-python 'python)
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-guess-indent-offset-verbose nil))
;;;

;;; Javascript/Typescript
(use-package jest-test-mode
  :ensure t
  :bind (:map js2-mode-map
              ("C-c C-t" . jest-test-debug-run-at-point)
         :map typescript-mode-map
              ("C-c C-t". jest-test-debug-run-at-point)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :interpreter "node"
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-always-indent 'complete)
  (setq-local js-indent-level 2))

(use-package js2-jsx-mode
  :after (js2-mode)
  :mode ("\\.jsx$" . js2-jsx-mode))

(use-package typescript-mode
  :ensure t
  :mode ((("\\.tsx$" . typescript-mode) ("\\.ts$" . typescript-mode)))
  :config
  (progn
    (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-always-indent 'complete)
    (setq-local js-indent-level 4)
    (setq-local tab-width 4)))

;;; Bash
(use-package company-shell
  :hook sh-mode
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package sh-script
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 4))

;;; C/C++
(use-package cc-mode
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (setq-local c-default-style "linux")
  (setq-local tab-always-indent t)
  (setq-local indent-tabs-mode nil))
;;;

;;; Make
(use-package make-mode
  :config
  (add-hook 'before-save-hook '(lambda () (tabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode +1)
  (setq-local tab-width 4))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("README\\.rst\\'" . gfm-mode)
         ("\\.rst\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-c l" . markdown-live-preview-mode))
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq markdown-command "multimarkdown")
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 4))
;;;

;;; Rust
(use-package rust-mode
  :ensure t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 2))
;;;

;;; Ruby
(use-package ruby-mode
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 2))
;;;

;;; SQL
(use-package sql
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 4))
;;;

;;; Assembly x86
(use-package nasm-mode
  :ensure t)
;;;

;;; Terraform
(use-package company-terraform
  :ensure t
  :after company
  :config
  (company-terraform-init))

(use-package terraform-mode
  :ensure t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 2))
;;;

;;; Web
(use-package web-mode
  :ensure t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t)
  (setq-local indent-tabs-mode -1)
  (setq-local tab-width 2)
  (setq-local web-mode-markup-indent-offset 2)
  (setq-local web-mode-css-indent-offset 2)
  (setq-local web-mode-code-indent-offset 2)
  (setq-local web-mode-enable-auto-pairing t)
  (setq-local web-mode-enable-auto-closing t)
  (setq-local web-mode-enable-current-element-highlight t)
  (setq-local web-mode-enable-current-column-highlight -1)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode)))
;;;

;;; Dockerfile
(use-package dockerfile-mode
  :ensure t)
;;;

;;; Latex
(use-package company-math
  :ensure t
  :hook ((tex-mode . company-math-mode)
         (TeX-mode . company-math-mode))
  :config
  (setq-local company-backends (append '((company-math-symbols-latex
                                          company-latex-commands)) company-backends)))

(use-package latex-preview-pane
  :ensure t)
;;;

;; --- ;;
