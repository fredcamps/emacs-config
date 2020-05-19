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
  "Set repository channels."
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

;;; Perfomance
(setq gc-cons-threshold 100000000)
;;;

;;; Encoding
(set-language-environment 'utf-8)
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
;(setq max-lisp-eval-depth 10000)
;(setq max-specpdl-size 10000)
(setq visible-bell t)
(setq load-prefer-newer t)
(electric-pair-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default save-interprogram-paste-before-kill t
              uniquify-buffer-name-style 'forward
              require-final-newline t
              auto-save-default nil
              make-backup-files nil)
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory ".emacs-places"))
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
(setq eldoc-idle-delay 1)
(setq browse-url-generic-program "firefox")
;;;

;;; Indentation settings
(setq-default fill-column 100)
(electric-indent-mode +1)
(setq-default js-indent-level 2
              tab-width 2
              tab-always-indent 'complete
              indent-tabs-mode nil)
;;;

;;; Appearance settings
(setq-default inhibit-splash-screen t
              inhibit-startup-message t)
(transient-mark-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(display-time-mode -1)
(display-battery-mode -1)
(line-number-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(which-function-mode -1)
(defun bootstrap-theme ()
  "Function that load theme and set custom faces."
  (load-theme 'wombat t)
  (custom-set-faces
   `(region ((t (:background, "#919199"))))
   `(hl-line ((t (:background, "#424242"))))
   `(show-paren-match ((t (:background, "#6a6a6b"))))
   `(show-paren-match ((t (:background, "#6a6a6b"))))
   `(which-func ((t (:foreground, "gray"))))))

(add-hook 'after-init-hook 'bootstrap-theme)
;;;

;;; Keybindings settings
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)
(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)
(define-key global-map (kbd "M-/") 'hippie-expand)
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

;;; Perfomance hooks
(add-hook 'minibuffer-setup-hook #'(lambda() (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda() (setq gc-cons-threshold 800000)))
;;;

;;; Indentation hooks
(add-hook 'make-file-mode-hook '(lambda()
                                  (setq-local tab-width 4)
                                  (setq-local indent-tabs-mode t)) nil t)
(add-hook 'markdown-mode-hook '(lambda() (setq-local tab-width 4)) nil t)
(add-hook 'sql-mode-hook '(lambda() (setq-local tab-width 4)) nil t)
;;;
;; --- ;;

;; --- Extensions settings --- ;;
;; (use-package esup
;;   :ensure t
;;   :no-require t
;;   :commands (esup)
;;   :config
;;   (setenv "TERM" "screen-256color"))

(use-package epc
  :ensure t
  :no-require t
  :defer t)

(use-package protbuf
  :load-path "3rd-party"
  :commands protect-buffer-from-kill-mode
  :config
  (protect-buffer-from-kill-mode nil (get-buffer "*scratch*")))

(use-package auto-compile
  :hook elisp-mode-hook
  :demand t
  :ensure t
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest t)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package redo+
  :load-path "3rd-party"
  :no-require t
  :bind (("C-x u" . undo-only)
         ("C-x U" . redo)))

(use-package move-lines
  :load-path "3rd-party"
  :no-require t
  :commands move-lines-mode
  :init
  (add-hook 'after-init-hook 'move-lines-mode))

(use-package ido
  :commands ido-everywhere
  :custom
  (ido-enable-flex-matching t)
  (ido-create-new-buffer 'prompt)
  (ido-use-faces nil)
  (ido-completion-buffer nil)
  (ido-completion-buffer-all-completions nil)
  :config
  (ido-everywhere t)
  (ido-mode +1))

(use-package flycheck
  :ensure t
  :no-require t
  :defer t
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'lines)
  (flycheck-idle-change-delay 0.5)
  (flycheck-check-syntax-automatically '(mode-enabled
                                         save
                                         idle-change
                                         new-line
                                         idle-buffer-switch))
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (flymake-mode -1)
  :config
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

;;; Company
(use-package company
  :ensure t
  :no-require t
  :defer t
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.5)
  (company-transformers nil)
  (company-minimum-prefix-length 3)
  :config
  (bind-key "C-c c" 'company-capf)
  (bind-key "C-c d" 'company-show-doc-buffer)
  (bind-key "C-c v" 'company-show-location)
  (when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode -1)))

(use-package company-dict
  :ensure t
  :no-require t
  :custom
  (company-dict-enable-fuzzy nil)
  (company-dict-enable-yasnippet +1)
  :init
  (setq-local company-dict-dir (concat user-emacs-directory "dict"))
  (add-to-list 'company-backends 'company-dict))
;;;

(use-package projectile
  :ensure t
  :after (smart-mode-line)
  :custom
  (projectile-enable-caching t)
  (projectile-dynamic-mode-line t)
  (projectile-require-project-root 't)
  (projectile-completion-system 'ido)
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file))
;;;

(use-package yasnippet
  :ensure t
  :no-require t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (let ((snippets-dir)
        (yasnippet-snippets-dir))
    (setq snippets-dir (concat user-emacs-directory "snippets"))
    (setq yas-snippet-dirs (list snippets-dir))))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

(use-package git-gutter
  :defer t
  :config
  (global-git-gutter-mode +1)
  :ensure t)

(use-package magit
  :ensure t
  :no-require t
  :defer t)

(use-package magit-gitflow
  :ensure t
  :defer t
  :no-require t)

(use-package loccur
  :ensure t
  :no-require t
  :bind (:map prog-mode-map
              ("C-c o" . loccur)))

;;; Rest Client
(use-package company-restclient
  :hook restclient-mode
  :no-require t
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient)
  :ensure t)

(use-package restclient
  :mode "\\.rest$'"
  :no-require t
  :ensure t)
;;;

(use-package skewer-mode
  :ensure t
  :no-require t
  :requires (simple-httpd)
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (html-mode . skewer-html-mode))
  :custom
  (httpd-port 54322))

;; (use-package ag
;;   :ensure t)

(use-package column-enforce-mode
  :ensure t
  :no-require t
  :hook (prog-mode . column-enforce-mode)
  :custom
  (column-enforce-column 100))

(use-package indent-guide
  :no-require t
  :defer t
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :no-require t
  :defer t
  :pin gnu)

(use-package rainbow-delimiters
  :ensure t
  :no-require t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smart-mode-line
  :ensure t
  :no-require t
  :custom
  (sml/shorten-modes t)
  (sml/mode-width 'full)
  (sml/shorten-directory nil)
  (sml/show-client t)
  (sml/theme 'smart-mode-line-dark)
  (sml/no-confirm-load-theme t)
  :config
  (sml/setup))

(use-package xclip
  :pin gnu
  :ensure t
  :defer t
  :config
  (xclip-mode 1))

;;; Language server protocol
(use-package lsp-mode
  :ensure t
  :no-require t
  :commands (lsp-find-definition)
  :custom
  (lsp-idle-delay 1)
  (lsp-inhibit-lsp-hooks t)
  (lsp-disabled-clients '(lsp-pyls))
  (lsp-auto-configure -1)
  (lsp-eldoc-render-all t)
  (lsp-log-io nil)
  (lsp-enable-folding nil)
  (lsp-print-performance t)
  (lsp-enable-indentation t)
  (lsp-enable-xref t)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-completion-at-point -1)
  (lsp-enable-on-type-formatting t)
  (lsp-diagnostic-package :flycheck)
  (lsp-restart 'auto-restart)
  ;; (lsp-auto-guess-root t)
  :init
  (eldoc-mode)
  :config
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-,") #'xref-pop-marker-stack)
  (define-key lsp-mode-map (kbd "C-c ,") #'xref-pop-marker-stack)
  (define-key lsp-mode-map (kbd "C-x 4 .") #'xref-find-definitions-other-window)
  (flycheck-mode +1))

(use-package lsp-ui
  :ensure t
  :no-require t
  :commands (lsp-ui-sideline-enable
             lsp-ui-peek-enable
             lsp-ui-doc-enable
             lsp-ui-flycheck-enable
             lsp-ui-imenu-enable)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate -1)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-flycheck-list-position 'bottom)
  (lsp-ui-peek-enable nil)
  (lsp-ui-peek-show-directory nil)
  (lsp-ui-imenu-enable nil))

(use-package company-lsp
  :ensure t
  :no-require t
  :defer t
  :config
  (add-to-list 'company-backends 'company-lsp)
  (setq company-lsp-enable-snippet t
        company-lsp-enable-recompletion t
        company-lsp-async t
        company-lsp-cache-candidates nil))
;;;

;; --- ;;

;; --- My Custom utilities --- ;;
(use-package utils
  :no-require t
  :load-path "site-lisp"
  :bind (("C-c SPC" . utils:toggle-invisibles)
         ("C-c k" . utils:kill-all-buffers)
         ("C-c n" . utils:new-buffer-frame)))
;; --- ;;

;; --- Programming/Markdown/Serialization Languages Settings --- ;;
;;; JSON/Yaml
(use-package json-mode
  :no-require t
  :ensure t
  :bind (:map json-mode-map
              ("C-c b" . json-mode-beautify))
  :init
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t))

(use-package yaml-mode
  :no-require t
  :ensure t
  :defer t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Elisp
(use-package elisp-slime-nav
  :no-require t
  :ensure t
  :hook ((emacs-lisp-mode-hook . elisp-slime-nav-mode)
         (ielm-mode-hook . elisp-slime-nav-mode)))

(use-package package-lint
  :no-require t
  :hook emacs-lisp-mode-hook
  :ensure t)

(use-package elisp-format
  :no-require t
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c b" . elisp-format-buffer)))

(use-package elisp-mode
  :no-require t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))

;;; Python
(use-package virtualenvwrapper
  :no-require t
  :defer t
  :ensure t)

(use-package importmagic
  :ensure t
  :no-require t
  :commands importmagic-fix-imports
  :bind (:map python-mode-map
              ("C-c C-l" . importmagic-fix-imports)))

(use-package python-black
  :ensure t
  :no-require t
  :bind (:map python-mode-map
              ("C-c b" . python-black-buffer)))

(use-package pytest
  :ensure t
  :no-require t
  :bind (:map python-mode-map
              ("C-c C-t" . pytest-pdb-one))
  :custom
  (pytest-project-root-files '(".projectile" "pyproject.toml" ".dir-locals.el"))
  (pytest-cmd-format-string "cd '%s' ; and %s %s '%s'"))

(use-package python
  :no-require t
  :commands (python-indent-shift-left python-indent-shift-right)
  :bind (:map python-mode-map
              ("<tab>" . 'python-indent-shift-right)
              ("<backtab>" . 'python-indent-shift-left)
              ("C-c C-p" . 'run-python))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-indent-guess-indent-offset-verbose nil)
  :init
  (when (executable-find "ipython")
    (setq-local python-shell-interpreter "ipython")
    (setq-local python-shell-interpreter-args "-i --simple-prompt"))
  :config
  (defalias 'run-python 'python)
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))

(use-package lsp-python-ms
  :ensure t
  :no-require t
  :defer t
  :custom
  (lsp-python-ms-disabled [])
  (lsp-python-ms-python-executable-cmd "python")
  (lsp-python-ms-cache "System")
  (lsp-python-ms-executable
   "/opt/pyls/output/bin/Release/Microsoft.Python.LanguageServer"))

(use-package elisp-python
  :load-path "site-lisp"
  :no-require t
  :commands python:bootstrap
  :hook ((python-mode . python:bootstrap)
         (after-hack-local-variables . (lambda () (lsp 1)))))
;;;

;;; Javascript/Typescript
(use-package jest-test-mode
  :ensure t
  :no-require t
  :bind (:map js2-mode-map
              ("C-c C-t" . jest-test-debug-run-at-point)
         :map typescript-mode-map
              ("C-c C-t". jest-test-debug-run-at-point)))

(use-package js2-mode
  :ensure t
  :no-require t
  :mode ("\\.js$" . js2-mode)
  :interpreter "node"
  :hook lsp
  :config
  (with-eval-after-load "flycheck"
    (add-to-list 'flycheck-disabled-checkers
                 '(javascript-eslint javascript-jshint javascript-standard)))
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t))

(use-package js2-jsx-mode
  :no-require t
  :mode ("\\.jsx$" . js2-jsx-mode)
  :hook lsp
  :after js2-mode
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t))

(use-package typescript-mode
  :ensure t
  :no-require t
  :mode ((("\\.tsx$" . typescript-mode) ("\\.ts$" . typescript-mode)))
  :hook lsp
  :custom
  (typescript-indent-level 4)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) t t))
;;;

;;; Bash
(use-package company-shell
  :no-require t
  :after company
  :hook sh-mode
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package sh-script
  :no-require t
  :defer t
  :custom
  (sh-basic-offset 4)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; C/C++
(use-package cc-mode
  :no-require t
  :hook lsp
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (c-tab-always-indent t)
  :config
  (with-eval-after-load "company-clangd"
    (setq-local company-clang-executable "clang-9"))
  (with-eval-after-load "lsp-clients"
    (setq-local lsp-clients-clangd-executable "clangd-9"))
  (with-eval-after-load "flycheck"
    (add-to-list 'flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Make
(use-package make-mode
  :no-require t
  :config
  (add-hook 'before-save-hook '(lambda () (tabify (point-min) (point-max))) nil t))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :no-require t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("README\\.rst\\'" . gfm-mode)
         ("\\.rst\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-c l" . markdown-live-preview-mode))
  :custom
  (markdown-command "multimarkdown")
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Rust
(use-package rust-mode
  :ensure t
  :no-require t
  :hook lsp
  :custom
  (rust-indent-offset 4)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Ruby
(use-package ruby-mode
  :no-require t
  :hook lsp
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs nil)
  :init
  (with-eval-after-load "lsp-clients"
    (setq-local lsp-solargraph-use-bundler t))
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; SQL
(use-package sql
  :no-require t
  :defer t
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Assembly x86
(use-package nasm-mode
  :ensure t
  :defer t
  :no-require t
  :custom
  (nasm-basic-offset 4))
;;;

;;; Terraform
(use-package company-terraform
  :ensure t
  :hook terraform-mode
  :no-require t
  :after company
  :config
  (company-terraform-init))

(use-package terraform-mode
  :ensure t
  :no-require t
  :defer t
  :custom
  (terraform-indent-level 2)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Web
(use-package web-mode
  :ensure t
  :no-require t
  :mode (("\\.css?\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight nil)
  :config
  (add-hook 'before-save-hook '(lambda () (untabify (point-min) (point-max))) nil t))
;;;

;;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :defer t
  :no-require t)
;;;

;;; Latex
(use-package company-math
  :ensure t
  :no-require t
  :after company
  :hook ((tex-mode . company-math-mode)
         (TeX-mode . company-math-mode))
  :init
  (add-to-list 'company-backends '(company-math-symbols-latex
                                   company-latex-commands)))

(use-package latex-preview-pane
  :ensure t
  :defer t
  :no-require t)
;;;

;; --- ;;

(provide 'local-init)
;;; local-init ends here
