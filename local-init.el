;;; local-init.el --- Package that setup Emacs
;;
;;; Commentary:
;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; M-x eval-buffer for reload config
;;
;; Some helpful bindings
;; <C-x r t> # insert multiple lines on rectlangle/region
;; <C-q c-j> # quote mode and insert new line
;; <C-x SPC> # rentangle-mark-mode
;; <M-g g>   # goto-line
;; <C-x d> # dired-mode
;; t-toggle_all
;; m-mark
;; u-unmark
;; A query-regexp
;; B query-replace
;;
;;
;;
;;; Code:
;;

(defun init:setup-repository ()
  "Set repository channels."
  (setq package-enable-at-startup nil)
  (setq package-check-signature nil)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  (setq package-archive-priorities '(("melpa"  . 10)
                                     ("gnu"    . 5)
                                     ("nongnu" . 1)))
  (package-refresh-contents))

(defun install-el-package ()
  "Install an arbitrary package from repository." (interactive)
  (init:setup-repository)
  (call-interactively #'package-install))

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (init:setup-repository)
    (package-install 'use-package)
    (require 'use-package))
  (unless (package-installed-p 'use-package-ensure-system-package)
    (init:setup-repository))
  ;; (setq use-package-always-pin "melpa")
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; --- Emacs settings --- ;;
;;; Perfomance
(setq gc-cons-threshold 100000000)
;(setq max-lisp-eval-depth 10000)
;(setq max-specpdl-size 10000)
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
(setq search-whitespace-regexp ".*?")
(setq load-prefer-newer t)
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(electric-pair-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default save-interprogram-paste-before-kill t uniquify-buffer-name-style 'forward require-final-newline t auto-save-default nil make-backup-files nil)
(save-place-mode 1)
;; (setq save-place-file (concat user-emacs-directory ".emacs-places"))
(setq temporary-file-directory (concat user-emacs-directory "tmp"))
(delete-selection-mode t)
(custom-set-variables '(comint-scroll-to-bottom-on-input t) ; always insert at the bottom
                      '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
                      '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
                      '(comint-completion-autolist t) ; show completion list when ambiguous
                      '(comint-input-ignoredups t) ; no duplicates in command history
                      '(comint-completion-addsuffix t) ; insert space/slash after file completion
                      )
(setq completion-auto-help t)
;;(setq doc-view-continuous t)

(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode t))

;;;

;;; Misc settings
(setq eldoc-idle-delay 2)
(setq browse-url-generic-program "firefox")
;;;

;;; Indentation settings
(setq-default fill-column 100)
(electric-indent-mode +1)
(setq-default js-indent-level 2 tab-width 2 tab-always-indent 'complete indent-tabs-mode nil)
;;;

;;; Appearance settings
(setq-default inhibit-splash-screen t inhibit-startup-message t)
(transient-mark-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-time-mode -1)
(display-battery-mode -1)
(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(setq show-paren-priority -1)
(which-function-mode -1)
;;;

;;; Keybindings settings
(global-set-key (kbd "M-g C-p") 'windmove-up)
(global-set-key (kbd "M-g <up>") 'windmove-up)
(global-set-key (kbd "M-g <down>") 'windmove-down)
(global-set-key (kbd "M-g C-n") 'windmove-down)
(global-set-key (kbd "M-g C-b") 'windmove-left)
(global-set-key (kbd "M-g <left>") 'windmove-left)
(global-set-key (kbd "M-g C-f") 'windmove-right)
(global-set-key (kbd "M-g <right>") 'windmove-right)
(global-set-key (kbd "C-c -") 'shrink-window-horizontally)
(global-set-key (kbd "C-c +") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x -") 'shrink-window)
(global-set-key (kbd "C-x +") 'enlarge-window)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-x t") #'ansi-term)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-go-back)
(global-set-key (kbd "C-x 4 ,") #'xref-find-definitions-other-window)
(global-set-key (kbd "<backtab>") #'indent-for-tab-command)
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

;;; Remote access
(setq tramp-default-method "ssh" tramp-backup-directory-alist backup-directory-alist tramp-ssh-controlmaster-options "ssh")
;;;

;;; Hooks
(add-hook 'completion-setup-hook (lambda ()
                                   (run-at-time 3 nil (lambda ()
                                                        (delete-windows-on "*Completions*")))))
(add-hook 'dirtrack-mode-hook (function (lambda ()
                                          (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(add-hook 'ein:notebook-multilang-mode-hook (lambda ()
                                              (spacemacs/toggle-whitespace-cleanup-off)))

(add-hook 'before-save-hook 'delete-trailing-whitespace t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode t)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode t)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode t)
;;;

;;; Perfomance hooks
(add-hook 'minibuffer-setup-hook (lambda()
                                   (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda()
                                  (setq gc-cons-threshold 800000)))
;;;

;;; Indentation hooks
(add-hook 'make-file-mode-hook (lambda()
                                 (setq-local tab-width 4)
                                 (setq-local indent-tabs-mode t)) t t)
(add-hook 'markdown-mode-hook (lambda()
                                (setq-local tab-width 4)) t t)
(add-hook 'sql-mode-hook (lambda()
                           (setq-local tab-width 4)) t t)
;;;
;; --- ;;

;; --- Extensions settings --- ;;
(use-package use-package-ensure-system-package)

;;; Profile Emacs startup
(use-package esup
  :no-require t
  :commands (esup)
  :config (setenv "TERM" "screen-24bits"))
;;;

;;; RPC stack
(use-package epc
  :no-require t
  :defer t)
;;;

;;; Move lines
(advice-add 'move-text-up
            :after 'indent-region-advice)
(advice-add 'move-text-down
            :after 'indent-region-advice)


(use-package move-text
  :no-require t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))
;;;

;;; Auto compile elisp packages
(use-package auto-compile
  :hook elisp-mode-hook
  :demand t
  :custom (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest t)
  (auto-compile-update-autoloads t)
  :config (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))
;;;

;;; Get shell environment variables
(use-package exec-path-from-shell
  :no-require t
  :config (when (memq window-system '(mac ns x))
            (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME"))
            (setq exec-path-from-shell-arguments nil)  ;; "-i for fish"
            (exec-path-from-shell-initialize )))


;;; Better than IDO
(use-package counsel
  :no-require t
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq swiper-stay-on-quit -1)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --color never '%s' %s")
  (global-set-key "\C-s" 'swiper)
  ;; (global-set-key "\C-r" 'counsel-grep-backward)
  (global-set-key (kbd "C-c r") 'ivy-resume)
  (global-set-key (kbd "C-c C-o") 'ivy-occur)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c C-k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :no-require t
  :bind (("C-c f" . counsel-projectile-find-file)
         ("C-c p" . counsel-projectile-switch-project)))
;;;

;;; Project management support
(use-package projectile
  :defer t
  :no-require t
  :after (doom-modeline)
  :custom (projectile-enable-caching t)
  (projectile-dynamic-mode-line t)
  (projectile-indexing-method 'alien)
  (projectile-require-project-root 't)
  (projectile-completion-system 'ivy)
  (projectile-tags-backend 'auto)
  :config (projectile-mode +1))
;;;

;;; Snippets support
(use-package yasnippet
  :no-require t
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config (let ((snippets-dir)
                (yasnippet-snippets-dir))
            (setq snippets-dir (concat user-emacs-directory "snippets"))
            (setq yas-snippet-dirs (list snippets-dir))))

(use-package yasnippet-snippets
  :no-require t
  :defer t
  :after yasnippet)


;;;Debuggers
(use-package realgud
  :no-require t
  :commands (realgud:pdb realgud:pdb-remote realgud:gdb))

(use-package realgud-trepan-ni ;;js/ts debugger
  :no-require t
  :commands (realgud:trepan-ni)
  :hook ((typescript-mode . (lambda ()
                              (setq realgud:trepan-ni-command-name "trepan-ni --require ts-node/register ")))
         (js2-mode . (lambda ()
                       (setq realgud:trepan-ni-command-name "trepan-ni ")))
         (js2-jsx-mode . (lambda ()
                           (setq realgud:trepan-ni-command-name "trepan-ni ")))))
;;;

;;; git support
(use-package git-gutter
  :no-require t
  :custom (git-gutter:handled-backends '(git hg bzr svn))
  (git-gutter:hide-gutter t)
  :config (global-git-gutter-mode +1))

(use-package magit
  :no-require t
  :defer t)

(use-package magit-gitflow
  :defer t
  :no-require t)
;;;

;;; Search ripgrep
(use-package rg
  :ensure-system-package rg
  :defer t
  :no-require t
  :config (rg-enable-default-bindings))

;;; Occur and folding
(use-package loccur
  :no-require t
  :bind (:map prog-mode-map ("C-c o" . loccur)))
;;;

;;; Rest Client
(use-package company-restclient
  :hook restclient-mode
  :no-require t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))

(use-package restclient
  :defer t
  :no-require t)
;;;

;;; Static/markup files preview
(use-package skewer-mode
  :no-require t
  :requires (simple-httpd)
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (html-mode . skewer-html-mode))
  :custom (httpd-port 54322))
;;;

;;; Indent guide line
(use-package indent-guide
  :no-require t
  :defer t)
;;;

;;; Show color by hex
(use-package rainbow-mode
  :no-require t
  :defer t
  :pin gnu)
;;;

;;; Colorize delimiters by scope
(use-package rainbow-delimiters
  :no-require t
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;;;

;;; Doom-mode
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (when (boundp 'projectile-project-root)
    (doom-modeline-project-detection 'projectile))
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-check-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-persp-icon t)
  (doom-modeline-workspace-name t)
  (doom-modeline-env-version t)
  (doom-modeline-lsp t)
  (doom-modeline-modal-icon t)
  (doom-modeline-gnus t)
  (doom-modeline-gnus-timer 2)
  (doom-modeline-env-version t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)
  (doom-modeline-env-load-string "..."))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors") ;; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-tomorrow-night t)
  (set-face-attribute 'region nil
                      :foreground "#01F"
                      :background "#182566")
  (set-face-attribute 'hl-line nil
                      :background "#141414"
                      :bold t)
  (set-face-attribute 'show-paren-match nil
                      :foreground "#0F0"
                      :background "#000"
                      :bold t)
  (set-face-attribute 'show-paren-match-expression nil
                      :foreground "#0F0"
                      :background "#000"
                      :bold t)
  (set-face-attribute 'show-paren-mismatch nil
                      :foreground "red"
                      :foreground "#000"))
;;;


;;; Show Icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))

;; (use-package icons-in-terminal
;;   :ensure t
;;   :unless (display-graphic-p))
;;;

;;; Clipboard sharing
(use-package xclip
  :unless (display-graphic-p)
  :pin gnu
  :no-require t
  :config (xclip-mode +1))
;;;

;;; keybindings discoverable
(use-package which-key
  :no-require t
  :config (which-key-mode))
;;;

;;; Generic code jumping
(use-package dumb-jump
  :no-require t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :init (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))
;;;

;;; better than built-in python-shell-send
(use-package eval-in-repl
  :no-require t
  :init (setq eir-repl-placement 'right)
  (setq eir-ielm-eval-in-current-buffer t)
  (add-hook 'sh-mode-hook (lambda()
                            (local-set-key (kbd "C-c C-c") 'eir-eval-in-bash)))
  (add-hook 'js2-mode-hook (lambda ()
                             (local-set-key (kbd "C-c C-c") 'eir-eval-in-javascript)))
  (add-hook 'python-mode-hook (lambda ()
                                (local-set-key (kbd "C-c C-c") 'eir-eval-in-python))))
;;;

;;; terminal emulator better than built-in
(use-package multi-vterm
  :no-require t
  :config (setq vterm-always-compile-module t)
  (global-set-key (kbd "C-x t") 'multi-vterm))
;;;

;;; easy compile and run
(use-package quickrun
  :bind (:map prog-mode-map ("C-c e" . quickrun))
  :no-require t)
;;;

;;; easy disassembly while compiling
(use-package rmsbolt
  :defer t
  :no-require t)
;;;

;;; Local local environment variables
(use-package envrc
  :config (envrc-global-mode))
;;;


;;; Editor Config
(use-package editorconfig
  :no-require t
  :config (editorconfig-mode 1))
;;;
;; --- ;;

;; --- My Custom utilities --- ;;
(use-package utils
  :no-require t
  :load-path "site-lisp"
  :commands (utils:smart-tabify utils:replace-string-in-file utils:generate-project-files)
  :bind (("C-c SPC" . utils:toggle-invisibles)
         ("C-c k" . utils:kill-all-buffers)
         ("C-c n" . utils:new-buffer))
  :config (add-hook 'before-save-hook 'utils:smart-tabify t t))
;; --- ;;
;;;

;;; Company completion framework
(use-package company
  :no-require t
  :defer t
  :init (global-company-mode)
  :custom (company-idle-delay 0)
  (company-transformers nil)
  (company-minimum-prefix-length 3)
  :config (bind-key "C-c c" 'company-capf)
  (bind-key "C-c d" 'company-show-doc-buffer)
  (bind-key "C-c v" 'company-show-location)
  (when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode -1)))

(use-package company-dict
  :no-require t
  :custom (company-dict-enable-fuzzy nil)
  (company-dict-enable-yasnippet +1)
  :config (setq company-dict-dir (concat user-emacs-directory "dict"))
  (add-to-list 'company-backends 'company-dict))

(use-package company-tabnine) ;; AI completion
;;;

;; Flymake
(use-package flymake
  :no-require t
  :hook (prog-mode . flymake-mode))
;;;

(use-package eglot
  :defer t
  :no-require t
  :bind (:map eglot-mode-map ("C-c C-r" . elglot-rename)
              ("C-c C-?" . eldoc)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

;; --- Programming/Markdown/Serialization Languages Settings --- ;;
;;; JSON/Yaml
(use-package json-mode
  :no-require t
  :bind (:map json-mode-map ("C-c b" . json-mode-beautify)))

(use-package yaml-mode
  :no-require t)
;;;

;;; Elisp
(setq-default lisp-indent-offset 2)
(use-package elisp-slime-nav
  :no-require t
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode))

(use-package package-lint
  :no-require t)

(use-package elisp-lint
  :no-require t
  :hook emacs-lisp-mode-hook)

(use-package elisp-format
  :no-require t
  :bind (:map emacs-lisp-mode-map ("C-c b" . elisp-format-buffer)))

;;;

;;; Python
(use-package virtualenvwrapper
  :no-require t
  :defer t
  :ensure t)

(use-package python
  :hook
  (python-mode . python-ts-mode)
  (python-ts-mode . python:setup)
  :custom
  (python-shell-completion-native-enable t)
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  :config
  (defcustom python:virtualenv-name nil "virtualenv name." :type 'string :group 'python)
  (defun python:--ask-create-virtual-env ()
    "Create virtual env"
    (if (y-or-n-p "Do you want to create virtualenv?")
        (progn (call-interactively #'venv-mkvirtualenv))
      (call-interactively #'venv-workon)))

  (defun python:--replace-template-variables (dir-locals-file)
    "Replace variables from .dir-locals.el file.  DIR-LOCALS-FILE."
    (utils:replace-string-in-file dir-locals-file
                                  "{{ VENV-NAME }}" venv-current-name))

  (defun python:setup ()
    "Function that setups 'python-mode'."
    (message "Hacking local variables!!!")
    (hack-local-variables)
    (when python:virtualenv-name
      (venv-workon python:virtualenv-name))
    (setq-default eglot-workspace-configuration '((:pylsp . (:configurationSources ["flake8"]
                                                              :plugins (:jedi_completion (:fuzzy t)
                                                                         :pycodestyle (:enabled :json-false)
                                                                         :mccabe (:enabled :json-false)
                                                                         :pyflakes (:enabled :json-false)
                                                                         :pylint (:enabled :json-false)
                                                                         :flake8 (:enabled :json-false :maxLineLength 88)
                                                                         :ruff (:enabled t :lineLength 88)
                                                                         :pydocstyle (:enabled t :convention "numpy")
                                                                         :yapf (:enabled :json-false)
                                                                         :autopep8 (:enabled :json-false)
                                                                         :black (:enabled t :line_length 88  :cache_config t))))))

    (eglot-ensure)
    (let ((project-root))
      (projectile-mode +1)
      (with-eval-after-load "projectile"
        (setq project-root (projectile-project-root))))

    (if (executable-find "ipython")
        (setq python-shell-interpreter (executable-find "ipython"))
      (setq python-shell-interpreter-args "-i --simple-prompt")))

  (defun python:init ()
    "Initialize project conf for 'python-mode'."
    (interactive)
    (let ((dir-locals-file) (project-root))
      (with-eval-after-load "projectile"
        (setq project-root (projectile-project-root))
        (setq dir-locals-file (concat project-root ".dir-locals.el"))
        (python:--ask-create-virtual-env)
        (utils:generate-project-files "python")
        (python:--replace-template-variables dir-locals-file)
        (python:setup)))))

(use-package importmagic
  :no-require t
  :commands importmagic-fix-imports
  :bind (:map python-mode-map
          ("C-c C-l" . importmagic-fix-imports)))

(use-package python-black
  :no-require t
  :bind (:map python-mode-map
          ("C-c b" . python-black-buffer)))

(use-package pytest
  :no-require t
  :init
  (add-hook 'python-mode-hook
    (lambda () (define-key python-mode-map (kbd "C-c C-t") 'pytest-pdb-one)))
  :config
    ;; hacking for suppress Symbol’s value void variable
  (setq python-shell--interpreter (executable-find "ipython"))
  (setq python-shell--interpreter-args "-i --simple-prompt")
  :custom
  (pytest-project-root-files '(".projectile" "pyproject.toml" ".dir-locals.el"))
  (pytest-cmd-format-string "cd '%s' ; and %s %s '%s'"))

(use-package ein) ;; python-notebook
;;;


;;; Go
(use-package go-mode
  :no-require t
  :hook (go-mode . eglot-ensure))
;;;


;;; java
(use-package eglot-java
  :no-require t
  :hook (java-mode . eglot-ensure))
;;;

;;; Javascript/Typescript
(defun js-ts-bootstrap ()
  "Init javascript/typescript configuration."
  ;; I'm not sure why this is needed, but it throws an error if I remove it
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))

  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions
    'my-project-try-tsconfig-json nil nil)

  (add-to-list 'eglot-server-programs
    '((typescript-mode) "typescript-language-server" "--stdio"))
  (eglot-ensure))

(use-package typescript-mode
  :hook ((js-ts-mode . js-ts-bootstrap)
         (typescript-mode . js-ts-bootstrap)
         (typescript-ts-mode . js-ts-bootstrap)
         (typescriptreact-mode . js-ts-bootstrap)
         (tsx-ts-mode . js-ts-bootstrap))
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescriptreact-mode)))

(use-package js2-mode
  :no-require t
  :mode ("\\.js$" . js2-mode)
  :interpreter "node"
  :hook ((js2-mode . js-ts-bootstrap)
         (js-ts-mode . js-ts-bootstrap)))

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
          ("\\.tsx\\'" . jtsx-tsx-mode)
          ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
          (jtsx-tsx-mode . hs-minor-mode)
          (jtsx-typescript-mode . hs-minor-mode)))

(use-package js-comint
  :no-require t
  :bind (:map js2-mode-map
          ("C-c C-e" . 'js-send-last-sexp)
          ("C-c C-b" . 'js-send-buffer)
          ("C-c C-g" . 'js-send-buffer-and-go))
  :config
  (setq js-comint-program-command "node")
  (setq js-comint-program-arguments '("--interactive"))
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t))

(use-package ts-comint
  :no-require t
  :bind (:map typescript-mode-map
          ("C-c C-e" . 'ts-send-last-sexp)
          ("C-c C-b" . 'ts-send-buffer)
          ("C-c C-g" . 'ts-send-buffer-and-go)))

(use-package add-node-modules-path
  :no-require t
  :hook ((js2-jsx-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)))

(use-package jest-test-mode
  :no-require t
  :bind (:map js2-mode-map ("C-c C-t" . 'jest-test-debug-run-at-point)
         :map typescript-mode-map ("C-c C-t" . 'jest-test-debug-run-at-point)))

;;; Bash/Shell Scripting
(use-package company-shell
  :no-require t
  :after company
  :hook sh-mode
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package sh-script
  :no-require t
  :hook
  (sh-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  :custom
  (sh-basic-offset 4)
  :config
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))
;;;

;; ;;; C/C++
(use-package cc-mode
  :no-require t
  :hook (c-mode-common . eglot-ensure)
  :ensure-system-package clangd
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (c-tab-always-indent t)
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))
;; ;;;

;;; Make
(use-package make-mode
  :no-require t)

;;; Markdown
(use-package markdown-mode
  :no-require t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map ("C-c C-c l" . markdown-live-preview-mode)))
;;;

;;; Rust
(use-package rust-mode
  :no-require t
  :ensure-system-package rust-analyzer
  :hook (rust-mode . eglot-ensure)
  :custom
  (rust-indent-offset 4)
  :config
  ;; https://rust-analyzer.github.io/manual.html#configuration
  (setq-local eglot-workspace-configuration
    ;; Setting the workspace configuration for every
    ;; rust-mode buffer, you can also set it with dir-local
    ;; variables, should you want different configuration
    ;; per project/directory.
    '(:rust-analyzer
       ( :procMacro ( :attributes (:enable t)
                      :enable t)
         :cargo (:buildScripts (:enable t))
         :diagnostics (:disabled ["unresolved-proc-macro"
                                  "unresolved-macro-call"])))))

(use-package cargo-mode
  :config
  (setq compilation-scroll-output t)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
;;;

;;; Ruby
(use-package ruby-mode
  :no-require t
  :hook (ruby-mode . eglot-ensure)
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs nil))
;;;

;;; SQL
(use-package sql
  :no-require t)
;;;

;;; Assembly x86
(use-package nasm-mode
  :no-require t
  :custom (nasm-basic-offset 4))
;;;

;;; Fish shell
(use-package fish-mode
  :no-require t
  :defer t)

;;; Terraform
(use-package company-terraform
  :hook terraform-mode
  :no-require t
  :after company
  :config (company-terraform-init))

(use-package terraform-mode
  :no-require t
  :custom (terraform-indent-level 2))
;;;

;;; Web
(use-package web-mode
  :no-require t
  :mode (("\\.css?\\'" . web-mode)
          ("\\.html?\\'" . web-mode)
          ("\\.djhtml\\'" . web-mode)
          ("\\.jinja2\\'" . web-mode))
  :custom (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight nil))
;;;

;;; Dockerfile
(use-package dockerfile-mode
  :no-require t)
;;;

;;; Latex
(use-package company-math
  :no-require t
  :after company
  :hook ((tex-mode . company-math-mode)
          (TeX-mode . company-math-mode))
  :init (add-to-list 'company-backends '(company-math-symbols-latex company-latex-commands)))

(use-package latex-preview-pane
  :defer t
  :no-require t)
;;;
;; --- ;;

(provide 'local-init)
;;; local-init ends here
