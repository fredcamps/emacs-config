;;; utils.el --- package that have some utilities
;;
;;; Commentary:
;; Some utilities for Emacs usage
;;
;;; Code:
(require 'projectile)


(defcustom utils:fl-blankspace +1
  "Flag for toggling whitespace."
  :type 'bool
  :group 'utils)

(defun utils:--get-template-directory ()
  "Get template directory."
  (let (template-directory)
    (setq template-directory
          (file-name-as-directory (concat user-emacs-directory "templates")))))

(defun utils:kill-all-buffers ()
  "Kill all buffers except for that are on list."
  (interactive)
  (let (not-to-kill-list)
    (setq not-to-kill-list '("*scratch*"))
    (dolist (current-buffer (buffer-list))
      (if (member (buffer-name current-buffer) not-to-kill-list)
          (bury-buffer)
        (kill-buffer (buffer-name current-buffer))))))

(defun utils:indent-region (N)
  "Indent region N."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun utils:unindent-region (N)
  "Unindent region N."

  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun utils:get-file-contents(fpath)
  "Return the contents of a file as a string.  FPATH."

  (with-temp-buffer
    (insert-file-contents fpath)
    (buffer-string)))

(defun utils:toggle-invisibles()
  "Turn on/off whitespace and indent guides."
  (interactive)

  (indent-guide-mode utils:fl-blankspace)
  (if (eq utils:fl-blankspace -1)
      (progn
        (whitespace-mode utils:fl-blankspace)
        (setq utils:fl-blankspace +1))
    (progn
      (whitespace-mode)
      (setq utils:fl-blankspace -1))))

(defun utils:new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*Untitled*")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(defun utils:generate-project-files(language)
  "API for generate project .dir-locals.el file if doesn't exists.  LANGUAGE."
  (unless (projectile-project-root)
    (error "Project root not found or not configured, this mode will not work properly!"))
  (unless (file-exists-p (concat (projectile-project-root) ".dir-locals.el"))
    (let ((template-folder)
          (destination-folder)
          (destination-file)
          (template-files))
      (setq destination-folder (projectile-project-root))
      (setq template-folder
            (file-name-as-directory (concat (utils:--get-template-directory) language)))
      (setq template-files (cdr (cdr (directory-files template-folder))))
      (dolist (tpl-file template-files)
        (setq destination-file
              (concat "." (replace-regexp-in-string "\.tmpl" "" tpl-file)))
        (copy-file (concat template-folder tpl-file)
                   (concat destination-folder destination-file) t)))))

(defun utils:replace-string-in-file (dest-file pattern new-value-string)
  "Replace string in file.  DEST-FILE PATTERN NEW-VALUE-STRING."
  (let ((file-contents)
        (tmp-file))
    (setq tmp-file (concat dest-file "-tmp"))
    (setq file-contents (replace-regexp-in-string
                         pattern new-value-string (utils:get-file-contents dest-file) t))
    (append-to-file file-contents nil tmp-file)
    (copy-file tmp-file dest-file t)
    (delete-file tmp-file nil)))

(defun utils:--proc-force-wait (process)
  "Block until PROCESS exits successfully."
  (while (process-live-p process)
    (sit-for 0.1 t)))

(defun utils:run-process(new-buffer proc-name &rest args)
  "Run sync PROCESS and show output on a new named BUFFER.  NEW-BUFFER PROC-NAME &REST ARGS."
  (let ((proc))
    (setq proc (apply #'start-process "running-buffer-ps" new-buffer proc-name args))
    (set-process-sentinel proc
                          #'(lambda (p event)
                              (when (= 0 (process-exit-status p))
                                (message "Process: %s had event '%s'" p event))))
    (delete-other-windows)
    (switch-to-buffer-other-window new-buffer)
    (utils:--proc-force-wait proc))
  (display-buffer (current-buffer)))

(defun utils:set-project-root-on-current-directory ()
  "Include .projectile file on current-directory."
  (interactive)
  (write-region "" nil (expand-file-name ".projectile")))

(provide 'utils)
;;; utils ends here
