(require 'eglot)

(defvar lsp-python (executable-find "python"))
(defvar lsp-server-path "/opt/pyls/output/bin/Release/")
(defvar lsp-search-paths [])

(defclass eglot-mspyls (eglot-lsp-server) ()
  :documentation
  "MS Python Language Server.")

(setq-local eglot-workspace-configuration
              '((:python :autoComplete (:extraPaths nil)
                         :analysis (:autoSearchPaths :json-false :usePYTHONPATH :json-false))))

(cl-defmethod eglot-initialization-options ((_server eglot-mspyls))
  `(:interpreter
    (:properties
     (:InterpreterPath ,lsp-python))
     :searchPaths ,lsp-search-paths))

(add-to-list 'eglot-server-programs
             `(python-mode eglot-mspyls
                           (executable-find "dotnet")
                           (concat lsp-server-path "Microsoft.Python.LanguageServer.dll")))

(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'eglot-mspyls)
