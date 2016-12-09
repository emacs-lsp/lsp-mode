;;; -*- lexical-binding: t -*-
(require 'lsp-document)
(require 'lsp-receive)
(require 'projectile)
(require 'lsp-send)
(require 'cl-lib)

(cl-defstruct lsp--client
  (language-id :read-only t)
  (send-sync :read-only t)
  (send-async :read-only t)
  (type :read-only t)
  (new-connection :read-only t)
  (get-root :read-only t))
(defvar lsp--defined-clients (make-hash-table))

(defun lsp-define-client (major-mode language-id type &rest args)
  "Define a LSP client.
MAJOR-MODE is the major-mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server."
  (let ((client))
    (case type
      ('stdio (setq client (make-lsp--client
			     :language-id language-id
			     :send-sync 'lsp--stdio-send-sync
			     :send-async 'lsp--stdio-send-async
			     :type type
			     :new-connection (lsp--make-stdio-connection
					      (plist-get args :name)
					      (plist-get args :command))
			     :get-root (or (plist-get args :get-root)
					   #'projectile-project-root)))))
    (puthash major-mode client lsp--defined-clients)))

(defun lsp--make-stdio-connection (name command)
  (lambda ()
    (make-process
     :name name
     :connection-type 'pipe
     :command (if (consp command) command (list command))
     :filter #'lsp--process-filter)))

(defun lsp--rust-get-root ()
  (let ((dir default-directory))
    (while (not (or (file-exists-p (concat (file-name-as-directory dir)
					    "Cargo.toml"))
		     (string= dir "/")))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if (string= dir "/")
	(user-error "Couldn't find Rust project")
      dir)))

;;;###autoload
(define-minor-mode global-lsp-mode ""
  nil nil nil
  :global t

  (add-hook 'find-file-hook #'lsp-on-open)
  (add-hook 'after-save-hook #'lsp-on-save)
  (add-hook 'after-change-functions #'lsp-on-change)
  (lsp-define-client 'rust-mode "rust"
		     'stdio
		     :command "rls"
		     :name "Rust Language Server"
		     :get-root #'lsp--rust-get-root)

  (lsp-define-client 'go-mode "go" 'stdio
		     :command '("langserver-go" "-mode=stdio")
		     :name "Go Language Server"
		     :get-root #'projectile-project-root))

(provide 'lsp-mode)
