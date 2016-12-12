;;; -*- lexical-binding: t -*-
(require 'lsp-methods)
(require 'lsp-receive)
(require 'lsp-send)
(require 'cl-lib)

(defun lsp-define-client (major-mode language-id type get-root &rest args)
  "Define a LSP client.
MAJOR-MODE is the major-mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server.
Optional arguments:
`:name' is the process name for the language server.
`:command' is the command to run if `TYPE' is 'stdio.
`:on-initialize' is the function to call when a new project/workspace is initialized."
  (let ((client))
    (setq client
	  (cl-case type
	    ('stdio (make-lsp--client
		     :language-id language-id
		     :send-sync 'lsp--stdio-send-sync
		     :send-async 'lsp--stdio-send-async
		     :type type
		     :new-connection (lsp--make-stdio-connection
				      (plist-get args (or :name
							  (format
							   "%s language server"
							   major-mode)))
				      (plist-get args :command))
		     :get-root get-root
		     :on-initialize (plist-get args :on-initialize)))
	    (t (error "Invalid TYPE for LSP client"))))
    (puthash major-mode client lsp--defined-clients)))

(defsubst lsp--make-stdio-connection (name command)
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
  (lsp-define-client 'rust-mode "rust" 'stdio #'lsp--rust-get-root
		     :command "rls"
		     :name "Rust Language Server")

  (lsp-define-client 'go-mode "go" 'stdio #'default-directory
		     :command '("langserver-go" "-mode=stdio")
		     :name "Go Language Server"))

(defconst lsp--sync-type
  `((0 . "None")
    (1 . "Full Document")
    (2 . "Incremental Changes")))

(defsubst lsp--bool-to-str (b)
  (if b
      "Yes"
    "No"))

(defun lsp-show-capabilities ()
  "View all capabilities for the language server associated with this buffer."
  (interactive)
  (unless lsp--cur-workspace
    (user-error "No language server is associated with this buffer"))
  (let ((capabilities (lsp--server-capabilities)))
    (with-current-buffer (get-buffer-create
			  "lsp-capabilities")
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Document sync method: %s\n" (alist-get
						    (lsp--capability
						     "textDocumentSync"
						     capabilities)
						    lsp--sync-type)))
      (insert (format "Completion support: %s\n" (lsp--capability
						  "completionProvider"
						  capabilities)))
      (insert (format "Help on hover support: %s\n" (lsp--bool-to-str
						     (lsp--capability
						      "hoverProvider"
						      capabilities))))
      (insert (format "Goto-definition support: %s\n" (lsp--bool-to-str
						       (lsp--capability
							"definitionProvider"
							capabilities))))
      (insert (format "Support for finding references: %s\n" (lsp--bool-to-str
							      (lsp--capability
							       "referencesProvider"
							       capabilities))))
      (read-only-mode t))
    (split-window-right)
    (switch-to-buffer "lsp-capabilities")))


(provide 'lsp-mode)
