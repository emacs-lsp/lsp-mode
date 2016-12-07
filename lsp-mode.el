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
  (new-connection :read-only t)
  (get-root :read-only t))
(defvar lsp--defined-clients (make-hash-table))

(defun lsp-define-client (major-mode type language-id &rest args)
  (let ((client))
    (case type
      ('stdout (setq client (make-lsp--client
			     :language-id language-id
			     :send-sync 'lsp--stdin-send-sync
			     :send-async 'lsp--stdin-send-async
			     :new-connection (lsp--make-stdout-connection
					      (plist-get args :name)
					      (plist-get args :command))
			     :get-root (or (plist-get args :get-root)
					   #'projectile-project-root)))))
    (puthash major-mode client lsp--defined-clients)))

(defun lsp--make-stdout-connection (name command)
  (lambda ()
    (make-process
     :name name
     :connection-type 'pty
     :command (list command)
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
  (lsp-define-client 'rust-mode 'stdout "rust" :command "rls" :name "rls"
		     :get-root #'lsp--rust-get-root))

(provide 'lsp-mode)
