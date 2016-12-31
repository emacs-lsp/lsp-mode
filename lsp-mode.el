;;; -*- lexical-binding: t -*-

;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'lsp-methods)
(require 'lsp-receive)
(require 'lsp-send)
(require 'cl-lib)

(defconst lsp-version "2.0"
  "Version of the Language Server Procotol implemented by this package.")

(defsubst lsp--make-stdio-connection (name command)
  (lambda ()
    (let ((final-command (if (consp command) command (list command))))
      (unless (executable-find (nth 0 final-command))
	(error (format "Couldn't find executable %s" (nth 0 final-command))))
      (make-process
       :name name
       :connection-type 'pipe
       :command final-command
       :filter #'lsp--process-filter))))

(defun lsp-define-client (mode language-id type get-root &rest args)
  "Define a LSP client.
MODE is the major mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server.
Optional arguments:
`:name' is the process name for the language server.
`:command' is the command to run if `TYPE' is 'stdio.
`:on-initialize' is the function to call when a new project/workspace is initialized."
  (let ((client))
    (setq client
	  (cl-case type
	    ('stdio (make-lsp--client
		     :language-id (lsp--assert-type language-id #'stringp)
		     :send-sync 'lsp--stdio-send-sync
		     :send-async 'lsp--stdio-send-async
		     :type (lsp--assert-type type #'integerp)
		     :new-connection (lsp--make-stdio-connection
				      (plist-get args (or :name
							  (format
							   "%s language server"
							   mode)))
				      (plist-get args :command))
		     :get-root (lsp--assert-type get-root #'functionp)
		     :on-initialize (plist-get args :on-initialize)))
	    (t (error "Invalid TYPE for LSP client"))))
    (puthash mode client lsp--defined-clients)))

(defun lsp--rust-get-root ()
  (let ((dir default-directory))
    (while (not (or (file-exists-p (concat (file-name-as-directory dir)
					   "Cargo.toml"))
		    (string= dir "/")))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if (string= dir "/")
	(user-error "Couldn't find Rust project")
      dir)))

(defun lsp--rust-rls-command ()
  (let ((rls-root (getenv "RLS_ROOT")))
    (if rls-root
	`("cargo" "run" "--quiet" ,(concat
				    "--manifest-path="
				    (concat
				     (file-name-as-directory
				      (expand-file-name rls-root))
				     "Cargo.toml"))
	  "--release")
      "rls")))

(defun lsp--haskell-get-root ()
  (let ((dir default-directory))
    (if (string= dir "/")
        (user-error "Couldn't find Haskell XXX")
      dir)))

;;;###autoload
(define-minor-mode global-lsp-mode ""
  nil nil nil
  :global t

  (add-hook 'find-file-hook #'lsp-on-open)
  (add-hook 'after-save-hook #'lsp-on-save)
  (add-hook 'kill-buffer-hook #'lsp-on-close)
  (lsp-define-client 'rust-mode "rust" 'stdio #'lsp--rust-get-root
		     :command (lsp--rust-rls-command)
		     :name "Rust Language Server")

  (lsp-define-client 'go-mode "go" 'stdio #'(lambda () default-directory)
		     :command '("langserver-go" "-mode=stdio")
		     :name "Go Language Server")

  (lsp-define-client 'haskell-mode "haskell" 'stdio #'lsp--haskell-get-root
                   ;; :command '("hie" "--lsp" "-d" "-l" (make-temp-file "hie" nil ".log"))
                   :command '("hie" "--lsp" "-d" "-l" "/tmp/hie.log")
                   :name "Haskell Language Server"))

(defconst lsp--sync-type
  `((0 . "None")
    (1 . "Full Document")
    (2 . "Incremental Changes")))

(defconst lsp--capabilities
  `(("textDocumentSync" . ("Document sync method" .
			   ((1 . "None")
			    (2 . "Send full contents")
			    (3 . "Send incremental changes."))))
    ("hoverProvider" . ("The server provides hover support" . boolean))
    ("completionProvider" . ("The server provides completion support" . boolean))
    ("definitionProvider" . ("The server provides goto definition support" . boolean))
    ("referencesProvider" . ("The server provides references support" . boolean))
    ("documentSymbolProvider" . ("The server provides file symbol support" . boolean))
    ("workspaceSymbolProvider" . ("The server provides project symbol support" . boolean))
    ("codeActionProvider" . ("The server provides code actions" . boolean))
    ("codeLensProvider" . ("The server provides code lens" . boolean))
    ("documentFormattingProvider" . ("The server provides file formatting" . boolean))
    (("documentRangeFormattingProvider" . ("The server provides region formatting" . boolean)))
    (("renameProvider" . ("The server provides rename support" . boolean)))))

(defun lsp--cap-str (cap)
  (let* ((elem (assoc cap lsp--capabilities))
	 (desc (cadr elem))
	 (type (cddr elem))
	 (value (gethash cap (lsp--server-capabilities))))
    (when (and elem desc type value)
      (concat desc (cond
		    ((listp type) (concat ": " (cadr (assoc value type))))) "\n"))))

(defun lsp-capabilities ()
  "View all capabilities for the language server associated with this buffer."
  (interactive)
  (unless lsp--cur-workspace
    (user-error "No language server is associated with this buffer"))
  (let ((str (mapconcat #'lsp--cap-str (reverse (hash-table-keys
						 (lsp--server-capabilities))) "")))
    (with-current-buffer (get-buffer-create
			  "lsp-capabilities")
      (view-mode -1)
      (erase-buffer)
      (insert str)
      (view-mode 1))
    (switch-to-buffer "lsp-capabilities")))

(provide 'lsp-mode)
