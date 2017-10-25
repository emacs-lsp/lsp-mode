;;; lsp-mode.el --- Minor mode for interacting with Language Servers -*- lexical-binding: t -*-

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

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; URL: https://github.com/emacs-lsp/lsp-mode
;; Package-Requires: ((emacs "25.1") (flycheck "30"))
;; Version: 3.0

;;; Commentary:

;;; Code:

(require 'lsp-methods)
(require 'lsp-receive)
(require 'lsp-send)
(require 'cl-lib)
(require 'network-stream)

(defun lsp--make-stdio-connection (name command command-fn)
  (lambda (filter sentinel)
    (let* ((command (if command-fn (funcall command-fn) command))
            (final-command (if (consp command) command (list command))))
      (unless (executable-find (nth 0 final-command))
        (error (format "Couldn't find executable %s" (nth 0 final-command))))
      (make-process
        :name name
        :connection-type 'pipe
        :coding 'no-conversion
        :command final-command
        :filter filter
        :sentinel sentinel
        :stderr (generate-new-buffer-name (concat "*" name " stderr*"))))))

(defun lsp--make-tcp-connection (name command command-fn host port)
  (lambda (filter sentinel)
    (let* ((command (if command-fn (funcall command-fn) command))
            (final-command (if (consp command) command (list command)))
            proc tcp-proc)
      (unless (executable-find (nth 0 final-command))
        (error (format "Couldn't find executable %s" (nth 0 final-command))))
      (setq proc (make-process
                   :name name
                   :coding 'no-conversion
                   :command final-command
                   :sentinel sentinel
                   :stderr (generate-new-buffer-name (concat "*" name " stderr*")))
        tcp-proc (open-network-stream (concat name " TCP connection")
                   nil host port
                   :type 'plain))
      (set-process-filter tcp-proc filter)
      (cons proc tcp-proc))))

(defun lsp--verify-regexp-list (l)
  (cl-assert (cl-typep l 'list) nil
    "lsp-define-client: :ignore-regexps is not a list")
  (dolist (e l l)
    (cl-assert (cl-typep e 'string)
      nil
      (format
        "lsp-define-client: :ignore-regexps element %s is not a string"
        e))))

;; --------------------------------------------------------

(defun lsp-define-stdio-client (name language-id get-root command &rest args)
  "Define a LSP client using stdio.
NAME is the symbol to use for the name of the client.
MODE is the major mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server.
COMMAND is the command to run.
Optional arguments:
`:ignore-regexps' is a list of regexps which when matched will be ignored by the output parser.
`:command-fn' is a function that returns the command string/list to be used to launch the language server. If non-nil, COMMAND is ignored.
`:initialize' is a function called when the client is intiailized. It takes a single argument, the newly created client.
"
  (lsp-define-interactive-client name)
  (apply #'lsp-define-stdio-client-noninteractive name language-id get-root command args)
  )

(defmacro lsp-define-interactive-client (name)
  (let ((enable-noninteractive (intern (format "%s-enable-hook" name)))
        (enable-interactive    (intern (format "%s-enable" name)))
        )
    `(defun ,enable-interactive (&optional ask)
       (interactive)
       (,enable-noninteractive t))))

(defmacro lsp-define-stdio-client-noninteractive (name language-id get-root command &rest args)
   "Define a LSP client using stdio.
NAME is the symbol to use for the name of the client.
MODE is the major mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server.
COMMAND is the command to run.
Optional arguments:
`:ignore-regexps' is a list of regexps which when matched will be ignored by the output parser.
`:command-fn' is a function that returns the command string/list to be used to launch the language server. If non-nil, COMMAND is ignored.
`:initialize' is a function called when the client is intiailized. It takes a single argument, the newly created client.
"
  (let ((enable-noninteractive (intern (format "%s-enable-hook" name)))
        (enable-interactive    (intern (format "%s-enable" name)))
        (disable (intern (format "%s-disable" name)))
        )
    `(defun ,enable-noninteractive (&optional ask)
       ,(plist-get args :docstring)
       (interactive)
       (let ((client (make-lsp--client
                      :language-id ,(lsp--assert-type language-id #'stringp)
                      :send-sync #'lsp--stdio-send-sync
                      :send-async #'lsp--stdio-send-async
                      :new-connection (lsp--make-stdio-connection ,(symbol-name name) ,command
                                                                  ,(plist-get args :command-fn))
                      :get-root ,get-root
                      :ignore-regexps ,(plist-get args :ignore-regexps))))
         (unless lsp-mode
           ,(when (plist-get args :initialize)
              `(funcall ,(plist-get args :initialize) client))
           (let ((root (funcall (lsp--client-get-root client))))
             (if (lsp--should-start-p root)
                 (progn
                   (lsp-mode 1)
                   (lsp--start client))
               (if ask
                   (let ((question (format-message "Add %s to lsp-project-whitelist? " root)))
                     (if (y-or-n-p question) ;; cannot use when , side effects problem
                         (progn (message "About to customize lsp-project-whitelist")
                                (customize-save-variable 'lsp-project-whitelist (add-to-list 'lsp-project-whitelist root)))
                       t)
                     ;; start regardless, this is called interactively
                     (lsp-mode 1)
                     (lsp--start client))
                 (message "Not initializing project %s" root))))))))
  )

(defmacro lsp-define-tcp-client (name language-id get-root command host port &rest args)
  "Define a LSP client using TCP.
NAME is the symbol to use for the name of the client.
MODE is the major mode for which this client will be invoked.
LANGUAGE-ID is the language id to be used when communication with the Language Server.
COMMAND is the command to run.
HOST is the host address.
PORT is the port number.
Optional arguments:
`:ignore-regexps' is a list of regexps which when matched will be ignored by the output parser.
`:command-fn' is a function that returns the command string/list to be used to launch the language server. If non-nil, COMMAND is ignored.
`:initialize' is a function called when the client is intiailized. It takes a single argument, the newly created client."
  (let ((enable (intern (format "%s-enable" name)))
         (disable (intern (format "%s-disable" name))))
    `(defun ,enable ()
       ,(plist-get args :docstring)
       (interactive)
       (let ((client (make-lsp--client
                       :language-id ,(lsp--assert-type language-id #'stringp)
                       :send-sync #'lsp--stdio-send-sync
                       :send-async #'lsp--stdio-send-async
                       :new-connection (lsp--make-tcp-connection ,(symbol-name name) ,command ,(plist-get args :command-fn) ,host ,port)
                       :get-root ,get-root
                       :ignore-regexps ,(plist-get args :ignore-regexps))))
         (unless lsp-mode
           ,(when (plist-get args :initialize)
              `(funcall ,(plist-get args :initialize) client))
           (if (lsp--should-start-p (funcall (lsp--client-get-root client)))
             (progn
               (lsp-mode 1)
               (lsp--start client))
             (message "Not initializing project %s" root)))))))

;;;###autoload
(define-minor-mode lsp-mode ""
  nil nil nil
  :lighter " LSP"
  :group 'lsp-mode)

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
     (("documentHighlightProvider" . ("The server provides document highlight support." . boolean)))
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
                     ((listp type) (concat ": " (cdr (assoc value type))))) "\n"))))

(defun lsp-capabilities ()
  "View all capabilities for the language server associated with this buffer."
  (interactive)
  (unless lsp--cur-workspace
    (user-error "No language server is associated with this buffer"))
  (let ((str (mapconcat #'lsp--cap-str (reverse (hash-table-keys
                                                  (lsp--server-capabilities))) ""))
         (buffer-name (generate-new-buffer-name "lsp-capabilities"))
         )
    (get-buffer-create buffer-name)
    (with-current-buffer buffer-name
      (view-mode -1)
      (erase-buffer)
      (insert str)
      (view-mode 1))
    (switch-to-buffer buffer-name)))

(provide 'lsp-mode)
;;; lsp-mode.el ends here
