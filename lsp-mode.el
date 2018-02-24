;;; lsp-mode.el --- Minor mode for interacting with Language Servers -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Vibhav Pant <vibhavp@gmail.com>

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
;; Version: 3.4

;;; Commentary:

;;; Code:

(require 'lsp-methods)
(require 'lsp-io)
(require 'cl-lib)
(require 'network-stream)

(defvar lsp-version-support "3.0"
  "This is the version of the Language Server Protocol currently supported by ‘lsp-mode’.")

(defun lsp--make-stdio-connection (name command command-fn stderr)
  (lambda (filter sentinel)
    (let* ((command (if command-fn (funcall command-fn) command))
           (final-command (if (consp command) command (list command))))
      (unless (executable-find (nth 0 final-command))
        (error (format "Couldn't find executable %s" (nth 0 final-command))))
      (let ((proc (make-process
                    :name name
                    :connection-type 'pipe
                    :coding 'no-conversion
                    :command final-command
                    :filter filter
                    :sentinel sentinel
                    :stderr stderr
                    :noquery t)))
        ;; TODO: This is redundant with :noquery above, but due to a
        ;; bug pre-Emacs 26 it is still needed
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30031
        (set-process-query-on-exit-flag (get-buffer-process (get-buffer stderr)) nil)
        proc))))

(defun lsp--make-tcp-connection (name command command-fn host port stderr)
  (lambda (filter sentinel)
    (let* ((command (if command-fn (funcall command-fn) command))
           (final-command (if (consp command) command (list command)))
           proc tcp-proc)
      (unless (executable-find (nth 0 final-command))
        (error (format "Couldn't find executable %s" (nth 0 final-command))))
      (setq proc (make-process
                  :name name
                  :connection-type 'pipe
                  :coding 'no-conversion
                  :command final-command
                  :sentinel sentinel
                  :stderr stderr
                  :noquery t)
            tcp-proc (open-network-stream (concat name " TCP connection")
                                          nil host port
                                          :type 'plain))
      ;; TODO: Same :noquery issue (see above)
      (set-process-query-on-exit-flag (get-buffer-process (get-buffer stderr)) nil)
      (set-process-query-on-exit-flag tcp-proc nil)
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

(cl-defmacro lsp-define-whitelist-enable (name get-root
                                               &key docstring)
  "Define a function to add the project root for the current buffer to the whitleist.
NAME is the base name for the command.
GET-ROOT is the language-specific function to determine the project root for the current buffer."
  (let ((whitelist-add      (intern (format "%s-whitelist-add" name)))
        (enable-interactive (intern (format "%s-enable" name))))
    `(defun ,whitelist-add ()
       ,docstring
       (interactive)
       (let ((root (funcall ,get-root)))
         (customize-save-variable 'lsp-project-whitelist
           (add-to-list 'lsp-project-whitelist (concat "^" (regexp-quote root) "$")))
         (,enable-interactive)
         ))))

(cl-defmacro lsp-define-whitelist-disable (name get-root
                                            &key docstring)
  "Define a function to remove the project root for the current buffer from the whitleist.
NAME is the base name for the command.
GET-ROOT is the language-specific function to determine the project root for the current buffer."
  (let ((whitelist-remove (intern (format "%s-whitelist-remove" name))))
    `(defun ,whitelist-remove ()
       ,docstring
       (interactive)
       (let ((root (funcall ,get-root)))
         (customize-save-variable 'lsp-project-whitelist
           (remove (concat "^" (regexp-quote root) "$") 'lsp-project-whitelist))))))

(cl-defmacro lsp-define-stdio-client (name language-id get-root command
                                       &key docstring
                                       language-id-fn
                                       command-fn
                                       ignore-regexps
                                       ignore-messages
                                       extra-init-params
                                       initialize)
  "Define a LSP client using stdio.
NAME is the symbol to use for the name of the client.
LANGUAGE-ID is the language id to be used when communication with
the Language Server.  COMMAND is the command to run.

Optional arguments:
`:docstring' is an optional docstring used for the entrypoint function created by
`lsp-define-stdio-client'.

`:ignore-regexps' is a list of regexps.  When a data packet from the LSP server
 matches any of these regexps, it will be ignored.  This is intended for dealing
 with LSP servers that output non-protocol data.

`:ignore-messages' is a list of regexps.  When a message from the LSP server
 matches any of these regexps, it will be ignored.  This is useful for filtering
 out unwanted messages; such as servers that send nonstandard message types, or
 extraneous `logMessage's.

`:command-fn' is a function that returns the command string/list to be used to
 launch the language server. If non-nil, COMMAND is ignored.

`:language-id-fn' is a function that returns the language-id string to be used
 while opening a new file. If non-nil, LANGUAGE-ID is ignored.

`:extra-init-params' is a plist that specifies any (optional)
 initializeOptions parameters required by the LSP server. A function taking
 a single argument (LSP workspace) and returning a plist is also accepted.

`:initialize' is a function called when the client is intiailized. It takes a
 single argument, the newly created client."
  (cl-check-type name symbol)
  (macroexp-let2 (lambda (_) t) get-root get-root
    `(progn
       (lsp-define-whitelist-enable ,name ,get-root)
       (lsp-define-whitelist-disable ,name ,get-root)
       (defun ,(intern (format "%s-enable" name)) ()
         ,docstring
         (interactive)
         (lsp--enable-stdio-client ',name
                                   :language-id ,language-id
                                   :language-id-fn ,language-id-fn
                                   :root-directory-fn ,get-root
                                   :command ,command
                                   :command-fn ,command-fn
                                   :ignore-regexps ,ignore-regexps
                                   :ignore-messages ,ignore-messages
                                   :extra-init-params ,extra-init-params
                                   :initialize-fn ,initialize)))))



(cl-defun lsp--enable-stdio-client (name &key language-id language-id-fn
                                         root-directory-fn command command-fn
                                         ignore-regexps ignore-messages
                                         extra-init-params initialize-fn)
  (cl-check-type name symbol)
  (cl-check-type language-id (or null string))
  (cl-check-type language-id-fn (or null function))
  (cl-check-type root-directory-fn (or null function))
  (cl-check-type command list)
  (cl-check-type command-fn (or null function))
  (cl-check-type ignore-regexps list)
  (cl-check-type ignore-messages list)
  (cl-check-type extra-init-params (or list function))
  (cl-check-type initialize-fn (or null function))
  (when (and (not lsp-mode) (buffer-file-name))
    (let* ((stderr (generate-new-buffer-name
                    (concat "*" (symbol-name name) " stderr*")))
           (client (make-lsp--client
                    :language-id (or language-id-fn (lambda (_) language-id))
                    :new-connection (lsp--make-stdio-connection
                                     (symbol-name name)
                                     command
                                     command-fn
                                     stderr)
                    :stderr stderr
                    :get-root root-directory-fn
                    :ignore-regexps ignore-regexps
                    :ignore-messages ignore-messages)))
      (when initialize-fn
        (funcall initialize-fn client))
      (let ((root (funcall (lsp--client-get-root client))))
        (if (lsp--should-start-p root)
            (progn
              (lsp-mode 1)
              (lsp--start client extra-init-params))
          (message "Not initializing project %s" root))))))

(cl-defmacro lsp-define-tcp-client (name language-id get-root command host port
                                     &key docstring
                                     language-id-fn
                                     command-fn
                                     ignore-regexps
                                     ignore-messages
                                     extra-init-params
                                     initialize)
  "Define a LSP client using TCP.
NAME is the symbol to use for the name of the client.
LANGUAGE-ID is the language id to be used when communication with
the Language Server.  COMMAND is the command to run.  HOST is the
host address.  PORT is the port number.

Optional arguments:
`:ignore-regexps' is a list of regexps.  When a data packet from the LSP server
 matches any of these regexps, it will be ignored.  This is intended for dealing
 with LSP servers that output non-protocol data.

`:ignore-messages' is a list of regexps.  When a message from the LSP server
 matches any of these regexps, it will be ignored.  This is useful for filtering
 out unwanted messages; such as servers that send nonstandard message types, or
 extraneous `logMessage's.

`:command-fn' is a function that returns the command string/list to be used to
 launch the language server. If non-nil, COMMAND is ignored.

`:language-id-fn' is a function that returns the language-id string to be used
 while opening a new file. If non-nil, LANGUAGE-ID is ignored.

`:extra-init-params' is a plist that specifies any (optional)
 initializeOptions parameters required by the LSP server. A function taking
 a single argument (LSP workspace) and returning a plist is also accepted.

`:initialize' is a function called when the client is initialized. It takes a
  single argument, the newly created client."
  (cl-check-type name symbol)
  (macroexp-let2 nil get-root get-root
    `(progn
       (lsp-define-whitelist-enable ,name ,get-root)
       (lsp-define-whitelist-disable ,name ,get-root)
       (defun ,(intern (format "%s-enable" name)) ()
         ,docstring
         (interactive)
         (lsp--enable-tcp-client ',name
                                 :language-id ,language-id
                                 :language-id-fn ,language-id-fn
                                 :root-directory-fn ,get-root
                                 :command ,command
                                 :command-fn ,command-fn
                                 :host ,host
                                 :port ,port
                                 :ignore-regexps ,ignore-regexps
                                 :ignore-messages ,ignore-messages
                                 :extra-init-params ,extra-init-params
                                 :initialize-fn ,initialize)))))

(cl-defun lsp--enable-tcp-client (name &key language-id language-id-fn
                                       root-directory-fn command command-fn
                                       host port
                                       ignore-regexps ignore-messages
                                       extra-init-params initialize-fn)
  (cl-check-type name symbol)
  (cl-check-type language-id (or null string))
  (cl-check-type language-id-fn (or null function))
  (cl-check-type root-directory-fn (or null function))
  (cl-check-type command list)
  (cl-check-type command-fn (or null function))
  (cl-check-type host string)
  (cl-check-type port (integer 1 #xFFFF))
  (cl-check-type ignore-regexps list)
  (cl-check-type ignore-messages list)
  (cl-check-type extra-init-params (or list function))
  (cl-check-type initialize-fn (or null function))
  (when (and (not lsp-mode) (buffer-file-name))
    (let* ((stderr (generate-new-buffer-name
                    (concat "*" (symbol-name name) " stderr*")))
           (client (make-lsp--client
                    :language-id (or language-id-fn (lambda (_) language-id))
                    :new-connection (lsp--make-tcp-connection
                                     (symbol-name name)
                                     command
                                     command-fn
                                     host port
                                     stderr)
                    :stderr stderr
                    :get-root root-directory-fn
                    :ignore-regexps ignore-regexps
                    :ignore-messages ignore-messages)))
      (when initialize-fn
        (funcall initialize-fn client))
      (let ((root (funcall (lsp--client-get-root client))))
        (if (lsp--should-start-p root)
            (progn
              (lsp-mode 1)
              (lsp--start client extra-init-params))
          (message "Not initializing project %s" root))))))

(defvar-local lsp-status nil
  "The current status of the LSP server.")

(defun lsp-mode-line ()
  "Construct the mode line text."
  (concat " LSP" lsp-status))

;;;###autoload
(define-minor-mode lsp-mode ""
  nil nil nil
  :lighter (:eval (lsp-mode-line))
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
