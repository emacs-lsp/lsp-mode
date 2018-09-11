;; Copyright (C) 2016-2018  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'json)
(require 'xref)
(require 'subr-x)
(require 'widget)
(require 'lsp-io)
(require 'lsp-common)
(require 'pcase)
(require 'inline)
(require 'em-glob)

(defconst lsp--file-change-type
  `((created . 1)
    (changed . 2)
    (deleted . 3)))

;; A ‘lsp--client’ object describes the client-side behavior of a language
;; server.  It is used to start individual server processes, each of which is
;; represented by a ‘lsp--workspace’ object.  Client objects are normally
;; created using ‘lsp-define-stdio-client’ or ‘lsp-define-tcp-client’.  Each
;; workspace refers to exactly one client, but there can be multiple workspaces
;; for a single client.
(cl-defstruct lsp--client
  ;; ‘language-id’ is a function that receives a buffer as a single argument
  ;; and should return the language identifier for that buffer.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#textdocumentitem
  ;; for a list of language identifiers.  Also consult the documentation for
  ;; the language server represented by this client to find out what language
  ;; identifiers it supports or expects.
  (language-id nil :read-only t)

  ;; send-async and send-sync are unused field, but haven't been
  ;; removed so as to avoid breaking byte-compiled clients.
  ;; FIXME: We shouldn’t need to take binary compatibility into account,
  ;; especially since the ‘lsp--client’ structure is internal.  These fields
  ;; should just be removed.
  (send-sync nil :read-only t)
  (send-async nil :read-only t)

  ;; FIXME: This field is apparently unused and should be removed.
  (type nil :read-only t)

  ;; ‘new-connection’ is a function that should start a language server process
  ;; and return a cons (COMMAND-PROCESS . COMMUNICATION-PROCESS).
  ;; COMMAND-PROCESS must be a process object representing the server process
  ;; just started.  COMMUNICATION-PROCESS must be a process (including pipe and
  ;; network processes) that ‘lsp-mode’ uses to communicate with the language
  ;; server using the language server protocol.  COMMAND-PROCESS and
  ;; COMMUNICATION-PROCESS may be the same process; in that case
  ;; ‘new-connection’ may also return that process as a single
  ;; object. ‘new-connection’ is called with two arguments, FILTER and
  ;; SENTINEL.  FILTER should be used as process filter for
  ;; COMMUNICATION-PROCESS, and SENTINEL should be used as process sentinel for
  ;; COMMAND-PROCESS.
  (new-connection nil :read-only t)

  ;; ‘stderr’ is the name of a buffer to write the standard error to.
  ;; FIXME: ‘stderr’ should be the actual buffer, and it should be a field of
  ;; the ‘lsp--workspace’.
  (stderr nil :read-only t)

  ;; ‘get-root’ is a function that should return the workspace root directory
  ;; for the current buffer.  It may return either a directory name or a
  ;; directory file name.  The ‘get-root’ function is called without arguments.
  ;; ‘lsp-mode’ will start one server process per client and root directory.
  ;; It passes the root directory to the ‘initialize’ method of the language
  ;; server; see
  ;; https://microsoft.github.io/language-server-protocol/specification#initialize.
  ;; Also consult the documentation of your language server for information
  ;; about what it expects as workspace root.
  (get-root nil :read-only t)

  ;; ‘ignore-regexps’ is a list of regexps.  When a data packet from the
  ;; language server matches any of these regexps, it will be ignored.  This is
  ;; intended for dealing with language servers that output non-protocol data.
  (ignore-regexps nil :read-only t)

  ;; ‘ignore-messages’ is a list of regexps.  When a message from the language
  ;; server matches any of these regexps, it will be ignored.  This is useful
  ;; for filtering out unwanted messages; such as servers that send nonstandard
  ;; message types, or extraneous log messages.
  (ignore-messages nil :read-only t)

  ;; ‘notification-handlers’ is a hash table mapping notification method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a notification, ‘lsp-mode’ will call the associated handler
  ;; function passing two arguments, the ‘lsp--workspace’ object and the
  ;; deserialized notification parameters.
  (notification-handlers (make-hash-table :test 'equal) :read-only t)

  ;; ‘request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a request, ‘lsp-mode’ will call the associated handler function
  ;; passing two arguments, the ‘lsp--workspace’ object and the deserialized
  ;; request parameters.
  (request-handlers (make-hash-table :test 'equal) :read-only t)

  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t)

  ;; ‘string-renderers’ is an alist mapping MarkedString language identifiers
  ;; (see
  ;; https://microsoft.github.io/language-server-protocol/specification#textDocument_hover)
  ;; to functions that can render the respective languages.  The rendering
  ;; functions are called with a single argument, the MarkedString value.  They
  ;; should return a propertized string with the rendered output.
  (string-renderers '())
  ;; ‘last-id’ is the last JSON-RPC identifier used.
  ;; FIXME: ‘last-id’ should be in ‘lsp--workspace’.
  (last-id 0)

  ;; Function to enable the client for the current buffer, called without
  ;; arguments.
  (enable-function nil :read-only t)

  ;; ‘prefix-function’ is called for getting the prefix for completion.
  ;; The function takes no parameter and returns a cons (start . end) representing
  ;; the start and end bounds of the prefix. If it's not set, the client uses a
  ;; default prefix function."
  (prefix-function nil :read-only t)

  ;; Contains mapping of scheme to the function that is going to be used to load
  ;; the file.
  (uri-handlers (make-hash-table :test #'equal) :read-only t)
  ;; ‘action-handlers’ is a hash table mapping action to a handler function. It
  ;; can be used in `lsp-execute-code-action' to determine whether the action
  ;; current client is interested in executing the action instead of sending it
  ;; to the server.
  (action-handlers (make-hash-table :test 'equal) :read-only t)

  ;; ‘default-renderer’ is the renderer that is going to be used when there is
  ;; no concrete "language" specified for the current MarkedString. (see
  ;; https://microsoft.github.io/language-server-protocol/specification#textDocument_hover)
  (default-renderer nil))

(cl-defstruct lsp--registered-capability
  (id "" :type string)
  (method " " :type string)
  (options nil))

;; A ‘lsp--workspace’ object represents exactly one language server process.
(cl-defstruct lsp--workspace
  ;; ‘parser’ is a ‘lsp--parser’ object used to parse messages for this
  ;; workspace.  Parsers are not shared between workspaces.
  (parser nil :read-only t)

  ;; ‘file-versions’ is a hashtable of files "owned" by the workspace.  It maps
  ;; file names to file versions.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#versionedtextdocumentidentifier.
  (file-versions nil :read-only t)

  ;; ‘server-capabilities’ is a hash table of the language server capabilities.
  ;; It is the hash table representation of a LSP ServerCapabilities structure;
  ;; cf. https://microsoft.github.io/language-server-protocol/specification#initialize.
  (server-capabilities nil)

  ;; ‘registered-server-capabilities’ is a list of hash tables that represent
  ;; dynamically-registered Registration objects.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#client_registerCapability.
  (registered-server-capabilities nil)

  ;; ‘root’ is a directory name or a directory file name for the workspace
  ;; root.  ‘lsp-mode’ passes this directory to the ‘initialize’ method of the
  ;; language server; see
  ;; https://microsoft.github.io/language-server-protocol/specification#initialize.
  (root nil :ready-only t)

  ;; ‘client’ is the ‘lsp--client’ object associated with this workspace.
  (client nil :read-only t)

  ;; FIXME: ‘change-timer-disabled’ is unused and should be removed.
  (change-timer-disabled nil)

  ;; ‘proc’ is a process object; it may represent a regular process, a pipe, or
  ;; a network connection.  ‘lsp-mode’ communicates with ‘proc’ using the
  ;; language server protocol.  ‘proc’ corresponds to the COMMUNICATION-PROCESS
  ;; element of the return value of the client’s ‘get-root’ field, which see.
  (proc nil)

  ;; ‘proc’ is a process object; it must represent a regular process, not a
  ;; pipe or network process.  It represents the actual server process that
  ;; corresponds to this workspace.  ‘cmd-proc’ corresponds to the
  ;; COMMAND-PROCESS element of the return value of the client’s ‘get-root’
  ;; field, which see.
  (cmd-proc nil)

  ;; ‘buffers’ is a list of buffers associated with this workspace.
  (buffers nil)

  ;; ‘highlight-overlays’ is a hash table mapping buffers to a list of overlays
  ;; used for highlighting the symbol under point.
  (highlight-overlays (make-hash-table :test 'eq) :read-only t)

  ;; Extra client capabilities provided by third-party packages using
  ;; `lsp-register-client-capabilities'. It's value is an alist of (PACKAGE-NAME
  ;; . CAPS), where PACKAGE-NAME is a symbol of the third-party package name,
  ;; and CAPS is either a plist of the client capabilities, or a function that
  ;; takes no argument and returns a plist of the client capabilities or nil.")
  (extra-client-capabilities nil)

  ;; Workspace status
  (status nil)

  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal))

  ;; contains all the file notification watches that have been created for the
  ;; current workspace in format filePath->file notification handle.
  (watches (make-hash-table :test 'equal)))

(defvar lsp--workspaces (make-hash-table :test #'equal)
  "Table of known workspaces, indexed by the project root directory.")

(defvar lsp--ignored-workspace-roots (make-hash-table :test #'equal)
  "Table of project roots which should not have a workspace,
indexed by the project root directory.

This is populated when the user declines to open a workspace
for a file in the workspace.")

(defcustom lsp-render-markdown-markup-content nil
  "Function to be use for rendering MarkupContent.

It should take two arguments - a string denoting the type of markup content
and a string containing the text to be rendered.  The returned value should
be a string that may be fontified/propertized.

When nil, MarkupContent is rendered as plain text."
  :type 'function
  :group 'lsp-mode)

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-open-hook nil
  "List of functions to be called before a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-open-hook nil
  "List of functions to be called after a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--sync-methods
  '((0 . none)
    (1 . full)
    (2 . incremental)))
(defvar-local lsp--server-sync-method nil
  "Sync method recommended by the server.")

;;;###autoload
(defgroup lsp-mode nil
  "Customization group for ‘lsp-mode’."
  :group 'tools)

;;;###autoload
(defgroup lsp-faces nil
  "Faces for ‘lsp-mode’."
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." 'none)
                 (const :tag "Documents are synced by always sending the full content of the document." 'full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." 'incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-project-blacklist nil
  "A list of project directory regexps for which LSP shouldn't be initialized.
LSP should be initialized if the given project root matches one pattern in the
whitelist, or does not match any pattern in the blacklist."
  :type '(repeat regexp)
  :group 'lsp-mode)

(defcustom lsp-project-whitelist nil
  "A list of project directory regexps for which LSP should be initialized."
  :type '(repeat regexp)
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-eldoc t
  "Enable `eldoc-mode' integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-eldoc-render-all t
  "Define whether all of the returned by document/onHover will be displayed.

If `lsp-markup-display-all' is set to nil `eldoc' will show only
the symbol information."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-highlight-symbol-at-point t
  "Highlight the symbol under the point."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-codeaction t
  "Enable code action processing."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-completion-at-point t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the language server."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-before-save-edits t
  "If non-nil, `lsp-mode' will apply edits suggested by the language server
before saving a document."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-hover-text-function 'lsp--text-document-hover-string
  "The LSP method to use to display text on hover."
  :type '(choice (function :tag "textDocument/hover"
                           lsp--text-document-hover-string)
                 (function :tag "textDocument/signatureHelp"
                           lsp--text-document-signature-help))
  :group 'lsp-mode)

;;;###autoload
(defface lsp-face-highlight-textual
  '((((background dark))  :background "saddle brown")
    (((background light)) :background "yellow"))
  "Face used for textual occurances of symbols."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-highlight-read
  '((((background dark))  :background "firebrick")
    (((background light)) :background "red"))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-highlight-write
  '((((background dark))  :background "sea green")
     (((background light)) :background "green"))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(defun lsp-client-register-uri-handler (client scheme handler)
  (cl-check-type client lsp--client)
  (cl-check-type scheme string)
  (cl-check-type handler function)
  (puthash scheme handler (lsp--client-uri-handlers client)))

(defun lsp-client-on-notification (client method callback)
  (cl-check-type client lsp--client)
  (cl-check-type method string)
  (cl-check-type callback function)
  (puthash method callback (lsp--client-notification-handlers client)))

(defun lsp-client-on-request (client method callback)
  (cl-check-type client lsp--client)
  (cl-check-type method string)
  (cl-check-type callback function)
  (puthash method callback (lsp--client-request-handlers client)))

(defun lsp-client-on-action (client method callback)
  (cl-check-type client lsp--client)
  (cl-check-type method string)
  (cl-check-type callback function)
  (puthash method callback (lsp--client-action-handlers client)))

(defun lsp-workspace-set-metadata (key value &optional workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (lsp--workspace-metadata (or workspace lsp--cur-workspace))))

(defun lsp-workspace-get-metadata (key &optional workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (lsp--workspace-metadata (or workspace lsp--cur-workspace))))

(define-inline lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (inline-quote
    (plist-put (lsp--make-notification ,method ,params)
      :id (cl-incf (lsp--client-last-id (lsp--workspace-client lsp--cur-workspace))))))

(defun lsp-make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (lsp--make-request method params))

(defun lsp--make-response-error (code message data)
  (cl-check-type code number)
  (cl-check-type message string)
  `(:code ,code :message ,message :data ,data))

(defun lsp--make-response (id result error)
  (cl-check-type error list)
  `(:jsonrpc "2.0" :id ,id :result ,result :error ,error))

(define-inline lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (inline-quote
    (progn (cl-check-type ,method string)
      (list :jsonrpc "2.0" :method ,method :params ,params))))

;; Define non-inline public aliases to avoid breaking binary compatibility.
(defun lsp-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(define-inline lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (inline-quote
    (let* ((json-encoding-pretty-print lsp-print-io)
           (json-false :json-false)
           (body (json-encode ,params)))
      (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body))))

(define-inline lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (inline-quote
    (lsp--send-no-wait
      (lsp--make-message ,body)
      (lsp--workspace-proc lsp--cur-workspace))))

(defun lsp-send-notification (body)
  "Send BODY as a notification to the language server."
  (lsp--send-notification body))

(define-inline lsp--cur-workspace-check ()
  (inline-quote
    (progn
      (cl-assert lsp--cur-workspace nil
        "No language server is associated with this buffer.")
      (cl-assert (lsp--workspace-p lsp--cur-workspace)))))

(define-inline lsp--cur-parser ()
  (inline-quote (lsp--workspace-parser lsp--cur-workspace)))

(defun lsp--send-request (body &optional no-wait)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response."
  (let* ((parser (lsp--cur-parser))
          (message (lsp--make-message body))
          (process (lsp--workspace-proc lsp--cur-workspace)))
    (setf (lsp--parser-waiting-for-response parser) (not no-wait))
    (if no-wait
      (lsp--send-no-wait message process)
      (lsp--send-wait message process parser))
    (when (not no-wait)
      (prog1 (lsp--parser-response-result parser)
        (setf (lsp--parser-response-result parser) nil)))))

(defalias 'lsp-send-request 'lsp--send-request
  "Send BODY as a request to the language server and return the response synchronously.

\n(fn BODY)")

(defun lsp--send-request-async (body callback)
  "Send BODY as a request to the language server, and call CALLBACK with
the response recevied from the server asynchronously."
  (let ((client (lsp--workspace-client lsp--cur-workspace))
        (id (plist-get body :id)))
    (cl-assert id nil "body missing id field")
    (puthash id callback (lsp--client-response-handlers client))
    (lsp--send-no-wait (lsp--make-message body)
      (lsp--workspace-proc lsp--cur-workspace))
    body))

(defalias 'lsp-send-request-async 'lsp--send-request-async)

(define-inline lsp--inc-cur-file-version ()
  (inline-quote (cl-incf (gethash (current-buffer)
                           (lsp--workspace-file-versions lsp--cur-workspace)))))

(define-inline lsp--cur-file-version ()
  "Return the file version number.  If INC, increment it before."
  (inline-quote
    (gethash (current-buffer) (lsp--workspace-file-versions lsp--cur-workspace))))

(define-inline lsp--make-text-document-item ()
  "Make TextDocumentItem for the currently opened file.

interface TextDocumentItem {
    uri: string; // The text document's URI
    languageId: string; // The text document's language identifier.
    version: number;
    text: string;
}"
  (inline-quote
    (let ((language-id-fn (lsp--client-language-id (lsp--workspace-client lsp--cur-workspace))))
      (list :uri (lsp--buffer-uri)
	      :languageId (funcall language-id-fn (current-buffer))
	      :version (lsp--cur-file-version)
	      :text (buffer-substring-no-properties (point-min) (point-max))))))

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  (with-demoted-errors "Error in ‘lsp--global-teardown’: %S"
    (maphash (lambda (_k value) (lsp--teardown-workspace value)) lsp--workspaces)))

(defun lsp--teardown-workspace (workspace)
  (setq lsp--cur-workspace workspace)
  (lsp--shutdown-cur-workspace))

(defun lsp--shutdown-cur-workspace ()
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (lsp--send-request (lsp--make-request "shutdown" (make-hash-table)) t)
    (lsp--send-notification (lsp--make-notification "exit" nil)))
  (lsp--uninitialize-workspace))

(defun lsp--uninitialize-workspace ()
  "When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (lsp-kill-watch (lsp--workspace-watches lsp--cur-workspace))

  (let (proc
        (root (lsp--workspace-root lsp--cur-workspace)))
    (with-current-buffer (current-buffer)
      (setq proc (lsp--workspace-proc lsp--cur-workspace))
      (if (process-live-p proc)
        (kill-process (lsp--workspace-proc lsp--cur-workspace)))
      (setq lsp--cur-workspace nil)
      (lsp--unset-variables)
      (kill-local-variable 'lsp--cur-workspace))
    (remhash root lsp--workspaces)))

(defun lsp-restart-workspace ()
  "Shut down and then restart the current workspace.
This involves uninitializing each of the buffers associated with
the workspace, closing the process managing communication with
the client, and then starting up again."
  (interactive)
  (when (and (lsp-mode) (buffer-file-name) lsp--cur-workspace)
    (let ((old-buffers (lsp--workspace-buffers lsp--cur-workspace))
           (restart (lsp--client-enable-function (lsp--workspace-client lsp--cur-workspace)))
           (proc (lsp--workspace-proc lsp--cur-workspace)))
      (lsp--remove-cur-overlays)
      ;; Shut down the LSP mode for each buffer in the workspace
      (dolist (buffer old-buffers)
        (with-current-buffer buffer
          (lsp--text-document-did-close)
          (setq lsp--cur-workspace nil)
          (lsp-mode -1)))

      ;; Let the process actually shut down
      (while (process-live-p proc)
        (accept-process-output proc))

      ;; Re-enable LSP mode for each buffer
      (dolist (buffer old-buffers)
        (with-current-buffer buffer
          (funcall restart))))))

;; NOTE: Possibly make this function subject to a setting, if older LSP servers
;; are unhappy
(defun lsp--client-capabilities ()
  "Return the client capabilites."
  (apply #'lsp--merge-plists
    `(:workspace    ,(lsp--client-workspace-capabilities)
       :textDocument ,(lsp--client-textdocument-capabilities))
    (seq-map (lambda (extra-capabilities-cons)
               (let* ((package-name (car extra-capabilities-cons))
                       (value (cdr extra-capabilities-cons))
                       (capabilities (if (functionp value) (funcall value)
                                       value)))
                 (if (and capabilities (not (listp capabilities)))
                   (progn
                     (message "Capabilities provided by %s are not a plist: %s" package-name value)
                     nil)
                   capabilities)))
      (lsp--workspace-extra-client-capabilities lsp--cur-workspace))))

(defun lsp--merge-plists (first &rest rest)
  "Deeply merge plists.

FIRST is the plist to be merged into. The rest of the arguments
can be either plists or nil. The non-nil plists in the rest of
the arguments will be merged into FIRST.

Return the merged plist."
  (cl-check-type first list)
  (seq-each
    (lambda (pl) (setq first (lsp--merge-two-plists first pl)))
    rest)
  first)

(defun lsp--merge-two-plists (first second)
  "Deeply merge two plists.

All values in SECOND are merged into FIRST.  FIRST can be nil or
a plist.  SECOND must be a plist.

Return the merged plist."
  (when second
    (if (not (listp second))
      (warn "Cannot merge non-list value into a plist. The value is %s" second)
      (cl-loop for (key second-value) on second
        collect (progn
                  (let ((first-value (plist-get first key))
                         merged-value)
                    (cond
                      ((null second-value)) ; do nothing
                      ((null first-value)
                        (if (listp second-value)
                          ;; Deep copy second-value so that the original value won't
                          ;; be modified.
                          (setq merged-value
                            (lsp--merge-two-plists nil second-value)))
                        (setq merged-value second-value))
                      ((and (listp first-value) (listp second-value))
                        (setq merged-value (lsp--merge-two-plists first-value second-value)))
                      ;; Otherwise, the first value is a leaf entry and should
                      ;; not be overridden.
                      )
                    (when merged-value
                      (setq first (plist-put first key merged-value))))))))
  first)

(defun lsp--server-register-capability (reg)
  (lsp--cur-workspace-check)
  (let ((method (gethash "method" reg)))
    (push
      (make-lsp--registered-capability
        :id (gethash "id" reg)
        :method method
        :options (gethash "registerOptions" reg))
      (lsp--workspace-registered-server-capabilities lsp--cur-workspace))))

(defun lsp--server-unregister-capability (unreg)
  (let* ((id (gethash "id" unreg))
          (fn (lambda (e) (equal (lsp--registered-capability-id e) id))))
    (setf (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
      (seq-remove fn
        (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))))

(defun lsp--client-workspace-capabilities ()
  "Client Workspace capabilities according to LSP."
  `(:applyEdit t
     :executeCommand (:dynamicRegistration t)))

(defun lsp--client-textdocument-capabilities ()
  "Client Text document capabilities according to LSP."
  `(:synchronization (:willSave t :didSave t :willSaveWaitUntil t)
                     :symbol (:symbolKind (:valueSet ,(cl-coerce (cl-loop for kind from 1 to 25 collect kind) 'vector)))
                     :formatting (:dynamicRegistration t)
                     :codeAction (:dynamicRegistration t)))

(defun lsp-register-client-capabilities (package-name caps)
  "Register extra client capabilities for the current workspace.

This function must be called before the initialize request is
sent.  It's recommended to to call it in the
`lsp-before-initialize-hook'.

PACKAGE name is the symbol of the name of the package that
registers the capabilities.  CAPS is either a plist of the
capabilities, or a function that takes no argument and return a
plist of the client capabilties or nil.

Registered capabilities are merged into the default capabilities
before sending to the server via the initialize request.  If two
packages provide different values for the same leaf capability
entry, the value is set to the one that registers later.  Default
leaf capability entries can not be overwritten."
  (lsp--cur-workspace-check)
  (cl-check-type package-name symbolp)
  (cl-check-type caps (or list function))
  (let ((extra-client-capabilities
          (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))
    (if (alist-get package-name extra-client-capabilities)
        (message "%s has already registered client capabilities" package-name)
      (push `(,package-name . ,caps)
        (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))))

(defun lsp-unregister-client-capabilities (package-name)
  "Unregister extra capabilities provided by PACKAGE-NAME for the current workspace.

PACKAGE-NAME is a symbol of the name of the package that has
registered client capabilities by calling
`lsp-register-client-capabilities'."
  (lsp--cur-workspace-check)
  (cl-check-type package-name symbol)
  (let ((extra-client-capabilities
          (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))
    (setf (lsp--workspace-extra-client-capabilities lsp--cur-workspace)
      (assq-delete-all package-name extra-client-capabilities))))

(define-inline lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (inline-quote (lsp--workspace-server-capabilities lsp--cur-workspace)))

(defun lsp--server-has-sync-options-p ()
  "Return whether the server has a TextDocumentSyncOptions object in
ServerCapabilities.textDocumentSync."
  (hash-table-p (gethash "textDocumentSync" (lsp--server-capabilities))))

(defun lsp--send-open-close-p ()
  "Return whether open and close notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
      (gethash "openClose" sync))))

(defun lsp--send-will-save-p ()
  "Return whether will save notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
      (gethash "willSave" sync))))

(defun lsp--send-will-save-wait-until-p ()
  "Return whether will save wait until notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
      (and (hash-table-p sync)
        (gethash "willSaveWaitUntil" sync))))

(defun lsp--save-include-text-p ()
  "Return whether save notifications should include the text document's contents."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
      (hash-table-p (gethash "save" sync nil))
      (gethash "includeText" (gethash "save" sync)))))

(defun lsp--set-sync-method ()
  (let* ((sync (gethash "textDocumentSync" (lsp--server-capabilities)))
          (kind (if (hash-table-p sync) (gethash "change" sync) sync))
          (method (alist-get kind lsp--sync-methods)))
    (setq lsp--server-sync-method (or lsp-document-sync-method
                                    method))))

(defun lsp--workspace-apply-edit-handler (_workspace params)
  (lsp--apply-workspace-edit (gethash "edit" params)))

(defun lsp--make-sentinel (workspace)
  (cl-check-type workspace lsp--workspace)
  (lambda (process exit-str)
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
        ;; Server has exited.  Uninitialize all buffer-local state for this
        ;; workspace.
        (message "%s: %s has exited (%s)"
                 (lsp--workspace-root workspace)
                 (process-name (lsp--workspace-proc workspace))
                 (string-trim-right exit-str))
        (dolist (buf (lsp--workspace-buffers workspace))
          (with-current-buffer buf
            (lsp--uninitialize-workspace)))
        ;; Kill standard error buffer only if the process exited normally.
        ;; Leave it intact otherwise for debugging purposes.
        (when (and (eq status 'exit) (zerop (process-exit-status process)))
          ;; FIXME: The client structure should store the standard error
          ;; buffer, not its name.
          ;; FIXME: Probably the standard error buffer should be per workspace,
          ;; not per client.
          (let ((stderr (get-buffer (lsp--client-stderr
                                     (lsp--workspace-client workspace)))))
            (when (buffer-live-p stderr)
              (kill-buffer stderr))))))))

(defun lsp--should-start-p (root)
  "Consult `lsp-project-blacklist' and `lsp-project-whitelist' to
determine if a server should be started for the given ROOT
directory."
  (or
    (cl-some (lambda (p) (string-match-p p root))
      lsp-project-whitelist)
    (cl-notany (lambda (p) (string-match-p p root))
      lsp-project-blacklist)))

(defun lsp--start (client &optional extra-init-params)
  (when lsp--cur-workspace
    (user-error "LSP mode is already enabled for this buffer"))
  (cl-assert client)
  (let* ((root (file-truename (funcall (lsp--client-get-root client))))
         (workspace (gethash root lsp--workspaces))
         new-conn response init-params
         parser proc cmd-proc)
    (if workspace
        (progn
          (setq lsp--cur-workspace workspace)
          (lsp-mode 1))

      (setf
       parser (make-lsp--parser)
       lsp--cur-workspace (make-lsp--workspace
                           :parser parser
                           :file-versions (make-hash-table :test 'equal)
                           :root root
                           :client client)
       (lsp--parser-workspace parser) lsp--cur-workspace
       new-conn (funcall
                 (lsp--client-new-connection client)
                 (lsp--parser-make-filter parser (lsp--client-ignore-regexps client))
                 (lsp--make-sentinel lsp--cur-workspace))
       ;; the command line process invoked
       cmd-proc (if (consp new-conn) (car new-conn) new-conn)
       ;; the process we actually communicate with
       proc (if (consp new-conn) (cdr new-conn) new-conn)

       (lsp--workspace-proc lsp--cur-workspace) proc
       (lsp--workspace-cmd-proc lsp--cur-workspace) cmd-proc)

      (puthash root lsp--cur-workspace lsp--workspaces)
      (lsp-mode 1)
      (run-hooks 'lsp-before-initialize-hook)
      (setq init-params
            `(:processId ,(emacs-pid)
                         :rootPath ,root
                         :rootUri ,(lsp--path-to-uri root)
                         :capabilities ,(lsp--client-capabilities)
                         :initializationOptions ,(if (functionp extra-init-params)
                                                     (funcall extra-init-params lsp--cur-workspace)
                                                   extra-init-params)))
      (setf response (lsp--send-request
                      (lsp--make-request "initialize" init-params)))
      (unless response
        (signal 'lsp-empty-response-error (list "initialize")))
      (setf (lsp--workspace-server-capabilities lsp--cur-workspace)
            (gethash "capabilities" response))
      ;; Version 3.0 now sends an "initialized" notification to allow registration
      ;; of server capabilities
      (lsp--send-notification (lsp--make-notification "initialized" (make-hash-table)))
      (run-hooks 'lsp-after-initialize-hook))
    (lsp--text-document-did-open)))

(defun lsp--text-document-did-open ()
  (run-hooks 'lsp-before-open-hook)
  (puthash (current-buffer) 0 (lsp--workspace-file-versions lsp--cur-workspace))
  (push (current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp--send-notification (lsp--make-notification
                           "textDocument/didOpen"
                           `(:textDocument ,(lsp--make-text-document-item))))

  (add-hook 'after-save-hook #'lsp-on-save nil t)
  (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)

  (when lsp-enable-eldoc
    ;; XXX: The documentation for `eldoc-documentation-function' suggests
    ;; using `add-function' for modifying its value, use that instead?
    (setq-local eldoc-documentation-function #'lsp--on-hover)
    (eldoc-mode 1))

  (when (and lsp-enable-indentation
             (lsp--capability "documentRangeFormattingProvider"))
    (setq-local indent-region-function #'lsp-format-region))

  (when (and lsp-enable-xref
             (lsp--capability "referencesProvider")
             (lsp--capability "definitionProvider"))
    (setq-local xref-backend-functions (list #'lsp--xref-backend)))

  (when (and lsp-enable-completion-at-point (lsp--capability "completionProvider"))
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t))

  ;; Make sure the hook is local (last param) otherwise we see all changes for all buffers
  (add-hook 'before-change-functions #'lsp-before-change nil t)
  (add-hook 'after-change-functions #'lsp-on-change nil t)
  (add-hook 'after-revert-hook #'lsp-on-revert nil t)
  (add-hook 'before-save-hook #'lsp--before-save nil t)
  (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
  (lsp--set-sync-method)
  (run-hooks 'lsp-after-open-hook))

(define-inline lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (inline-quote (list :uri (lsp--buffer-uri))))

(define-inline lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (inline-quote (plist-put (lsp--text-document-identifier)
                  :version (lsp--cur-file-version))))

(define-inline lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.

interface Position {
    line: number;
    character: number;
}"
  (inline-quote (list :line ,line :character ,char)))

(define-inline lsp--cur-line ()
  (inline-quote (1- (line-number-at-pos))))

(define-inline lsp--cur-column ()
  (inline-quote (- (point) (line-beginning-position))))

(define-inline lsp--cur-position ()
  "Make a Position object for the current point."
  (inline-quote
   (save-restriction
     (widen)
     (lsp--position (lsp--cur-line) (lsp--cur-column)))))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (lsp--cur-position)))

(define-inline lsp--position-p (p)
  (inline-quote
    (and (numberp (plist-get ,p :line)) (numberp (plist-get ,p :character)))))

(define-inline lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  ;; make sure start and end are Position objects
  (inline-quote
    (progn
      (cl-check-type ,start (satisfies lsp--position-p))
      (cl-check-type ,end (satisfies lsp--position-p))
      (list :start ,start :end ,end))))

(define-inline lsp--region-to-range (start end)
  "Make Range object for the current region."
  (inline-quote (lsp--range (lsp--point-to-position ,start)
                  (lsp--point-to-position ,end))))

(defun lsp--current-region-or-pos ()
  "If the region is active return that, else get the point."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point) (point))))

(defun lsp--get-start-position ()
  "Get the start of the region if active, else current point."
  (let ((pos (if (use-region-p)
                 (region-beginning)
               (point))))
    (lsp-point-to-position pos)))

(defun lsp--get-end-position ()
  "Get the end of the region if active, else current point."
  (let ((pos (if (use-region-p)
                 (region-end)
               (point))))
    (lsp-point-to-position pos)))

(define-inline lsp--range-start-line (range)
  "Return the start line for a given LSP range, in LSP coordinates"
  (inline-quote (plist-get (plist-get ,range :start) :line)))

(define-inline lsp--range-end-line (range)
  "Return the end line for a given LSP range, in LSP coordinates"
  (inline-quote (plist-get (plist-get ,range :end) :line)))

(defun lsp--apply-workspace-edit (edit)
  "Apply the WorkspaceEdit object EDIT.

interface WorkspaceEdit {
  changes?: { [uri: string]: TextEdit[]; };
  documentChanges?: TextDocumentEdit[];
}"
  (let ((changes (gethash "changes" edit))
         (document-changes (gethash "documentChanges" edit)))
    (if document-changes
      (seq-do #'lsp--apply-text-document-edit document-changes)

      (when (hash-table-p changes)
        (maphash
          (lambda (uri text-edits)
            (let ((filename (lsp--uri-to-path uri)))
              (with-current-buffer (find-file-noselect filename)
                (lsp--apply-text-edits text-edits))))
          changes)))))

(defun lsp--apply-text-document-edit (edit)
  "Apply the TextDocumentEdit object EDIT.
If the file is not being visited by any buffer, it is opened with
`find-file-noselect'.
Because lsp-mode does not store previous document versions, the edit is only
applied if the version of the textDocument matches the version of the
corresponding file.

interface TextDocumentEdit {
  textDocument: VersionedTextDocumentIdentifier;
  edits: TextEdit[];
}"
  (let* ((ident (gethash "textDocument" edit))
          (filename (lsp--uri-to-path (gethash "uri" ident)))
          (version (gethash "version" ident)))
    (with-current-buffer (find-file-noselect filename)
      (when (and version (= version (lsp--cur-file-version)))
        (lsp--apply-text-edits (gethash "edits" edit))))))

(defun lsp--text-edit-sort-predicate (e1 e2)
  (let ((start1 (lsp--position-to-point (gethash "start" (gethash "range" e1))))
          (start2 (lsp--position-to-point (gethash "start" (gethash "range" e2)))))
    (if (= start1 start2)
      (let ((end1 (lsp--position-to-point (gethash "end" (gethash "range" e1))))
             (end2 (lsp--position-to-point (gethash "end" (gethash "range" e2)))))
        (> end1 end2))

      (> start1 start2))))

(define-inline lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object in EDITS."
  (inline-quote
    ;; We sort text edits so as to apply edits that modify earlier parts of the
    ;; document first. Furthermore, because the LSP spec dictates that:
    ;; "If multiple inserts have the same position, the order in the array
    ;; defines which edit to apply first."
    ;; We reverse the initial list to make sure that the order among edits with
    ;; the same position is preserved.

    (seq-do #'lsp--apply-text-edit (sort (nreverse ,edits) #'lsp--text-edit-sort-predicate))))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (let* ((range (gethash "range" text-edit))
         (start-point (lsp--position-to-point (gethash "start" range)))
         (end-point (lsp--position-to-point (gethash "end" range))))
    (save-excursion
      (goto-char start-point)
      (delete-region start-point end-point)
      (insert (gethash "newText" text-edit)))))

(define-inline lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (inline-quote (gethash ,cap (or ,capabilities (lsp--server-capabilities) (make-hash-table)))))

(define-inline lsp--registered-capability (method)
  (inline-quote
   (seq-find (lambda (reg) (equal (lsp--registered-capability-method reg) ,method))
             (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
             nil)))

(define-inline lsp--registered-capability-by-id (id)
  (inline-quote
   (seq-find (lambda (reg) (equal (lsp--registered-capability-id reg) ,id))
             (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
             nil)))

(defvar-local lsp--before-change-vals nil
  "Store the positions from the `lsp-before-change' function
  call, for validation and use in the `lsp-on-change' function.")

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":5,"character":6}
  ;;             ,"end" :{"line":5,"character":6}}
  ;;             ,"rangeLength":0
  ;;             ,"text":"\nbb = 5"}
  ;;
  ;; And (47 47 7) means delete 7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":6,"character":0}
  ;;            ,"end"  :{"line":7,"character":0}}
  ;;            ,"rangeLength":7
  ;;            ,"text":""}
  ;;
  ;; (208 221 3) means delete 3 chars starting at pos 208, and replace them with
  ;; 13 chars. So it must become
  ;;   {"range":{"start":{"line":5,"character":8}
  ;;             ,"end" :{"line":5,"character":11}}
  ;;             ,"rangeLength":3
  ;;             ,"text":"new-chars-xxx"}
  ;;

  ;; Adding text:
  ;;   lsp-before-change:(start,end)=(33,33)
  ;;   lsp-on-change:(start,end,length)=(33,34,0)
  ;;
  ;; Changing text:
  ;;   lsp-before-change:(start,end)=(208,211)
  ;;   lsp-on-change:(start,end,length)=(208,221,3)
  ;;
  ;; Deleting text:
  ;;   lsp-before-change:(start,end)=(19,27)
  ;;   lsp-on-change:(start,end,length)=(19,19,8)

  (if (eq length 0)
    ;; Adding something only, work from start only
    `(:range ,(lsp--range (lsp--point-to-position start)
                (lsp--point-to-position start))
       :rangeLength 0
       :text ,(buffer-substring-no-properties start end))

    (if (eq start end)
      ;; Deleting something only
      (if (lsp--bracketed-change-p start end length)
        ;; The before-change value is bracketed, use it
        `(:range ,(lsp--range (lsp--point-to-position start)
                    (plist-get lsp--before-change-vals :end-pos))
           :rangeLength ,length
           :text "")
        ;; If the change is not bracketed, send a full change event instead.
        (lsp--full-change-event))

      ;; Deleting some things, adding others
      (if (lsp--bracketed-change-p start end length)
        ;; The before-change value is valid, use it
        `(:range ,(lsp--range (lsp--point-to-position start)
                    (plist-get lsp--before-change-vals :end-pos))
           :rangeLength ,length
           :text ,(buffer-substring-no-properties start end))
        (lsp--full-change-event)))))


;; TODO: Add tests for this function.
(defun lsp--bracketed-change-p (start _end length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (and (eq start (plist-get lsp--before-change-vals :start) )
       (eq length (- (plist-get lsp--before-change-vals :end)
                     (plist-get lsp--before-change-vals :start)))))

;; Observed from vscode for applying a diff replacing one line with
;; another. Emacs on-change shows this as a delete followed by an
;; add.

;; 2017-04-22 17:43:59 [ThreadId 11] DEBUG haskell-lsp - ---> {"jsonrpc":"2.0","method":"textDocument/didChange","params":
;; {"textDocument":{"uri":"file:///home/alanz/tmp/haskell-hie-test-project/src/Foo.hs","version":2}
;; ,"contentChanges":[{"range":{"start":{"line":7,"character":0}
;;                             ,"end":  {"line":7,"character":8}}
;;                     ,"rangeLength":8
;;                     ,"text":"baz ="}]}}


(defun lsp--full-change-event ()
  (save-restriction
    (widen)
    `(:text ,(buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-before-change (start end)
  "Executed before a file is changed.
Added to `before-change-functions'."
  ;; Note:
  ;;
  ;; This variable holds a list of functions to call when Emacs is about to
  ;; modify a buffer. Each function gets two arguments, the beginning and end of
  ;; the region that is about to change, represented as integers. The buffer
  ;; that is about to change is always the current buffer when the function is
  ;; called.
  ;;
  ;; WARNING:
  ;;
  ;; Do not expect the before-change hooks and the after-change hooks be called
  ;; in balanced pairs around each buffer change. Also don't expect the
  ;; before-change hooks to be called for every chunk of text Emacs is about to
  ;; delete. These hooks are provided on the assumption that Lisp programs will
  ;; use either before- or the after-change hooks, but not both, and the
  ;; boundaries of the region where the changes happen might include more than
  ;; just the actual changed text, or even lump together several changes done
  ;; piecemeal.
  ;; (message "lsp-before-change:(start,end)=(%s,%s)" start end)
  (with-demoted-errors "Error in ‘lsp-before-change’: %S"
    (setq lsp--before-change-vals
          (list :start start
                :end end
                :start-pos (lsp--point-to-position start)
                :end-pos (lsp--point-to-position end)))))

(defun lsp-on-change (start end length)
  "Executed when a file is changed.
Added to `after-change-functions'."
  ;; Note:
  ;;
  ;; Each function receives three arguments: the beginning and end of the region
  ;; just changed, and the length of the text that existed before the change.
  ;; All three arguments are integers. The buffer that has been changed is
  ;; always the current buffer when the function is called.
  ;;
  ;; The length of the old text is the difference between the buffer positions
  ;; before and after that text as it was before the change. As for the
  ;; changed text, its length is simply the difference between the first two
  ;; arguments.
  ;;
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; So (47 47 7) means delete 7 chars starting at pos 47
  ;; (message "lsp-on-change:(start,end,length)=(%s,%s,%s)" start end length)
  ;; (message "lsp-on-change:(lsp--before-change-vals)=%s" lsp--before-change-vals)
  (with-demoted-errors "Error in ‘lsp-on-change’: %S"
    (save-match-data
      ;; A (revert-buffer) call with the 'preserve-modes parameter (eg, as done
      ;; by auto-revert-mode) will cause this hander to get called with a nil
      ;; buffer-file-name. We need the buffer-file-name to send notifications;
      ;; so we skip handling revert-buffer-caused changes and instead handle
      ;; reverts separately in lsp-on-revert
      (when (and lsp--cur-workspace (not revert-buffer-in-progress-p))
        (lsp--inc-cur-file-version)
        (unless (eq lsp--server-sync-method 'none)
          (lsp--send-notification
           (lsp--make-notification
            "textDocument/didChange"
            `(:textDocument
              ,(lsp--versioned-text-document-identifier)
              :contentChanges
              ,(pcase lsp--server-sync-method
                 ('incremental (vector (lsp--text-document-content-change-event
                                        start end length)))
                 ('full (vector (lsp--full-change-event))))))))))))

(defun lsp-on-revert ()
  "Executed when a file is reverted.
Added to `after-revert-hook'."
  (let ((n (buffer-size))
        (revert-buffer-in-progress-p nil))
    (lsp-on-change 0 n n)))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (when lsp--cur-workspace
    (with-demoted-errors "Error on ‘lsp--text-document-did-close’: %S"
      (let ((file-versions (lsp--workspace-file-versions lsp--cur-workspace))
            (old-buffers (lsp--workspace-buffers lsp--cur-workspace)))
        ;; remove buffer from the current workspace's list of buffers
        ;; do a sanity check first
        (when (memq (current-buffer) old-buffers)
          (setf (lsp--workspace-buffers lsp--cur-workspace)
                (delq (current-buffer) old-buffers))

          (remhash (current-buffer) file-versions)
          (with-demoted-errors "Error sending didClose notification in ‘lsp--text-document-did-close’: %S"
            (lsp--send-notification
             (lsp--make-notification
              "textDocument/didClose"
              `(:textDocument ,(lsp--versioned-text-document-identifier)))))
          (when (= 0 (hash-table-count file-versions))
            (lsp--shutdown-cur-workspace)))))))

(define-inline lsp--will-save-text-document-params (reason)
  (cl-check-type reason number)
  (inline-quote
    (list :textDocument (lsp--text-document-identifier)
      :reason ,reason)))

(defun lsp--before-save ()
  (when lsp--cur-workspace
    (with-demoted-errors "Error in ‘lsp--before-save’: %S"
      (let ((params (lsp--will-save-text-document-params 1)))
        (when (lsp--send-will-save-p)
          (lsp--send-notification
            (lsp--make-notification "textDocument/willSave" params)))
        (when (and (lsp--send-will-save-wait-until-p) lsp-before-save-edits)
          (lsp--apply-text-edits
           (lsp--send-request (lsp--make-request
                               "textDocument/willSaveWaitUntil" params))))))))

(defun lsp--on-auto-save ()
  (when (and lsp--cur-workspace
          (lsp--send-will-save-p))
    (with-demoted-errors "Error in ‘lsp--on-auto-save’: %S"
      (lsp--send-notification
        (lsp--make-notification
          "textDocument/willSave" (lsp--will-save-text-document-params 2))))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when lsp--cur-workspace
    (with-demoted-errors "Error on ‘lsp--text-document-did-save: %S’"
      (lsp--send-notification
       (lsp--make-notification
        "textDocument/didSave"
         `(:textDocument ,(lsp--versioned-text-document-identifier)
                         :text ,(if (lsp--save-include-text-p)
                                    (save-excursion
                                      (widen)
                                      (buffer-substring-no-properties (point-min) (point-max)))
                                  nil)))))))

(define-inline lsp--text-document-position-params (&optional identifier position)
  "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document identifier
and the position respectively."
  (inline-quote (list :textDocument (or ,identifier (lsp--text-document-identifier))
                      :position (or ,position (lsp--cur-position)))))

(define-inline lsp--text-document-code-action-params ()
  "Make CodeActionParams for the current region in the current document."
  (inline-quote (list :textDocument (lsp--text-document-identifier)
                  :range (lsp--current-region-or-pos)
                  :context (list :diagnostics (lsp--cur-line-diagnotics)))))

(defun lsp--cur-line-diagnotics ()
  "Return any diagnostics that apply to the current line."
  (let* ((diags (gethash buffer-file-name lsp--diagnostics nil))
         (range (lsp--current-region-or-pos))
         (start-line (lsp--range-start-line range))
         (end-line (lsp--range-end-line range))
         (diags-in-range (cl-remove-if-not
                          (lambda (diag)
                            (let ((line (lsp-diagnostic-line diag)))
                              (and (>= line start-line) (<= line end-line))))
                          diags)))
    (cl-coerce (seq-map #'lsp-diagnostic-original diags-in-range) 'vector)))

(defconst lsp--completion-item-kind
  [nil
   "Text"
   "Method"
   "Function"
   "Constructor"
   "Field"
   "Variable"
   "Class"
   "Interface"
   "Module"
   "Property"
   "Unit"
   "Value"
   "Enum"
   "Keyword"
   "Snippet"
   "Color"
   "File"
   "Reference"
   "Folder"
   "EnumMember"
   "Constant"
   "Struct"
   "Event"
   "Operator"
   "TypeParameter"
   ])

(defun lsp--gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value,
unless KEY not found or its value is falsy, when it returns DFLT.
DFLT defaults to nil.

Needed for completion request fallback behavior for the fields
'sortText', 'filterText', and 'insertText' as described here:

https://microsoft.github.io/language-server-protocol/specification#textDocument_completion"

  (let ((result (gethash key table dflt)))
    (when (member result '(nil "" 0 :json-false))
      (setq result dflt))
    result))

(defun lsp--make-completion-item (item)
  (propertize (lsp--gethash "insertText" item (gethash "label" item ""))
              'lsp-completion-item
              item))

(defun lsp--annotate (item)
  (let* ((table (plist-get (text-properties-at 0 item) 'lsp-completion-item))
         (detail (gethash "detail" table nil))
         (kind-index (gethash "kind" table nil)))
    ;; We need check index before call `aref'.
    (when kind-index
      (setq kind (aref lsp--completion-item-kind kind-index))
      (concat
       " "
       detail
       (when kind (format " (%s)" kind))))
    ))

(defun lsp--sort-string (c)
  (lsp--gethash "sortText" c (gethash "label" c "")))

(defun lsp--sort-completions (completions)
  (sort completions (lambda (c1 c2)
                      (string-lessp
                        (lsp--sort-string c1)
                        (lsp--sort-string c2)))))

(defun lsp--default-prefix-function ()
  (bounds-of-thing-at-point 'symbol))

(defun lsp--get-completions ()
  (with-demoted-errors "Error in ‘lsp--get-completions’: %S"
    (let* ((prefix-function (or (lsp--client-prefix-function
                                  (lsp--workspace-client lsp--cur-workspace))
                             #'lsp--default-prefix-function))
            (bounds (funcall prefix-function)))
      (list
       (if bounds (car bounds) (point))
       (if bounds (cdr bounds) (point))
       (completion-table-dynamic
        #'(lambda (_)
            ;; *we* don't need to know the string being completed
            ;; the language server does all the work by itself
            (let* ((resp (lsp--send-request
                          (lsp--make-request
                           "textDocument/completion"
                           (lsp--text-document-position-params))))
                   (items (cond
                           ((null resp) nil)
                           ((hash-table-p resp) (gethash "items" resp nil))
                           ((sequencep resp) resp))))
              (seq-map #'lsp--make-completion-item items))))
       :annotation-function #'lsp--annotate
       :display-sort-function #'lsp--sort-completions))))

(defun lsp--resolve-completion (item)
  (lsp--cur-workspace-check)
  (cl-assert item nil "Completion item must not be nil")
  (if (gethash "resolveProvider" (lsp--capability "completionProvider"))
    (lsp--send-request
      (lsp--make-request
        "completionItem/resolve"
        item))
    item))

(defun lsp--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun lsp--xref-make-item (filename location)
  "Return a xref-item from a LOCATION in FILENAME."
  (let* ((range (gethash "range" location))
         (pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make (or line filename)
               (xref-make-file-location filename
                                        (1+ (gethash "line" pos-start))
                                        (gethash "character" pos-start)))))

(defun lsp--get-xrefs-in-file (file)
  "Return all references that contain a file.
FILE is a cons where its car is the filename and the cdr is a list of Locations
within the file.  We open and/or create the file/buffer only once for all
references.  The function returns a list of `xref-item'."
  (let* ((filename (car file))
         (visiting (find-buffer-visiting filename))
         (fn (lambda (loc) (lsp--xref-make-item filename loc))))
    (if visiting
        (with-current-buffer visiting
          (mapcar fn (cdr file)))
      (when (file-readable-p filename)
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (mapcar fn (cdr file)))))))

(defun lsp--locations-to-xref-items (locations)
  "Return a list of `xref-item' from LOCATIONS.
LOCATIONS is an array of Location objects:

interface Location {
  uri: DocumentUri;
  range: Range;
}"
  (when locations
    (let* ((fn (lambda (loc) (lsp--uri-to-path (gethash "uri" loc))))
            ;; locations-by-file is an alist of the form
            ;; ((FILENAME . LOCATIONS)...), where FILENAME is a string of the
            ;; actual file name, and LOCATIONS is a list of Location objects
            ;; pointing to Ranges inside that file.
            (locations-by-file (seq-group-by fn locations))
            ;; items-by-file is a list of list of xref-item
            (items-by-file (mapcar #'lsp--get-xrefs-in-file locations-by-file)))
      ;; flatten the list
      (apply #'append items-by-file))))

(defun lsp--get-definitions ()
  "Get definition of the current symbol under point.
Returns xref-item(s)."
  (let ((defs (lsp--send-request (lsp--make-request
                                  "textDocument/definition"
                                   (lsp--text-document-position-params)))))
    ;; textDocument/definition returns Location | Location[]
    (lsp--locations-to-xref-items (if (listp defs) defs (list defs)))))

(defun lsp--make-reference-params (&optional td-position include-declaration)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead.
If INCLUDE-DECLARATION is non-nil, request the server to include declarations."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
      :context `(:includeDeclaration ,(or include-declaration json-false)))))

(defun lsp--get-references ()
  "Get all references for the symbol under point.
Returns xref-item(s)."
  (let ((refs  (lsp--send-request (lsp--make-request
                                   "textDocument/references"
                                   (lsp--make-reference-params)))))
    (lsp--locations-to-xref-items refs)))

(defun lsp--cancel-request (id)
  (lsp--cur-workspace-check)
  (cl-check-type id (or number string))
  (let ((response-handlers (lsp--client-response-handlers (lsp--workspace-client
                                                            lsp--cur-workspace))))
    (remhash id response-handlers)
    (lsp--send-notification (lsp--make-notification "$/cancelRequest"
                              `(:id ,id)))))

(defun lsp--on-hover ()
  ;; This function is used as ‘eldoc-documentation-function’, so it’s important
  ;; that it doesn’t fail.
  (with-demoted-errors "Error in ‘lsp--on-hover’: %S"
    (when (and (lsp--capability "documentHighlightProvider")
               lsp-highlight-symbol-at-point)
      (lsp-symbol-highlight))
    (when (and (or (lsp--capability "codeActionProvider")
                   (lsp--registered-capability "textDocument/codeAction"))
               lsp-enable-codeaction)
      (lsp--text-document-code-action))
    (when (and (lsp--capability "hoverProvider") lsp-enable-eldoc)
      (funcall lsp-hover-text-function))))

(defun lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (lsp--cur-workspace-check)
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
         (contents (gethash "contents" (lsp--send-request
                                        (lsp--make-request "textDocument/hover"
                                                           (lsp--text-document-position-params))))))
    (pop-to-buffer
     (with-current-buffer (get-buffer-create "*lsp-help*")
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (lsp--render-on-hover-content contents client t))
         (goto-char (point-min))
         (view-mode t)
         (current-buffer))))))

(defvar-local lsp--cur-hover-request-id nil)

(defun lsp--text-document-hover-string ()
  "interface Hover {
    contents: MarkedString | MarkedString[];
    range?: Range;
}

type MarkedString = string | { language: string; value: string };"
  (lsp--cur-workspace-check)
  (when lsp--cur-hover-request-id
    (lsp--cancel-request lsp--cur-hover-request-id))
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
          bounds body)
    (when (symbol-at-point)
      (setq bounds (bounds-of-thing-at-point 'symbol)
        body (lsp--send-request-async (lsp--make-request "textDocument/hover"
                                        (lsp--text-document-position-params))
               (lsp--make-hover-callback client (car bounds) (cdr bounds)
                 (current-buffer)))
        lsp--cur-hover-request-id (plist-get body :id))
      (cl-assert (integerp lsp--cur-hover-request-id)))))

(defun lsp--render-markup-content-1 (kind content)
  (if (functionp lsp-render-markdown-markup-content)
    (let ((out (funcall lsp-render-markdown-markup-content kind content)))
      (cl-assert (stringp out) t
        "value returned by lsp-render-markdown-markup-content should be a string")
      out)
    content))

(defun lsp--render-markup-content (content)
  "Render MarkupContent object CONTENT.

export interface MarkupContent {
        kind: MarkupKind;
        value: string;
}"
  (let ((kind (gethash "kind" content))
        (content (gethash "value" content)))
    (lsp--render-markup-content-1 kind content)))

(define-inline lsp--point-is-within-bounds-p (start end)
  "Return whether the current point is within START and END."
  (inline-quote
    (let ((p (point)))
      (and (>= p ,start) (<= p ,end)))))

(define-inline lsp--markup-content-p (obj)
  (inline-letevals (obj)
    (inline-quote (and (hash-table-p ,obj)
                    (gethash "kind" ,obj nil) (gethash "value" ,obj nil)))))

(defun lsp--render-on-hover-content (contents client render-all)
  "Render the content received from 'document/onHover' request.

CLIENT - client to use.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL if set to nil render only the first element from CONTENTS."
  (let ((renderers (lsp--client-string-renderers client))
        (default-client-renderer (lsp--client-default-renderer client)))
    (string-join
     (seq-map
      (lambda (e)
        (let (renderer)
          (cond
           ;; hash table, language renderer set
           ((and (hash-table-p e)
                 (setq renderer
                       (if-let (language (gethash "language" e))
                           (cdr (assoc-string language renderers))
                         default-client-renderer)))
            (when (gethash "value" e nil)
              (funcall renderer (gethash "value" e))))

           ;; hash table - workspace renderer not set
           ;; trying to render using global renderer
           ((lsp--markup-content-p e) (lsp--render-markup-content e))

           ;; hash table - anything other has failed
           ((hash-table-p e) (gethash "value" e nil))

           ;; string, default workspace renderer set
           (default-client-renderer (funcall default-client-renderer  e))

           ;; no rendering
           (t e))))
      (if (sequencep contents)
          (if render-all
              contents
            (seq-take contents 1))
        (list contents)))
     "\n")))

;; start and end are the bounds of the symbol at point
(defun lsp--make-hover-callback (client start end buffer)
  (lambda (hover)
    (with-current-buffer buffer
      (setq lsp--cur-hover-request-id nil))
    (when (and hover
               (lsp--point-is-within-bounds-p start end)
               (eq (current-buffer) buffer) (eldoc-display-message-p))
      (let ((contents (gethash "contents" hover)))
        (when contents
          (eldoc-message (lsp--render-on-hover-content contents
                                                       client
                                                       lsp-eldoc-render-all)))))))

(defun lsp-provide-marked-string-renderer (client language renderer)
  (cl-check-type language string)
  (cl-check-type renderer function)
  (setf (alist-get language (lsp--client-string-renderers client)) renderer))

(defun lsp-provide-default-marked-string-renderer (client renderer)
  "Set the RENDERER for CLIENT.

It will be used when no language has been specified in document/onHover result."
  (cl-check-type renderer function)
  (setf (lsp--client-default-renderer client) renderer))

(defun lsp-info-under-point ()
  "Show relevant documentation for the thing under point."
  (interactive)
  (lsp--text-document-hover-string))

(defvar-local lsp--current-signature-help-request-id nil)

(defun lsp--text-document-signature-help ()
  "interface SignatureHelp {
signatures: SignatureInformation[];
activeSignature?: number;
activeParameter?: number;
};

interface SignatureInformation {
label: string;
documentation?: string | MarkupContent;
parameters?: ParameterInformation[];
};

interface ParameterInformation {
label: string;
documentation?: string | MarkupContent;
};

interface MarkupContent {
kind: MarkupKind;
value: string;
};

type MarkupKind = 'plaintext' | 'markdown';"
  (lsp--cur-workspace-check)
  (when lsp--current-signature-help-request-id
    (lsp--cancel-request lsp--current-signature-help-request-id))
  (let (bounds body)
    (when (symbol-at-point)
      (setq bounds (bounds-of-thing-at-point 'symbol)
            body (lsp--send-request-async
                  (lsp--make-request "textDocument/signatureHelp"
                                     (lsp--text-document-position-params))
                  (lsp--make-text-document-signature-help-callback
                   (car bounds) (cdr bounds) (current-buffer)))
            lsp--current-signature-help-request-id (plist-get body :id))
      (cl-assert (integerp lsp--current-signature-help-request-id)))))

(defun lsp--make-text-document-signature-help-callback (start end buffer)
  (lambda (signature-help)
    (with-current-buffer buffer
      (setq lsp--current-signature-help-request-id nil))
    (when (and signature-help
               (lsp--point-is-within-bounds-p start end)
               (eq (current-buffer) buffer) (eldoc-display-message-p))
      (let* ((active-signature-number
              (or (gethash "activeSignature" signature-help) 0))
             (active-signature (nth
                                active-signature-number
                                (gethash "signatures" signature-help))))
        (when active-signature
          (eldoc-message (gethash "label" active-signature)))))))

;; NOTE: the code actions cannot currently be applied. There is some non-GNU
;; code to do this in the lsp-haskell module. We still need a GNU version, here.
;; PRs accepted.
(defvar-local lsp-code-actions nil
  "Code actions for the buffer.")

(defvar-local lsp-code-action-params nil
  "The last code action params.")

(defun lsp--text-document-code-action ()
  "Request code action to automatically fix issues reported by
the diagnostics."
  (lsp--cur-workspace-check)
  (unless (or (lsp--capability "codeActionProvider")
              (lsp--registered-capability "textDocument/codeAction"))
    (signal 'lsp-capability-not-supported (list "codeActionProvider")))
  (let ((params (lsp--text-document-code-action-params)))
    (lsp--send-request-async
     (lsp--make-request "textDocument/codeAction" params)
     (lambda (actions)
       (lsp--set-code-action-params (current-buffer) actions params)))))

(defun lsp--command-get-title (cmd)
  "Given a Command object CMD, get the title.
If title is nil, return the name for the command handler."
  (gethash "title" cmd (gethash "command" cmd)))

(defun lsp--set-code-action-params (buf actions params)
  "Update set `lsp-code-actions' to ACTIONS and `lsp-code-action-params' to PARAMS in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (equal params (lsp--text-document-code-action-params))
        (setq lsp-code-actions actions)
        (setq lsp-code-action-params params)))))

(defun lsp--command-p (cmd)
  (and (cl-typep cmd 'hash-table)
    (cl-typep (gethash "title" cmd) 'string)
    (cl-typep (gethash "command" cmd) 'string)))

(defun lsp--select-action (actions)
  "Select an action to execute from ACTIONS."
  (if actions
      (let ((name->action (mapcar (lambda (a)
                                    (list (lsp--command-get-title a) a))
                                  actions)))
        (cadr (assoc
               (completing-read "Select code action: " name->action)
               name->action)))
    (error "No actions to select from")))

(defun lsp-get-or-calculate-code-actions ()
  "Get or calculate the current code actions.

The method will either retrieve the current code actions or it will calculate the actual one."
  (let ((current-code-action-params (lsp--text-document-code-action-params)))
    (when (not (equal current-code-action-params lsp-code-action-params))
      (let* ((request-params (lsp--make-request
                             "textDocument/codeAction"
                             (lsp--text-document-code-action-params)))
             (actions (lsp--send-request request-params)))
        (setq lsp-code-action-params current-code-action-params)
        (lsp--set-code-action-params (current-buffer)
                                     actions
                                     current-code-action-params)))
    lsp-code-actions))

(defun lsp-execute-code-action (action)
  "Execute code action ACTION.

If ACTION is not set it will be selected from `lsp-code-actions'."
  (interactive (list
                (lsp--select-action (lsp-get-or-calculate-code-actions))))
  (lsp--cur-workspace-check)
  (let* ((command (gethash "command" action))
         (action-handler (gethash command
                                  (lsp--client-action-handlers
                                   (lsp--workspace-client lsp--cur-workspace)))))
    (if action-handler
        (funcall action-handler action)
      (lsp--execute-command action))))

(defvar-local lsp-code-lenses nil
  "A list of code lenses computed for the buffer.")

(defun lsp--update-code-lenses (&optional callback)
  "Update the list of code lenses for the current buffer.
Optionally, CALLBACK is a function that accepts a single argument, the code lens object."
  (lsp--cur-workspace-check)
  (when callback
    (cl-check-type callback function))
  (when (gethash "codeLensProvider" (lsp--server-capabilities))
    (lsp--send-request-async (lsp--make-request "textDocument/codeLens"
                               `(:textDocument ,(lsp--text-document-identifier)))
      (let ((buf (current-buffer)))
        #'(lambda (lenses)
            (with-current-buffer buf
              (setq lsp-code-lenses lenses)
              (when callback
                (funcall callback lenses))))))))

(defun lsp--make-document-formatting-options ()
  (let ((json-false :json-false))
    `(:tabSize ,tab-width :insertSpaces
               ,(if indent-tabs-mode json-false t))))

(defun lsp--make-document-formatting-params ()
  `(:textDocument ,(lsp--text-document-identifier)
                  :options ,(lsp--make-document-formatting-options)))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (unless (or (lsp--capability "documentFormattingProvider")
              (lsp--registered-capability "textDocument/formatting"))
    (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/formatting"
                                   (lsp--make-document-formatting-params)))))
    (if (fboundp 'replace-buffer-contents)
        (let ((current-buffer (current-buffer)))
          (with-temp-buffer
            (insert-buffer-substring-no-properties current-buffer)
            (lsp--apply-text-edits edits)
            (let ((temp-buffer (current-buffer)))
              (with-current-buffer current-buffer
                (replace-buffer-contents temp-buffer)))))
      (let ((point (point))
            (w-start (window-start)))
        (lsp--apply-text-edits edits)
        (goto-char point)
        (goto-char (line-beginning-position))
        (set-window-start (selected-window) w-start)))))

(defun lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}"
  (plist-put (lsp--make-document-formatting-params)
             :range (lsp--region-to-range start end)))

(defconst lsp--highlight-kind-face
  '((1 . lsp-face-highlight-textual)
    (2 . lsp-face-highlight-read)
    (3 . lsp-face-highlight-write)))

(defun lsp--remove-cur-overlays ()
  (let ((overlays (lsp--workspace-highlight-overlays lsp--cur-workspace))
         (buf (current-buffer)))
    (dolist (overlay (gethash buf overlays))
      (delete-overlay overlay))
    (remhash buf overlays)))

(defun lsp-symbol-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (lsp--send-request-async (lsp--make-request "textDocument/documentHighlight"
                             (lsp--text-document-position-params))
    (lsp--make-symbol-highlight-callback (current-buffer))))

(defun lsp--make-symbol-highlight-callback (buf)
  "Create a callback to process the reply of a
'textDocument/documentHightlight' message for the buffer BUF.
A reference is highlighted only if it is visible in a window."
  (cl-check-type buf buffer)
  (lambda (highlights)
    (with-current-buffer buf
      (lsp--remove-cur-overlays)
      (when (and highlights (/= (length highlights) 0))
        (let* ((windows-on-buffer (get-buffer-window-list nil nil 'visible))
               (overlays (lsp--workspace-highlight-overlays lsp--cur-workspace))
               (buf-overlays (gethash (current-buffer) overlays))
               wins-visible-pos)
          (save-restriction
            (widen)
            ;; Save visible portions of the buffer
            (dolist (win windows-on-buffer)
              (let* ((win-start (window-start win))
                     (win-end (window-end win)))
                (push (cons (1- (line-number-at-pos win-start))
                            (1+ (line-number-at-pos win-end)))
                      wins-visible-pos)))
            (seq-doseq (highlight highlights)
              (let* ((range (gethash "range" highlight nil))
                     (kind (gethash "kind" highlight 1))
                     (start (gethash "start" range))
                     (end (gethash "end" range))
                     overlay)
                (dolist (win wins-visible-pos)
                  (let* ((start-window (car win))
                         (end-window (cdr win)))
                    ;; Make the overlay only if the reference is visible
                    (when (and (> (1+ (gethash "line" start)) start-window)
                               (< (1+ (gethash "line" end)) end-window))
                      (setq overlay (make-overlay (lsp--position-to-point start)
                                                  (lsp--position-to-point end)))
                      (overlay-put overlay 'face
                                   (cdr (assq kind lsp--highlight-kind-face)))
                      (push overlay buf-overlays)
                      (puthash (current-buffer) buf-overlays overlays))))))))))))

(defconst lsp--symbol-kind
  '((1 . "File")
     (2 . "Module")
     (3 . "Namespace")
     (4 . "Package")
     (5 . "Class")
     (6 . "Method")
     (7 . "Property")
     (8 . "Field")
     (9 . "Constructor"),
     (10 . "Enum")
     (11 . "Interface")
     (12 . "Function")
     (13 . "Variable")
     (14 . "Constant")
     (15 . "String")
     (16 . "Number")
     (17 . "Boolean")
     (18 . "Array")
     (19 . "Object")
     (20 . "Key")
     (21 . "Null")
     (22 . "Enum Member")
     (23 . "Struct")
     (24 . "Event")
     (25 . "Operator")
     (26 . "Type Parameter")))

(defun lsp--symbol-information-to-xref (symbol)
  "Return a `xref-item' from SYMBOL information."
  (let* ((location (gethash "location" symbol))
         (uri (gethash "uri" location))
         (range (gethash "range" location))
         (start (gethash "start" range)))
    (xref-make (format "[%s] %s"
                       (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                       (gethash "name" symbol))
               (xref-make-file-location (lsp--uri-to-path uri)
                                        (1+ (gethash "line" start))
                                        (gethash "character" start)))))

(defun lsp-format-region (s e)
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/rangeFormatting"
                                   (lsp--make-document-range-formatting-params s e)))))
    (lsp--apply-text-edits edits)))

(defun lsp--location-to-td-position (location)
  "Convert LOCATION to a TextDocumentPositionParams object."
  `(:textDocument (:uri ,(gethash "uri" location))
                  :position ,(gethash "start" (gethash "range" location))))

(defun lsp--symbol-info-to-identifier (symbol)
  (let ((td-params (lsp--location-to-td-position (gethash "location" symbol))))
    (propertize (gethash "name" symbol)
      'ref-params (lsp--make-reference-params td-params)
      'def-params td-params)))

(defun lsp--get-document-symbols ()
  (lsp--cur-workspace-check)
  (lsp--send-request (lsp--make-request
                       "textDocument/documentSymbol"
                       `(:textDocument ,(lsp--text-document-identifier)))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (symbol-name (symbol-at-point))
              'def-params (lsp--text-document-position-params)
              'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (let ((json-false :json-false)
        (symbols (lsp--get-document-symbols)))
    (seq-map #'lsp--symbol-info-to-identifier symbols)))

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
;;   nil)

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (let* ((maybeparams (get-text-property 0 'def-params identifier))
         ;; In some modes (such as haskell-mode), xref-find-definitions gets
         ;; called directly without applying the properties expected here. So we
         ;; must test if the properties are present, and if not use the current
         ;; point location.
         (params (if (null maybeparams)
                     (lsp--text-document-position-params)
                   maybeparams))
         (defs (lsp--send-request (lsp--make-request
                                   "textDocument/definition"
                                   params))))
    (lsp--locations-to-xref-items (if (listp defs) defs (list defs)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
         (params (plist-get properties 'ref-params))
         (refs (lsp--send-request (lsp--make-request
                                   "textDocument/references"
                                   (or params (lsp--make-reference-params))))))
    (lsp--locations-to-xref-items refs)))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (let ((symbols (lsp--send-request (lsp--make-request
                                     "workspace/symbol"
                                     `(:query ,pattern)))))
    (seq-map #'lsp--symbol-information-to-xref symbols)))

(defun lsp--make-document-rename-params (newname)
  "Make DocumentRangeFormattingParams for selected region.
interface RenameParams {
    textDocument: TextDocumentIdentifier;
    position: Position;
    newName: string;
}"
  `(:position ,(lsp--cur-position)
              :textDocument ,(lsp--text-document-identifier)
              :newName ,newname))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (read-string "Rename to: " (thing-at-point 'symbol))))
  (lsp--cur-workspace-check)
  (unless (lsp--capability "renameProvider")
    (signal 'lsp-capability-not-supported (list "renameProvider")))
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/rename"
                                   (lsp--make-document-rename-params newname)))))
    (when edits
      (lsp--apply-workspace-edit edits))))

(defun lsp-find-custom (method &optional extra)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters."
  (let ((loc (lsp--send-request
              (lsp--make-request method
                                 (append (lsp--text-document-position-params) extra)))))
    (if loc
        (xref--show-xrefs
         (lsp--locations-to-xref-items (if (listp loc) loc (list loc))) nil)
      (message "Not found for: %s" (thing-at-point 'symbol t)))))

(defun lsp-goto-implementation ()
  "Resolve, and go to the implementation(s) of the symbol under point."
  (interactive)
  (lsp--cur-workspace-check)
  (unless (lsp--capability "implementationProvider")
    (signal 'lsp-capability-not-supported (list "implementationProvider")))
  (lsp-find-custom "textDocument/implementation"))

(defun lsp-goto-type-definition ()
  "Resolve, and go to the type definition(s) of the symbol under point."
  (interactive)
  (lsp--cur-workspace-check)
  (unless (lsp--capability "typeDefinitionProvider")
    (signal 'lsp-capability-not-supported (list "typeDefinitionProvider")))
  (lsp-find-custom "textDocument/typeDefinition"))

(define-inline lsp--execute-command (command)
  "Given a COMMAND returned from the server, create and send a
'workspace/executeCommand' message."
  (inline-letevals (command)
    (inline-quote
      (progn
        (cl-check-type ,command (satisfies lsp--command-p))
        (lsp--send-execute-command
          (gethash "command" ,command)
          (gethash "arguments" ,command nil))))))

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having
command COMMAND and optionsl ARGS"
  (lsp--cur-workspace-check)
  (unless (or (lsp--capability "executeCommandProvider")
              (lsp--registered-capability "workspace/executeCommand"))
    (signal 'lsp-capability-not-supported (list "executeCommandProvider")))
  (lsp--send-request
   (lsp--make-request
    "workspace/executeCommand"
    (lsp--make-execute-command-params command args))))

(defun lsp--make-execute-command-params (cmd &optional args)
  (if args
      (list :command cmd :arguments args)
    (list :command cmd)))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-get-start-position #'lsp--get-start-position)
(defalias 'lsp-get-end-position #'lsp--get-end-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
;; (defalias 'lsp-on-change #'lsp--text-document-did-change)
(defalias 'lsp-completion-at-point #'lsp--get-completions)

(defun lsp--unset-variables ()
  (when lsp-enable-eldoc
    (setq-local eldoc-documentation-function 'ignore))
  (when lsp-enable-xref
    (setq-local xref-backend-functions nil))
  (when lsp-enable-completion-at-point
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t))
  (remove-hook 'after-change-functions #'lsp-on-change t)
  (remove-hook 'after-revert-hook #'lsp-on-revert t)
  (remove-hook 'before-change-functions #'lsp-before-change t))

(defun lsp--set-configuration (settings)
  "Set the configuration for the lsp server."
  (lsp--send-notification (lsp--make-notification
                           "workspace/didChangeConfiguration"
                           `(:settings , settings))))

(defun lsp-workspace-register-watch (to-watch &optional workspace)
  "Monitor for file change and trigger workspace/didChangeConfiguration.

TO-WATCH is a list of the directories and regexp in the following format:
'((root-dir1 (glob-pattern1 glob-pattern2))
  (root-dir2 (glob-pattern3 glob-pattern4)))

If WORKSPACE is not specified the `lsp--cur-workspace' will be used."
  (setq workspace (or workspace lsp--cur-workspace))
  (let ((watches (lsp--workspace-watches workspace)))
    (cl-loop for (dir glob-patterns) in to-watch do
      (lsp-create-watch
        dir
        (mapcar 'eshell-glob-regexp glob-patterns)
        (lambda (event)
          (let ((lsp--cur-workspace workspace))
            (lsp-send-notification
              (lsp-make-notification
                "workspace/didChangeWatchedFiles"
                (list :changes
                  (list
                    :type (alist-get (cadr event) lsp--file-change-type)
                    :uri (lsp--path-to-uri (caddr event))))))))
        watches))))

(declare-function lsp-mode "lsp-mode" (&optional arg))

(provide 'lsp-methods)
;;; lsp-methods.el ends here
