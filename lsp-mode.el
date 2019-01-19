;;; lsp-mode.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Vibhav Pant, Ivan Yonchovski

;; Author: Vibhav Pant, Fangrui Song, Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (spinner "1.7.3"))
;; Version: 6.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(unless (version< emacs-version "26")
  (require 'project)
  (require 'flymake))

(eval-when-compile
  (require 'cl))

(require 'cl-lib)
(require 'compile)
(require 'dash)
(require 'em-glob)
(require 'f)
(require 'filenotify)
(require 'files)
(require 'ht)
(require 'imenu)
(require 'inline)
(require 'json)
(require 'network-stream)
(require 'pcase)
(require 'seq)
(require 'spinner)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'widget)
(require 'xref)
(require 'tree-widget)

(defconst lsp--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

(defconst lsp--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")
    (-32002 "Server Not Initialized")
    (-32001 "Unknown Error Code")
    (-32800 "Request Cancelled"))
  "Alist of error codes to user friendly strings.")

(defconst lsp--silent-errors '(-32800)
  "Error codes that are okay to not notify the user about.")

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
   "TypeParameter"])

(defcustom lsp-print-io nil
  "If non-nil, print all messages to and from the language server to *Messages*."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-inhibit-message t
  "If non-nil, inhibit the message echo via `inhibit-message'."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-report-if-no-buffer t
  "If non nil the errors will be reported even when the file is not open."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-keep-workspace-alive t
  "If non nil keep workspace alive when the last workspace buffer is closed."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-snippet t
  "Enable/disable snippet completion support."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-auto-require-clients t
  "Auto require lsp-clients."
  :group 'lsp-mode
  :type 'boolean)

(defvar-local lsp--cur-workspace nil)

(defvar lsp--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defvar-local lsp-buffer-uri nil
  "If set, return it instead of calculating it using `buffer-file-name'.")

(define-error 'lsp-error "Unknown lsp-mode error")
(define-error 'lsp-empty-response-error
  "Empty response from the language server" 'lsp-error)
(define-error 'lsp-timed-out-error
  "Timed out while waiting for a response from the language server" 'lsp-error)
(define-error 'lsp-capability-not-supported
  "Capability not supported by the language server" 'lsp-error)

(defcustom lsp-auto-guess-root nil
  "Automatically guess the project root using projectile/project."
  :group 'lsp
  :type 'bool)

(defcustom lsp-restart 'interactive
  "Defines how server exited event must be handled."
  :group 'lsp
  :type '(choice (const interactive)
                 (const auto-restart)
                 (const ignore)))

(defcustom lsp-session-file (expand-file-name (locate-user-emacs-file ".lsp-session-v1"))
  "Automatically guess the project root using projectile/project."
  :group 'lsp
  :type 'file)

(defcustom lsp-auto-configure t
  "Auto configure `lsp-mode'.
When set to t `lsp-mode' will auto-configure `lsp-ui' and `company-lsp'."
  :group 'lsp
  :type 'bool)

(defvar lsp-clients (make-hash-table :test 'eql)
  "Hash table server-id -> client.
It contains all of the clients that are currently regitered.")

(defvar lsp-last-id 0
  "Last request id.")

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized for a new workspace."
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

(defcustom lsp-before-uninitialized-hook nil
  "List of functions to be called before a Language Server has been uninitialized."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--sync-methods
  '((0 . none)
    (1 . full)
    (2 . incremental)))

(defvar-local lsp--server-sync-method nil
  "Sync method recommended by the server.")

(defgroup lsp-mode nil
  "Customization group for ‘lsp-mode’."
  :group 'tools)

(defgroup lsp-faces nil
  "Faces for ‘lsp-mode’."
  :group 'lsp-mode)

(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." 'none)
                 (const :tag "Documents are synced by always sending the full content of the document." 'full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." 'incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

(defcustom lsp-auto-execute-action t
  "Auto-execute single action."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-enable-hover t
  "If non-nil, eldoc will display hover info when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-enable-signature-help t
  "If non-nil, eldoc will display signature help when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-prefer-signature-help t
  "If non-nil, eldoc will display signature help when both hover and signature help are present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-render-all nil
  "Define whether all of the returned by document/onHover will be displayed.
If `lsp-markup-display-all' is set to nil `eldoc' will show only
the symbol information."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-completion-at-point t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the language server."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-on-type-formatting t
  "Enable `textDocument/onTypeFormatting' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-before-save-edits t
  "If non-nil, `lsp-mode' will apply edits suggested by the language server before saving a document."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received."
  :type 'hook
  :group 'lsp-mode)

(defconst lsp--sync-type
  `((0 . "None")
    (1 . "Full Document")
    (2 . "Incremental Changes")))

(defcustom lsp-workspace-folders-changed-hook nil
  "Hooks to run after the folders has changed.
The hook will receive two parameters list of added and removed folders."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-on-hover-hook nil
  "The hooks that run after on hover and signature information has been loaded.
The hook is called with two params: the signature information and hover data."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-eldoc-hook '(lsp-document-highlight lsp-hover)
  "Hooks to run for eldoc."
  :type 'hook
  :group 'lsp-mode)

(defgroup lsp-imenu nil
  "Customization group for `lsp-imenu'."
  :group 'lsp-mode)

(defcustom lsp-imenu-show-container-name t
  "Display the symbol's container name in an imenu entry."
  :type 'boolean
  :group 'lsp-imenu)

(defcustom lsp-imenu-container-name-separator "/"
  "Separator string to use to separate the container name from the symbol while displaying imenu entries."
  :type 'string
  :group 'lsp-imenu)

(defcustom lsp-imenu-sort-methods '(kind name)
  "How to sort the imenu items.

The value is a list of `kind' `name' or `position'. Priorities
are determined by the index of the element."
  :type '(repeat (choice (const name)
                         (const position)
                         (const kind))))

;; vibhavp: Should we use a lower value (5)?
(defcustom lsp-response-timeout 10
  "Number of seconds to wait for a response from the language server before timing out."
  :type 'number
  :group 'lsp-mode)

(defconst lsp--imenu-compare-function-alist
  (list (cons 'name #'lsp--imenu-compare-name)
        (cons 'kind #'lsp--imenu-compare-kind)
        (cons 'position #'lsp--imenu-compare-position))
  "An alist of (METHOD . FUNCTION).
METHOD is one of the symbols accepted by
`lsp-imenu-sort-methods'.

FUNCTION takes two hash tables representing DocumentSymbol. It
returns a negative number, 0, or a positive number indicating
whether the first parameter is less than, equal to, or greater
than the second parameter.")

(defcustom lsp-prefer-flymake t
  "Auto-configure to prefer `flymake' over `lsp-ui' if both are present."
  :type 'boolean
  :group 'lsp-mode)

(defvar-local lsp--flymake-report-fn nil)
(defvar-local lsp--flymake-report-pending nil)

(defvar lsp-language-id-configuration '((java-mode . "java")
                                        (python-mode . "python")
                                        (gfm-view-mode . "markdown")
                                        (rust-mode . "rust")
                                        (css-mode . "css")
                                        (less-mode . "less")
                                        (sass-mode . "sass")
                                        (scss-mode . "scss")
                                        (xml-mode . "xml")
                                        (c-mode . "c")
                                        (c++-mode . "cpp")
                                        (objc-mode . "objective-c")
                                        (web-mode . "html")
                                        (html-mode . "html")
                                        (sgml-mode . "html")
                                        (mhtml-mode . "html")
                                        (go-mode . "go")
                                        (haskell-mode . "haskell")
                                        (php-mode . "php")
                                        (json-mode . "json")
                                        (rjsx-mode . "javascript")
                                        (js2-mode . "javascript")
                                        (typescript-mode . "typescript")
                                        (reason-mode . "reason")
                                        (caml-mode . "ocaml")
                                        (tuareg-mode . "ocaml")
                                        (swift-mode . "swift")
                                        (elixir-mode . "elixir")
                                        (f90-mode . "fortran"))
  "Language id configuration.")

(defvar lsp-method-requirements
  '(("textDocument/onTypeFormatting" :capability "documentOnTypeFormattingProvider")
    ("workspace/executeCommand"
     :capability "executeCommandProvider"
     :registered-capability "workspace/executeCommand")
    ("textDocument/hover" :capability "hoverProvider")
    ("textDocument/documentSymbol" :capability "documentSymbolProvider")
    ("textDocument/documentHighlight" :capability "documentHighlightProvider")
    ("textDocument/definition" :capability "definitionProvider")
    ("workspace/symbol" :capability "workspaceSymbolProvider"))

  "Contain method to requirements mapping.
It is used by send request functions to determine which server
must be used for handling a particular message.")

(defconst lsp--file-change-type
  `((created . 1)
    (changed . 2)
    (deleted . 3)))

(defface lsp-face-highlight-textual
  '((t :inherit highlight))
  "Face used for textual occurances of symbols."
  :group 'lsp-faces)

(defface lsp-face-highlight-read
  '((t :inherit highlight :underline t))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-face-highlight-write
  '((t :inherit highlight :italic t))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(defcustom lsp-lens-check-interval 0.1
  "The interval for checking for changes in the buffer state."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-lens-debounce-interval 0.7
  "Debounce interval for loading lenses."
  :group 'lsp-mode
  :type 'boolean)

(defface lsp-lens-mouse-face
  '((t :height 0.8 :inherit link))
  "The face used for code lens overlays."
  :group'lsp-mode)

(defface lsp-lens-face
  '((t :height 0.8 :inherit shadow))
  "The face used for code lens overlays."
  :group 'lsp-mode)

(defvar-local lsp--lens-overlays nil
  "Current lenses.")

(defvar-local lsp--lens-modified-tick 0
  "The tick last time the lenses where modified.")

(defvar-local lsp--lens-page nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar lsp-lens-backends '(lsp-lens-backend)
  "Backends providing lenses.")

(defvar-local lsp--lens-refresh-timer nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-idle-timer  nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-data nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-backend-cache nil)

(defvar-local lsp--buffer-workspaces ()
  "List of the buffer workspaces.")

(defvar lsp--session nil
  "Contain the `lsp-session' for the current Emacs instance.")

(defvar lsp--tcp-port 10000)

(cl-defgeneric lsp-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(defun lsp--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

(defun lsp--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'warning) (apply #'format format args)))

(defun lsp--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'error) (apply #'format format args)))

(defun lsp--eldoc-message (&optional msg)
  "Show MSG in eldoc."
  (run-with-idle-timer 0 nil (lambda () (eldoc-message msg))))

(defun lsp-message (format &rest args)
  "Wrapper over `message' which preserves the `eldoc-message'.
FORMAT with ARGS are the original message formats."
  (let ((inhibit-message lsp-inhibit-message)
        (last eldoc-last-message))
    (apply #'message format args)

    (when lsp-inhibit-message
      (lsp--eldoc-message last))))

(defalias 'lsp-ht 'ht)

(defun lsp--merge-results (results method)
  "Merge RESULTS by filtering the empty hash-tables and merging the lists.
METHOD is the executed method so the results could be merged
depending on it."
  (let ((results (-filter 'identity results)))
    (pcase method
      ("textDocument/hover" (let ((results (--filter (not (hash-table-empty-p it)) results)))
                              (if (not (cdr results))
                                  (car results)
                                (let ((merged (make-hash-table :test 'equal)))
                                  (--each results
                                    (let ((to-add (gethash "contents" it)))
                                      (puthash "contents" (append (if (sequencep to-add)
                                                                      to-add
                                                                    (list to-add))
                                                                  (gethash "contents" merged))
                                               merged)))
                                  merged))))
      (_ (if (not (cdr results))
             (car results)
           (apply 'append (--map (if (or (listp it) (vectorp it))
                                     it
                                   (list it))
                                 results)))))))
(defun lsp--spinner-start ()
  "Start spinner indication."
  (condition-case _err (spinner-start 'progress-bar-filled) (error)))

(defun lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp-workspaces ()
  "Return the lsp workspaces associated with the current project."
  (if lsp--cur-workspace (list lsp--cur-workspace) lsp--buffer-workspaces))

(defun lsp--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide tranformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((result (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt (-map 'first result)
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion result))))

(cl-defstruct lsp--parser
  (waiting-for-response nil)
  (response-result nil)
  (headers '()) ;; alist of headers
  (body nil) ;; message body
  (reading-body nil) ;; If non-nil, reading body
  (body-length nil) ;; length of current message body
  (body-received 0) ;; amount of current message body currently stored in 'body'
  (leftovers nil) ;; Leftover data from previous chunk; to be processed
  (queued-notifications nil) ;; Unused field
  (queued-requests nil)
  (workspace nil))

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

  ;; ‘add-on?’ when set to t the server will be started no matter whether there
  ;; is another server hadling the same mode.
  (add-on? nil :read-only t)
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

  ;; Use the native JSON API in Emacs 27 and above. If non-nil, JSON arrays will
  ;; be parsed as vectors.
  (use-native-json nil)

  ;; major modes supported by the client.
  (major-modes)
  ;; Function that will be called to decide if this language client
  ;; should manage a particular buffer. The function will be passed
  ;; the file name and major mode to inform the decision. Setting
  ;; `activation-fn' will override `major-modes' and `remote?', if
  ;; present.
  (activation-fn)
  ;; Break the tie when major-mode is supported by multiple clients.
  (priority 0)
  ;; Unique identifier for
  (server-id)
  ;; defines whether the client supports multi root workspaces.
  (multi-root)
  ;; Initialization options or a function that returns initialization options.
  (initialization-options)
  ;; Function which returns the folders that are considered to be not projects but library files.
  ;; The function accepts one parameter currently active workspace.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/225.
  (library-folders-fn)
  ;; function which will be called when opening file in the workspace to perfom
  ;; client specific initialization. The function accepts one parameter
  ;; currently active workspace.
  (before-file-open-fn)
  ;; Function which will be called right after a workspace has been intialized.
  (initialized-fn)
  ;; ‘remote?’ indicate whether the client can be used for LSP server over TRAMP.
  (remote? nil))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (-let [(&hash "line" "character") params]
    (save-excursion
      (save-restriction
        (condition-case _err
            (progn
              (widen)
              (goto-char (point-min))
              (forward-line line)
              (forward-char character)
              (point))
          (error (point)))))))

(defun lsp--range-to-region (range)
  (cons (lsp--position-to-point (gethash "start" range))
        (lsp--position-to-point (gethash "end" range))))

(defun lsp-warn (message &rest args)
  "Display a warning message made from (`format-message' MESSAGE ARGS...).
This is equivalent to `display-warning', using `lsp-mode' as the type and
`:warning' as the level."
  (display-warning 'lsp-mode (apply #'format-message message args)))

(defun lsp--get-uri-handler (scheme)
  "Get uri handler for SCHEME in the current workspace."
  (--some (gethash scheme (lsp--client-uri-handlers (lsp--workspace-client it)))
          (or (lsp-workspaces) (lsp--session-workspaces (lsp-session)))))

(defun lsp--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp--uri-to-path (uri)
  "Convert URI to a file path."
  (let* ((url (url-generic-parse-url (url-unhex-string uri)))
         (type (url-type url))
         (file (url-filename url))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          (error "Unsupported file scheme: %s" uri))
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))

    (lsp--fix-path-casing
     (concat (-some 'lsp--workspace-host-root (lsp-workspaces)) file-name))))

(defun lsp--buffer-uri ()
  "Return URI of the current buffer."
  (or lsp-buffer-uri
      (lsp--path-to-uri
       (or buffer-file-name (ignore-errors (buffer-file-name (buffer-base-buffer)))))))

(defun lsp-register-client-capabilities (&rest _args)
  "Implemented only to make `company-lsp' happy.
DELETE when `lsp-mode.el' is deleted.")

(defun lsp--path-to-uri (path)
  "Convert PATH to a uri."
  (concat lsp--uri-file-prefix
          (url-hexify-string (file-truename (or (file-remote-p path 'localname t) path))
                             url-path-allowed-chars)))

(defun lsp--string-match-any (regex-list str)
  "Given a list of REGEX-LIST and STR return the first matching regex if any."
  (--first (string-match it str) regex-list))

(defun lsp-create-watch (dir file-regexp-list callback &optional watches root-dir)
  "Create recursive file notificaton watch in DIR monitoring FILE-REGEXP-LIST.
CALLBACK is the will be called when there are changes in any of
the monitored files. WATCHES is a hash table directory->file
notification handle which contains all of the watches that
already have been created."
  (let ((all-dirs (->> (directory-files-recursively dir ".*" t)
                       (seq-filter (lambda (f) (file-directory-p f)))
                       (list* dir)))
        (watches (or watches (make-hash-table :test 'equal)))
        (root-dir (or root-dir dir)))
    (seq-do
     (lambda (dir-to-watch)
       (puthash
        dir-to-watch
        (file-notify-add-watch
         dir-to-watch
         '(change)
         (lambda (event)
           (let ((file-name (caddr event))
                 (event-type (cadr event)))
             (cond
              ((and (file-directory-p file-name)
                    (equal 'created event-type))

               (lsp-create-watch file-name file-regexp-list callback watches root-dir)

               ;; process the files that are already present in
               ;; the directory.
               (->> (directory-files-recursively file-name ".*" t)
                    (seq-do (lambda (f)
                              (when (and (lsp--string-match-any
                                          file-regexp-list
                                          (concat "/" (file-relative-name f root-dir)))
                                         (not (file-directory-p f)))
                                (funcall callback (list nil 'created f)))))))
              ((and (not (file-directory-p file-name))
                    (lsp--string-match-any
                     file-regexp-list
                     (concat "/" (file-relative-name file-name root-dir))))
               (funcall callback event))))))
        watches))
     all-dirs)
    watches))

(defun lsp-kill-watch (watches)
  "Delete WATCHES."
  (->> watches hash-table-values (-map 'file-notify-rm-watch)))

(defmacro with-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--cur-workspace ,workspace)) ,@body))

(defmacro with-lsp-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against multiple WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--buffer-workspaces ,workspaces)) ,@body))

(defmacro lsp-foreach-workspace (&rest body)
  "Execute BODY for each of the current workspaces."
  (declare (debug (form body)))
  `(--map (with-lsp-workspace it ,@body) (lsp-workspaces)))

(defmacro when-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context if present."
  (declare (debug (form body))
           (indent 1))
  `(when-let (lsp--cur-workspace ,workspace) ,@body))

(defun lsp--window-show-message (workspace params)
  "Send the server's messages to message.
Inhibit if `lsp-inhibit-message' is set,or the message matches
one of this client's :ignore-messages. PARAMS - the data sent
from WORKSPACE."
  (let* ((inhibit-message (or inhibit-message lsp-inhibit-message))
         (message (gethash "message" params))
         (client (lsp--workspace-client workspace)))
    (when (or (not client)
              (cl-notany (lambda (r) (string-match-p r message))
                         (lsp--client-ignore-messages client)))
      (lsp-message (lsp--propertize message (gethash "type" params))))))

(defun lsp--window-show-message-request (params)
  "Display a message request to the user and send the user's selection back to the server."
  (let* ((type (gethash "type" params))
         (message (lsp--propertize (gethash "message" params) type))
         (choices (mapcar (lambda (choice) (gethash "title" choice))
                          (gethash "actions" params))))
    (if choices
        (completing-read (concat message " ") choices nil t)
      (lsp-message message))))

(defun lsp-diagnostics ()
  "Return the diagnostics from all workspaces."
  (let ((result (make-hash-table :test 'equal)))
    (-> (lsp-session)
        (lsp--session-workspaces)
        (--each (maphash
                 (lambda (file-name diagnostics)
                   (puthash file-name
                            (append (gethash file-name result) diagnostics)
                            result))
                 (lsp--workspace-diagnostics it))))
    result))

(cl-defstruct lsp-diagnostic
  (range nil :read-only t)
  ;; range has the form (:start (:line N :column N) :end (:line N :column N) )
  ;; where N are zero-indexed numbers
  (line nil :read-only t)
  (column nil :read-only t)
  (severity nil :read-only t) ;; 1 - error, 2 - warning, 3 - information, 4 - hint
  (code nil :read-only t) ;; the diagnostic's code
  (source nil :read-only t) ;;
  (message nil :read-only t) ;; diagnostic's message
  (original nil :read-only t))

(defun lsp--make-diag (diag)
  "Make a `lsp-diagnostic' from DIAG."
  (-let* (((&hash "message" "code" "source" "severity"
                  "range" (&hash "start" (&hash "line"      start-line
                                                "character" start-character)
                                 "end"   (&hash "line"      end-line
                                                "character" end-character))) diag))
    (make-lsp-diagnostic
     :range (list :start (list :line start-line :column start-character)
                  :end   (list :line end-line   :column end-character))
     :line start-line
     :column start-character
     :severity severity
     :code code
     :source source
     :message (if source (format "%s: %s" source message) message)
     :original diag)))

(defun lsp--on-diagnostics (workspace params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}
PARAMS contains the diagnostics data.
WORKSPACE is the workspace that contains the diagnostics."
  (let* ((file (lsp--uri-to-path (gethash "uri" params)))
         (diagnostics (gethash "diagnostics" params))
         (buffer (find-buffer-visiting file))
         (workspace-diagnostics (lsp--workspace-diagnostics workspace)))

    (if diagnostics
        (when (or lsp-report-if-no-buffer buffer)
          (puthash file (mapcar #'lsp--make-diag diagnostics) workspace-diagnostics))
      (remhash file workspace-diagnostics))

    (when buffer
      (with-current-buffer buffer
        (run-hooks 'lsp-after-diagnostics-hook)))))

(with-eval-after-load 'flymake
  ;; (put 'lsp-note 'flymake-category 'flymake-note)
  ;; (put 'lsp-warning 'flymake-category 'flymake-warning)
  ;; (put 'lsp-error 'flymake-category 'flymake-error)

  (defun lsp--flymake-setup()
    "Setup flymake."
    (flymake-mode 1)
    (add-hook 'flymake-diagnostic-functions 'lsp--flymake-backend nil t)
    (add-hook 'lsp-after-diagnostics-hook 'lsp--flymake-after-diagnostics nil t))

  (defun lsp--flymake-after-diagnostics ()
    "Handler for `lsp-after-diagnostics-hook'"
    (when lsp--flymake-report-fn
      (lsp--flymake-backend lsp--flymake-report-fn)
      (remove-hook 'lsp-after-diagnostics-hook 'lsp--flymake-after-diagnostics t)))

  (defun lsp--flymake-backend (report-fn &rest _args)
    "Flymake backend."
    (setq lsp--flymake-report-fn report-fn)
    (funcall report-fn
             (-some->> (lsp-diagnostics)
                       (gethash buffer-file-name)
                       (--map (-let* (((&hash "message" "severity" "range") (lsp-diagnostic-original it))
                                      ((start . end) (lsp--range-to-region range)))
                                (when (= start end)
                                  (-let* (((&hash "line" "character") (gethash "start" range))
                                          (region (flymake-diag-region (current-buffer) (1+ line) character)))
                                    (setq start (car region) end (cdr region))))
                                (flymake-make-diagnostic (current-buffer)
                                                         start
                                                         end
                                                         (case severity
                                                           (1 :error)
                                                           (2 :warning)
                                                           (_ :note))
                                                         message)))))))

(defun lsp--ht-get (tbl &rest keys)
  "Get nested KEYS in TBL."
  (let ((val tbl))
    (while (and keys val)
      (setq val (ht-get val (first keys)))
      (setq keys (rest keys)))
    val))

;; lenses support

(defun lsp--lens-text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line."
  ;; `current-column' takes prettification into account
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun lsp--lens-update (ov)
  "Redraw quick-peek overlay OV."
  (let ((offset (lsp--lens-text-width (save-excursion
                                          (beginning-of-visual-line)
                                          (point))
                                        (save-excursion
                                          (beginning-of-line-text)
                                          (point)))))
    (save-excursion
      (goto-char (overlay-start ov))
      (overlay-put ov
                   'before-string
                   (concat (make-string offset ?\s)
                           (overlay-get ov 'lsp--lens-contents)
                           "\n")))))

(defun lsp--lens-overlay-ensure-at (pos)
  "Find or create a lens for the line at POS."
  (or (car (cl-remove-if-not (lambda (ov) (lsp--lens-overlay-matches-pos ov pos)) lsp--lens-overlays))
      (let* ((ov (save-excursion
                   (goto-char pos)
                   (make-overlay (point-at-bol) (1+ (point-at-eol))))))
        (overlay-put ov 'lsp-lens t)
        ov)))

(defun lsp--lens-show (str pos)
  "Show STR in an inline window at POS."
  (let ((ov (lsp--lens-overlay-ensure-at pos)))
    (save-excursion
      (goto-char pos)
      (setf (overlay-get ov 'lsp--lens-contents) str)
      (lsp--lens-update ov))
    ov))

(defun lsp--lens-overlay-matches-pos (ov pos)
  "Check if OV is a lens covering POS."
  (and (overlay-get ov 'lsp-lens)
       (<= (overlay-start ov) pos)
       (< pos (overlay-end ov))))

(defun lsp--lens-idle-function (&optional buffer)
  "Create idle function for buffer BUFFER."
  (when (or (not buffer) (eq (current-buffer) buffer))
    (cond
     ((/= (buffer-modified-tick) lsp--lens-modified-tick)
      (lsp--lens-schedule-refresh t))

     ((not (equal (cons (window-start) (window-end)) lsp--lens-page))
      (lsp--lens-schedule-refresh nil)))))

(defun lsp--lens-schedule-refresh (buffer-modified?)
  "Call each of the backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (-some-> lsp--lens-refresh-timer cancel-timer)

  (setq-local lsp--lens-modified-tick (buffer-modified-tick))
  (setq-local lsp--lens-page (cons (window-start) (window-end)))
  (setq-local lsp--lens-refresh-timer
              (run-with-timer lsp-lens-debounce-interval nil 'lsp--lens-refresh buffer-modified?)))

(defun lsp--lens-keymap (command)
  (let ((map (make-sparse-keymap))
        (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))))
    (define-key map [mouse-1]
      (lambda ()
        (interactive)
        (lsp-execute-command server-id
                             (intern (gethash "command" command))
                             (gethash "arguments" command))))
    map))

(defun lsp--lens-display (lenses)
  "Show LENSES."
  (let ((overlays
         (->> lenses
              (--filter (gethash "command" it))
              (--group-by (lsp--ht-get it "range" "start" "line"))
              (-map
               (-lambda ((_ . lenses))
                 (let ((sorted (--sort (< (lsp--ht-get it "range" "start" "character")
                                          (lsp--ht-get other "range" "start" "character"))
                                       lenses)))
                   (list (lsp--position-to-point (lsp--ht-get (first sorted) "range" "start"))
                         (s-join (propertize "|" 'face 'lsp-lens-face)
                                 (-map
                                  (-lambda ((lens &as &hash "command" (command &as &hash "title")))
                                    (propertize
                                     title
                                     'face 'lsp-lens-face
                                     'mouse-face 'lsp-lens-mouse-face
                                     'local-map (lsp--lens-keymap command)))
                                  sorted))))))
              (-map (-lambda ((position str))
                      (lsp--lens-show str position))))))
    (--each lsp--lens-overlays
      (unless (-contains? overlays it)
        (delete-overlay it)))
    (setq-local lsp--lens-overlays overlays)))

(defun lsp--lens-refresh (buffer-modified?)
  "Refresh lenses using lenses backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (setq-local lsp--lens-modified-tick (buffer-modified-tick))
  (dolist (backend lsp-lens-backends)
    (funcall backend buffer-modified?
             (lambda (lenses)
               (lsp--process-lenses backend lenses)))))

(defun lsp--process-lenses (backend lenses)
  "Process LENSES originated from BACKEND."
  (setq-local lsp--lens-data (or lsp--lens-data (make-hash-table)))
  (puthash backend lenses lsp--lens-data)
  (lsp--lens-display (-flatten (ht-values lsp--lens-data))))

(defun lsp-lens-show ()
  "Display lenses in the buffer."
  (interactive)
  (->> (lsp-request "textDocument/codeLens"
                    `(:textDocument (:uri ,(lsp--path-to-uri buffer-file-name))))
       (--map (if (gethash "command" it)
                  it
                (lsp-request "codeLens/resolve" it)))
       lsp--lens-display))

(defun lsp-lens-hide ()
  "Delete all lenses."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (mapc 'delete-overlay lsp--lens-overlays)
    (setq-local lsp--lens-overlays nil)))

(defun lsp--lens-backend-not-loaded? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command" "pending") lens))
    (and (< (window-start) (lsp--position-to-point start) (window-end))
         (not command)
         (not pending))))

(defun lsp--lens-backend-present? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command") lens))
    (or command
        (not (< (window-start) (lsp--position-to-point start) (window-end))))))

(defun lsp--lens-backend-fetch-missing (lenses tick callback)
  "Fetch LENSES without command in for the current window.

TICK is the buffer modified tick. If it does not match
`buffer-modified-tick' at the time of receiving the updates the
updates must be discarded..
CALLBACK - the callback for the lenses."
  (--each (-filter #'lsp--lens-backend-not-loaded? lenses)
    (puthash "pending" t it)
    (lsp-request-async "codeLens/resolve" it
                       (lambda (lens)
                         (when (= tick (buffer-modified-tick))
                           (remhash "pending" it)
                           (puthash "command" (gethash "command" lens) it)
                           (when (-all? #'lsp--lens-backend-present?  lenses)
                             (funcall callback lenses))))
                       :mode 'detached)))

(defun lsp-lens-backend (modified? callback)
  "Lenses backend using `textDocument/codeLens'.
MODIFIED? - t when buffer is modified since the last invocation.
CALLBACK - callback for the lenses."
  (when (lsp--find-workspaces-for "textDocument/codeLens")
    (if modified?
        (let ((tick lsp--lens-modified-tick))
          (setq-local lsp--lens-backend-cache nil)
          (lsp-request-async "textDocument/codeLens"
                             `(:textDocument (:uri ,(lsp--path-to-uri buffer-file-name)))
                             (lambda (lenses)
                               (when (= tick (buffer-modified-tick))
                                 (setq-local lsp--lens-backend-cache lenses)
                                 (if (--every? (gethash "command" it) lenses)
                                     (funcall callback lenses)
                                   (lsp--lens-backend-fetch-missing lenses tick callback))))
                             :mode 'detached))
      (if (-all? #'lsp--lens-backend-present? lsp--lens-backend-cache)
          (funcall callback lsp--lens-backend-cache)
        (lsp--lens-backend-fetch-missing lsp--lens-backend-cache lsp--lens-modified-tick callback)))))

(defun lsp--lens-stop-timer ()
  "Stop `lsp--lens-idle-timer'."
  (-some-> lsp--lens-idle-timer cancel-timer)
  (setq-local lsp--lens-idle-timer nil))

(define-minor-mode lsp-lens-mode
  "toggle code-lens overlays"
  :group 'lsp-mode
  :global nil
  :init-value nil
  :lighter "Lens"
  (cond
   (lsp-lens-mode
    (setq-local lsp--lens-idle-timer (run-with-idle-timer
                                      lsp-lens-check-interval t #'lsp--lens-idle-function (current-buffer)))
    (lsp--lens-refresh t)
    (add-hook 'kill-buffer-hook #'lsp--lens-stop-timer nil t)
    (add-hook 'after-save-hook #'lsp--lens-idle-function nil t))
   (t
    (lsp--lens-stop-timer)
    (lsp-lens-hide)
    (remove-hook 'kill-buffer-hook #'lsp--lens-stop-timer t)
    (remove-hook 'after-save-hook #'lsp--lens-idle-function t))))



(define-minor-mode lsp-mode ""
  nil nil nil
  :lighter (:eval (lsp-mode-line))
  :group 'lsp-mode)

(defun lsp-mode-line ()
  "Construct the mode line text."
  (if-let (workspaces (lsp-workspaces))
      (concat " LSP" (string-join (--map (format "[%s]" (lsp--workspace-print it))
                                         workspaces)))
    (concat " LSP" (propertize "[Disconnected]" 'face 'warning))))

(defalias 'make-lsp-client 'make-lsp--client)

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

  ;; ‘host-root’ contains the host root info as derived from `file-remote-p'. It
  ;; used to deriver the file path in `lsp--uri-to-path' when using tramp
  ;; connection.
  (host-root nil)

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
  (watches (make-hash-table :test 'equal))

  ;; list of workspace folders
  (workspace-folders nil)

  ;; ‘last-id’ the last request id for the current workspace.
  (last-id 0)

  ;; ‘status-string’ allows extensions to specify custom status string based on
  ;; the Language Server specific messages.
  (status-string nil)

  ;; ‘shutdown-action’ flag used to mark that workspace should not be restarted(e. g. it
  ;; was stopped).
  (shutdown-action)

  ;; ‘diagnostics’ a hashmap with workspace diagnostics.
  (diagnostics (make-hash-table :test 'equal)))

(cl-defstruct lsp-session
  ;; contains the folders that are part of the current session
  (folders)
  ;; contains the folders that must not be imported in the current workspace.
  (folders-blacklist)
  ;; contains the list of folders that must be imported in a project in case of
  ;; multi root LSP server.
  (server-id->folders (make-hash-table :test 'equal) :read-only t)
  ;; folder to list of the servers that are associated with the folder.
  (folder->servers (make-hash-table :test 'equal) :read-only t)

  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal)))

(defun lsp-workspace-status (status-string &optional workspace)
  "Set current workspace status to STATUS-STRING.
If WORKSPACE is not specified defaults to lsp--cur-workspace."
  (setf (lsp--workspace-status-string (or workspace lsp--cur-workspace)) status-string))

(defun lsp-session-set-metadata (key value &optional _workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-set-metadata 'lsp-session-set-metadata)

(defun lsp-session-get-metadata (key &optional _workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-get-metadata 'lsp-session-get-metadata)

(define-inline lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (inline-quote
   (progn (cl-check-type ,method string)
          (list :jsonrpc "2.0" :method ,method :params ,params))))

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defalias 'lsp-make-request 'lsp--make-request)

(defun lsp--make-response (request result)
  "Create reponse for REQUEST with RESULT."
  `(:jsonrpc "2.0" :id ,(gethash "id" request) :result ,result))

(defun lsp-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (lsp--cur-workspace-check)
  (let* ((json-encoding-pretty-print lsp-print-io)
         (json-false :json-false)
         (client (lsp--workspace-client lsp--cur-workspace))
         (body (if (and (lsp--client-use-native-json client)
                        (fboundp 'json-serialize))
                   (json-serialize params :null-object nil
                                   :false-object json-false)
                 (json-encode params))))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (lsp-foreach-workspace
   (lsp--send-no-wait (lsp--make-message body)
                      (lsp--workspace-proc lsp--cur-workspace))))

(defalias 'lsp-send-notification 'lsp--send-notification)

(defun lsp-notify (method params)
  (lsp--send-notification (lsp--make-notification method params)))

(defun lsp--cur-workspace-check ()
  "Check whether buffer lsp workspace(s) are set."
  (cl-assert (lsp-workspaces) nil
             "No language server(s) is associated with this buffer."))

(defun lsp--send-request (body &optional no-wait)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response."
  (let ((method (plist-get body :method)))
    (if-let ((target-workspaces (lsp--find-workspaces-for body)))
        (lsp--merge-results
         (--map
          (with-lsp-workspace it
            (let* ((body (plist-put body :id (cl-incf lsp-last-id)))
                   (parser (lsp--workspace-parser lsp--cur-workspace))
                   (message (lsp--make-message body))
                   (process (lsp--workspace-proc lsp--cur-workspace)))
              (setf (lsp--parser-waiting-for-response parser) (not no-wait))
              (if no-wait
                  (lsp--send-no-wait message process)
                (lsp--send-wait message process parser))
              (when (not no-wait)
                (prog1 (lsp--parser-response-result parser)
                  (setf (lsp--parser-response-result parser) nil)))))
          target-workspaces)
         method)
      (error "No workspace could handle %s" method))))

(defalias 'lsp-send-request 'lsp--send-request
  "Send BODY as a request to the language server and return the response synchronously.
\n(fn BODY)")

(cl-defun lsp-request (method params &key no-wait)
  (lsp--send-request `(:jsonrpc "2.0" :method ,method :params ,params) no-wait))

(cl-defun lsp-request-async (method params callback &key mode)
  "Send request METHOD with PARAMS."
  (lsp--send-request-async `(:jsonrpc "2.0" :method ,method :params ,params) callback mode))

(defun lsp--create-async-callback-wrapper (count callback mode method)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method."
  (let ((buf (current-buffer))
        results)
    (pcase mode
      ('detached (lambda (result)
                   (push result results)
                   (when (and (eq (length results) count))
                     (funcall callback (lsp--merge-results results method)))))
      ('alive (lambda (result)
                (push result results)
                (when (and (eq (length results) count)
                           (buffer-live-p buf))
                  (with-current-buffer buf
                    (funcall callback (lsp--merge-results results method))))))
      (_ (lambda (result)
           (push result results)
           (when (and (eq (length results) count)
                      (buffer-live-p buf)
                      (eq buf (current-buffer)))
             (funcall callback (lsp--merge-results results method))))))))

(defun lsp--send-request-async (body callback &optional mode)
  "Send BODY as a request to the language server.
Call CALLBACK with the response recevied from the server
asynchronously. MODE determines when the callback will be called
depending on the condition of the original buffer."
  (if-let ((target-workspaces (lsp--find-workspaces-for body)))
      (let* ((async-callback (lsp--create-async-callback-wrapper
                              (length target-workspaces) callback mode (plist-get body :method)))
             (id (cl-incf lsp-last-id))
             (body (plist-put body :id id)))
        (--each target-workspaces
          (with-lsp-workspace it
            (let ((message (lsp--make-message body)))
              (puthash id async-callback
                       (-> lsp--cur-workspace
                           lsp--workspace-client
                           lsp--client-response-handlers))
              (lsp--send-no-wait message (lsp--workspace-proc lsp--cur-workspace)))))
        body)
    (error "No workspace could handle %s" (plist-get body :method))))

(defalias 'lsp-send-request-async 'lsp--send-request-async)

(defun lsp--cur-file-version ()
  "Return the file version number."
  (gethash (current-buffer) (lsp--workspace-file-versions (first (lsp-workspaces)))))

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  "Unload working workspaces."
  (lsp-foreach-workspace (lsp--shutdown-workspace)))

(defun lsp--shutdown-workspace ()
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (lsp-request "shutdown" nil :no-wait t)
    (lsp-notify "exit" nil))
  (lsp--uninitialize-workspace))

(defun lsp--uninitialize-workspace ()
  "Cleanup buffer state.
When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (run-hooks 'lsp-workspace-uninitialized-hook)
  (lsp-kill-watch (lsp--workspace-watches lsp--cur-workspace))

  (let ((proc (lsp--workspace-cmd-proc lsp--cur-workspace)))
    (when (process-live-p proc)
      (kill-process proc))
    (unless (lsp-workspaces)
      (lsp--managed-mode -1))))

(defun lsp--client-capabilities ()
  "Return the client capabilites."
  `(:workspace (
                :applyEdit t
                :executeCommand (:dynamicRegistration :json-false)
                :workspaceFolders t)
               :textDocument (
                              :declaration (:linkSupport t)
                              :definition (:linkSupport t)
                              :implementation (:linkSupport t)
                              :typeDefinition (:linkSupport t)
                              :synchronization (:willSave t :didSave t :willSaveWaitUntil t)
                              :documentSymbol (:symbolKind (:valueSet ,(cl-coerce
                                                                        (cl-loop for kind from 1 to 25 collect kind)
                                                                        'vector))
                                                           :hierarchicalDocumentSymbolSupport t)
                              :formatting (:dynamicRegistration t)
                              :codeAction (:dynamicRegistration t)
                              :completion (:completionItem (:snippetSupport ,lsp-enable-snippet))
                              :signatureHelp (:signatureInformation (:parameterInformation (:labelOffsetSupport t))))))

(defun lsp--server-register-capability (reg)
  "Register capability REG."
  (lsp--cur-workspace-check)
  (-let (((&hash "method" "id" "registerOptions") reg))
    (push
     (make-lsp--registered-capability :id id :method method :options registerOptions)
     (lsp--workspace-registered-server-capabilities lsp--cur-workspace))))

(defun lsp--server-unregister-capability (unreg)
  "Unregister capability UNREG."
  (let* ((id (gethash "id" unreg))
         (fn (lambda (e) (equal (lsp--registered-capability-id e) id))))
    (setf (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
          (seq-remove fn
                      (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))))

(defun lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (->> (lsp-workspaces)
       (-map 'lsp--workspace-server-capabilities)
       (-filter 'identity)
       (apply 'ht-merge)))

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

(defun lsp--suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (car (project-roots project))))))

(defun lsp--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (with-demoted-errors "Failed to read file with message %S"
      (with-temp-buffer
        (insert-file-contents-literally file)
        (first (read-from-string
                (buffer-substring-no-properties (point-min) (point-max))))))))

(defun lsp--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME."
  (with-demoted-errors "Failed to persist file: %S"
    (with-temp-file file-name
      (erase-buffer)
      (insert (prin1-to-string to-persist)))))

(defun lsp-workspace-folders-add (project-root)
  "Add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
                              (or (lsp--suggest-project-root) default-directory) nil t)))
  (push project-root (lsp-session-folders (lsp-session))))

(defun lsp-workspace-folders-remove (project-root)
  "Remove PROJECT-ROOT to the list of workspace folders."
  (interactive (list (completing-read "Select folder to remove: "
                                      (lsp-session-folders (lsp-session)) nil t
                                      (lsp-find-session-folder (lsp-session) default-directory))))
  (lsp-notify "workspace/didChangeWorkspaceFolders"
              `(:event (:removed ,(vector (list :uri (lsp--path-to-uri project-root))))))

  ;; turn off servers in the removed directories
  (let* ((session (lsp-session))
         (folder->servers  (lsp-session-folder->servers session)))
    ;; turn off the servers in directories if they are not multifolder
    (-each (gethash project-root folder->servers)
      (lambda (workspace)
        (unless (lsp--client-multi-root (lsp--workspace-client workspace))
          (lsp--info "Shutdown %s since folder %s is removed..." (lsp--workspace-print workspace) project-root)
          (setf (lsp--workspace-shutdown-action workspace) 'shutdown)
          (with-lsp-workspace workspace (lsp--shutdown-workspace)))))

    (setf (lsp-session-folders session)
          (-remove-item project-root (lsp-session-folders session))))
  (run-hook-with-args 'lsp-workspace-folders-changed-hook nil (list project-root)))

(defun lsp-workspace-folders-switch()
  "Switch to another workspace folder from the current session."
  (interactive)
  (find-file (completing-read "Switch to folder: " (lsp-session-folders (lsp-session)) nil t)))

(define-minor-mode lsp--managed-mode
  "Mode for source buffers managed by lsp-mode."
  nil nil nil
  (cond
   (lsp--managed-mode
    (when (and lsp-enable-indentation
               (lsp--capability "documentRangeFormattingProvider"))
      (setq-local indent-region-function #'lsp-format-region))

    (add-function :before-until (local 'eldoc-documentation-function) #'lsp-eldoc-function)
    (eldoc-mode 1)
    (add-hook 'after-change-functions #'lsp-on-change nil t)
    (add-hook 'after-revert-hook #'lsp-on-revert nil t)
    (add-hook 'after-save-hook #'lsp-on-save nil t)
    (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
    (add-hook 'before-change-functions #'lsp-before-change nil t)
    (add-hook 'before-save-hook #'lsp--before-save nil t)
    (when (and lsp-enable-completion-at-point (lsp--capability "completionProvider"))
      (setq-local completion-at-point-functions nil)
      (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t))
    (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)
    (add-hook 'post-self-insert-hook #'lsp--on-self-insert nil t)
    (when lsp-enable-xref
      (add-hook 'xref-backend-functions #'lsp--xref-backend nil t)))
   (t
    (setq-local indent-region-function nil)
    (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)

    (remove-hook 'after-change-functions #'lsp-on-change t)
    (remove-hook 'after-revert-hook #'lsp-on-revert t)
    (remove-hook 'after-save-hook #'lsp-on-save t)
    (remove-hook 'auto-save-hook #'lsp--on-auto-save t)
    (remove-hook 'before-change-functions #'lsp-before-change t)
    (remove-hook 'before-save-hook #'lsp--before-save t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'kill-buffer-hook #'lsp--text-document-did-close t)
    (remove-hook 'post-self-insert-hook #'lsp--on-self-insert t)
    (remove-hook 'xref-backend-functions #'lsp--xref-backend t))))

(defun lsp--text-document-did-open ()
  "'document/didOpen event."
  (run-hooks 'lsp-before-open-hook)
  (puthash (current-buffer) 0 (lsp--workspace-file-versions lsp--cur-workspace))
  (pushnew (current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (alist-get major-mode lsp-language-id-configuration "")
               :version (lsp--cur-file-version)
               :text (buffer-substring-no-properties (point-min) (point-max)))))

  (lsp--managed-mode 1)

  (let* ((sync (gethash "textDocumentSync" (lsp--server-capabilities)))
         (kind (if (hash-table-p sync) (gethash "change" sync) sync)))
    (setq lsp--server-sync-method (or lsp-document-sync-method
                                      (alist-get kind lsp--sync-methods))))
  (run-hooks 'lsp-after-open-hook))

(define-inline lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (inline-quote (list :uri (lsp--buffer-uri))))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (plist-put (lsp--text-document-identifier)
             :version (lsp--cur-file-version)))

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

(defun lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  ;; make sure start and end are Position objects
  (list :start start :end end))

(define-inline lsp--region-to-range (start end)
  "Make Range object for the current region."
  (inline-quote (lsp--range (lsp--point-to-position ,start)
                            (lsp--point-to-position ,end))))

(defun lsp--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point-at-bol) (point-at-eol))))

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
      (when (or (null version) (equal version (lsp--cur-file-version)))
        (lsp--apply-text-edits (gethash "edits" edit))))))

(defun lsp--text-edit-sort-predicate (e1 e2)
  (let ((start1 (lsp--position-to-point (gethash "start" (gethash "range" e1))))
        (start2 (lsp--position-to-point (gethash "start" (gethash "range" e2)))))
    (if (= start1 start2)
        (let ((end1 (lsp--position-to-point (gethash "end" (gethash "range" e1))))
              (end2 (lsp--position-to-point (gethash "end" (gethash "range" e2)))))
          (> end1 end2))
      (> start1 start2))))

(defun lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object in EDITS."
  ;; We sort text edits so as to apply edits that modify latter parts of the
  ;; document first. Furthermore, because the LSP spec dictates that:
  ;; "If multiple inserts have the same position, the order in the array
  ;; defines which edit to apply first."
  ;; We reverse the initial list and sort stably to make sure the order among
  ;; edits with the same position is preserved.
  (atomic-change-group
    (mapc #'lsp--apply-text-edit
          (sort (nreverse edits) #'lsp--text-edit-sort-predicate))))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (-let* (((&hash "newText" "range") text-edit)
          ((start . end) (lsp--range-to-region range)))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert newText))))

(defun lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (gethash cap (or capabilities
                   (lsp--server-capabilities)
                   (make-hash-table))))

(defun lsp--registered-capability (method)
  "Check whether there is workspace providing METHOD."
  (--first
   (seq-find (lambda (reg)
               (equal (lsp--registered-capability-method reg) method))
             (lsp--workspace-registered-server-capabilities it))
   (lsp-workspaces)))

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

(defun lsp--bracketed-change-p (start _end length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (and (eq start (plist-get lsp--before-change-vals :start) )
       (eq length (- (plist-get lsp--before-change-vals :end)
                     (plist-get lsp--before-change-vals :start)))))

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
  (when (not revert-buffer-in-progress-p)
    (--map
     (with-lsp-workspace it
       (with-demoted-errors "Error in ‘lsp-on-change’: %S"
         (save-match-data
           ;; A (revert-buffer) call with the 'preserve-modes parameter (eg, as done
           ;; by auto-revert-mode) will cause this hander to get called with a nil
           ;; buffer-file-name. We need the buffer-file-name to send notifications;
           ;; so we skip handling revert-buffer-caused changes and instead handle
           ;; reverts separately in lsp-on-revert
           (cl-incf (gethash (current-buffer)
                             (lsp--workspace-file-versions lsp--cur-workspace)))
           (unless (eq lsp--server-sync-method 'none)
             (lsp-notify
              "textDocument/didChange"
              `(:textDocument
                ,(lsp--versioned-text-document-identifier)
                :contentChanges
                ,(pcase lsp--server-sync-method
                   ('incremental (vector (lsp--text-document-content-change-event
                                          start end length)))
                   ('full (vector (lsp--full-change-event))))))))))
     (lsp-workspaces))))

(defun lsp--on-self-insert ()
  "Self insert handling.
Applies on type formatting."
  (-when-let* ((provider (and lsp-enable-on-type-formatting
                              (lsp--capability "documentOnTypeFormattingProvider")))
               (ch last-command-event))
    (when (or (eq (string-to-char (gethash "firstTriggerCharacter" provider)) ch)
              (cl-find ch (gethash "moreTriggerCharacter" provider) :key #'string-to-char))
      (let ((tick (buffer-chars-modified-tick)))
        (lsp-request-async "textDocument/onTypeFormatting"
                           (append (lsp--make-document-formatting-params)
                                   `(:ch ,(char-to-string ch) :position ,(lsp--cur-position)))
                           (lambda (edits)
                             (when (= tick (buffer-chars-modified-tick)) (lsp--apply-text-edits edits))))))))

(defun lsp-buffer-language ()
  "Get language corresponding current buffer."
  (alist-get major-mode lsp-language-id-configuration))

(defun lsp-workspace-root (&optional path)
  "Find the workspace root for the current file or PATH."
  (when-let (file-name (or path (buffer-file-name)))
    (->> (lsp-session)
         (lsp-session-folders)
         (--first (f-ancestor-of? it file-name)))))

(defun lsp-on-revert ()
  "Executed when a file is reverted.
Added to `after-revert-hook'."
  (let ((n (buffer-size))
        (revert-buffer-in-progress-p nil))
    (lsp-on-change 0 n n)))

(defun lsp--text-document-did-close (&optional keep-workspace-alive)
  "Executed when the file is closed, added to `kill-buffer-hook'.

If KEEP-WORKSPACE-ALIVE is non-nil, do not shutdown the workspace
if it's closing the last buffer in the workspace."
  (lsp-foreach-workspace
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
           (lsp-notify
            "textDocument/didClose"
            `(:textDocument ,(lsp--versioned-text-document-identifier))))
         (when (and (not lsp-keep-workspace-alive)
                    (not keep-workspace-alive)
                    (= 0 (hash-table-count file-versions)))
           (setf (lsp--workspace-shutdown-action lsp--cur-workspace) 'shutdown)
           (lsp--shutdown-workspace)))))))

(define-inline lsp--will-save-text-document-params (reason)
  (cl-check-type reason number)
  (inline-quote
   (list :textDocument (lsp--text-document-identifier)
         :reason ,reason)))

(defun lsp--before-save ()
  "Before save handler."
  (with-demoted-errors "Error in ‘lsp--before-save’: %S"
    (let ((params (lsp--will-save-text-document-params 1)))
      (when (lsp--send-will-save-p)
        (lsp-notify "textDocument/willSave" params))
      (when (and (lsp--send-will-save-wait-until-p) lsp-before-save-edits)
        (lsp--apply-text-edits
         (lsp-request "textDocument/willSaveWaitUntil" params))))))

(defun lsp--on-auto-save ()
  "Handler for auto-save."
  (when (lsp--send-will-save-p)
    (with-demoted-errors "Error in ‘lsp--on-auto-save’: %S"
      (lsp-notify "textDocument/willSave" (lsp--will-save-text-document-params 2)))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (with-demoted-errors "Error on ‘lsp--text-document-did-save: %S’"
    (lsp-notify "textDocument/didSave"
                `(:textDocument ,(lsp--versioned-text-document-identifier)
                                :text ,(if (lsp--save-include-text-p)
                                           (save-excursion
                                             (widen)
                                             (buffer-substring-no-properties (point-min) (point-max)))
                                         nil)))))

(define-inline lsp--text-document-position-params (&optional identifier position)
  "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document identifier
and the position respectively."
  (inline-quote (list :textDocument (or ,identifier (lsp--text-document-identifier))
                      :position (or ,position (lsp--cur-position)))))

(defun lsp--cur-line-diagnotics ()
  "Return any diagnostics that apply to the current line."
  (-let* (((&plist :start (&plist :line start) :end (&plist :line end)) (lsp--region-or-line))
          (diags-in-range (cl-remove-if-not
                           (lambda (diag)
                             (let ((line (lsp-diagnostic-line diag)))
                               (and (>= line start) (<= line end))))
                           (gethash buffer-file-name (lsp-diagnostics) nil))))
    (cl-coerce (seq-map #'lsp-diagnostic-original diags-in-range) 'vector)))

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
  "Annotate ITEM detail."
  (-let (((&hash "detail" "kind" kind-index) (plist-get (text-properties-at 0 item) 'lsp-completion-item))
         kind)
    ;; We need check index before call `aref'.
    (when kind-index
      (setq kind (aref lsp--completion-item-kind kind-index))
      (concat " " detail (when kind (format " (%s)" kind))))))

(defun lsp--default-prefix-function ()
  "Default prefix function."
  (bounds-of-thing-at-point 'symbol))

(defun lsp--get-completions ()
  "Get lsp completions."
  (with-demoted-errors "Error in ‘lsp--get-completions’: %S"
    (let* ((prefix-function (or (lsp--client-prefix-function
                                 (lsp--workspace-client (first (lsp-workspaces))))
                                #'lsp--default-prefix-function))
           (bounds (funcall prefix-function)))
      (list
       (if bounds (car bounds) (point))
       (if bounds (cdr bounds) (point))
       (completion-table-dynamic
        #'(lambda (_)
            ;; *we* don't need to know the string being completed
            ;; the language server does all the work by itself
            (let* ((resp (lsp-request "textDocument/completion"
                                      (lsp--text-document-position-params)))
                   (items (cond
                           ((null resp) nil)
                           ((hash-table-p resp) (gethash "items" resp nil))
                           ((sequencep resp) resp))))
              (seq-map #'lsp--make-completion-item items))))
       :annotation-function #'lsp--annotate))))

(defun lsp--sort-string (c)
  (lsp--gethash "sortText" c (gethash "label" c "")))

(defun lsp--sort-completions (completions)
  (seq-into (sort completions
                  (lambda (c1 c2)
                    (string-lessp (lsp--sort-string c1) (lsp--sort-string c2))))
            'list))

(defun lsp--resolve-completion (item)
  "Resolve completion ITEM."
  (cl-assert item nil "Completion item must not be nil")
  (or (-first 'identity
              (lsp-foreach-workspace
               (when (gethash "resolveProvider" (lsp--capability "completionProvider"))
                 (lsp-request "completionItem/resolve" item))))
      item))

(defun lsp--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun lsp--xref-make-item (filename range)
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((pos-start (gethash "start" range))
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

(defun lsp--locations-to-xref-items (locations)
  "Return a list of `xref-item' from Location[] or LocationLink[]."
  (when locations
    (cl-labels ((get-xrefs-in-file
                 (file-locs location-link)
                 (let* ((filename (car file-locs))
                        (visiting (find-buffer-visiting filename))
                        (fn (lambda (loc)
                              (lsp--xref-make-item filename
                                                   (if location-link (or (gethash "targetSelectionRange" loc)
                                                                         (gethash "targetRange" loc))
                                                     (gethash "range" loc))))))
                   (if visiting
                       (with-current-buffer visiting
                         (mapcar fn (cdr file-locs)))
                     (when (file-readable-p filename)
                       (with-temp-buffer
                         (insert-file-contents-literally filename)
                         (mapcar fn (cdr file-locs))))))))
      (apply #'append
             (if (gethash "uri" (car locations))
                 (--map (get-xrefs-in-file it nil)
                        (--group-by (lsp--uri-to-path (gethash "uri" it)) locations))
               (--map (get-xrefs-in-file it t)
                      (--group-by (lsp--uri-to-path (gethash "targetUri" it)) locations)))))))

(defun lsp--make-reference-params (&optional td-position include-declaration)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead.
If INCLUDE-DECLARATION is non-nil, request the server to include declarations."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
               :context `(:includeDeclaration ,(or include-declaration json-false)))))

(defun lsp--cancel-request (id)
  "Cancel requiest with ID in all workspaces."
  (lsp--cur-workspace-check)
  (cl-check-type id (or number string))
  (--each (lsp-workspaces)
    (with-lsp-workspace it
      (let ((response-handlers (lsp--client-response-handlers (lsp--workspace-client
                                                               lsp--cur-workspace))))
        (remhash id response-handlers)
        (lsp-notify "$/cancelRequest" `(:id ,id))))))

(defun lsp-eldoc-function ()
  (run-hook-wrapped
   'lsp-eldoc-hook
   (lambda (fn)
     (condition-case nil
         (funcall fn)
       (lsp-capability-not-supported nil))
     nil)) nil)

(defun lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (gethash "contents"))))
    (if (and contents (not (equal contents "")) )
        (pop-to-buffer
         (with-current-buffer (get-buffer-create "*lsp-help*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (lsp--render-on-hover-content contents t))
             (goto-char (point-min))
             (view-mode t)
             (current-buffer))))
      (lsp--info "No content at point."))))

(defun lsp--point-in-bounds-p (bounds)
  "Return whether the current point is within BOUNDS."
  (and (<= (car bounds) (point)) (< (point) (cdr bounds))))

(defun lsp-get-renderer (language)
  "Get renderer for LANGUAGE."
  (lambda (str)
    (lsp--render-string str language)))

(defun lsp--fontlock-with-mode (str mode)
  "Fontlock STR with MODE."
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (funcall mode))
        (insert str)
        (font-lock-ensure)
        (buffer-string))
    (error str)))

(defun lsp--render-string (str language)
  "Render STR using `major-mode' corresponding to LANGUAGE.
When language is nil render as markup if `markdown-mode' is loaded."
  (if-let (mode (-some (-lambda ((mode . lang))
                         (when (and (equal lang language) (functionp mode))
                           mode))
                       lsp-language-id-configuration))
      (lsp--fontlock-with-mode str mode)
    str))

(defun lsp--render-element (content)
  "Render CONTENT element."
  ;; MarkedString
  (cond
   ((and (hash-table-p content)
         (gethash "language" content))
    (-let [(&hash "language" "value") content]
      (lsp--render-string value language)))

   ;; MarkupContent
   ((and (hash-table-p content)
         (gethash "kind" content))
    (-let [(&hash "value" "kind") content]
      (lsp--render-string value kind)))
   ;; plain string
   ((stringp content) (lsp--render-string content "markdown"))
   (t (error "Failed to handle %s" content))))

(defun lsp--render-on-hover-content (contents render-all)
  "Render the content received from 'document/onHover' request.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL - nil if only the signature should be rendered."
  (if (and (hash-table-p contents) (gethash "kind" contents))
      ;; MarkupContent, deprecated by LSP but actually very flexible.
      ;; It tends to be long and is not suitable in echo area.
      (if render-all (lsp--render-element contents) "")
    ;; MarkedString -> MarkedString[]
    (when (or (hash-table-p contents) (stringp contents))
      (setq contents (list contents)))
    ;; Consider the signature consisting of the elements who have a renderable
    ;; "language" property. When render-all is nil, ignore other elements.
    (string-join
     (seq-map
      #'lsp--render-element
      (if render-all
          contents
        (--filter (and (hash-table-p it) (lsp-get-renderer (gethash "language" it)))
                  contents)))
     "\n")))

(defvar-local lsp--hover-saved-bounds nil)
(defvar-local lsp--eldoc-saved-message nil)

(defun lsp--signature->eldoc-message (signature-help)
  "Generate eldoc message from SIGNATURE-HELP response."

  (-when-let* (((&hash "activeSignature" active-signature-index
                       "activeParameter" active-parameter
                       "signatures") signature-help)
               (signature (seq-elt signatures (or active-signature-index 0)))
               (result (lsp--fontlock-with-mode (gethash "label" signature) major-mode)))
    (-when-let* ((selected-param-label (-some->> (gethash "parameters" signature)
                                                 (nth active-parameter)
                                                 (gethash "label")))
                 (start (if (stringp selected-param-label)
                            (s-index-of selected-param-label result)
                          (car selected-param-label)))
                 (end (if (stringp selected-param-label) (+ start (length selected-param-label)) (cadr selected-param-label))))
      (add-face-text-property start end 'eldoc-highlight-function-argument nil result))
    result))

(defvar-local lsp-hover-request-id 0)

(defun lsp-hover ()
  "Display signature or hover info based on the current position."
  (if (and lsp--hover-saved-bounds
           (lsp--point-in-bounds-p lsp--hover-saved-bounds))
      (lsp--eldoc-message lsp--eldoc-saved-message)

    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil)
    (let ((request-id (cl-incf lsp-hover-request-id)) (pending 0))
      (when (and lsp-eldoc-enable-hover (lsp--capability "hoverProvider"))
        (cl-incf pending)
        (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (lambda (hover)
           (when (and (eq request-id lsp-hover-request-id))
             (when hover
               (when-let (range (gethash "range" hover))
                 (setq lsp--hover-saved-bounds (lsp--range-to-region range)))
               (-let (((&hash "contents") hover))
                 (when-let (message
                            (and contents (lsp--render-on-hover-content contents lsp-eldoc-render-all)))
                   (when (or (and (not lsp-eldoc-prefer-signature-help) (setq pending 1))
                             (not lsp--eldoc-saved-message))
                     (setq lsp--eldoc-saved-message message)))))
             (when (zerop (cl-decf pending))
               (lsp--eldoc-message lsp--eldoc-saved-message))
             (run-hook-with-args 'lsp-on-hover-hook hover)))))
      (when (and lsp-eldoc-enable-signature-help (lsp--capability "signatureHelpProvider"))
        (cl-incf pending)
        (lsp-request-async
         "textDocument/signatureHelp"
         (lsp--text-document-position-params)
         (lambda (signature)
           (when (eq request-id lsp-hover-request-id)
             (when-let (message (and signature (lsp--signature->eldoc-message signature)))
               (when (or (and lsp-eldoc-prefer-signature-help (setq pending 1))
                         (not lsp--eldoc-saved-message))
                 (setq lsp--eldoc-saved-message message)))
             (when (zerop (cl-decf pending))
               (lsp--eldoc-message lsp--eldoc-saved-message)))))))))

(defun lsp--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((not actions) (user-error "No actions to select from"))
   ((and (not (cdr actions)) lsp-auto-execute-action) (car actions))
   (t (lsp--completing-read "Select code action: " actions
                            (-lambda ((&hash "title" "command")) (or title command)) nil t))))

(defun lsp--find-action-handler (command)
  "Find action handler for particular COMMAND."
  (--some (-some->> it
                    (lsp--workspace-client)
                    (lsp--client-action-handlers)
                    (gethash command))
          (lsp-workspaces)))

(defun lsp--text-document-code-action-params ()
  "Code action params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))
        :context (list :diagnostics (lsp--cur-line-diagnotics))))

(defun lsp-code-actions-at-point ()
  "Retrieve the code actions for the active region or the current line."
  (lsp-request "textDocument/codeAction" (lsp--text-document-code-action-params)))

(defalias 'lsp-get-or-calculate-code-actions 'lsp-code-actions-at-point)

(defun lsp-execute-code-action (action)
  "Execute code action ACTION.
If ACTION is not set it will be selected from `lsp-code-actions'."
  (interactive (list (lsp--select-action
                      (lsp-code-actions-at-point))))
  (when-let ((edit (gethash "edit" action)))
    (lsp--apply-workspace-edit edit))
  (when-let ((command (gethash "command" action)))
    (if-let ((action-handler (lsp--find-action-handler command)))
        (funcall action-handler action)
      (lsp--send-execute-command (gethash "command" action)
                                 (gethash "arguments" action nil)))))

(defun lsp--make-document-formatting-params ()
  "Create document formatting params."
  `(:textDocument ,(lsp--text-document-identifier)
                  :options (:tabSize ,tab-width :insertSpaces
                                     ,(if indent-tabs-mode :json-false t))))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (unless (or (lsp--capability "documentFormattingProvider")
              (lsp--registered-capability "textDocument/formatting"))
    (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))
  (let ((edits (lsp-request "textDocument/formatting"
                            (lsp--make-document-formatting-params))))
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

(defun lsp-document-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-document-highlight-callback (current-buffer))))

(defun lsp--make-document-highlight-callback (buf)
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
    (9 . "Constructor")
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
  (let ((edits (lsp-request "textDocument/rangeFormatting"
                            (lsp--make-document-range-formatting-params s e))))
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
  (lsp-request "textDocument/documentSymbol"
               `(:textDocument ,(lsp--text-document-identifier))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (symbol-name (symbol-at-point))
              'def-params (lsp--text-document-position-params)
              'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (let ((json-false :json-false)
        (symbols (lsp--get-document-symbols)))
    (seq-map #'lsp--symbol-info-to-identifier (-filter 'identity symbols))))

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
    (lsp--locations-to-xref-items (if (sequencep defs) defs (list defs)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
         (params (plist-get properties 'ref-params))
         (refs (lsp-request "textDocument/references"
                            (or params (lsp--make-reference-params)))))
    (lsp--locations-to-xref-items refs)))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (mapcar #'lsp--symbol-information-to-xref
          (lsp-request "workspace/symbol" `(:query ,pattern))))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (read-string (format "Rename %s to: " (thing-at-point 'symbol t)))))
  (lsp--cur-workspace-check)
  (unless (lsp--capability "renameProvider")
    (signal 'lsp-capability-not-supported (list "renameProvider")))
  (let ((edits (lsp-request "textDocument/rename"
                            `(:textDocument ,(lsp--text-document-identifier)
                                            :position ,(lsp--cur-position)
                                            :newName ,newname))))
    (when edits
      (lsp--apply-workspace-edit edits))))

(cl-defun lsp-find-locations (method &optional extra &key display-action)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters."
  (let ((loc (lsp-request method
                          (append (lsp--text-document-position-params) extra))))
    (if loc
        (xref--show-xrefs
         (lsp--locations-to-xref-items (if (sequencep loc) loc (list loc)))
         display-action)
      (message "Not found for: %s" (thing-at-point 'symbol t)))))

(cl-defun lsp-find-declaration (&key display-action)
  "Find declarations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/declaration" nil :display-action display-action))

(cl-defun lsp-find-definition (&key display-action)
  "Find definitions of the symbol under point."
  (interactive)
  (unless (lsp--capability "definitionProvider")
    (signal 'lsp-capability-not-supported (list "definitionProvider")))
  (lsp-find-locations "textDocument/definition" nil :display-action display-action))

(cl-defun lsp-find-implementation (&key display-action)
  "Find implementations of the symbol under point."
  (interactive)
  (unless (lsp--capability "implementationProvider")
    (signal 'lsp-capability-not-supported (list "implementationProvider")))
  (lsp-find-locations "textDocument/implementation" nil :display-action display-action))

(cl-defun lsp-find-references (&optional include-declaration &key display-action)
  "Find references of the symbol under point."
  (interactive)
  (unless (lsp--capability "referencesProvider")
    (signal 'lsp-capability-not-supported (list "referencesProvider")))
  (lsp-find-locations "textDocument/references"
                      (list :context `(:includeDeclaration ,(or include-declaration json-false)))
                      :display-action display-action))

(cl-defun lsp-find-type-definition (&key display-action)
  "Find type definitions of the symbol under point."
  (interactive)
  (unless (lsp--capability "typeDefinitionProvider")
    (signal 'lsp-capability-not-supported (list "typeDefinitionProvider")))
  (lsp-find-locations "textDocument/typeDefinition" nil :display-action display-action))

(defalias 'lsp-find-custom #'lsp-find-locations)
(defalias 'lsp-goto-implementation #'lsp-find-implementation)
(defalias 'lsp-goto-type-definition #'lsp-find-type-definition)

(with-eval-after-load 'evil
  (evil-set-command-property 'lsp-find-definition :jump t)
  (evil-set-command-property 'lsp-find-implementation :jump t)
  (evil-set-command-property 'lsp-find-references :jump t)
  (evil-set-command-property 'lsp-find-type-definition :jump t))

(defun lsp--find-workspaces-for (msg)
  "Find all workspaces in the current that can handle MSG."
  (-if-let (reqs (cdr (assoc (plist-get msg :method) lsp-method-requirements)))
      (-let (((&plist :capability :registered-capability) reqs))
        (--filter
         (with-lsp-workspace it
           (or (when capability (lsp--capability capability))
               (when registered-capability
                 (lsp--registered-capability registered-capability))
               (and (not capability) (not registered-capability))))
         (lsp-workspaces)))
    (lsp-workspaces)))

(cl-defmethod lsp-execute-command (server command arguments)
  "Execute COMMAND on SERVER with `workspace/executeCommand'."
  (lsp-request "workspace/executeCommand"
               `(:command ,(format "%s" command) :arguments ,arguments)))

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having command COMMAND and optional ARGS."
  (let ((params (if args
                    (list :command command :arguments args)
                  (list :command command))))
    (lsp-request "workspace/executeCommand" params)))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
(defalias 'lsp-completion-at-point #'lsp--get-completions)

(defun lsp--set-configuration (settings)
  "Set the SETTINGS for the lsp server."
  (lsp-notify "workspace/didChangeConfiguration" `(:settings , settings)))

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
                  (lsp-notify
                   "workspace/didChangeWatchedFiles"
                   (list :changes
                         (list
                          :type (alist-get (cadr event) lsp--file-change-type)
                          :uri (lsp--path-to-uri (caddr event)))))))
              watches))))

(defun lsp--on-set-visitied-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp--cur-workspace
    (lsp--text-document-did-close t))
  (prog1 (apply old-func args)
    (when lsp--cur-workspace
      (lsp--text-document-did-open))))

(advice-add 'set-visited-file-name :around #'lsp--on-set-visitied-file-name)

(defun lsp--send-wait (message proc parser)
  "Send MESSAGE to PROC and wait for output from the process.
PARSER is the workspace parser used for handling the message."
  (when lsp-print-io
    (let ((inhibit-message t))
      (lsp-message ">>> %s(sync)\n%s"
                   (-> parser lsp--parser-workspace lsp--workspace-print)
                   message)))
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message)
  (with-local-quit
    (let* ((send-time (time-to-seconds (current-time)))
           ;; max time by which we must get a response
           (expected-time (+ send-time lsp-response-timeout)))
      (while (lsp--parser-waiting-for-response parser)
        ;; Wait for expected-time - current-time
        (accept-process-output proc (- expected-time (time-to-seconds (current-time))))
        ;; We have timed out when expected-time < (current-time)
        (when (< expected-time (time-to-seconds (current-time)))
          (signal 'lsp-timed-out-error nil))))))

(defun lsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."
  (when lsp-print-io
    (let ((inhibit-message t))
      (lsp-message ">>> %s(async)\n%s"
                   (lsp--workspace-print lsp--cur-workspace)
                   message)))
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message))

(define-error 'lsp-parse-error
  "Error parsing message from language server" 'lsp-error)
(define-error 'lsp-unknown-message-type
  "Unknown message type" '(lsp-error lsp-parse-error))
(define-error 'lsp-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(lsp-error lsp-parse-error))
(define-error 'lsp-no-content-length
  "Content-Length header missing in message" '(lsp-error lsp-parse-error))
(define-error 'lsp-invalid-header-name
  "Invalid header name" '(lsp-error lsp-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification
(defun lsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (string= (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'lsp-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
        'notification
      (signal 'lsp-unknown-message-type (list json-data)))))

(defconst lsp--default-notification-handlers
  (lsp-ht ("window/showMessage" 'lsp--window-show-message)
          ("window/logMessage" 'lsp--window-show-message)
          ("textDocument/publishDiagnostics" 'lsp--on-diagnostics)
          ("textDocument/diagnosticsEnd" 'ignore)
          ("textDocument/diagnosticsBegin" 'ignore)))

(defun lsp--on-notification (workspace notification)
  "Call the appropriate handler for NOTIFICATION."
  (-let* (((&hash "params" "method") notification))
    (if-let (handler (or (gethash method (lsp--client-notification-handlers (lsp--workspace-client workspace)))
                         (gethash method lsp--default-notification-handlers)))
        (funcall handler workspace params)
      (unless (string-prefix-p "$" method)
        (lsp-warn "Unknown method: %s" method)))))

(defun lsp--on-request (workspace request)
  "Call the appropriate handler for REQUEST, and send the return value to the server.
WORKSPACE is the active workspace."
  (-let* (((&hash "params" "method") request)
          (client (lsp--workspace-client workspace))
          (process (lsp--workspace-proc workspace))
          (empty-response (lsp--make-response request nil))
          (response (pcase method
                      ("client/registerCapability"
                       (seq-doseq (reg (gethash "registrations" params))
                         (lsp--server-register-capability reg))
                       empty-response)
                      ("window/showMessageRequest"
                       (let ((choice (lsp--window-show-message-request params)))
                         (lsp--make-response request `(:title ,choice))))
                      ("client/unregisterCapability"
                       (seq-doseq (unreg (gethash "unregisterations" params))
                         (lsp--server-unregister-capability unreg))
                       empty-response)
                      ("workspace/applyEdit"
                       (lsp--apply-workspace-edit (gethash "edit" params))
                       empty-response)
                      (other
                       (-if-let (handler (gethash other (lsp--client-request-handlers client) nil))
                           (lsp--make-response request (funcall handler workspace params))
                         (lsp-warn "Unknown request method: %s" other)
                         empty-response)))))
    ;; Send response to the server.
    (lsp--send-no-wait (lsp--make-message response) process)))

(defun lsp--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
        (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
            message
            (or (car (alist-get code lsp--errors)) "Unknown error"))))

(defun lsp--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server our our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (substring s (+ 2 pos)))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c being the elements of val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun lsp--parser-reset (p)
  (setf
   (lsp--parser-leftovers p) ""
   (lsp--parser-body-length p) nil
   (lsp--parser-body-received p) nil
   (lsp--parser-headers p) '()
   (lsp--parser-body p) nil
   (lsp--parser-reading-body p) nil))

(defun lsp--read-json (str use-native-json)
  (let* ((use-native-json (and use-native-json (fboundp 'json-parse-string)))
         (json-array-type (if use-native-json 'vector 'list))
         (json-object-type 'hash-table)
         (json-false nil))
    (if use-native-json
        (json-parse-string str :object-type 'hash-table
                           :null-object nil :false-object nil)
      (json-read-from-string str))))

(defun lsp--parser-on-message (p msg)
  "Called when the parser P read a complete MSG from the server."
  (with-lsp-workspace (lsp--parser-workspace p)
    (let* ((client (lsp--workspace-client lsp--cur-workspace))
           (json-data (lsp--read-json msg (lsp--client-use-native-json client)))
           (id (gethash "id" json-data nil)))
      (pcase (lsp--get-message-type json-data)
        ('response
         (cl-assert id)
         (if-let (callback (gethash (if (stringp id)
                                        (string-to-number id)
                                      id)
                                    (lsp--client-response-handlers client)
                                    nil))
             (progn (funcall callback (gethash "result" json-data nil))
                    (remhash id (lsp--client-response-handlers client)))
           (setf (lsp--parser-response-result p)
                 (and json-data (gethash "result" json-data nil))
                 (lsp--parser-waiting-for-response p) nil)))
        ('response-error
         (let* ((err (gethash "error" json-data nil))
                (code (gethash "code" err nil)))
           (when (not (memq code lsp--silent-errors))
             (message (lsp--error-string err))))
         (setf (lsp--parser-response-result p) nil
               (lsp--parser-waiting-for-response p) nil))
        ('notification (lsp--on-notification lsp--cur-workspace json-data))
        ('request      (lsp--on-request lsp--cur-workspace json-data))))))

(defun lsp--parser-read (p output)
  (cl-assert (lsp--parser-workspace p) nil "Parser workspace cannot be nil.")
  (let* ((messages '())
         (output (string-as-unibyte output))
         (chunk (concat (lsp--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (lsp--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'lsp--parse-header
                                (split-string header-raw "\r\n")))
                       (body-length (lsp--get-body-length headers)))
                  (setf
                   (lsp--parser-headers p) headers
                   (lsp--parser-reading-body p) t
                   (lsp--parser-body-length p) body-length
                   (lsp--parser-body-received p) 0
                   (lsp--parser-body p) (make-string body-length ?\0)
                   (lsp--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (lsp--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (lsp--parser-body-length p))
               (received-body-length (lsp--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body
                (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (lsp--parser-body p) received-body-length this-body)
          (setf (lsp--parser-body-received p) (+ (lsp--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            ;; TODO: keep track of the Content-Type header, if
            ;; present, and use its value instead of just defaulting
            ;; to utf-8
            (push (decode-coding-string (lsp--parser-body p) 'utf-8) messages)
            (lsp--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun lsp--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun lsp--parser-make-filter (p ignore-regexps)
  "Make filter for the lsp parser P ignoring IGNORE-REGEXPS."
  #'(lambda (_proc output)
      (when (cl-loop for r in ignore-regexps
                     ;; check if the output is to be ignored or not
                     ;; TODO: Would this ever result in false positives?
                     when (string-match r output) return nil
                     finally return t)
        (-when-let (messages (condition-case err
                                 (lsp--parser-read p output)
                               (error
                                (let ((chunk (concat (lsp--parser-leftovers p) output)))
                                  (lsp--parser-reset p)
                                  (setf (lsp--parser-response-result p) nil
                                        (lsp--parser-waiting-for-response p) nil)
                                  (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s" chunk err)
                                  nil))))
          (dolist (m messages)
            (when lsp-print-io
              (let ((inhibit-message t))
                (lsp-message "<<<< %s\n%s"
                             (-> p lsp--parser-workspace lsp--workspace-print)
                             (lsp--json-pretty-print m))))
            (lsp--parser-on-message p m))))))


(define-inline lsp--point-to-marker (p)
  (inline-quote (save-excursion (goto-char ,p) (point-marker))))

(defun lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym)))
    (cons (if (and lsp-imenu-show-container-name container)
              (concat container lsp-imenu-container-name-separator name)
            name)
          start-point)))

(defun lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym)))
    (if (gethash "children" sym)
        (cons name
              (cons (cons (format "(%s)" (lsp--get-symbol-type sym)) start-point)
                    (lsp--imenu-create-hierarchical-index (gethash "children" sym))))
      (cons (format "%s (%s)" name (lsp--get-symbol-type sym)) start-point))))

(defun lsp--symbol-get-start-point (sym)
  "Get the start point of the name of SYM.

SYM can be either DocumentSymbol or SymbolInformation."
  (let* ((location (gethash "location" sym))
         (name-range (or (and location (gethash "range" location))
                         (gethash "selectionRange" sym)))
         (start-point (lsp--position-to-point
                       (gethash "start" name-range))))
    (if imenu-use-markers (lsp--point-to-marker start-point) start-point)))

(defun lsp--symbol-filter (sym)
  "Determine if SYM is for the current document."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the current
  ;; buffer file.
  (when-let (location (gethash "location" sym))
    (not (eq (find-buffer-visiting (lsp--uri-to-path (gethash "uri" (gethash "location" sym))))
             (current-buffer)))))

(defun lsp--get-symbol-type (sym)
  "The string name of the kind of SYM."
  (or (cdr (assoc (gethash "kind" sym) lsp--symbol-kind)) "Other"))

(defun lsp--imenu-create-index ()
  "Create imenu index from document symbols."
  (let ((symbols (lsp--get-document-symbols)))
    (if (lsp--imenu-hierarchical-p symbols)
        (lsp--imenu-create-hierarchical-index symbols)
      (mapcar (lambda (nested-alist)
                (cons (car nested-alist)
                      (mapcar #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
              (seq-group-by #'lsp--get-symbol-type (lsp--imenu-filter-symbols symbols))))))

(defun lsp--imenu-filter-symbols (symbols)
  "Filter out unsupported symbols from SYMBOLS."
  (seq-remove #'lsp--symbol-filter symbols))

(defun lsp--imenu-hierarchical-p (symbols)
  "Determine whether any element in SYMBOLS has children."
  (--some (gethash "children" it) symbols))

(defun lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (mapcar (lambda (sym)
              (lsp--symbol-to-hierarchical-imenu-elem sym))
            (sort (lsp--imenu-filter-symbols symbols)
                  (lambda (sym1 sym2)
                    (lsp--imenu-symbol-lessp sym1 sym2))))))

(defun lsp--imenu-symbol-lessp (sym1 sym2)
  (let* ((compare-results (mapcar (lambda (method)
                                    (funcall (alist-get method lsp--imenu-compare-function-alist)
                                             sym1 sym2))
                                  lsp-imenu-sort-methods))
         (result (seq-find (lambda (result)
                             (not (= result 0)))
                           compare-results
                           0)))
    (and (numberp result) (< result 0))))

(defun lsp--imenu-compare-kind (sym1 sym2)
  "Compare SYM1 and SYM2 by kind."
  (let ((kind1 (gethash "kind" sym1))
        (kind2 (gethash "kind" sym2)))
    (- kind1 kind2)))

(defun lsp--imenu-compare-position (sym1 sym2)
  "Compare SYM1 and SYM2 by position."
  (let ((position1 (lsp--symbol-get-start-point sym1))
        (position2 (lsp--symbol-get-start-point sym2)))
    (- position1 position2)))

(defun lsp--imenu-compare-name (sym1 sym2)
  "Compare SYM1 and SYM2 by name."
  (let* ((name1 (gethash "name" sym1))
         (name2 (gethash "name" sym2))
         (result (compare-strings name1 0 (length name1) name2 0 (length name2))))
    (if (numberp result) result 0)))

(defun lsp-enable-imenu ()
  "Use lsp-imenu for the current buffer."
  (setq-local imenu-create-index-function #'lsp--imenu-create-index))

(defun lsp-resolve-final-function (command)
  "Resolve final function COMMAND."
  (-let [command (if (functionp command) (funcall command) command)]
    (if (consp command) command (list command))))

(defun lsp-server-present? (final-command)
  "Check whether FINAL-COMMAND is present."
  (executable-find (nth 0 final-command)))

(defun lsp-stdio-connection (command)
  "Create LSP stdio connection named name.
COMMAND is either list of strings, string or function which
returns the command to execute."
  (list :connect (lambda (filter sentinel name)
                   (let ((final-command (lsp-resolve-final-function command))
                         (process-name (generate-new-buffer-name name)))
                     (unless (lsp-server-present? final-command)
                       (error (format "Couldn't find command %s" final-command)))
                     (let ((proc (make-process
                                  :name process-name
                                  :connection-type 'pipe
                                  :buffer (format "*%s*" process-name)
                                  :coding 'no-conversion
                                  :command final-command
                                  :filter filter
                                  :sentinel sentinel
                                  :stderr (format "*%s::stderr*" process-name)
                                  :noquery t)))
                       (set-process-query-on-exit-flag proc nil)
                       (cons proc proc))))
        :test? (lambda () (-> command lsp-resolve-final-function lsp-server-present?))))

(defun lsp--open-network-stream (host port name &optional retry-count sleep-interval)
  "Open network stream to HOST:PORT.
NAME will be passed to `open-network-stream'.
RETRY-COUNT is the number of the retries.
SLEEP-INTERVAL is the sleep interval between each retry."
  (let ((retries 0)
        connection)
    (while (and (not connection) (< retries (or retry-count 100)))
      (condition-case err
          (setq connection (open-network-stream name nil host port :type 'plain))
        (file-error
         (let ((inhibit-message t))
           (lsp--warn "Failed to connect to %s:%s with error message %s"
                      host
                      port
                      (error-message-string err))
           (sit-for (or sleep-interval 0.02))
           (cl-incf retries)))))
    connection))

(defun lsp--find-available-port (host starting-port)
  "Find available port on HOST starting from STARTING-PORT."
  (let ((success nil)
        (port starting-port))
    (while (and (not success))
      (condition-case _err
          (progn
            (delete-process (open-network-stream "*connection-test*" nil host port :type 'plain))
            (cl-incf port))
        (file-error (setq success t))))
    port))

(defun lsp-tcp-connection (command)
  "Create LSP TCP connection named name.
COMMAND-FN will be called to generate Language Server command."
  (list
   :connect (lambda (filter sentinel name)
              (let* ((host "localhost")
                     (port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (funcall command-fn port))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (executable-find (first final-command))
                          (user-error (format "Couldn't find executable %s" (first final-command)))))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr name :noquery t))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag (get-buffer-process (get-buffer (process-name proc))) nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () (-> command lsp-resolve-final-function lsp-server-present?))))

(defun lsp-tramp-connection (command-fn)
  "Create LSP TRAMP connection named.
COMMAND-FN will be called to generate Language Server command."
  (list
   :connect (lambda (filter sentinel name)
              (let* ((host (file-remote-p (buffer-file-name) 'host t))
                     (port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (funcall command-fn name port))
                     (proc (progn
                             (lsp--info "Starting %s" command)
                             (async-shell-command command
                                                  (generate-new-buffer-name (format "%s::stdout" name))
                                                  (generate-new-buffer-name (format "%s::stderr" name)))))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tramp::tcp"))))

                (set-process-sentinel proc sentinel)
                (set-process-query-on-exit-flag proc nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (-const t)))

(defun lsp-make-nc-tramp-command (program)
  "Return a function which will convert PROGRAM from STDIO command to TCP one."
  (lambda (_name port)
    (format "nc -l -p %s -c '%s'" port program)))

(defun lsp--auto-configure ()
  "Autoconfigure `lsp-ui', `company-lsp' if they are installed."

  (when (functionp 'lsp-ui-mode)
    (lsp-ui-mode))

  (cond
   ((and (not (version< emacs-version "26.1")) lsp-prefer-flymake)
    (lsp--flymake-setup))
   ((and (functionp 'lsp-ui-mode) (featurep 'flycheck))
    (require 'lsp-ui-flycheck)
    (lsp-ui-flycheck-enable t)
    (flycheck-mode 1)))

  (lsp-enable-imenu)

  (when (functionp 'company-lsp)
    (company-mode 1)
    (setq-local company-backends '(company-lsp))

    (when (functionp 'yas-minor-mode)
      (yas-minor-mode t))))

(defun lsp--make-workspace (client root)
  "Make workspace for the CLIENT and ROOT."
  (let* ((parser (make-lsp--parser))
         (workspace (make-lsp--workspace
                     :parser parser
                     :file-versions (make-hash-table :test 'equal)
                     :root root
                     :client client
                     :status 'starting
                     :buffers (list (current-buffer))
                     :host-root (file-remote-p root))))
    (setf (lsp--parser-workspace parser) workspace)
    workspace))

(defun lsp--restart-if-needed (workspace)
  "Handler restart for WORKSPACE."
  (when (or (eq lsp-restart 'auto-restart)
            (eq (lsp--workspace-shutdown-action workspace) 'restart)
            (and (eq lsp-restart 'interactive)
                 (let ((query (format "Server %s exited with status %s. Do you want to restart it?"
                                      (lsp--workspace-print workspace)
                                      (process-status (lsp--workspace-proc workspace)))))
                   (y-or-n-p query))))
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--info "Restarting LSP in buffer %s" (buffer-name))
          (lsp))))))

(defun lsp--update-key (table key fn)
  "Apply FN on value corresponding to KEY in TABLE."
  (let ((existing-value (gethash key table)))
    (if-let (new-value (funcall fn existing-value))
        (puthash key new-value table)
      (remhash key table))))

(defun lsp--create-sentinel (workspace)
  "Create sentinel handler for WORKSPACE."
  (lambda (process exit-str)
    (let* ((status (process-status process))
           (folder->workspaces (lsp-session-folder->servers (lsp-session)))
           (stderr (-> workspace lsp--workspace-proc process-name get-buffer)))

      (when (memq status '(exit signal))
        (lsp--warn "%s has exited (%s)"
                   (process-name (lsp--workspace-proc workspace))
                   (string-trim-right exit-str)))

      (with-lsp-workspace workspace
        ;; clean workspace related data in each of the buffers
        ;; in the workspace.
        (--each (lsp--workspace-buffers workspace)
          (when (buffer-live-p it)
            (with-current-buffer it
              (setq lsp--buffer-workspaces (delete workspace lsp--buffer-workspaces))
              (lsp--uninitialize-workspace)
              (lsp--spinner-stop))))

        ;; cleanup session from references to the closed workspace.
        (--each (hash-table-keys folder->workspaces)
          (lsp--update-key folder->workspaces it (apply-partially 'delete workspace)))

        ;; Kill standard error buffer only if the process exited normally.
        ;; Leave it intact otherwise for debugging purposes.
        (when (and (eq status 'exit) (zerop (process-exit-status process)) (buffer-live-p stderr))
          (kill-buffer stderr)))

      (if (eq (lsp--workspace-shutdown-action workspace) 'shutdown)
          (lsp--info "Workspace %s shutdown." (lsp--workspace-print workspace))
        (lsp--restart-if-needed workspace)))))

(defun lsp--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (lsp--spinner-start)
  (-let* ((default-directory root)
          (client (copy-lsp--client client-template))
          (workspace (lsp--make-workspace client root))
          ((proc . cmd-proc) (funcall
                              (or (plist-get (lsp--client-new-connection client) :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (lsp--parser-make-filter (lsp--workspace-parser workspace)
                                                       (lsp--client-ignore-regexps client))
                              (lsp--create-sentinel workspace)
                              (format "%s" (lsp--client-server-id client)))))
    (setf (lsp--workspace-proc workspace) proc
          (lsp--workspace-cmd-proc workspace) cmd-proc)

    ;; update (lsp-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (-map (lambda (project-root)
            (->> session
                 lsp-session-folder->servers
                 (gethash project-root)
                 (pushnew workspace)))
          (or (-map 'lsp--uri-to-path
                    (plist-get initialization-options :workspaceFolders))
              (list root)))

    (with-lsp-workspace workspace
      (run-hooks 'lsp-before-initialize-hook)
      (lsp-request-async "initialize"
                         (list :processId (emacs-pid)
                               :rootPath (expand-file-name root)
                               :rootUri (lsp--path-to-uri root)
                               :capabilities (lsp--client-capabilities)
                               :initializationOptions initialization-options)
                         (lambda (response)
                           (unless response
                             (lsp--spinner-stop)
                             (signal 'lsp-empty-response-error (list "initialize")))

                           (setf (lsp--workspace-server-capabilities workspace) (gethash "capabilities" response)
                                 (lsp--workspace-status workspace) 'initialized)

                           (with-lsp-workspace workspace
                             (lsp-notify "initialized" (make-hash-table)))

                           (--each (lsp--workspace-buffers workspace)
                             (with-current-buffer it
                               (lsp--open-in-workspace workspace)))

                           (when-let (initialize-fn (lsp--client-initialized-fn client))
                             (funcall initialize-fn workspace)))
                         :mode 'detached))
    workspace))

(defun lsp--load-default-session ()
  "Load default session."
  (setq lsp--session (or (lsp--read-from-file lsp-session-file)
                         (make-lsp-session))))

(defun lsp-session ()
  "Get the session associated with the current buffer."
  (or lsp--session (setq lsp--session (lsp--load-default-session))))

(defun lsp--find-clients (buffer-major-mode file-name)
  "Find clients which can handle BUFFER-MAJOR-MODE.
SESSION is the currently active session. The function will also
pick only remote enabled clients in case the FILE-NAME is on
remote machine and vice versa."
  (let ((remote? (file-remote-p file-name)))
    (--when-let (->> lsp-clients
                     hash-table-values
                     (-filter (-lambda (client)
                                (and (or
                                      (-some-> client lsp--client-activation-fn (funcall buffer-file-name buffer-major-mode))
                                      (and (-contains? (lsp--client-major-modes client) buffer-major-mode)
                                           (eq (---truthy? remote?) (---truthy? (lsp--client-remote? client)))))
                                     (-some-> client lsp--client-new-connection (plist-get :test?) funcall)))))
      (-let (((add-on-clients main-clients) (-separate 'lsp--client-add-on? it)))
        ;; Pick only one client (with the highest priority) that is not declared as add-on? t.
        (cons (and main-clients (--max-by (> (lsp--client-priority it) (lsp--client-priority other)) main-clients))
              add-on-clients)))))

(defun lsp-register-client (client)
  "Registers LSP client CLIENT."
  (puthash (lsp--client-server-id client) client lsp-clients))

(defun lsp--create-initialization-options (session client)
  "Create initialization-options from SESSION and CLIENT.
Add workspace folders depending on server being multiroot and
session workspce folder configuration for the server."
  (let* ((initialization-options-or-fn (lsp--client-initialization-options client))
         (initialization-options (if (functionp initialization-options-or-fn)
                                     (funcall initialization-options-or-fn)
                                   initialization-options-or-fn)))
    (if (lsp--client-multi-root client)
        (or (-some->> session
                      (lsp-session-server-id->folders)
                      (gethash (lsp--client-server-id client))
                      (-map 'lsp--path-to-uri)
                      (plist-put initialization-options :workspaceFolders))
            initialization-options)
      initialization-options)))

(defun lsp--start-connection (session client project-root)
  "Initiates connection created from CLIENT for PROJECT-ROOT.
SESSION is the active session."
  (when (lsp--client-multi-root client)
    (pushnew project-root (gethash (lsp--client-server-id client)
                                   (lsp-session-server-id->folders session)) ))
  (run-hook-with-args 'lsp-workspace-folders-changed-hook (list project-root) nil)

  (unwind-protect
      (lsp--start-workspace session client project-root (lsp--create-initialization-options session client))
    (lsp--spinner-stop)))

(define-derived-mode lsp-browser-mode special-mode "LspBrowser"
  "Define mode for displaying lsp sessions."
  (setq-local display-buffer-base-action '(nil . ((inhibit-same-window . t)))))

(defun lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
         (status (lsp--workspace-status workspace))
         (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name (propertize 'face 'bold-italic)))
         (pid (propertize (format "%s" (process-id proc)) 'face 'italic)))

    (if (eq 'initialized status)
        (format "%s:%s" server-id pid)
      (format "%s:%s status:%s" server-id pid status))))

(defun lsp--map-tree-widget (m)
  "Build `tree-widget' from a hash-table M."
  (when (hash-table-p m)
    (let (nodes)
      (maphash (lambda (k v)
                 (push `(tree-widget
                         :tag ,(if (hash-table-p v)
                                   (format "%s:" k)
                                 (format "%s: %s" k
                                         (propertize (format "%s" v)
                                                     'face
                                                     'font-lock-string-face)))
                         :open t
                         ,@(lsp--map-tree-widget v))
                       nodes))
               m)
      nodes)))

(defun lsp--render-workspace (workspace)
  "Tree node representation of WORKSPACE."
  `(tree-widget :tag ,(lsp--workspace-print workspace)
                :open t
                (tree-widget :tag ,(propertize "Buffers" 'face 'font-lock-function-name-face)
                             :open t
                             ,@(->> workspace
                                    (lsp--workspace-buffers)
                                    (--map `(tree-widget
                                             :tag ,(when (buffer-live-p it)
                                                     (if (with-current-buffer it buffer-read-only)
                                                         (propertize (buffer-name it)
                                                                     'face 'font-lock-constant-face)
                                                       (buffer-name it)))))))
                (tree-widget :tag ,(propertize "Capabilities" 'face 'font-lock-function-name-face)
                             ,@(-> workspace lsp--workspace-server-capabilities lsp--map-tree-widget))))

(defun lsp-describe-session ()
  "Describes current `lsp-session'."
  (interactive)
  (let ((session (lsp-session))
        (buf (get-buffer-create "*lsp session*")))
    (with-current-buffer buf
      (lsp-browser-mode)
      (cursor-sensor-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (--each (lsp-session-folders session)
          (widget-create
           `(tree-widget
             :tag ,(propertize it 'face 'font-lock-keyword-face)
             :open t
             ,@(->> session
                    (lsp-session-folder->servers)
                    (gethash it)
                    (-map 'lsp--render-workspace)))))))
    (pop-to-buffer buf)))

(defun lsp--session-workspaces (session)
  "Get all workspaces that are part of the SESSION."
  (-> session lsp-session-folder->servers hash-table-values -flatten -uniq))

(defun lsp--find-multiroot-workspace (session client project-root)
  "Look for a multiroot connection in SESSION created from CLIENT for PROJECT-ROOT and BUFFER-MAJOR-MODE."
  (when (lsp--client-multi-root client)
    (-when-let (multi-root-workspace (->> session
                                          (lsp--session-workspaces)
                                          (--first (eq (-> it lsp--workspace-client lsp--client-server-id)
                                                       (lsp--client-server-id client)))))
      (with-lsp-workspace multi-root-workspace
        (lsp-notify "workspace/didChangeWorkspaceFolders"
                    `(:event (:added ,(vector (list :uri (lsp--path-to-uri project-root)))))))

      (->> session (lsp-session-folder->servers) (gethash project-root) (pushnew multi-root-workspace))
      (->> session (lsp-session-server-id->folders) (gethash (lsp--client-server-id client)) (pushnew project-root))

      (lsp--persist-session session)

      (lsp--info "Opened folder %s in workspace %s" project-root (lsp--workspace-print multi-root-workspace))

      multi-root-workspace)))

(defun lsp--ensure-lsp-servers (session clients project-root ignore-multi-folder)
  "Ensure that SESSION contain server CLIENTS created for PROJECT-ROOT.
IGNORE-MULTI-FOLDER to ignore multi folder server."
  (-map (lambda (client)
          (or
           (lsp--find-workspace session client project-root)
           (unless ignore-multi-folder
             (lsp--find-multiroot-workspace session client project-root))
           (lsp--start-connection session client project-root)))
        clients))

(defun lsp--spinner-stop ()
  "Stop the spinner in case all of the workspaces are started."
  (when (--all? (eq (lsp--workspace-status it) 'initialized)
                lsp--buffer-workspaces)
    (spinner-stop)))

(defun lsp--open-in-workspace (workspace)
  "Open in existing WORKSPACE."
  (if (eq 'initialized (lsp--workspace-status workspace))
      ;; when workspace is initialized just call document did open.
      (progn
        (with-lsp-workspace workspace
          (when-let (before-document-open-fn (-> workspace
                                                 lsp--workspace-client
                                                 lsp--client-before-file-open-fn))
            (funcall before-document-open-fn workspace))
          (lsp--text-document-did-open))
        (lsp--spinner-stop))
    ;; when it is not initialized
    (lsp--spinner-start)
    (pushnew (current-buffer) (lsp--workspace-buffers workspace))))

(defun lsp--find-workspace (session client project-root)
  "Find server connection created with CLIENT in SESSION for PROJECT-ROOT."
  (when-let ((workspace (->> session
                             (lsp-session-folder->servers)
                             (gethash project-root)
                             (--first (eql (-> it lsp--workspace-client lsp--client-server-id)
                                           (lsp--client-server-id client))))))
    (lsp--open-in-workspace workspace)
    workspace))

(defun lsp--find-root-interactively (session)
  "Find project interactively.
Returns nil if the project should not be added to the current SESSION."
  (condition-case nil
      (let* ((project-root-suggestion (or (lsp--suggest-project-root) default-directory))
             (choices (list
                       (format "Import project root %s" project-root-suggestion)
                       "Import project by selecting root directory interactively."
                       (format "Do not ask more for the current project(add \"%s\" to lsp-session-folder-blacklist)"
                               project-root-suggestion)
                       "Do not ask more for the current project(select ignore path interactively)."
                       "Do nothing and ask me again when opening other files from the folder."))
             (action-index (cl-position
                            (completing-read (format "%s is not part of any project. Select action: "
                                                     (buffer-name))
                                             choices
                                             nil
                                             t)
                            choices
                            :test 'equal)))
        (case action-index
          (0 project-root-suggestion)
          (1 (read-directory-name "Select workspace folder to add: "
                                  (or project-root-suggestion default-directory)
                                  nil
                                  t))
          (2 (push project-root-suggestion (lsp-session-folders-blacklist session))
             (lsp--persist-session session)
             nil)
          (3 (push (read-directory-name "Select folder to blacklist: "
                                        (or project-root-suggestion default-directory)
                                        nil
                                        t)
                   (lsp-session-folders-blacklist session))
             (lsp--persist-session session)
             nil)
          (t nil)))
    ('quit)))

(defun lsp-find-session-folder (session file-name)
  "Look in the current SESSION for folder containing FILE-NAME."
  (->> session
       (lsp-session-folders)
       (--first (or (f-same? it file-name)
                    (f-ancestor-of? it file-name)))))

(defun lsp-find-workspace (server-id file-name)
  "Find workspace for SERVER-ID for FILE-NAME."
  (-when-let* ((session (lsp-session))
               (folder->servers (lsp-session-folder->servers session))
               (workspaces (if file-name
                               (gethash (lsp-find-session-folder session file-name) folder->servers)
                             (lsp--session-workspaces session))))

    (--first (eq (lsp--client-server-id (lsp--workspace-client it)) server-id) workspaces)))

(defun lsp--calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (f-ancestor-of? it file-name))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root
      (lsp--find-root-interactively session)))))

(defun lsp--try-open-in-library-workspace ()
  "Try opening current file as library file in any of the active workspace.
The library folders are defined by each client for each of the active workspace."
  (when-let (workspace (->> (lsp-session)
                            (lsp--session-workspaces)
                            (--first
                             (when-let (library-folders-fn
                                        (-> it lsp--workspace-client lsp--client-library-folders-fn))
                               (-first (lambda (library-folder)
                                         (f-ancestor-of? library-folder (buffer-file-name)))
                                       (funcall library-folders-fn it))))))
    (lsp--open-in-workspace workspace)
    (view-mode t)
    (list workspace)))

(defun lsp--persist-session (session)
  "Persist SESSION to `lsp-session-file'."
  (lsp--persist lsp-session-file (make-lsp-session
                                  :folders (lsp-session-folders session)
                                  :folders-blacklist (lsp-session-folders-blacklist session)
                                  :server-id->folders (lsp-session-server-id->folders session))))

(defun lsp--try-project-root-workspaces (ignore-multi-folder)
  "Try create opening file as a project file.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be openned in multi folder language server if there is
such."
  (-let ((session (lsp-session)))
    (-if-let (clients (if current-prefix-arg
                          (list (lsp--completing-read "Select server to start: "
                                                      (ht-values lsp-clients)
                                                      (-compose 'symbol-name 'lsp--client-server-id) nil t))
                        (lsp--find-clients major-mode (buffer-file-name))))
        (-if-let (project-root (lsp--calculate-root session (buffer-file-name)))
            (progn
              ;; update project roots if needed and persit the lsp session
              (unless (-contains? (lsp-session-folders session) project-root)
                (push project-root (lsp-session-folders session))
                (lsp--persist-session session))
              (lsp--ensure-lsp-servers session clients project-root ignore-multi-folder))
          (lsp--warn  "%s not in project." (buffer-name))
          nil)
      (lsp--warn "No LSP server for %s." major-mode)
      nil)))

(defun lsp-shutdown-workspace ()
  "Shutdown language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) (when (y-or-n-p (format "Are you sure tou want to stop server %s?"
                                                       (lsp--workspace-print workspace)))
                                 workspace))
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp--warn "Stopping %s" (lsp--workspace-print it))
    (setf (lsp--workspace-shutdown-action it) 'shutdown)
    (with-lsp-workspace it (lsp--shutdown-workspace))))

(defun lsp-restart-workspace ()
  "Restart language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp--warn "Restarting %s" (lsp--workspace-print it))
    (setf (lsp--workspace-shutdown-action it) 'restart)
    (with-lsp-workspace it (lsp--shutdown-workspace))))

;;;###autoload
(defun lsp (&optional ignore-multi-folder)
  "Entry point for the server startup.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be openned in multi folder language server if there is
such."
  (interactive)

  (when (and lsp-auto-configure lsp-auto-require-clients)
    (require 'lsp-clients))

  (when (and (buffer-file-name)
             (setq-local lsp--buffer-workspaces (or (lsp--try-open-in-library-workspace)
                                                    (lsp--try-project-root-workspaces ignore-multi-folder))))
    (lsp-mode 1)
    (when lsp-auto-configure (lsp--auto-configure))

    (lsp--info "Connected to %s."
               (apply 'concat (--map (format "[%s]" (lsp--workspace-print it))
                                     lsp--buffer-workspaces)))))

(provide 'lsp-mode)
;;; lsp-mode.el ends here
