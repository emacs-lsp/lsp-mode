;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

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
(require 'lsp-receive)
(require 'lsp-common)

;;; Code:

(cl-defstruct lsp--client
  (language-id nil :read-only t)
  ;; function to send a message and waits for the next message from the server
  (send-sync nil :read-only t)
  ;; function to send a message and not wait for the next response
  (send-async nil :read-only t)
  (type nil :read-only t)
  (new-connection nil :read-only t)
  (get-root nil :read-only t)
  (ignore-regexps nil :read-only t)
  (method-handlers (make-hash-table :test 'equal) :read-only t))

(defvar lsp--defined-clients (make-hash-table))

(cl-defstruct lsp--workspace
  (parser nil :read-only t)
  (language-id nil :read-only t)
  (last-id 0)
  ;; file-versions is a hashtable of files "owned" by the workspace
  (file-versions nil)
  (server-capabilities nil)
  (root nil :ready-only t)
  (client nil :read-only t)
  (change-timer-disabled nil)
  (proc nil :read-only t) ;; the process we communicate with
  (cmd-proc nil :read-only t) ;; the process we launch initially
  (buffers nil) ;; a list of buffers associated with this workspace
  (overlays nil) ;; a list of '(START . END) cons pairs with overlays on them
  )

(defvar-local lsp--cur-workspace nil)
(defvar lsp--workspaces (make-hash-table :test #'equal)
  "Table of known workspaces, indexed by the project root directory.")

(defvar lsp--ignored-workspace-roots (make-hash-table :test #'equal)
  "Table of project roots which should not have a workspace,
  indexed by the project root directory.

  This is populated when the user declines to open a workspace
  for a file in the workspace")

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized
for a new workspace."
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
  "Customization group for lsp-mode."
  :group 'tools)

;;;###autoload
(defgroup lsp-faces nil
  "Faces for lsp-mode."
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
(defcustom lsp-ask-before-initializing t
  "Always ask before initializing a new project."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-ask-before-initializing-once-only t
  "Only ask once per project root directory whether to initialize."
  :type 'boolean
  :group 'lsp-mode)


;;;###autoload
(defcustom lsp-enable-eldoc t
  "Enable `eldoc-mode' integration."
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
(defcustom lsp-enable-flycheck t
  "Enable flycheck integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defface lsp-face-highlight-textual
  '((t :background "yellow"))
  "Face used for textual occurances of symbols."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-hightlight-read
  '((t :background "red"))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-hightlight-write
  '((t :background "green"))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(defun lsp-client-on-notification (mode method callback)
  (cl-assert (cl-typep callback 'function) nil
    "lsp-client-on-notification: CALLBACK is not a function.")
  (let ((client (gethash mode lsp--defined-clients nil)))
    (cl-assert client nil (format "%s doesn't have a defined client" mode))
    (puthash method callback (lsp--client-method-handlers client))))

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (plist-put (lsp--make-notification method params)
    :id (cl-incf (lsp--workspace-last-id lsp--cur-workspace))))

(defun lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (unless (stringp method)
    (signal 'wrong-type-argument (list 'stringp method)))
  `(:jsonrpc "2.0" :method ,method :params ,params))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS."
  (let ((json-str (json-encode params)))
    (format
      "Content-Length: %d\r\n\r\n%s"
      (string-bytes json-str) json-str)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall (lsp--client-send-async (lsp--workspace-client lsp--cur-workspace))
    (lsp--make-message body)
    (lsp--workspace-proc lsp--cur-workspace)))

(defun lsp--cur-workspace-check ()
  (unless lsp--cur-workspace
    (user-error "No language server is associated with this buffer")))

(defun lsp--cur-parser ()
  (lsp--workspace-parser lsp--cur-workspace))

(defun lsp--send-request (body &optional no-wait)
  "Send BODY as a request to the language server, get the response.
If no-wait is non-nil, don't synchronously wait for a response."
  ;; lsp-send-sync should loop until lsp--from-server returns nil
  ;; in the case of Rust Language Server, this can be done with
  ;; 'accept-process-output`.'
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
          (parser (lsp--cur-parser))
          (send-func (if no-wait
                       (lsp--client-send-async client)
                       (lsp--client-send-sync client))))
    (setf (lsp--parser-waiting-for-response parser) (not no-wait))
    (funcall send-func
      (lsp--make-message body)
      (lsp--workspace-proc lsp--cur-workspace))
    (when (not no-wait)
      (prog1 (lsp--parser-response-result parser)
        (setf (lsp--parser-response-result parser) nil)))))

(defun lsp--inc-cur-file-version ()
  (puthash buffer-file-name (1+ (lsp--cur-file-version))
    (lsp--workspace-file-versions lsp--cur-workspace)))

(defun lsp--cur-file-version ()
  "Return the file version number.  If INC, increment it before."
  (gethash buffer-file-name (lsp--workspace-file-versions lsp--cur-workspace)))

(defun lsp--make-text-document-item ()
  "Make TextDocumentItem for the currently opened file.

interface TextDocumentItem {
    uri: string; // The text document's URI
    languageId: string; // The text document's language identifier.
    version: number;
    text: string;
}"
  `(:uri ,(concat "file://" buffer-file-name)
     :languageId ,(lsp--workspace-language-id lsp--cur-workspace)
     :version ,(lsp--cur-file-version)
     :text ,(buffer-substring-no-properties (point-min) (point-max))))

(defun lsp--initialize (language-id client parser &optional data)
  (let* ((cur-dir (expand-file-name default-directory))
          (cmd-proc (if (consp data) (car data) data))
          (proc (if (consp data) (cdr data) data))
          root response)
    (if (gethash cur-dir lsp--workspaces)
      (user-error "This workspace has already been initialized")
      (setq root (funcall (lsp--client-get-root client)))
      (setq lsp--cur-workspace (make-lsp--workspace
                                 :parser parser
                                 :language-id language-id
                                 :file-versions (make-hash-table :test #'equal)
                                 :last-id 0
                                 :root root
                                 :client client
                                 :proc proc
                                 :cmd-proc cmd-proc))
      (puthash root lsp--cur-workspace lsp--workspaces))
    (setf (lsp--parser-workspace parser) lsp--cur-workspace)
    (setq response (lsp--send-request (lsp--make-request "initialize"
                                        `(:processId ,(emacs-pid) :rootPath ,root
                                           :capabilities ,(lsp--client-capabilities)))))
    (setf (lsp--workspace-server-capabilities lsp--cur-workspace)
      (gethash "capabilities" response))
    (run-hooks lsp-after-initialize-hook)
    ;; Version 3.0 now sends an "initialized" notification to allow registration
    ;; of server capabilities
    (lsp--send-notification (lsp--make-notification "initialized" nil))))

(defun lsp--shutdown-cur-workspace ()
  "Shut down the language server process for lsp--cur-workspace"
  (lsp--send-request (lsp--make-request "shutdown" (make-hash-table)))
  (lsp--send-notification (lsp--make-notification "exit" nil))
  (lsp--uninitialize-workspace))

(defun lsp--uninitialize-workspace ()
  "When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (remhash (lsp--workspace-root lsp--cur-workspace) lsp--workspaces)
  (let ((old-root (lsp--workspace-root lsp--cur-workspace)))
    (with-current-buffer (current-buffer)
      (kill-process (lsp--workspace-proc lsp--cur-workspace))
      (setq lsp--cur-workspace nil)
      (lsp--unset-variables)
      (kill-local-variable 'lsp--cur-workspace))
    (message "workspace uninitialized: %s" old-root)))

;; NOTE: Possibly make this function subject to a setting, if older LSP servers
;; are unhappy
(defun lsp--client-capabilities ()
  "Return the client capabilites"
  `(:workspace    ,(lsp--client-workspace-capabilities)
    :textDocument ,(lsp--client-textdocument-capabilities)))

(defun lsp--client-workspace-capabilities ()
  "Client Workspace capabilities according to LSP"
  `(:executeCommand (:dynamicRegistration t)))

(defun lsp--client-textdocument-capabilities ()
  "Client Text document capabilities according to LSP"
  (make-hash-table))

(defun lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (lsp--workspace-server-capabilities lsp--cur-workspace))

(defun lsp--set-sync-method ()
  (setq lsp--server-sync-method (or lsp-document-sync-method
                                  (alist-get
                                    (gethash "textDocumentSync"
                                      (lsp--server-capabilities))
                                    lsp--sync-methods))))

(defun lsp--client-request-handlers ()
  "Handlers for requests originating from the server"
  (let* ((table (make-hash-table :test 'equal)))
    ;; ("client/registerCapability"   (error "client/registerCapability not implemented"))
    ;; ("client/unregisterCapability" (error "client/unregisterCapability not implemented"))
    (puthash "workspace/applyEdit" #'lsp--workspace-apply-edit-handler table)
    table))

(defun lsp--workspace-apply-edit-handler (workspace params)
  (lsp--apply-workspace-edits (gethash "edit" params))
  ;; TODO: send reply
  )

(defun lsp--should-initialize ()
  "Ask user if a new Language Server for the current file should be started.
If `lsp--dont-ask-init' is bound, return non-nil."
  (let* ((client (gethash major-mode lsp--defined-clients)) ;; should succeed, given call chain
         (root (funcall (lsp--client-get-root client))))
    (if (gethash root lsp--ignored-workspace-roots nil)
        nil
      (if lsp-ask-before-initializing
          (let ((ans (y-or-n-p "Start a new Language Server for this project? ")))
            (when (not ans) (puthash root t lsp--ignored-workspace-roots))
            ans)
           t))))

(defun lsp--text-document-did-open ()
  "Executed when a new file is opened, added to `find-file-hook'."
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace))
  (let ((cur-dir (expand-file-name default-directory))
         client data set-vars parser)
    (if (catch 'break
          (dolist (key (hash-table-keys lsp--workspaces))
            (when (string-prefix-p key cur-dir)
              (setq lsp--cur-workspace (gethash key lsp--workspaces))
              (throw 'break key))))
      (progn
        (setq set-vars t)
        (puthash buffer-file-name 0 (lsp--workspace-file-versions lsp--cur-workspace))
        (lsp--send-notification
          (lsp--make-notification
            "textDocument/didOpen"
            `(:textDocument ,(lsp--make-text-document-item))))
        (push (current-buffer) (lsp--workspace-buffers lsp--cur-workspace)))

      (setq client (gethash major-mode lsp--defined-clients))
      (when (and client (lsp--should-initialize))
        (setq parser (make-lsp--parser
                      :method-handlers (lsp--client-method-handlers client)
                      :request-handlers (lsp--client-request-handlers))
          data (funcall (lsp--client-new-connection client)
                 (lsp--parser-make-filter
                   parser
                   (lsp--client-ignore-regexps client))
                 #'(lambda (_p exit-str)
                     (when lsp--cur-workspace
                       (dolist (buffer (lsp--workspace-buffers lsp--cur-workspace))
                         (with-current-buffer buffer
                           (message "%s: %s has exited (%s)"
                             (lsp--workspace-root lsp--cur-workspace)
                             (process-name (lsp--workspace-proc lsp--cur-workspace))
                             exit-str)
                           (lsp--uninitialize-workspace)))))))
        (setq set-vars t)
        (lsp--initialize (lsp--client-language-id client)
          client parser data)
        (setq set-vars t)
        (puthash buffer-file-name 0 (lsp--workspace-file-versions lsp--cur-workspace))
        (lsp--send-notification
          (lsp--make-notification
            "textDocument/didOpen"
            `(:textDocument ,(lsp--make-text-document-item))))
        (push (current-buffer) (lsp--workspace-buffers lsp--cur-workspace))))

    (when set-vars
      (lsp--set-variables)
      (lsp--set-sync-method))))

(defun lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  `(:uri ,(concat "file://" buffer-file-name)))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (plist-put (lsp--text-document-identifier) :version (lsp--cur-file-version)))

(defun lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.
interface Position {
    line: number;
    character: number;
}"
  `(:line ,line :character ,char))

(defun lsp--cur-line ()
  (1- (line-number-at-pos)))

(defun lsp--cur-column ()
  (- (point) (line-beginning-position)))

(defun lsp--cur-position ()
  "Make a Position object for the current point."
  (lsp--position (lsp--cur-line) (lsp--cur-column)))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (lsp--cur-position)))

(defun lsp--position-p (p)
  (and (numberp (plist-get p :line))
    (numberp (plist-get p :character))))

(defun lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  ;; make sure start and end are Position objects
  (unless (lsp--position-p start)
    (signal 'wrong-type-argument `(lsp--position-p ,start)))
  (unless (lsp--position-p end)
    (signal 'wrong-type-argument `(lsp--position-p ,end)))

  `(:start ,start :end ,end))

(defun lsp--region-to-range (start end)
  "Make Range object for the current region."
  (lsp--range (lsp--point-to-position start)
    (lsp--point-to-position end)))

(defun lsp--current-region-or-pos ()
  "If the region is active return that, else get the point"
  (if mark-active
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point) (point)))) 

(defun lsp--range-start-line (range)
  "Return the start line for a given LSP range, in LSP coordinates"
  (plist-get (plist-get range :start) :line))

(defun lsp--range-end-line (range)
  "Return the end line for a given LSP range, in LSP coordinates"
  (plist-get (plist-get range :end) :line))

(defun lsp--apply-workspace-edits (edits)
  (cl-letf ((lsp-ask-before-initializing nil)
             ((lsp--workspace-change-timer-disabled lsp--cur-workspace) t))
    (maphash (lambda (key value)
               (lsp--apply-workspace-edit key value))
      (gethash "changes" edits))))

(defun lsp--apply-workspace-edit (uri edits)
  (let ((filename (string-remove-prefix "file://" uri)))
    ;; TODO: What if the buffer has been modified?
    ;;       Although, for incremental sync that should be fine
    (when (not (find-buffer-visiting filename))
      (progn (find-file filename)
             (lsp--text-document-did-open)))
    (lsp--apply-text-edits edits)))

(defun lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object in EDITS."
  (dolist (edit edits)
    (lsp--apply-text-edit edit)))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (let* ((range (gethash "range" text-edit))
          (start-point (lsp--position-to-point (gethash "start" range)))
          (end-point (lsp--position-to-point (gethash "end" range))))
    (save-excursion
      (goto-char start-point)
      (delete-region start-point end-point)
      (insert (gethash "newText" text-edit)))))

(defun lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (gethash cap (or capabilities (lsp--server-capabilities))))

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
  (if (eq length 0)
      ;; Adding something, work from start only
      `(:range ,(lsp--range (lsp--point-to-position start)
                            (lsp--point-to-position start))
               :rangeLength 0
               :text ,(buffer-substring-no-properties start end))

    ;; Deleting something
    `(:range ,(lsp--range (lsp--point-to-position start)
                          (lsp--point-to-position (+ end length)))
             :rangeLength ,length
             :text "")))

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
  `(:text ,(buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defcustom lsp-change-idle-delay 0.5
  "Number of seconds of idle timer to wait before sending file changes to the server."
  :group 'lsp-mode
  :type 'number)

(defvar lsp--change-idle-timer nil
  "Idle timer which sends changes to the language server.")
(defvar lsp--last-workspace-timer nil
  "The last workspace for which the onChange idle timer was set.")

(defvar-local lsp--changes [])
(defvar-local lsp--has-changes []
  "non-nil if the current buffer has any changes yet to be sent.")

(defun lsp--rem-idle-timer ()
  (when lsp--change-idle-timer
    (cancel-timer lsp--change-idle-timer)
    (setq
      lsp--change-idle-timer nil
      lsp--last-workspace-timer nil)))

(defun lsp--set-idle-timer (workspace)
  (setq lsp--change-idle-timer (run-at-time lsp-change-idle-delay nil
                                 #'(lambda ()
                                     (cl-assert workspace)
                                     (lsp--send-changes workspace)))
    lsp--last-workspace-timer lsp--cur-workspace))

(defun lsp--flush-other-workspace-changes ()
  "Flush changes for any other workspace."
  (when (and lsp--last-workspace-timer
          (not (eq lsp--last-workspace-timer lsp--cur-workspace)))
    ;; A timer for a different workspace was set, flush those
    ;; changes first.
    (lsp--send-changes lsp--last-workspace-timer t)))

(defun lsp--send-changes (workspace &optional no-flush-other)
  "Sends any queued changes for this WORKSPACE.
Called before any function that performs any query to the languge server related
to a text document."
  (unless no-flush-other
    (lsp--flush-other-workspace-changes))
  (lsp--rem-idle-timer)
  (dolist (buffer (lsp--workspace-buffers workspace))
    (with-current-buffer buffer
      (when (and lsp--has-changes
                 (not (eq lsp--server-sync-method 'none)))
        (lsp--inc-cur-file-version)
        (lsp--send-notification
          (lsp--make-notification
            "textDocument/didChange"
            `(:textDocument
               ,(lsp--versioned-text-document-identifier)
               :contentChanges
               ,(cl-case lsp--server-sync-method
                  ('incremental lsp--changes)
                  ('full `[,(lsp--full-change-event)])
                  ('none `[])))))
        (setq lsp--changes []
          lsp--has-changes nil)))))

(defun lsp--push-change (change-event)
  "Push CHANGE-EVENT to the buffer change vector."
  (setq lsp--changes (vconcat lsp--changes `(,change-event))))

(defun lsp-on-change (start end length)
    "Executed when a file is changed.
  Added to `after-change-functions'"
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
  (lsp--flush-other-workspace-changes)
  (when (and lsp--cur-workspace
          (not (or (eq lsp--server-sync-method 'none)
                 (eq lsp--server-sync-method nil))))
    (setq lsp--has-changes t)
    (lsp--rem-idle-timer)
    (when (eq lsp--server-sync-method 'incremental)
      (lsp--push-change (lsp--text-document-content-change-event start end length)))
    (if (lsp--workspace-change-timer-disabled lsp--cur-workspace)
      (lsp--send-changes lsp--cur-workspace)
      (lsp--set-idle-timer lsp--cur-workspace))))

;; (defun lsp--text-document-did-change (start end length)
;;   "Executed when a file is changed.
;; Added to `after-change-functions'"
;;   (when lsp--cur-workspace
;;     (unless (or (eq lsp--server-sync-method 'none)
;; 		(eq lsp--server-sync-method nil))
;;       (lsp--cur-file-version t)
;;       (lsp--send-notification
;;        (lsp--make-notification
;; 	"textDocument/didChange"
;; 	`(:textDocument
;; 	  ,(lsp--versioned-text-document-identifier)
;; 	  :contentChanges
;; 	  [,(lsp--text-document-content-change-event start end length)]))))))

(defun lsp--shut-down-p ()
  (y-or-n-p "Close the language server for this workspace? "))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace)
    (ignore-errors
      (let ((file-versions (lsp--workspace-file-versions lsp--cur-workspace))
             (old-buffers (lsp--workspace-buffers lsp--cur-workspace)))
        ;; remove buffer from the current workspace's list of buffers
        ;; do a sanity check first
        (cl-assert (memq (current-buffer) old-buffers) t
          "Current buffer isn't in the workspace's list of buffers.")
        (setf (lsp--workspace-buffers lsp--cur-workspace)
          (delq (current-buffer) old-buffers))

        (remhash buffer-file-name file-versions)
        (lsp--send-notification
          (lsp--make-notification
            "textDocument/didClose"
            `(:textDocument ,(lsp--versioned-text-document-identifier))))
        (when (and (= 0 (hash-table-count file-versions)) (lsp--shut-down-p))
          (lsp--shutdown-cur-workspace))))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace)
    (lsp--send-notification
      (lsp--make-notification
        "textDocument/didSave"
        `(:textDocument ,(lsp--versioned-text-document-identifier))))))

(defun lsp--text-document-position-params ()
  "Make TextDocumentPositionParams for the current point in the current document."
  `(:textDocument ,(lsp--text-document-identifier)
     :position ,(lsp--position (lsp--cur-line)
                  (lsp--cur-column))))

(defun lsp--text-document-code-action-params ()
  "Make CodeActionParams for the current region in the current document."
  `(:textDocument ,(lsp--text-document-identifier)
     :range ,(lsp--current-region-or-pos)
     :context (:diagnostics ,(lsp--cur-line-diagnotics))))

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
    (cl-coerce (mapcar #'lsp-diagnostic-original diags-in-range) 'vector)))


(defconst lsp--completion-item-kind
  `(
     (1 . "Text")
     (2 . "Method")
     (3 . "Function")
     (4 . "Constructor")
     (5 . "Field")
     (6 . "Variable")
     (7 . "Class")
     (8 . "Interface")
     (9 . "Module")
     (10 . "Property")
     (11 . "Unit")
     (12 . "Value")
     (13 . "Enum")
     (14 . "Keyword")
     (15 . "Snippet")
     (16 . "Color")
     (17 . "File")
     (18 . "Reference")))

(defun lsp--make-completion-item (item)
  (propertize (gethash "insertText" item (gethash "label" item))
    'lsp-completion-item
    item))

(defun lsp--annotate (item)
  (let* ((table (plist-get (text-properties-at 0 item) 'lsp-completion-item))
          (detail (gethash "detail" table nil))
          (kind (alist-get (gethash "kind" table nil) lsp--completion-item-kind)))
    (concat
      " "
      detail
      (when kind " ")
      (when kind (format "(%s)" kind)))))

(defun lsp--sort-string (c)
  (gethash "sortText" c (gethash "label" c "")))

(defun lsp--sort-completions (completions)
  (sort completions #'(lambda (c1 c2)
                        (when (string-lessp
                                (lsp--sort-string c1)
                                (lsp--sort-string c2))
                          t))))

(defun lsp--get-completions ()
  (lsp--send-changes lsp--cur-workspace)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (list
      (if bounds (car bounds) (point))
      (if bounds (cdr bounds) (point))
      (completion-table-dynamic
        #'(lambda (_)
            ;; *we* don't need to know the string being completed
            ;; the language server does all the work by itself
            (let* ((resp (lsp--send-request (lsp--make-request
                                             "textDocument/completion"
                                              (lsp--text-document-position-params))))
                    (items (cond
                             ((null resp) nil)
                             ((hash-table-p resp) (gethash "items" resp nil))
                             ((sequencep resp) resp))))
              (mapcar #'lsp--make-completion-item items))))
      :annotation-function #'lsp--annotate
      :display-sort-function #'lsp--sort-completions)))

;;; TODO: implement completionItem/resolve

(defun lsp--location-to-xref (location)
  "Convert Location object LOCATION to an xref-item.
interface Location {
    uri: string;
    range: Range;
}"
  (lsp--send-changes lsp--cur-workspace)
  (let ((uri (string-remove-prefix "file://" (gethash "uri" location)))
         (ref-pos (gethash "start" (gethash "range" location))))
    (xref-make uri
      (xref-make-file-location uri
        (1+ (gethash "line" ref-pos))
        (gethash "character" ref-pos)))))

(defun lsp--get-defitions ()
  "Get definition of the current symbol under point.
Returns xref-item(s)."
  (lsp--send-changes lsp--cur-workspace)
  (let ((location (lsp--send-request (lsp--make-request
                                       "textDocument/definition"
                                       (lsp--text-document-position-params)))))
    (if (consp location) ;;multiple definitions
      (mapcar 'lsp--location-to-xref location)
      (lsp--location-to-xref location))))

(defun lsp--make-reference-params (&optional td-position)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
      :context `(:includeDeclaration ,json-false))))

(defun lsp--get-references ()
  "Get all references for the symbol under point.
Returns xref-item(s)."
  (lsp--send-changes lsp--cur-workspace)
  (let ((location  (lsp--send-request (lsp--make-request
                                        "textDocument/references"
                                        (lsp--make-reference-params)))))
    (if (consp location)
      (mapcar 'lsp--location-to-xref location)
      (and location (lsp--location-to-xref location)))))

(defun lsp--marked-string-to-string (contents)
  "Convert the MarkedString object to a user viewable string."
  (if (hash-table-p contents)
    (gethash "value" contents)
    contents))

(defun lsp-eldoc ()
  (when (and (gethash "codeActionProvider" (lsp--server-capabilities))
             lsp-enable-codeaction)
    (lsp--text-document-code-action))
  (lsp--text-document-hover-string))

(defun lsp--text-document-hover-string ()
  "interface Hover {
    contents: MarkedString | MarkedString[];
    range?: Range;
}

type MarkedString = string | { language: string; value: string };"
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (if (symbol-at-point)
    (let* ((hover (lsp--send-request (lsp--make-request
                                       "textDocument/hover"
                                       (lsp--text-document-position-params))))
            (contents (gethash "contents" (or hover (make-hash-table)))))
      (lsp--marked-string-to-string (if (consp contents)
                                      (car contents)
                                      contents)))
    nil))

(defun lsp-info-under-point ()
  "Show relevant documentation for the thing under point."
  (interactive)
  (lsp--text-document-hover-string))

(defun lsp--text-document-code-action ()
  "Request code action to automatically fix issues reported by
the diagnostics"
  (lsp--cur-workspace-check)
  (let* ((actions (lsp--send-request (lsp--make-request
                                    "textDocument/codeAction"
                                    (lsp--text-document-code-action-params))
                                     )))
    (setq lsp-code-actions (cl-union actions lsp-code-actions))
    nil))

(defun lsp--make-document-formatting-options ()
  (let ((json-false :json-false))
    `(:tabSize ,tab-width :insertSpaces
       ,(if indent-tabs-mode json-false t))))

(defun lsp--make-document-formatting-params ()
  `(:textDocument ,(lsp--text-document-identifier)
     :options ,(lsp--make-document-formatting-options)))

(defun lsp--text-document-format ()
  "Ask the server to format this document."
  (lsp--send-changes lsp--cur-workspace)
  (let ((edits (lsp--send-request (lsp--make-request
                                    "textDocument/formatting"
                                    (lsp--make-document-formatting-params)))))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

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
     (2 . lsp-face-hightlight-read)
     (3 . lsp-face-hightlight-write)))

(defun lsp--remove-cur-overlays ()
  (dolist (pair (lsp--workspace-overlays lsp--cur-workspace))
    (remove-overlays (car pair) (cdr pair))))

(defun lsp-symbol-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (lsp--send-changes lsp--cur-workspace)
  (lsp--remove-cur-overlays)
  (let ((highlights (lsp--send-request (lsp--make-request
                                         "textDocument/documentHighlight"
                                         (lsp--make-document-formatting-params))))
         kind start-point end-point range)
    (dolist (highlight highlights)
      (setq range (gethash "range" highlight nil)
        kind (gethash "kind" highlight 1)
        start-point (lsp--position-to-point (gethash "start" range))
        end-point (lsp--position-to-point (gethash "end" range)))
      (overlay-put (make-overlay start-point end-point) 'face
        (cdr (assq kind lsp--highlight-kind-face)))
      (push (lsp--workspace-overlays lsp--cur-workspace)
        (cons start-point end-point)))))

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
     (18 . "Array")))

(defun lsp--symbol-information-to-xref (symbol)
  (xref-make (format "%s %s"
               (alist-get (gethash "kind" symbol) lsp--symbol-kind)
               (gethash "name" symbol))
    (lsp--location-to-xref (gethash "location" symbol))))

(defun lsp-format-region (s e)
  (lsp--send-changes lsp--cur-workspace)
  (let ((edits (lsp--send-request (lsp--make-request
                                    "textDocument/rangeFormatting"
                                    (lsp--make-document-range-formatting-params s e)))))
    (lsp--apply-text-edits edits)))

(defun lsp--location-to-td-position (location)
  "Convert LOCATION to a TextDocumentPositionParams object."
  `(:textDocument (:uri ,(gethash "uri" location))
     :position ,(gethash "start" (gethash "range" location))))

(defun lsp--symbol-info-to-identifier (symbol)
  (propertize (gethash "name" symbol)
    'ref-params (lsp--make-reference-params
                  (lsp--location-to-td-position (gethash "location" symbol)))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (symbol-name (symbol-at-point))
    'def-params (lsp--text-document-position-params)
    'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (let ((json-false :json-false)
         (symbols (lsp--send-request (lsp--make-request
                                       "textDocument/documentSymbol"
                                       `(:textDocument ,(lsp--text-document-identifier))))))
    (mapcar #'lsp--symbol-info-to-identifier symbols)))

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
;;   nil)

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
          (params (plist-get properties 'def-params))
          (def (lsp--send-request (lsp--make-request
                                    "textDocument/definition"
                                    params))))
    (if (consp def)
      (mapcar 'lsp--location-to-xref def)
      (and def `(,(lsp--location-to-xref def))))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
          (params (plist-get properties 'ref-params))
          (ref (lsp--send-request (lsp--make-request
                                    "textDocument/references"
                                    params))))
    (if (consp ref)
      (mapcar 'lsp--location-to-xref ref)
      (and ref `(,(lsp--location-to-xref ref))))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (let ((symbols (lsp--send-request (lsp--make-request
                                      "workspace/symbol"
                                      `(:query ,pattern)))))
    (mapcar 'lsp--symbol-information-to-xref symbols)))

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
  (interactive "sRename to: ")
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (let ((edits (lsp--send-request (lsp--make-request
                                    "textDocument/rename"
                                    (lsp--make-document-rename-params newname)))))
    (lsp--apply-workspace-edits edits)))

(defun lsp--execute-lsp-server-command (command)
  "Given a COMMAND returned from the server via e.g.
'textDocument/codeAction' ceate and send a
'workspace/executeCommand' message"

  (lsp--send-execute-command (gethash "command" command) (gethash "arguments" command nil))
  )

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having
command COMMAND and optionsl ARGS"
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (lsp--send-request
   (lsp--make-request
    "workspace/executeCommand"
    (lsp--make-execute-command-params command args))))

(defun lsp--make-execute-command-params (cmd &optional args)
  (if args
      (list :command cmd :arguments args)
    (list :command cmd)))


(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
;; (defalias 'lsp-on-change #'lsp--text-document-did-change)
(defalias 'lsp-on-close #'lsp--text-document-did-close)
;; (defalias 'lsp-eldoc #'lsp--text-document-hover-string)
(defalias 'lsp-completion-at-point #'lsp--get-completions)
(defalias 'lsp-error-explainer #'lsp--error-explainer)

(defun lsp--unset-variables ()
  (when lsp-enable-eldoc
    (setq-local eldoc-documentation-function 'ignore))
  (when lsp-enable-xref
    (setq-local xref-backend-functions nil))
  (when lsp-enable-completion-at-point
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point))
  (remove-hook 'after-change-functions #'lsp-on-change))

(defun lsp--set-variables ()
  (when (or lsp-enable-eldoc lsp-enable-codeaction)
    (setq-local eldoc-documentation-function #'lsp-eldoc)
    (eldoc-mode 1))
  (when (and lsp-enable-flycheck (featurep 'flycheck))
    (setq-local flycheck-check-syntax-automatically nil)
    (setq-local flycheck-checker 'lsp)
    (unless (memq 'lsp flycheck-checkers)
      (add-to-list 'flycheck-checkers 'lsp))
    (unless (memq 'flycheck-buffer lsp-after-diagnostics-hook)
      (add-hook 'lsp-after-diagnostics-hook #'(lambda ()
                                                (when flycheck-mode
                                                  (flycheck-buffer))))))
  ;; (setq-local indent-region-function #'lsp-format-region)
  (when lsp-enable-xref
    (setq-local xref-backend-functions #'lsp--xref-backend))
  (when (and (gethash "completionProvider" (lsp--server-capabilities))
          lsp-enable-completion-at-point)
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point))
  ;; Make sure the hook is local (last param) otherwise we see all changes for all buffers
  (add-hook 'after-change-functions #'lsp-on-change nil t))


(defun lsp--error-explainer (fc-error)
    "Proof of concept to use this flycheck function to apply a
    codeAction. This should eventually make use of the completion of
    https://github.com/flycheck/flycheck/pull/1022 and
    https://github.com/flycheck/flycheck/issues/530#issuecomment-235224763"
  (message "lsp--error-explainer: got %s" fc-error))

;;----------------------------------------------------------------------
;; AZ: Not sure where this section should go, putting it here for now

;; AZ: This section based on/inspired by the intero 'intero-apply-suggestions' code, at
;; https://github.com/commercialhaskell/intero/blob/master/elisp/intero.el

(defvar-local lsp-code-actions nil
  "Code actions for the buffer.")

(defun lsp-apply-commands ()
  "Prompt and apply any codeAction commands."
  (interactive)
  (if (null lsp-code-actions)
      (message "No actions to apply")
    (let ((to-apply
           (lsp--intero-multiswitch
            (format "There are %d suggestions to apply:" (length lsp-code-actions))
            (cl-remove-if-not
             #'identity
             (mapcar
              (lambda (suggestion)
                ;; (cl-case (plist-get suggestion :type)
                ;;   (add-extension
                ;;    (list :key suggestion
                ;;          :title (concat "Add {-# LANGUAGE "
                ;;                         (plist-get suggestion :extension)
                ;;                         " #-}")
                ;;          :default t))
                ;;   (redundant-constraint
                ;;    (list :key suggestion
                ;;          :title (concat
                ;;                  "Remove redundant constraints: "
                ;;                  (string-join (plist-get suggestion :redundancies)
                ;;                               ", ")
                ;;                  "\n    from the "
                ;;                  (plist-get suggestion :signature))
                ;;          :default nil)))
                ;; (message "lsp-apply-command:suggestion command=%s"    (gethash "command" suggestion))
                ;; (message "lsp-apply-command:suggestion ommand=args%s" (gethash "arguments" suggestion))
                (list :key   (gethash "title" suggestion)
                      :title (gethash "title" suggestion)
                      :type  "codeAction"
                      :default t
                      :command suggestion)
                )
              lsp-code-actions)))))
      (if (null to-apply)
          (message "No changes selected to apply.")
        (let ((sorted (sort to-apply
                            (lambda (lt gt)
                              (let ((lt-line   (or (plist-get lt :line)   0))
                                    (lt-column (or (plist-get lt :column) 0))
                                    (gt-line   (or (plist-get gt :line)   0))
                                    (gt-column (or (plist-get gt :column) 0)))
                                (or (> lt-line gt-line)
                                    (and (= lt-line gt-line)
                                         (> lt-column gt-column))))))))
          ;; # Changes unrelated to the buffer
          (cl-loop
           for suggestion in sorted
           do ;; (message "lsp-apply-commands:suggestion=%s" suggestion)
              (cl-case (plist-get suggestion :type)
                (otherwise
                 (lsp--execute-lsp-server-command suggestion))))
          ;; # Changes that do not increase/decrease line numbers
          ;;
          ;; Update in-place suggestions

          ;; # Changes that do increase/decrease line numbers
          ;;

          ;; Add extensions to the top of the file
          )))))

;; The following is copied directly from intero. I suspect it would be better to
;; have it in a dependency somewhere

(defun lsp--intero-multiswitch (title options)
  "Displaying TITLE, read multiple flags from a list of OPTIONS.
Each option is a plist of (:key :default :title) wherein:

  :key should be something comparable with EQUAL
  :title should be a string
  :default (boolean) specifies the default checkedness"
  (let ((available-width (window-total-width)))
    (save-window-excursion
      (lsp--intero-with-temp-buffer
        (rename-buffer (generate-new-buffer-name "multiswitch"))
        (widget-insert (concat title "\n\n"))
        (widget-insert (propertize "Hit " 'face 'font-lock-comment-face))
        (widget-create 'push-button :notify
                       (lambda (&rest ignore)
                         (exit-recursive-edit))
                       "C-c C-c")
        (widget-insert (propertize " to apply these choices.\n\n" 'face 'font-lock-comment-face))
        (let* ((me (current-buffer))
               (choices (mapcar (lambda (option)
                                  (append option (list :value (plist-get option :default))))
                                options)))
          (cl-loop for option in choices
                   do (widget-create
                       'toggle
                       :notify (lambda (widget &rest ignore)
                                 (setq choices
                                       (mapcar (lambda (choice)
                                                 (if (equal (plist-get choice :key)
                                                            (plist-get (cdr widget) :key))
                                                     (plist-put choice :value (plist-get (cdr widget) :value))
                                                   choice))
                                               choices)))
                       :on (concat "[x] " (plist-get option :title))
                       :off (concat "[ ] " (plist-get option :title))
                       :value (plist-get option :default)
                       :key (plist-get option :key)
                       :command (plist-get option :command)))
          (let ((lines (line-number-at-pos)))
            (select-window (split-window-below))
            (switch-to-buffer me)
            (goto-char (point-min)))
          (use-local-map
           (let ((map (copy-keymap widget-keymap)))
             (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
             (define-key map (kbd "C-g") 'abort-recursive-edit)
             map))
          (widget-setup)
          (recursive-edit)
          (kill-buffer me)
          (mapcar (lambda (choice)
                    (plist-get choice :command))
                  (cl-remove-if-not (lambda (choice)
                                      (plist-get choice :value))
                                    choices)))))))

;; The following is copied directly from intero. I suspect it would be better to
;; have it in a dependency somewhere
(defmacro lsp--intero-with-temp-buffer (&rest body)
  "Run BODY in `with-temp-buffer', but inherit certain local variables from the current buffer first."
  (declare (indent 0) (debug t))
  `(let ((initial-buffer (current-buffer)))
     (with-temp-buffer
       (lsp--intero-inherit-local-variables initial-buffer)
       ,@body)))

;; The following is copied directly from intero. I suspect it would be better to
;; have it in a dependency somewhere
(defun lsp--intero-inherit-local-variables (buffer)
  "Make the current buffer inherit values of certain local variables from BUFFER."
  (let ((variables '(
                     ;; TODO: shouldn’t more of the above be here?
                     )))
    (cl-loop for v in variables do
             (set (make-local-variable v) (buffer-local-value v buffer)))))


(provide 'lsp-methods)
;;; lsp-methods.el ends here
