;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'xref)
(require 'subr-x)
(require 'lsp-receive)
(require 'lsp-common)

(cl-defstruct lsp--client
  (language-id :read-only t)
  (send-sync :read-only t)
  (send-async :read-only t)
  (type :read-only t)
  (new-connection :read-only t)
  (get-root :read-only t)
  (on-initialize :read-only))
(defvar lsp--defined-clients (make-hash-table))

(cl-defstruct lsp--workspace
  (language-id :read-only t)
  (last-id 0)
  (file-versions)
  (server-capabilities)
  (root :ready-only t)
  (client :read-only t)
  (data :read-only t))

(defvar-local lsp--cur-workspace nil)
(defvar lsp--workspaces (make-hash-table :test 'equal))

(defvar lsp--sync-methods
  '((0 . none)
    (1 . full)
    (2 . incremental)))
(defvar-local lsp--server-sync-method nil
  "Sync method recommended by the server.")

(defgroup lsp-mode nil
  "Customization group for lsp-mode.")

(defcustom lsp-document-sync-method 'full
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." 'none)
		 (const :tag "Documents are synced by always sending the full content of the document." 'full)
		 (const :tag "Documents are synced by always sending incremental changes to the document." 'incremental)
		 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

(defcustom lsp-enable-eldoc t
  "Enable `eldoc-mode' integration."
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
     (length json-str) json-str)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall (lsp--client-send-async (lsp--workspace-client lsp--cur-workspace))
	   (lsp--make-message body)
	   (lsp--workspace-data lsp--cur-workspace)))

(defun lsp--send-request (body &optional no-wait)
  "Send BODY as a request to the language server, get the response.
If wait-for-response is non-nil, don't synchronously wait for a response."
  (setq lsp--waiting-for-response (not no-wait))
  ;; lsp-send-sync should loop until lsp--from-server returns nil
  ;; in the case of Rust Language Server, this can be done with
  ;; 'accept-process-output`.'
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
	(send-func (if no-wait
		       (lsp--client-send-async client)
		     (lsp--client-send-sync client))))
    (funcall send-func
	     (lsp--make-message body)
	     (lsp--workspace-data lsp--cur-workspace))
    (when (not no-wait)
      (prog1 lsp--response-result (setq lsp--response-result nil)))))

(defun lsp--cur-file-version (&optional inc)
  "Return the file version number.  If INC, increment it before."
  (let* ((file-versions (lsp--workspace-file-versions lsp--cur-workspace))
	 (rev (gethash buffer-file-name file-versions)))
    (when inc
      (cl-incf rev)
      (puthash buffer-file-name rev file-versions))
    rev))

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

(defun lsp--initialize (language-id client &optional data)
  (let ((root)
	(cur-dir (expand-file-name default-directory))
	(response)
	(capabilities)
	(on-init (lsp--client-on-initialize client)))
    (if (gethash cur-dir lsp--workspaces)
	(user-error "This workspace has already been initialized")
      (setq lsp--cur-workspace (make-lsp--workspace
				:language-id language-id
				:file-versions (make-hash-table :test 'equal)
				:last-id 0
				:root (setq root
					    (funcall (lsp--client-get-root client)))
				:client client
				:data data))
      (puthash cur-dir lsp--cur-workspace lsp--workspaces))
    (setq response (lsp--send-request
		    (lsp--make-request
		     "initialize"
		     `(:processId ,(emacs-pid) :rootPath ,root
				  :capabilities ,(make-hash-table)))))
    (setf (lsp--workspace-server-capabilities lsp--cur-workspace)
	  (setq capabilities (gethash "capabilities" response)))
    (setq lsp--server-sync-method (or lsp-document-sync-method
			    (alist-get
			     (gethash "textDocumentSync" capabilities)
			     lsp--sync-methods)))
    (when on-init (funcall on-init))))

(defsubst lsp--should-initialize ()
  "Ask user if a new Language Server for the current file should be started."
  (y-or-n-p "Start a new Language Server for this project? "))

(defun lsp--text-document-did-open ()
  "Executed when a new file is opened, added to `find-file-hook'."
  (let ((cur-dir (expand-file-name default-directory))
	(key)
	(client)
	(data)
	(set-vars))
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
	    `(:textDocument ,(lsp--make-text-document-item)))))

      (setq client (gethash major-mode lsp--defined-clients))
      (when (and client (lsp--should-initialize))
	(setq data (funcall (lsp--client-new-connection client)))
	(setq set-vars t)
	(lsp--initialize (lsp--client-language-id client)
			 client
			 data)
	(lsp--text-document-did-open)))

    (when set-vars (lsp--set-variables))))

(defsubst lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  `(:uri ,(concat "file://" buffer-file-name)))

(defsubst lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (plist-put (lsp--text-document-identifier) :version (lsp--cur-file-version)))

(defsubst lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.
interface Position {
    line: number;
    character: number;
}"
  `(:line ,line :character ,char))

(defsubst lsp--cur-line ()
  (1- (line-number-at-pos)))

(defsubst lsp--cur-column ()
  (- (point) (line-beginning-position)))

(defsubst lsp--cur-position ()
  "Make a Position object for the current point."
  (lsp--position (lsp--cur-line) (lsp--cur-column)))

(defsubst lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (lsp--cur-position)))

(defsubst lsp--position-p (p)
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

(defsubst lsp--region-to-range (start end)
  "Make Range object for the current region."
  (lsp--range (lsp--point-to-position start)
	      (lsp--point-to-position end)))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (let* ((range (gethash "range" text-edit))
	 (start-point (lsp--position-to-point (gethash "start" range)))
	 (end-point (lsp--position-to-point (gethash "end" range))))
    (save-excursion
      (goto-char start-point)
      (delete-region start-point end-point)
      (insert (gethash "newText" text-edit)))))

(defsubst lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (lsp--workspace-server-capabilities lsp--cur-workspace))

(defsubst lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (gethash cap (or capabilities (lsp--server-capabilities))))

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  `(:range ,(lsp--range (lsp--point-to-position start)
			  (lsp--point-to-position end))
	     :rangeLength ,(abs (- start end))
	     :text ,(buffer-substring-no-properties start end)))

(defsubst lsp--full-change-event ()
  `(:text ,(buffer-substring-no-properties (point-min) (point-max))))

(defvar lsp--change-idle-timer nil)
(defcustom lsp-change-idle-delay 0.5
  "Number of seconds of idle timer to wait before sending file changes to the server."
  :group 'lsp-mode
  :type 'number)

(defvar-local lsp--changes [])

(defun lsp--rem-idle-timer ()
  (when lsp--change-idle-timer
    (cancel-timer lsp--change-idle-timer)
    (setq lsp--change-idle-timer nil)))

(defun lsp--set-idle-timer ()
   (setq lsp--change-idle-timer (run-at-time lsp-change-idle-delay nil
					    #'lsp--send-changes)))
(defun lsp--send-changes ()
  (lsp--rem-idle-timer)
  (lsp--cur-file-version t)
  (lsp--send-notification
   (lsp--make-notification
    "textDocument/didChange"
    `(:textDocument
      ,(lsp--versioned-text-document-identifier)
      :contentChanges
      ,(cl-case lsp--server-sync-method
	 ('incremental lsp--changes)
	 ('full `[,(lsp--full-change-event)])))))
  (setq lsp--changes []))

(defun lsp--push-change (change-event)
  "Push CHANGE-EVENT to the buffer change vector."
  (setq lsp--changes (vconcat lsp--changes `(,change-event))))

(defun lsp-on-change (start end length)
  (when (and lsp--cur-workspace
	     (not (or (eq lsp--server-sync-method 'none)
		      (eq lsp--server-sync-method nil))))
    (lsp--rem-idle-timer)
    (when (eq lsp--server-sync-method 'incremental)
      (lsp--push-change (lsp--text-document-content-change-event start end length)))
    (lsp--set-idle-timer)))

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

(defsubst lsp--shut-down-p ()
  (y-or-n-p "Close the language server for this workspace? "))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (when lsp--cur-workspace
    (let ((file-versions (lsp--workspace-file-versions lsp--cur-workspace)))
      (remhash buffer-file-name file-versions)
      (when (and (= 0 (hash-table-count file-versions)) (lsp--shut-down-p))
	(lsp--send-request (lsp--make-request "shutdown" (make-hash-table)) t))
      (lsp--send-notification
       (lsp--make-notification
	"textDocument/didClose"
	`(:textDocument ,(lsp--versioned-text-document-identifier)))))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when lsp--cur-workspace
    (lsp--send-notification
     (lsp--make-notification
      "textDocument/didSave"
      `(:textDocument ,(lsp--versioned-text-document-identifier))))))

(defun lsp--text-document-position-params ()
  "Make TextDocumentPositionParams for the current point in the current document."
  `(:textDocument ,(lsp--text-document-identifier)
		  :position ,(lsp--position (lsp--cur-line)
					    (lsp--cur-column))))

(defun lsp--make-completion-item (item)
  (list
   (gethash "insertText" item (gethash "label" item))
   (progn (remhash "insertText" item)
	  (remhash "label" item)
	  item)))

(defsubst lsp--annotate (item)
  (gethash "detail" item nil))

(defun lsp--make-completion-items (response)
  "If RESPONSE is a CompletionItem[], return RESPONSE as is.
Otherwise return the items field from response, since it would be a
CompletionList object."
  (when response
    (if (consp response)
	response
      (gethash "items" response))))

(defun lsp--get-completions ()
  (let ((response (lsp--send-request (lsp--make-request
				      "textDocument/completion"
				      (lsp--text-document-position-params))))
	(completing-field (string= "." (buffer-substring-no-properties
					(- (point) 1) (point))))
	(token (current-word t))
	(el)
	(completions))
    (dolist (el (lsp--make-completion-items response))
      (push (lsp--make-completion-item el) completions))
    (when (or token completing-field)
      (list
       (if completing-field
	   (point)
	 (save-excursion (left-word) (point)))
       (point)
       completions
       '(:annotation-function lsp--anotate)))))

;;; TODO: implement completionItem/resolve

(defun lsp--location-to-xref (location)
  "Convert Location object LOCATION to an xref-item.
interface Location {
    uri: string;
    range: Range;
}"
  (let ((uri (string-remove-prefix "file://" (gethash "uri" location)))
	(ref-pos (gethash "start" (gethash "range" location))))
    (xref-make uri
	       (xref-make-file-location uri
					(1+ (gethash "line" ref-pos))
					(gethash "character" ref-pos)))))
(defun lsp--get-defitions ()
  "Get definition of the current symbol under point.
Returns xref-item(s)."
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

(defun lsp--text-document-hover-string ()
  "interface Hover {
    contents: MarkedString | MarkedString[];
    range?: Range;
}

type MarkedString = string | { language: string; value: string };"
  (if (symbol-at-point)
      (let* ((hover (lsp--send-request (lsp--make-request
					"textDocument/hover"
					(lsp--text-document-position-params))))
	     (contents (gethash "contents" (or hover (make-hash-table)))))
	(lsp--marked-string-to-string (if (consp contents)
					  (car contents)
					contents)))
    nil))

(defsubst lsp--make-document-formatting-options ()
  (let ((json-false :json-false))
    `(:tabSize ,tab-width :insertSpaces
	       ,(if indent-tabs-mode json-false t))))

(defun lsp--make-document-formatting-params ()
  `(:textDocument ,(lsp--text-document-identifier)
		  :options ,(lsp--make-document-formatting-options)))

(defun lsp--text-document-format ()
  "Ask the server to format this document."
  (let ((edits (lsp--send-request (lsp--make-request
				   "textDocument/formatting"
				   (lsp--make-document-formatting-params))))
	(edit))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

(defsubst lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}"
  (plist-put (lsp--make-document-formatting-params)
	     :range (lsp--region-to-range start end)))

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
  (let ((edits (lsp--send-request (lsp--make-request
				   "textDocument/rangeFormatting"
				   (lsp--make-document-range-formatting-params s e))))
	(edit))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

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

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
;;   (let ((json-false :json-false)
;; 	(symbols (lsp--send-request (lsp--make-request
;; 				     "textDocument/documentSymbol"
;; 				     `(:textDocument ,(lsp--text-document-identifier))))))
;;     (mapcar #'lsp--symbol-info-to-identifier symbols)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  nil)

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

(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
;; (defalias 'lsp-on-change #'lsp--text-document-did-change)
(defalias 'lsp-on-close #'lsp--text-document-did-close)
(defalias 'lsp-eldoc #'lsp--text-document-hover-string)
(defalias 'lsp-completion-at-point #'lsp--get-completions)

(defun lsp--set-variables ()
  (when lsp-enable-eldoc
    (setq-local eldoc-documentation-function #'lsp-eldoc)
    (eldoc-mode))
  ;; (setq-local indent-region-function #'lsp-format-region)
  (when lsp-enable-xref
    (setq-local xref-backend-functions #'lsp--xref-backend))
  (when (and (gethash "completionProvider" (lsp--server-capabilities))
	     lsp-enable-completion-at-point)
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point))
  (add-hook 'after-change-functions #'lsp-on-change))

(provide 'lsp-methods)
;;; document.el ends here
