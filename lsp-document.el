(require 'cl)
(require 'json)
(require 'xref)
(require 'lsp-callback)

(cl-defstruct workspace
  (language-id :read-only t)
  (last-id 0)
  (file-versions)
  (root :ready-only t)
  (send-sync :read-only t) ;; send-sync should loop until lsp--from-server returns nil
  (send-async :read-only t)
  (data :read-only t))

(defvar-local cur-workspace nil)
(defvar workspaces (make-hash-table :test 'equal))

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (let ((request-body (lsp--make-notification method params)))
    (puthash "id" (incf (workspace-last-id cur-workspace)) request-body)
    request-body))

(defun lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (unless (stringp method)
    (signal 'wrong-type-argument (list 'stringp method)))
  (when params
    (unless (hash-table-p params)
      (signal 'wrong-type-argument (list 'hash-table-p params))))
  (let ((body (make-hash-table :test 'equal)))
    (puthash "jsonrpc" "2.0" body)
    (puthash "method" method body)
    (when params
      (puthash "params" params body))
    body))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS."
  (let ((json-str (json-encode params)))
    (format
     "Content-Length: %d\r
%s"
     (length json-str) json-str)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall (workspace-send-async cur-workspace) (lsp--make-message body)
	   (workspace-data cur-workspace)))

(defun lsp--send-request (body)
  "Send BODY as a request to the language server, get the response."
  (setq lsp--waiting-for-response t)
  ;; lsp-send-sync should loop until lsp--from-server returns nil
  ;; in the case of Rust Language Server, this can be done with
  ;; 'accept-process-output`.'
  (funcall (workspace-send-sync cur-workspace) (lsp--make-message body)
	   (workspace-data cur-workspace))
  lsp--response-result)

(defun lsp--cur-file-version (&optional inc)
  "Return the file version number.  If INC, increment it before."
  (let* ((file-versions (workspace-file-versions cur-workspace))
	 (rev (gethash buffer-file-name file-versions)))
    (when inc
      (incf rev)
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
  (let ((params (make-hash-table :test 'equal)))
    (puthash "uri" buffer-file-name params)
    (puthash "languageId" (workspace-language-id cur-workspace) params)
    (puthash "version" (lsp--cur-file-version) params)
    (puthash "text" (buffer-substring-no-properties (point-min) (point-max)) params)
    params))

(defun lsp--initialize (language-id send-sync send-async &optional data)
  (let ((params (make-hash-table :test 'equal))
	(cur-dir (expand-file-name default-directory)))
    (if (gethash cur-dir workspaces)
	(error "This workspace has already been initialized")
      (setq cur-workspace (make-workspace :language-id language-id
					  :last-id 0
					  :root cur-dir
					  :send-sync send-sync
					  :send-async send-async
					  :data data)))
    (puthash "processId" (emacs-pid) params)
    (puthash "rootPath" (expand-file-name cur-dir) params)
    (puthash "capabilities" json-null params)
    (lsp--send-request (lsp--make-request "initialize" params))))

(defun lsp--text-document-did-open ()
  "Executed when a new file is opened, added to `find-file-hook'."
  (let ((params (make-hash-table :test 'equal))
	(cur-dir (expand-file-name default-directory))
	(key))
    (when (catch 'break
	      (dolist (key (hash-table-keys workspaces)
			   (when (string-prefix-p key cur-dir)
			     (setq cur-workspace (gethash key workspaces))
			     (throw 'break key)))))
      (puthash buffer-file-name (workspace-file-versions cur-workspace) 0)
      (puthash "textDocument" (lsp--make-text-document-item) params)
      (lsp--send-notification
       (lsp--make-notification "textDocument/didOpen" params)))))

(defun lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (let ((params (make-hash-table :test 'equal)))
    (puthash "uri" buffer-file-name params)
    params))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (let ((params (lsp--text-document-identifier)))
    (puthash "version" (lsp--cur-file-version) params)
    params))

(defun lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.
interface Position {
    line: number;
    character: number;
}"
  (let ((params (make-hash-table :test 'equal)))
    (puthash "line" line params)
    (puthash "character" char params)
    params))

(defsubst lsp--current-char-offset ()
  (- (point) (save-excursion (beginning-of-line) (point))))

(defun lsp--cur-position ()
  "Make a Position object for the current point."
  (lsp--position (line-number-at-pos)
		 (lsp--current-char-offset)))

(defun lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (save-excursion
      (goto-char (point-min))
      (forward-line (1- (gethash "line" params)))
      (+ (point) (gethash "character" params))))

(defun lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  (let ((params (make-hash-table :test 'equal)))
    (puthash "start" start params)
    (puthash "end" end params)
    params))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (let* ((range (gethash "range" text-edit))
	 (start-point (lsp--position-to-point (gethash "start" range)))
	 (end-point (lsp--position-to-point (gethash "end" range))))
    (save-excursion
      (goto-char start-point)
      (delete-region start-point end-point)
      (insert (gethash "newText" text-edit)))))

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  (let ((params (make-hash-table :test 'equal)))
    ;; The range of the document that changed.
    (puthash "range" (lsp--range start end) params)
    ;; The length of the range that got replaced.
    (puthash "rangeLength" length params)
    ;; The new text of the document.
    (puthash "text" (buffer-substring-no-properties (point-min) (point-max)) params)
    params))

(defun lsp--text-document-did-change (start end length)
  "Executed when a file is changed.
Added to `after-change-functionsafter-change-functions'"
  (let ((params (make-hash-table :test 'equal)))
    (lsp--cur-file-version t) ;; incrememnt file version
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "contentChanges" (lsp--text-document-content-change-event
			       start end length)
	     params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didChange" params))))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (let ((params (make-hash-table :test 'equal)))
    (remhash buffer-file-name (workspace-file-versions cur-workspace))
    (puthash "textDocument" (lsp--versioned-text-document-identifier) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didClose" params))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (let ((params (make-hash-table :test 'equal)))
    (puthash "textDocument" (lsp--versioned-text-document-identifier) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didSave" params))))

(defun lsp--text-document-position-params ()
  "Make TextDocumentPositionParams for the current point in the current document."
  (let ((params (make-hash-table :test 'equal)))
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "position" (lsp--position (line-number-at-pos)
				       (lsp--current-char-offset))
	     params)
    params))

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
  (if (consp response)
      response
    (gethash "items" response)))

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
      (append completions (lsp--make-completion-item el)))
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
  (let ((uri (gethash location "uri"))
	(ref-pos (gethash "start" (gethash "range" location))))
    (xref-make uri
	       (xref-make-file-location uri
					(gethash "line" ref-pos)
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

(defun lsp--make-reference-params ()
  "Make a ReferenceParam object."
  (let ((params (lsp--text-document-position-params))
	(reference-context (make-hash-table :test 'equal))
	(json-false :json-false))
    (puthash "includeDeclaration" json-false reference-context)
    (puthash "context" reference-context params)
    params))

(defun lsp--get-references ()
  "Get all references for the symbol under point.
Returns xref-item(s)."
  (let ((location  (lsp--send-request (lsp--make-request
				       "textDocument/references"
				       (lsp--make-reference-params)))))
    (if (consp location)
	(mapcar 'lsp--location-to-xref location)
      (lsp--location-to-xref location))))

(defun lsp--text-document-hover-string ()
  "interface Hover {
    contents: MarkedString | MarkedString[];
    range?: Range;
}

type MarkedString = string | { language: string; value: string };"
  (let* ((hover (lsp--send-request (lsp--make-request
			"textDocument/hover"
			(lsp--text-document-position-params))))
	 (contents (gethash "contents" hover)))
    (if (hash-table-p contents)
	(gethash "value" contents)
      contents)))

(defun lsp--make-document-formatting-options ()
  (let ((params (make-hash-table :test 'equal)))
    (puthash "tabSize" tab-width params)
    (puthash "insertSpaces" (if indent-tabs-mode json-false t) params)
    params))

(defun lsp--make-document-formatting-params ()
  (let ((params (make-hash-table :test 'equal)))
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "options" (lsp--make-document-formatting-options) params)
    params))

(defun lsp--text-document-format ()
  "Ask the server to format this document."
  (let ((edits (lsp--send-request (lsp--make-request
				   "textDocument/formatting"
				   (lsp--make-document-formatting-params))))
	(edit))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

(provide 'lsp-document)
;;; document.el ends here
