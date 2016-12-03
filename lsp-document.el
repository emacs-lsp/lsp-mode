(require 'cl)
(require 'json)
(require 'xref)

(defvar-local lsp--last-id -1)
(defvar-local lsp--file-version-number -1)
(defvar-local lsp--language-id "")
(defvar-local lsp--send-response nil) ;; must return
(defvar-local lsp--send-no-response nil) ;;must not return

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (let ((request-body (lsp--make-notification method params)))
    (puthash "id" (incf lsp--last-id) request-body)
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

(defun lsp--json-read-from-string (str)
  "Like json-read-from-string(STR), but arrays are lists, and objects are hash tables."
  (let ((json-array-type 'list)
	(json-object-type 'hash-table))
    (json-read-from-string str)))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS."
  (let ((json-str (json-encode params)))
    (format
     "Content-Length: %d\r
%s"
     (length json-str) json-str)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall lsp--send-no-response (lsp--make-message body)))

(defun lsp--send-request (body)
  "Send BODY as a request to the language server, get the response."
  (funcall lsp--send-response (lsp--make-message body)))

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
    (puthash "languageId" lsp--language-id params)
    (puthash "version" (incf lsp--file-version-number) params)
    (puthash "text" (buffer-substring-no-properties (point-min) (point-max)) params)
    params))

(defun lsp--text-document-did-open ()
  "Executed when a new file is opened, added to `find-file-hook'."
  (let ((params (make-hash-table :test 'equal)))
    (puthash "textDocument" (lsp--make-text-document-item) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didOpen" params))))

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
    (puthash "version" lsp--file-version-number params)
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
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "contentChanges" (lsp--text-document-content-change-event
			       start end length)
	     params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didChange" params))))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (let ((params (make-hash-table :test 'equal)))
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

(defun lsp--text-document-signature-help ()
  ""
  )
(provide 'lsp-document)
;;; document.el ends here
