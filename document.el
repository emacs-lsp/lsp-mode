(require 'cl)
(require 'json)
(require 'xref)

(defvar-local lsp--last-id -1)
(defvar-local lsp--file-version-number -1)
(defvar-local lsp--language-id "")
(defvar-local lsp--send-request nil) ;; must return
(defvar-local lsp--send-notification nil) ;;must not return

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
  (let ((body (make-hash-table)))
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
Content-Type: application/vscode-jsonrpc; charset=utf8\r
%s"
     (length json-str) json-str)))

(defun lsp--send--notification (body)
  "Send BODY as a notification to the language server."
  (apply lsp--send-notification (lsp--make-message body)))

(defun lsp--send-request (body)
  "Send BODY as a request to the language server, get the response."
  (apply lsp--send-request (lsp--make-message body)))

(defun lsp--text-document-item ()
  "Make TextDocumentItem for the currently opened file.

interface TextDocumentItem {
    uri: string; // The text document's URI
    languageId: string; // The text document's language identifier.
    version: number;
    text: string;
}"
  (let ((params (make-hash-table)))
    (puthash "uri" buffer-file-name params)
    (puthash "languageId" lsp--language-id params)
    (puthash "version" (incf lsp--file-version-number) params)
    (puthash "text" (buffer-substring-no-properties (point-min) (point-max)) params)
    params))

(defun lsp--text-document-did-open ()
  "Executed when a new file is opened, added to `find-file-hook'."
  (let ((params (make-hash-table)))
    (puthash "textDocument" (lsp--get-text-document-item) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didOpen" params))))

(defun lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (let ((params (make-hash-table)))
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

(defun lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  (let ((params (make-hash-table)))
    (puthash "start" start params)
    (puthash "end" end params)
    params))

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  (let ((params (make-hash-table)))
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
  (let ((params (make-hash-table)))
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "contentChanges" (lsp--text-ducment-content-change-event
			       start end length)
	     params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didChange" params))))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (let ((params (make-hash-table)))
    (puthash "textDocument" (lsp--versioned-text-document-identifier) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didClose" params))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (let ((params (make-hash-table)))
    (puthash "textDocument" (lsp--versioned-text-document-identifier) params)
    (lsp--send-notification
     (lsp--make-notification "textDocument/didSave" params))))

(defun lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.
interface Position {
    line: number;
    character: number;
}"
  (let ((params (make-hash-table)))
    (puthash "line" line params)
    (puthash "character" char params)
    params))

(defun lsp--text-document-position-params ()
  "Make TextDocumentPositionParams for the current point in the current document."
  (let ((params (make-hash-table)))
    (puthash "textDocument" (lsp--text-document-identifier) params)
    (puthash "position" (lsp--position (line-number-at-pos) (point)) params)
    params))

(defun lsp--make-completion-item (item)
  (list
   (gethash "insertText" item (gethash "label" item))
   (progn (remhash "insertText" item)
	  (remhash "label" item)
	  item)))

(defsubst lsp--annotate (item)
  (gethash "detail" item nil))

(defun lsp--get-completions ()
  (let ((response (lsp--send-request (lsp--make-request
				      "textDocument/completion"
				      (lsp--text-document-position-params))))
	(completing-field (string= "." (buffer-substring-no-properties
					(- (point) 1) (point))))
	(token (current-word t))
	(el)
	(completions))
    (dolist (el response)
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
	(reference-context (make-hash-table))
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
      (lisp--location-to-xref location))))

(provide 'lsp-document)
;;; document.el ends here
