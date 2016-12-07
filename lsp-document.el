(require 'cl-lib)
(require 'json)
(require 'xref)
(require 'lsp-receive)

(cl-defstruct workspace
  (language-id :read-only t)
  (last-id 0)
  (file-versions)
  (root :ready-only t)
  (client :read-only t)
  (data :read-only t))

(defvar-local lsp--cur-workspace nil)
(defvar lsp--workspaces (make-hash-table :test 'equal))

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (plist-put (lsp--make-notification method params)
	     :id (incf (workspace-last-id lsp--cur-workspace))))

(defun lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (unless (stringp method)
    (signal 'wrong-type-argument (list 'stringp method)))
  `(:jsonrpc "2.0" :method ,method :params ,params))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS."
  (let ((json-str (json-encode params)))
    (format
     "Content-Length: %d\r
%s"
     (length json-str) json-str)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall (lsp--client-send-async (workspace-client lsp--cur-workspace))
	   (lsp--make-message body)
	   (workspace-data lsp--cur-workspace)))

(defun lsp--send-request (body)
  "Send BODY as a request to the language server, get the response."
  (setq lsp--waiting-for-response t)
  ;; lsp-send-sync should loop until lsp--from-server returns nil
  ;; in the case of Rust Language Server, this can be done with
  ;; 'accept-process-output`.'
  (funcall (lsp--client-send-sync (workspace-client lsp--cur-workspace))
	   (lsp--make-message body)
	   (workspace-data lsp--cur-workspace))
  (prog1 lsp--response-result (setq lsp--response-result nil)))

(defun lsp--cur-file-version (&optional inc)
  "Return the file version number.  If INC, increment it before."
  (let* ((file-versions (workspace-file-versions lsp--cur-workspace))
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
  `(:uri ,(concat "file://" buffer-file-name)
	 :languageId ,(workspace-language-id lsp--cur-workspace)
	 :version ,(lsp--cur-file-version)
	 :text ,(buffer-substring-no-properties (point-min) (point-max))))

(defun lsp--initialize (language-id client &optional data)
  (let ((root))
    (if (gethash cur-dir lsp--workspaces)
	(error "This workspace has already been initialized")
      (setq lsp--cur-workspace (make-workspace
				:language-id language-id
				:file-versions (make-hash-table :test 'equal)
				:last-id 0
				:root (setq root
					    (funcall (lsp--client-get-root client)))
				:client client
				:data data))
      (puthash cur-dir lsp--cur-workspace lsp--workspaces))
    (lsp--send-request
     (lsp--make-request
      "initialize"
      `(:processId ,(emacs-pid) :rootPath ,root
		   :capabilities ,(make-hash-table))))))

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
	  (puthash buffer-file-name 0 (workspace-file-versions lsp--cur-workspace))
	  (lsp--send-notification
	   (lsp--make-notification
	    "textDocument/didOpen"
	    `(:textDocument ,(lsp--make-text-document-item)))))

      (when (setq client (gethash major-mode lsp--defined-clients))
	(setq data (funcall (lsp--client-new-connection client)))
	(setq set-vars t)
	(lsp--initialize (lsp--client-language-id client)
			 client
			 data)
	(lsp--text-document-did-open)))

    (when set-vars (lsp--set-variables))))

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

(defsubst lsp--current-char-offset ()
  (- (point) (save-excursion (beginning-of-line) (point))))

(defun lsp--cur-position ()
  "Make a Position object for the current point."
  (lsp--position (line-number-at-pos)
		 (lsp--current-char-offset)))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (lsp--cur-position)))

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
  `(:start ,start :end ,end))

(defsubst lsp--cur-region-to-range ()
  "Make Range object for the current region."
  (lsp--range (lsp--point-to-position (region-beginning))
	      (lsp--point-to-position (region-end))))

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
  `(:range ,(lsp--range start end) :rangeLength ,length
	   :text ,(buffer-substring-no-properties (point-min) (point-max))))

(defun lsp--text-document-did-change (start end length)
  "Executed when a file is changed.
Added to `after-change-functions'"
  (when lsp--cur-workspace
    (lsp--cur-file-version t)
    (lsp--send-notification
     (lsp--make-notification
      "textDocument/didChange"
      `(:textDocument
	,(lsp--text-document-identifier)
	:contentChanges
	(,(lsp--text-document-content-change-event start end length)))))))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (when lsp--cur-workspace
    (remhash buffer-file-name (workspace-file-versions lsp--cur-workspace))
    (lsp--send-notification
     (lsp--make-notification
      "textDocument/didClose"
      `(:textDocument ,(lsp--versioned-text-document-identifier))))))

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
		  :position ,(lsp--position (line-number-at-pos)
					    (lsp--current-char-offset))))

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
  (let ((uri (string-remove-prefix "file://" (gethash "uri" location)))
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
  (let ((json-false :json-false))
    (plist-put (lsp--text-document-position-params)
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

(defun lsp--make-document-range-formatting-params ()
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}" 
  (plist-put (lsp--make-document-formatting-params)
	     :range (lsp--cur-region-to-range)))

(defun lsp-format-region (_s _e)
  (let ((edits (lsp--send-request (lsp--make-request
				   "textDocument/rangeFormatting"
				   (lsp--make-document-range-formatting-params))))
	(edit))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

(defun lsp--xref-backend () 'lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql lsp)))
  (propertize (symbol-name (symbol-at-point))
	      'def-params (lsp--text-document-position-params)
	      'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-definitions ((_backend (eql lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
	 (params (plist-get properties 'def-params))
	 (def (lsp--send-request (lsp--make-request
				  "textDocument/definition"
				  params))))
    (if (consp def)
	(mapcar 'lsp--location-to-xref def)
      (and def `(,(lsp--location-to-xref def))))))

(cl-defmethod xref-backend-references ((_backend (eql lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
	 (params (plist-get properties 'ref-params))
	 (ref (lsp--send-request (lsp-make-request
				  "textDocument/references"
				  params))))
    (if (consp ref)
	(mapcar 'lsp--location-to-xref ref)
      (and ref `(,(lsp--location-to-xref ref))))))

(defalias 'lsp-on-open 'lsp--text-document-did-open)
(defalias 'lsp-on-save 'lsp--text-document-did-save)
(defalias 'lsp-on-change 'lsp--text-document-did-change)
(defalias 'lsp-eldoc 'lsp--text-document-hover-string)
(defalias 'lsp-completion-at-point 'lsp--get-completions)

(defun lsp--set-variables ()
  (setq-local eldoc-documentation-function #'lsp-eldoc)
  (setq-local indent-region-function #'lsp-format-region)
  (setq-local xref-backend-functions #'lsp--xref-backend)
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions #'lsp-completion-at-point))

(provide 'lsp-document)
;;; document.el ends here
