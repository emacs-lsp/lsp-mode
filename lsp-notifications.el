(require 'lsp-common)

(require 'cl-lib)

(defsubst lsp--window-show-message (params)
  (message "%s" (lsp--propertize (gethash "message" params)
				 (gethash "type" params))))

(defvar lsp--diagnostics (make-hash-table :test 'equal)
  "Hash table storing the diagnostics per file.")

(cl-defstruct lsp-diagnostic
  (range :read-only t) ;; of the form (number . number), both are points
  (severity :read-only t) ;; 1 - error, 2 - warning, 3 - information, 4 - hint
  (code :read-only t) ;; the diagnostic's code
  (source :read-only t) ;;
  (message :read-only t) ;; diagnostic's message
  )

(defun lsp--make-diag (diag)
  "Make a `lsp-diagnostic' from DIAG."
  (let ((range (gethash "range" diag)))
    (make-lsp-diagnostic :range `(,(lsp--position-to-point (gethash "start" range))
				  ,(lsp--position-to-point (gethash "end" range)))
			 :severity (gethash "severity" diag)
			 :code (gethash "code" diag)
			 :source (gethash "source" diag)
			 :message (gethash "message" diag))))


(defun lsp--on-diagnostics (params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}"
  (let ((file (string-remove-prefix "file://" (gethash "uri" params)))
	(diagnostics (gethash "diagnostics" params)))
    (puthash file (mapcar #'lsp--make-diag diagnostics) lsp--diagnostics)))

(provide 'lsp-notifications)
