(setq-local reading-body nil)
(setq-local json-body nil)
(setq-local content-length nil)
(setq-local content-type nil)

(defun lsp--parser-reset-state ()
  "Reset the internal state of the parser."
  (setq reading-body nil
	content-length nil
	content-type nil
	json-body nil))

(defun lsp--parse-message (body)
  "Read BODY.
Returns the json string in BODY if it is complete.
Else returns nil, and should be called again with the remaining output."
  (let ((completed-read nil))
    (when (and (not reading-body) (not content-length))
      (if (string-match "Content\-Length: \\([0-9]+\\)" body)
	  (setq content-length (string-to-number (match-string 1 body)))
	(error "Received body without Content-Length")))
    (when (and (not reading-body) (not content-type))
      (when (string-match "Content\-Type: \\(.+\\)\r" body)
	(setq content-type (match-string 1 body)
	      reading-body t
	      json-body "")))
    (if (string-match "\r\n\r\n\\(.+\\)" body)
	(setq json-body (setq body (match-string 1 body)))
      (if (not (setq completed-read (= (length json-body) content-length)))
	  (setq json-body (concat json-body body))
	;;read a complete message, reset state
	(lsp--parser-reset-state)))
    (if (or completed-read (= (length json-body) content-length))
	(prog1 json-body (lsp--parser-reset-state))
      nil)))

(defun lsp--get-message-type (params)
  "Get the message type from PARAMS."
  (when (not (string= (gethash "jsonrpc" params "") "2.0"))
    (error "JSON-RPC version is not 2.0"))
  (if (gethash "id" params nil)
      (if (gethash "error" params nil)
	  'response-error
	'response)
    (if (gethash "method" params nil)
	'notification
      (error "Couldn't guess message type from params"))))

(setq-local request-callbacks (make-hash-table :test 'equal))

(defsubst lsp--callback-table-is-empty ()
  (= (hash-table-count request-callbacks) 0))

(defun lsp--call-response-callback (params)
  "Read response id from PARAMS, and call the callback for that."
  (let ((id (string-to-number (gethash "id" params)))
	(result (gethash "result" params))
	(callback (gethash id request-callbacks nil)))
    (if result
	(funcall callback result)
      (funcall callback))
    (remhash id params)))

(defun lsp--on-notification (params)
  ;;todo
)

(defun lsp--from-server (message)
  "Callback for when Emacs recives MESSAGE from client.
If lsp--from-server returns non-nil, the client library must SYNCHRONOUSLY
read the next message from the language server, else asynchronously."
  (let ((parsed (lsp--parse-message message)))
    (when parsed
      (pcase (lsp--get-message-type parsed)
	('response (lsp--call-response-callback parsed))
	('response-error (error "Received an error from the language server")) ;;TODO
	('notification (lsp-on-notification parsed))))
    (lsp--callback-table-is-empty)))

(provide 'lsp-callback)
;;; lsp-callback.el ends here
