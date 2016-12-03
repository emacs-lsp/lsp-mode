(defvar-local lsp--reading-body nil)
(defvar-local lsp--json-body nil)
(defvar-local lsp--content-length nil)
(defvar-local lsp--content-type nil)

(defun lsp--parser-reset-state ()
  "Reset the internal state of the parser."
  (setq lsp--reading-body nil
	lsp--content-length nil
	lsp--content-type nil
	lsp--json-body nil))

(defun lsp--parse-message (body)
  "Read BODY.
Returns the json string in BODY if it is complete.
Else returns nil, and should be called again with the remaining output."
  (let ((completed-read nil))
    (when (and (not lsp--reading-body) (not lsp--content-length))
      (if (string-match "Content\-Length: \\([0-9]+\\)" body)
	  (setq lsp--content-length (string-to-number (match-string 1 body)))
	(error "Received body without Lsp--Content-Length")))
    (when (and (not lsp--reading-body) (not lsp--content-type))
      (when (string-match "Content\-Type: \\(.+\\)\r" body)
	(setq lsp--content-type (match-string 1 body)
	      lsp--reading-body t
	      lsp--json-body "")))
    (if (string-match "\r\n\r\n\\(.+\\)" body)
	(setq lsp--json-body (setq body (match-string 1 body)))
      (if (not (setq completed-read (= (length lsp--json-body) lsp--content-length)))
	  (setq lsp--json-body (concat lsp--json-body body))
	;;read a complete message, reset state
	(lsp--parser-reset-state)))
    (if (or completed-read (= (length lsp--json-body) lsp--content-length))
	(prog1 lsp--json-body (lsp--parser-reset-state))
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

(defvar-local lsp--waiting-for-response nil)
(defvar-local lsp--response-result nil)
(defvar-local lsp--queued-notifications nil)

(defsubst lsp--flush-notifications ()
  "Flush any notifications that were queued while processing the last response."
  (unless lsp--response-result
    (let ((el))
      (dolist (el lsp--queued-notifications)
	(lsp--on-notification el t))
      (setq lsp--queued-notifications nil))))


;; How requests might work:
;; 1 call wrapper around lsp--send-message-sync.
;; 2. the implemented lsp--send-message-sync waits for next message,
;; calls lsp--from-server.
;; 3. lsp--from-server verifies this is the correct response, set the variable
;; lsp--response-result to the `result` field in the response.
;; 4. the wrapper around lsp--send-message-sync returns the value of lsp--response-result.

(defun lsp--on-notification (notification &optional dont-queue)
  ;;todo
  (if (and (not dont-queue) lsp--response-result)
      (setq lsp--queued-notifications (append lsp--queued-notifications notification))
    ;; else, call the appropriate handler
    )
)

(defun lsp--set-response (response)
  "Set lsp--response-result as per RESPONSE.
Set lsp--waiting-for-message to nil."
  (setq lsp--response-result (gethash "result" response nil))
  ;; no longer waiting for a response.
  (setq lsp--waiting-for-response nil))

(defun lsp--from-server (message)
  "Callback for when Emacs recives MESSAGE from client.
If lsp--from-server returns non-nil, the client library must SYNCHRONOUSLY
read the next message from the language server, else asynchronously."
  (let ((parsed (lsp--parse-message message)))
    (when parsed
      (pcase (lsp--get-message-type parsed)
	('response (lsp--set-response parsed))
	('response-error (error "Received an error from the language server")) ;;TODO
	('notification (lsp-on-notification parsed))))
    lsp--waiting-for-response))

(provide 'lsp-callback)
;;; lsp-callback.el ends here
