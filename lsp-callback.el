(require 'json)
(require 's)
(require 'ht)

(defun lsp--json-read-from-string (str)
  "Like json-read-from-string(STR), but arrays are lists, and objects are hash tables."
  (let ((json-array-type 'list)
	(json-object-type 'hash-table))
    (json-read-from-string str)))

(defun lsp--parse-message (body)
  "Read BODY.
Returns the json string in BODY if it is complete.
Else returns nil, and should be called again with the remaining output."
  (let ((completed-read nil)
	(json-body nil)
	(content-length nil)
	(content-type nil))
    (if (string-match "Content\-Length: \\([0-9]+\\)" body)
	(setq content-length (string-to-number (match-string 1 body)))
      (error "Received body without Content-Length"))
    (when (string-match "Content\-Type: \\(.+\\)\r" body)
      (setq content-type (match-string 1 body)))
    (when (string-match "\r\n\r\n\\(.+\\)" body)
	(setq json-body (setq body (match-string 1 body))))
    (when (not (= (length json-body) content-length))
	(error "Body's length != Content-Length"))
    (lsp--json-read-from-string json-body)))

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
  (let ((el))
    (dolist (el lsp--queued-notifications)
      (lsp--on-notification el t))
    (setq lsp--queued-notifications nil)))


;; How requests might work:
;; 1 call wrapper around lsp--send-message-sync.
;; 2. the implemented lsp--send-message-sync waits for next message,
;; calls lsp--from-server.
;; 3. lsp--from-server verifies this is the correct response, set the variable
;; lsp--response-result to the `result` field in the response.
;; 4. the wrapper around lsp--send-message-sync returns the value of lsp--response-result.

(defun lsp--on-notification (notification &optional dont-queue)
  "If response queue is empty, call the appropriate handler for NOTIFICATION.
Else it is queued (unless DONT-QUEUE is non-nil)"
  (if (and (not dont-queue) lsp--response-result)
      (setq lsp--queued-notifications
	    (if lsp--queued-notifications
		(append lsp--queued-notifications notification)
	      (list notification)))
    ;; else, call the appropriate handler
    ))

(defun lsp--set-response (response)
  "Set lsp--response-result as per RESPONSE.
Set lsp--waiting-for-message to nil."
  (setq lsp--response-result (gethash "result" response nil)
	;; no longer waiting for a response.
	lsp--waiting-for-response nil)
  (lsp--flush-notifications))

(defun lsp--from-server (data)
  "Callback for when Emacs recives DATA from client.
If lsp--from-server returns non-nil, the client library must SYNCHRONOUSLY
read the next message from the language server, else asynchronously."
  (let ((parsed (lsp--parse-message data)))
    (when parsed
      (pcase (lsp--get-message-type parsed)
	('response (lsp--set-response parsed))
	('response-error (error "Received an error from the language server")) ;;TODO
	('notification (lsp--on-notification parsed))))
    lsp--waiting-for-response))

(defvar lsp--process-pending-output (make-hash-table :test 'equal))
(defconst lsp--r-content-length "^Content-Length: .+\r\n"
  "Matches content-length ONLY.")
(defconst lsp--r-content-type "Content-Type: .+\r\n"
  "Matches content-type, DON'T use this as is.")

(defconst lsp--r-content-length-body (concat lsp--r-content-length "\r\n{.*}$")
  "Matches content-length, header end and json body (2 \r\n's).")

(defconst lsp--r-content-length-type-body (concat
					   lsp--r-content-length lsp--r-content-type
					   "\r\n{.*}$")
  "Matches content length, type, header end, and body (3 \r\n's).")
(defconst lsp--r-content-length-body-next (concat "\\("
					   lsp--r-content-length
					   "\r\n{.*}\\)\\(.+\\)$")
  "Matches content length, header end, body, and parts from the next message.
\(3 \r\n's\)")
(defconst lsp--r-content-length-type-body-next (concat "\\("
						lsp--r-content-length
						lsp--r-content-type
						"\r\n{.*}\\)\\(.+$\\)$")
  "Matches content length, type, header end, body and parts from next message.")

;; FIXME: This is highly inefficient. The same output is being matched *twice*
;; (once here, and in lsp--parse-message the second time.)
(defun lsp--process-filter (proc output)
  "Process filter for language servers that use stdout/stdin as transport.
PROC is the process.
OUTPUT is the output received from the process"
  (let ((pending (ht-get lsp--process-pending-output proc nil))
	(complete)
	(rem-pending)
	(next))
    (ht-set lsp--process-pending-output proc (setq output (concat pending output)))
    (case (s-count-matches "\r\n" output)
      ;; will never be zero
      (2 (setq complete t
	       rem-pending t)
	 (string-match lsp--r-content-length-body output))
      (3 (if (string-match lsp--r-content-length-type-body output)
	     (setq complete t
		   rem-pending t)
	   (when (string-match lsp--r-content-length-body-next output)
	     (ht-set lsp--process-pending-output proc (match-string 2 output))
	     (setq output (match-string 1 output)
		   complete t
		   next t))))
      ;; > 4
      (t (when (string-match lsp--r-content-length-type-body-next output)
	   (ht-set lsp--process-pending-output proc (match-string 2 output))
	   (setq output (match-string 1 output))
	   (setq complete t
		 next t))))
    (when rem-pending
      (ht-remove lsp--process-pending-output proc))
    (while (and complete (lsp-from-server output))
      (accept-process-output proc))
    (when next
      ;; stuff from the next response/notification was in this outupt.
      ;; try parsing it to see if it was a complete message.
      ;; unlikely, but might be possible
      (lsp--process-filter proc ""))))

(provide 'lsp-callback)
;;; lsp-callback.el ends here
