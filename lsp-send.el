(defun lsp--stdio-send-sync (message proc)
  ;;(message "sending %s" message)
  (process-send-string proc
		       message)
  (with-local-quit
    (accept-process-output proc)))

(defun lsp--stdio-send-async (message proc)
  (process-send-string proc
		       message))

(provide 'lsp-send)
