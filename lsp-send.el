(defun lsp--stdio-send-sync (message proc)
  ;; (message "lsp--stdio-send-sync: %s" message)
  (process-send-string proc
		       message)
  (with-local-quit
    (accept-process-output proc)))

(defun lsp--stdio-send-async (message proc)
  ;; (message "lsp--stdio-send-async: %s" message)
  (process-send-string proc
		       message))

(provide 'lsp-send)
