(defun lsp--stdin-send-sync (message proc)
  (process-send-string proc
		       message)
  (with-local-quit
    (accept-process-output proc)))

(defun lsp--stdin-send-async (message proc)
  (process-send-string proc
		       message))

(provide 'lsp-send)
