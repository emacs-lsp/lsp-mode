(defun lsp--stdin-send-sync (message proc)
  (process-send-string proc
		       (concat message))
  (process-send-eof proc)
  (with-local-quit
    (accept-process-output proc)))

(defun lsp--stdin-send-async (message proc)
  (process-send-string proc
		       (concat message))
  (process-send-eof proc))

(provide 'lsp-send)
