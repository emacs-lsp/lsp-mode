(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   '("emacs" "--script" "/home/necto/proj/lsp-mode/test/mock-lsp-server.el"))
  :major-modes '(awk-mode)
  :priority 1
  :request-handlers (lsp-ht)
  :notification-handlers (lsp-ht)
  :multi-root nil
  :add-on? t
  :server-id 'mock-server
  :action-handlers (lsp-ht)))
