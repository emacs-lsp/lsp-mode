(require 'lsp-mode)
(require 'lsp-test-utils)

;; Taken from lsp-integration-tests.el
(defconst lsp-test-location (file-name-directory (or load-file-name buffer-file-name)))

(defun register-mock-client ()
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("emacs" "--script" "/home/necto/proj/lsp-mode/test/mock-lsp-server.el"))
    :major-modes '(awk-mode)
    :priority 100
    :server-id 'mock-server)))

(ert-deftest lsp-mock-server-reports-issues ()
  (let ((lsp-clients (lsp-ht)) ; clear all clients
        (lsp-enable-snippets nil) ; Avoid warning that lsp-yasnippet is not intalled
        (sample-file (f-join lsp-test-location "fixtures/SamplesForMock/sample.awk")))
    (register-mock-client) ; register mock client as the one an only lsp client
    (let* ((buf (find-file-noselect sample-file)))
      (with-timeout (7 (error "Timeout trying to get diagnostics from mock server"))
        (with-current-buffer buf
          (lsp)
          (let* ((chain (lsp-test-wait (gethash sample-file (lsp-diagnostics t))))
                 (diagnostics (deferred:sync! chain)))
            (should (eq (length diagnostics) 3))))))))
