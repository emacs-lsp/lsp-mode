;;; test-mock-lsp-server.el --- unit test utilities -*- lexical-binding: t -*-

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

(defun lsp-test-total-server-count ()
  (hash-table-count (lsp-session-folder->servers (lsp-session))))

;; Should I add the fixtures/SamplesForMock folder to workspace folders?
(ert-deftest lsp-mock-server-reports-issues ()
  (let ((lsp-clients (lsp-ht)) ; clear all clients
        (lsp-enable-snippets nil) ; Avoid warning that lsp-yasnippet is not intalled
        (sample-file (f-join lsp-test-location "fixtures/SamplesForMock/sample.awk"))
        (initial-server-count (lsp-test-total-server-count)))
    (register-mock-client) ; register mock client as the one an only lsp client
    (let* ((buf (find-file-noselect sample-file)))
      (unwind-protect
          (with-timeout (15 (error "Timeout trying to get diagnostics from mock server"))
            (with-current-buffer buf
              (lsp)
              (should (eq (lsp-test-total-server-count) (1+ initial-server-count)))
              ;; Why does lsp not send the "shutdown" message on error?
              (let* ((chain (lsp-test-wait (gethash sample-file (lsp-diagnostics t))))
                     (diagnostics (deferred:sync! chain)))
                (should (eq (length diagnostics) 3)))))
        (kill-buffer buf)
        (with-timeout (10 (error "LSP server refuses to stop"))
          (deferred:sync! (lsp-test-wait (= initial-server-count (lsp-test-total-server-count)))))))))
