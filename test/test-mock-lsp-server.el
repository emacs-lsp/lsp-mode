;;; test-mock-lsp-server.el --- unit test utilities -*- lexical-binding: t -*-

(require 'lsp-mode)
(require 'lsp-test-utils)

;; Taken from lsp-integration-tests.el
(defconst lsp-test-location (file-name-directory (or load-file-name buffer-file-name)))

(defconst lsp-test-mock-server-location
  (expand-file-name "mock-lsp-server.el" lsp-test-location))

(defconst lsp-test-mock-server-command-file
  (expand-file-name "mock-server-commands.el" lsp-test-location))

(defun lsp-test-send-command-to-mock-server (command)
  ;; Can run only one command at a time
  (should (not (file-exists-p lsp-test-mock-server-command-file)))
  (write-region command nil lsp-test-mock-server-command-file nil nil nil 'excl)
  ;; Nudge the server to find and execute the command
  (lsp-notify "$/setTrace" '(:value "messages")))

(defun register-mock-client ()
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     `("emacs" "--script" ,lsp-test-mock-server-location))
    :major-modes '(awk-mode)
    :priority 100
    :server-id 'mock-server)))

(defun lsp-test-total-server-count ()
  (hash-table-count (lsp-session-folder->servers (lsp-session))))

(defun lsp-test--find-line (file-content line)
  (let ((lines (split-string file-content "\n"))
        (line-number 0)
        (found nil))
    (while (and lines (not found))
      (when (string= (car lines) line)
        (setq found line-number))
      (setq lines (cdr lines))
      (setq line-number (1+ line-number)))
    (when (not found)
      (error "Line %s not found" line))
    found))

(defun lsp-test-diag-make (file-content line marker)
  (let ((line-number (lsp-test--find-line file-content line)))
    (should-not (null line-number))
    (should (eq (length marker) (length line)))
    (should (string-match "^ *\\(\\^+\\) *$" marker))
    (list :line line-number :from (match-beginning 1) :to (match-end 1))))

(defun lsp-test-diag-get (diagnostic)
  (let* ((range (ht-get diagnostic "range"))
         (start (ht-get range "start"))
         (end (ht-get range "end")))
    (should (eq (ht-get start "line") (ht-get end "line")))
    (list :line (ht-get start "line")
          :from (ht-get start "character")
          :to (ht-get end "character"))))

(defun lsp-test-make-diagnostics (for-file contents)
  (let ((forbidden-word "broming"))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (let (diagnostics)
        (while (re-search-forward forbidden-word nil t)
          (let ((line (- (line-number-at-pos (point)) 1))
                (end-col (current-column))
                (start-col (- (current-column) (length forbidden-word))))
            (push (list :source "mockS"
                        :code "E001"
                        :range (list :start (list :line line :character start-col)
                                     :end (list :line line :character end-col))
                        :message (format "Do not use word '%s'" forbidden-word)
                        :severity 2)
                  diagnostics)))
        `(:path ,for-file :diags ,diagnostics)))))

(defun lsp-test-command-send-diags (file-path file-contents)
  (let ((diags (lsp-test-make-diagnostics file-path file-contents)))
    (lsp-test-send-command-to-mock-server
     (format "(publish-diagnostics '%s)"
             (prin1-to-string diags)))))

(ert-deftest lsp-mock-server-reports-issues ()
  (let ((lsp-clients (lsp-ht)) ; clear all clients
        (lsp-diagnostic-package :none)
        (lsp-enable-snippets nil) ; Avoid warning that lsp-yasnippet is not intalled
        (workspace-root (f-join lsp-test-location "fixtures/SamplesForMock"))
        (sample-file (f-join lsp-test-location "fixtures/SamplesForMock/sample.awk"))
        (initial-server-count (lsp-test-total-server-count)))
    (register-mock-client) ; register mock client as the one an only lsp client
    (lsp-workspace-folders-add workspace-root)
    (let* ((buf (find-file-noselect sample-file)))
      (unwind-protect
          (with-timeout (5 (error "Timeout trying to get diagnostics from mock server"))
            (with-current-buffer buf
              (lsp)
              ;; Make sure the server started
              (should (eq (lsp-test-total-server-count) (1+ initial-server-count)))

              (lsp-test-wait (eq 'initialized
                                 (lsp--workspace-status (cl-first (lsp-workspaces)))))
              (lsp-test-command-send-diags sample-file (buffer-string))
              ;; FIXME: in the case of a failed test, this will hang forever,
              ;; need to find a way to terminate it cleanly
              (deferred:sync! (lsp-test-wait (progn
                                               ;; TODO: check workspace still exists
                                               ;; If I crash the server, I get a type error
                                               ;; ~~ wrong type of argument lsp--workspace
                                               (gethash sample-file (lsp-diagnostics t)))))
              (should (eq (length (gethash sample-file (lsp-diagnostics t))) 1))
              (should (equal (lsp-test-diag-get (car (gethash sample-file (lsp-diagnostics t))))
                             (lsp-test-diag-make (buffer-string)
                                                 "line 1 is here broming and here"
                                                 "               ^^^^^^^         ")))))
        (kill-buffer buf)
        (lsp-workspace-folders-remove workspace-root)
        ;; Remove possibly unhandled commands
        (when (file-exists-p lsp-test-mock-server-command-file)
            (delete-file lsp-test-mock-server-command-file))))
    (with-timeout (5 (error "LSP server refuses to stop"))
      ;; Make sure the server stopped
      (deferred:sync! (lsp-test-wait (= initial-server-count (lsp-test-total-server-count)))))))
