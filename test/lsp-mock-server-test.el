;;; lsp-mock-server-test.el --- Unit test utilities -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 emacs-lsp maintainers

;; Author: Arseniy Zaostrovnykh
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1
;; License: GPL-3.0-or-later

;; URL: https://github.com/emacs-lsp/lsp-mode
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A collection of tests that check lsp-mode interaction with
;; an lsp server mocked by mock-lsp-server.el
;; The tests define a custom lsp client execute scenarios
;; such as opening a file, chagning it, or receiving updated diagnostics
;; and assert how lsp-mode updates the diagnostics.

;;; Code:

(require 'lsp-mode)

;; Taken from lsp-integration-tests.el
(defconst lsp-test-location (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the tests containing mock server and fixtures.")

(defconst lsp-test-mock-server-location
  (expand-file-name "mock-lsp-server.el" lsp-test-location)
  "Path to the mock server script.")

(defconst lsp-test-mock-server-command-file
  (expand-file-name "mock-server-commands.el" lsp-test-location)
  "File mock server reads commands from.")

(defconst lsp-test-sample-file
  (f-join lsp-test-location "fixtures/SamplesForMock/sample.txt")
  "The sample file used to conduct tests upon.")

(defun lsp-test-send-command-to-mock-server (command)
  "Pass the given COMMAND to the mock server.

It uses the pre-configured file to write command to, then sends a
notification to the server so that it looks into the file and
executes the command."
  ;; Can run only one command at a time
  (should (not (file-exists-p lsp-test-mock-server-command-file)))
  (write-region command nil lsp-test-mock-server-command-file nil nil nil 'excl)
  ;; Nudge the server to find and execute the command
  (lsp-notify "$/setTrace" '(:value "messages")))

(defun register-mock-client ()
  "Register mock client that spawns the mock server."
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     `("emacs" "--script" ,lsp-test-mock-server-location))
    :major-modes '(prog-mode)
    :priority 100
    :server-id 'mock-server)))

(defun lsp-test-total-folder-count ()
  "Count total number of active root folders in the session."
  (hash-table-count (lsp-session-folder->servers (lsp-session))))

(defun lsp-test--find-line (file-content line)
  "Find LINE in the multi-line FILE-CONTENT string."
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

(defun lsp-test-range-make (file-content line marker)
  "Create a single-line diagnostics range summary.

Find LINE in FILE-CONTENT and take that as the line number.
Set the :from and :to characters to reflect the position of
`^^^^' in the MARKER.

Example (suppose line #3 of current buffer is \"full line\"):

(lsp-test-range-make (buffer-string)
                     \"full line\"
                     \"     ^^^^\")

-> (:line 3 :from 5 :to 8)
"
  (let ((line-number (lsp-test--find-line file-content line)))
    (should-not (null line-number))
    (should (eq (length marker) (length line)))
    (should (string-match "^ *\\(\\^+\\) *$" marker))
    (list :line line-number :from (match-beginning 1) :to (match-end 1))))

(defun lsp-test-diag-get (diagnostic)
  "Get the single-line diagnostics range summary of DIAGNOSTIC.

DIAGNOSTIC must have a single-line range.
Returns its range converted to `(:line .. :from .. :to ..)' format."
  (let* ((range (ht-get diagnostic "range"))
         (start (ht-get range "start"))
         (end (ht-get range "end")))
    (should (eq (ht-get start "line") (ht-get end "line")))
    (list :line (ht-get start "line")
          :from (ht-get start "character")
          :to (ht-get end "character"))))

(defun lsp-test-find-all-words (contents word)
  "Find all occurences of WORD in CONTENTS and return a list of ranges."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (let (locs)
      (while (re-search-forward word nil t)
        (let ((line (- (line-number-at-pos (point)) 1))
              (end-col (current-column))
              (start-col (- (current-column) (length word))))
          (push (list :start (list :line line :character start-col)
                      :end (list :line line :character end-col))
                locs)))
      locs)))

(defun lsp-test-make-diagnostics (for-file contents forbidden-word)
  "Come up with a diagnostic highlighting FORBIDDEN-WORD.

Scan CONTENTS for FORBIDDEN-WORD and produce diagnostics for each occurence.
Returns a p-list compatible with the mock server.

FOR-FILE is the path to the file to include in the diagnostics.
It might not contain exactly CONTENTS because it the diagnostic
might be generated for a modified and not saved buffer content."
  (let ((diagnostics (mapcar (lambda (loc)
                               (list :source "mockS"
                                     :code "E001"
                                     :range loc
                                     :message (format "Do not use word '%s'" forbidden-word)
                                     :severity 2))
                             (lsp-test-find-all-words contents forbidden-word))))
    ;; Use vconcat diagnostics to ensure proper JSON serialization of the list
    `(:uri ,(concat "file://" for-file) :diagnostics ,(vconcat diagnostics))))

(defun lsp-test-command-send-diags (file-path file-contents forbidden-word)
  "Generate and command the mock server to publish diagnostics.

Command the mock server to publish diagnostics highlighting every occurence of
FORBIDDEN-WORD in FILE-CONTENTS that corresponds to FILE-PATH."
  (let ((diags (lsp-test-make-diagnostics file-path file-contents forbidden-word)))
    (lsp-test-send-command-to-mock-server
     (format "(publish-diagnostics '%s)"
             (prin1-to-string diags)))))

(defun lsp-test-crash-server-with-message (message)
  "Command the mock server to crash with MESSAGE."
  (lsp-test-send-command-to-mock-server (format "(error %S)" message)))

;; I could not figure out how to use lsp-test-wait safely
;; (e.g., aborting it after a failed test), so I use a simpler
;; version.
(defun lsp-test--sync-wait-for (condition-func)
  "Synchronously waiting for CONDITION-FUNC to return non-nil.

Returns the non-nil return value of CONDITION-FUNC."
  (let ((result (funcall condition-func)))
    (while (not result)
      (sleep-for 0.05)
      (setq result (funcall condition-func)))
    result))

(defmacro lsp-test-sync-wait (condition)
  "Wait for the CONDITION to become non-nil and return it."
  `(lsp-test--sync-wait-for (lambda () ,condition)))

(defun lsp-mock--run-with-mock-server (test-body)
  "Run TEST-BODY function with mock LSP client connected to the mock server.

This is an environment function that configures lsp-mode, mock lsp-client,
opens the `lsp-test-sample-file' and starts the mock server."
  (let ((lsp-clients (lsp-ht)) ; clear all clients
        (lsp-diagnostics-provider :none) ; focus on LSP itself, not its UI integration
        (lsp-restart 'ignore) ; Avoid restarting the server or prompting user on a crash
        (lsp-enable-snippet nil) ; Avoid warning that lsp-yasnippet is not intalled
        (lsp-warn-no-matched-clients nil) ; Mute warning LSP can't figure out src lang
        (workspace-root (file-name-directory lsp-test-sample-file))
        (initial-server-count (lsp-test-total-folder-count)))
    (register-mock-client) ; register mock client as the one an only lsp client
    (lsp-workspace-folders-add workspace-root)
    (let* ((buf (find-file-noselect lsp-test-sample-file)))
      (unwind-protect
          (with-timeout (5 (error "Timeout running a test with mock server"))
            (with-current-buffer buf
              (prog-mode)
              (lsp)
              ;; Make sure the server started
              (should (eq (lsp-test-total-folder-count) (1+ initial-server-count)))
              (lsp-test-sync-wait (eq 'initialized
                                      (lsp--workspace-status (cl-first (lsp-workspaces)))))
              (funcall test-body)))
        (with-current-buffer buf
          (set-buffer-modified-p nil); Inhibut the "kill unsaved buffer"p prompt
          (kill-buffer buf))
        (lsp-workspace-folders-remove workspace-root)
        ;; Remove possibly unhandled commands
        (when (file-exists-p lsp-test-mock-server-command-file)
            (delete-file lsp-test-mock-server-command-file))))
    (with-timeout (5 (error "LSP mock server refuses to stop"))
      ;; Make sure the server stopped
      (lsp-test-sync-wait (= initial-server-count (lsp-test-total-folder-count))))))

(defmacro lsp-mock-run-with-mock-server (&rest test-body)
  "Evaluate TEST-BODY in the context of a mock client connected to mock server.

Opens the `lsp-test-sample-file' and initiates the LSP session.
TEST-BODY can interact with the mock server."
  `(lsp-mock--run-with-mock-server (lambda () ,@test-body)))

(ert-deftest lsp-mock-server-reports-issues ()
  (lsp-mock-run-with-mock-server
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
   (lsp-test-sync-wait (progn (should (lsp-workspaces))
                              (gethash lsp-test-sample-file (lsp-diagnostics t))))
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         ")))))

(ert-deftest lsp-mock-server-crashes ()
  "Test that the mock server crashes when instructed so."
  (let ((initial-serv-count (lsp-test-total-folder-count)))
    (when-let ((buffer (get-buffer "*mock-server::stderr*")))
      (kill-buffer buffer))
    (lsp-mock-run-with-mock-server
     (should (eq (lsp-test-total-folder-count) (1+ initial-serv-count)))
     (lsp-test-crash-server-with-message "crashed by command")
     (lsp-test-sync-wait (eq initial-serv-count (lsp-test-total-folder-count)))
     (let ((buffer (get-buffer "*mock-server::stderr*")))
       (should buffer)
       (with-current-buffer buffer
         (goto-char (point-min))
         (should (search-forward "crashed by command"))
         (goto-char (point-max)))))))

(defun lsp-mock-get-first-diagnostic-line ()
  "Get the line number of the first diagnostic on `lsp-test-sample-file'."
  (let ((diags (gethash lsp-test-sample-file (lsp-diagnostics t))))
    (when diags
      (let* ((diag (car diags))
             (range (ht-get diag "range"))
             (start (ht-get range "start")))
        (ht-get start "line")))))

(ert-deftest lsp-mock-server-updates-diagnostics ()
  "Test that mock server can update diagnostics and lsp-mode reflects that."
  (lsp-mock-run-with-mock-server
   ;; There are no diagnostics at first
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

   ;; Server found diagnostic
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
   (lsp-test-sync-wait (progn (should (lsp-workspaces))
                              (gethash lsp-test-sample-file (lsp-diagnostics t))))
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

   ;; The diagnostic is properly received
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         ")))

   ;; Server found a different diagnostic
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "fegam")
   (let ((old-line (lsp-mock-get-first-diagnostic-line)))
     (lsp-test-sync-wait (progn (should (lsp-workspaces))
                                (not (equal old-line (lsp-mock-get-first-diagnostic-line))))))

   ;; The new diagnostics is properly displayed instead of the old one
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "Line 0 unique word fegam and common"
                                       "                   ^^^^^           ")))))

(ert-deftest lsp-mock-server-updates-diags-with-delay ()
  "Test demonstrating delay in the diagnostics update.

If server takes noticeable time to update diagnostics after a
document change, and `lsp-diagnostic-clean-after-change' is
nil (default), diagnostic ranges will be off until server
publishes the update. This test demonstrates this behavior."
  (lsp-mock-run-with-mock-server
   ;; There are no diagnostics at first
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

   ;; Server found diagnostic
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
   (lsp-test-sync-wait (progn (should (lsp-workspaces))
                              (gethash lsp-test-sample-file (lsp-diagnostics t))))
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

   ;; The diagnostic is properly received
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         ")))

   ;; Change the text: remove the first line
   (goto-char (point-min))
   (kill-line 1)
   (should (string-equal (buffer-string)
                         "line 1 unique word broming + common
line 2 unique word normalw common here
line 3 words here and here
"))
   ;; Give it some time to update
   (sleep-for 0.5)
   ;; The diagnostic is not updated and now points to a wrong line
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 2 unique word normalw common here"
                                       "                   ^^^^^^^            ")))

   ;; Server sent an update
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")

   (let ((old-line (lsp-mock-get-first-diagnostic-line)))
     (lsp-test-sync-wait (progn (should (lsp-workspaces))
                                (not (equal old-line (lsp-mock-get-first-diagnostic-line))))))

   ;; Now the diagnostic is correct again
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         ")))))

(ert-deftest lsp-mock-server-updates-diags-clears-up ()
  "Test ensuring diagnostics are cleared after a change."
  (let ((lsp-diagnostic-clean-after-change t))
  (lsp-mock-run-with-mock-server
   ;; There are no diagnostics at first
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

   ;; Server found diagnostic
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
   (lsp-test-sync-wait (progn (should (lsp-workspaces))
                              (gethash lsp-test-sample-file (lsp-diagnostics t))))
   (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

   ;; The diagnostic is properly received
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         ")))

   ;; Change the text: remove the first line
   (goto-char (point-min))
   (kill-line 1)

   ;; After a short while, diagnostics are cleared up
   (lsp-test-sync-wait (progn (should (lsp-workspaces))
                              (null (gethash lsp-test-sample-file (lsp-diagnostics t)))))

   ;; Server sent an update
   (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")

   (let ((old-line (lsp-mock-get-first-diagnostic-line)))
     (lsp-test-sync-wait (progn (should (lsp-workspaces))
                                (not (equal old-line (lsp-mock-get-first-diagnostic-line))))))

   ;; Now the diagnostic is correct again
   (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                  (lsp-test-range-make (buffer-string)
                                       "line 1 unique word broming + common"
                                       "                   ^^^^^^^         "))))))

(defun lsp-test-xref-loc-to-range (xref-loc)
  "Convert XREF-LOC to a range p-list.

XREF-LOC is an xref-location object. The function returns a p-list
in the form of `(:line .. :from .. :to ..)'."
  (let ((line (- (xref-location-line (xref-item-location xref-loc)) 1))
        (len (xref-match-length xref-loc))
        (col (xref-file-location-column (xref-item-location xref-loc))))
    (list :line line :from col :to (+ col len))))

(defun lsp-test-make-references (for-file contents word)
  "Come up with a list of references to WORD in CONTENTS.

Scan CONTENTS for all occurences of WORD and compose a list of references."
  (let ((add-uri (lambda (range) `(:uri ,(concat "file://" for-file)
                              :range ,range))))
    (vconcat (mapcar add-uri (lsp-test-find-all-words contents word)))))

(ert-deftest lsp-mock-server-provides-referencs ()
  "Test ensuring that lsp-mode accepts correct locations for references."
  (let* (found-xrefs
         (xref-show-xrefs-function (lambda (fetcher &rest _params)
                                     (setq found-xrefs (funcall fetcher)))))
    (lsp-mock-run-with-mock-server
     (lsp-test-send-command-to-mock-server
      (format "(schedule-response \"textDocument/references\" '%s)"
              (lsp-test-make-references
               lsp-test-sample-file (buffer-string) "unique")))
     (lsp-find-references)
     (message "%s" found-xrefs)
     (should found-xrefs)
     (should (eq (length found-xrefs) 3))
     (should (equal (lsp-test-xref-loc-to-range (nth 0 found-xrefs))
                    (lsp-test-range-make (buffer-string)
                                         "Line 0 unique word fegam and common"
                                         "       ^^^^^^                      ")))
     (should (equal (lsp-test-xref-loc-to-range (nth 1 found-xrefs))
                    (lsp-test-range-make (buffer-string)
                                         "line 1 unique word broming + common"
                                         "       ^^^^^^                      ")))
     (should (equal (lsp-test-xref-loc-to-range (nth 2 found-xrefs))
                    (lsp-test-range-make (buffer-string)
                                         "line 2 unique word normalw common here"
                                         "       ^^^^^^                         "))))))

;;; lsp-mock-server-test.el ends here
