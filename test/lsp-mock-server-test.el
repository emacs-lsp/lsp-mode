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

(require 'seq)
(require 'lsp-mode)
(require 'flycheck)

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

(defun lsp-test-full-range (short-range)
  "Convert SHORT-RANGE to a full range.

SHORT-RANGE is a p-list with :line, :from, and :to keys.
Returns a full range p-list with :start and :end keys."
  (list :start (list :line (plist-get short-range :line)
                     :character (plist-get short-range :from))
        :end (list :line (plist-get short-range :line)
                   :character (plist-get short-range :to))))

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
(defun lsp-test--sync-wait-for (timeout timeout-desc condition-func)
  "Synchronously waiting CONDITION-FUNC to return non-nil.

TIMEOUT (seconds) is the maximum time to wait in seconds.
If after TIMEOUT seconds CONDITION-FUNC does not retur non-nul
raise `error' with TIMEOUT-DESC (string).
Returns the non-nil return value of CONDITION-FUNC."
  (with-timeout (timeout (error timeout-desc))
    (let ((result (funcall condition-func)))
      (while (not result)
        (sleep-for 0.05)
        (setq result (funcall condition-func)))
      result)))

(defmacro lsp-test-sync-wait (timeout timeout-desc &rest body)
  "Repeatedly evaluate BODY until its last form evaluates to non-nil.

TIMEOUT (seconds) is the maximum time to busy-wait.
If after TIMEOUT seconds CONDITION does not become non-nil
raise `error' with \"Timeout wiaitng for\" + TIMEOUT-DESC (string).
BODY is implicitly wrapped in `progn'.
Once last form of BODY evaluates to non-nil, return its result."
  (declare (indent 2))
  `(lsp-test--sync-wait-for
    ,timeout ,(concat "Timeout waiting for " timeout-desc) (lambda () ,@body)))

(defun lsp-test-relevant-overlays ()
  "Collect all overlays that might have been produced by LSP."
  (seq-filter (lambda (ovl) (null (overlay-get ovl 'pulse-delete)))
              (overlays-in (point-min) (point-max))))

(defun lsp-mock--run-with-mock-server (sample-file diags-provider test-body)
  "Run TEST-BODY function with mock LSP client connected to the mock server.

This is an environment function that configures lsp-mode, mock lsp-client,
opens the SAMPLE-FILE and starts the mock server.
It sets DIAGS-PROVIDER as the diagnostics provider for the test,
which should be a value valid for `lsp-diagnostics-provider'."
  (let ((lsp-clients (lsp-ht)) ; clear all clients
        (lsp-diagnostics-provider diags-provider) ; focus on LSP itself, not its UI integration
        (lsp-restart 'ignore) ; Avoid restarting the server or prompting user on a crash
        (lsp-enable-snippet nil) ; Avoid warning that lsp-yasnippet is not intalled
        (lsp-warn-no-matched-clients nil) ; Mute warning LSP can't figure out src lang
        (workspace-root (file-name-directory sample-file))
        (initial-server-count (lsp-test-total-folder-count))
        (initial-overlay-count nil))
    (register-mock-client) ; register mock client as the one an only lsp client

    ;; xref in emacs 27.2 does not have these vars,
    ;; but lsp-mode uses them in lsp-show-xrefs.
    ;; For the purpose of this test, it does not matter.
    (unless (boundp 'xref-auto-jump-to-first-xref)
      (defvar xref-auto-jump-to-first-xref nil))
    (unless (boundp 'xref-auto-jump-to-first-definition)
      (defvar xref-auto-jump-to-first-definition nil))

    (lsp-workspace-folders-add workspace-root)
    (let* ((buf (find-file-noselect sample-file)))
      (unwind-protect
          (progn
            (with-timeout (5 (error "Timeout running a test with mock server"))
              (with-current-buffer buf
                (prog-mode)
                (setq initial-overlay-count (length (lsp-test-relevant-overlays)))
                (lsp)
                ;; Make sure the server started
                (should (eq (lsp-test-total-folder-count) (1+ initial-server-count)))
                (lsp-test-sync-wait 4 "LSP workspace initialization"
                                    (eq 'initialized
                                        (lsp--workspace-status (cl-first (lsp-workspaces)))))
                (funcall test-body)))
            (with-current-buffer buf
              (lsp-test-sync-wait
               4 "extra overlays to dissolve"
               (equal initial-overlay-count (length (lsp-test-relevant-overlays))))))
        (with-current-buffer buf
          (set-buffer-modified-p nil); Inhibut the "kill unsaved buffer"p prompt
          (kill-buffer buf))
        (lsp-workspace-folders-remove workspace-root)
        ;; Remove possibly unhandled commands
        (when (file-exists-p lsp-test-mock-server-command-file)
          (delete-file lsp-test-mock-server-command-file))))
    ;; Make sure the server stopped
    (lsp-test-sync-wait 5 "LSP mock server to stop"
                        (= initial-server-count (lsp-test-total-folder-count)))))

(defmacro lsp-mock-run-with-mock-server (sample-file diags-provider &rest test-body)
  "Evaluate TEST-BODY in the context of a mock client connected to mock server.

Opens the SAMPLE-FILE and initiates the LSP session.
It sets DIAGS-PROVIDER as the diagnostics provider for the test,
which should be a value valid for `lsp-diagnostics-provider'.
TEST-BODY can interact with the mock server."
  (declare (indent 2))
  `(lsp-mock--run-with-mock-server
    ,sample-file
    ,diags-provider
    (lambda () ,@test-body)))

(ert-deftest lsp-mock-server-reports-diagnostics ()
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
    (lsp-test-sync-wait
     4 "lsp mode to get the diagnostic"
     (should (lsp-workspaces))
     (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))
    (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))
    ;; Clean up diagnostics
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "non-existing")))

(ert-deftest lsp-mock-server-crashes ()
  "Test that the mock server crashes when instructed so."
  (let ((initial-serv-count (lsp-test-total-folder-count)))
    (when-let ((buffer (get-buffer "*mock-server::stderr*")))
      (kill-buffer buffer))
    (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
      (should (eq (lsp-test-total-folder-count) (1+ initial-serv-count)))
      (lsp-test-crash-server-with-message "crashed by command")
      (lsp-test-sync-wait
       4 "LSP server to crash by command"
       (eq initial-serv-count (lsp-test-total-folder-count)))
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
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    ;; There are no diagnostics at first
    (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

    ;; Server found diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
    (lsp-test-sync-wait
     4 "LSP mode to receive initial diagnostic"
     (should (lsp-workspaces))
     (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

    ;; The diagnostic is properly received
    (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Server found a different diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "fegam")
    (lsp-test-wait-for-diagnostic-update)

    ;; The new diagnostics is properly displayed instead of the old one
    (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))
    (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                   (lsp-test-range-make (buffer-string)
                                        "Line 0 unique word fegam and common"
                                        "                   ^^^^^           ")))
    ;; Clean up diagnostics
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "non-existing")))

(ert-deftest lsp-mock-server-updates-diags-with-delay ()
  "Test demonstrating delay in the diagnostics update.

If server takes noticeable time to update diagnostics after a
document change, and `lsp-diagnostic-clean-after-change' is
nil (default), diagnostic ranges will be off until server
publishes the update. This test demonstrates this behavior."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    ;; There are no diagnostics at first
    (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

    ;; Server found diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
    (lsp-test-sync-wait
     4 "LSP mode to receive initial diagnostic"
     (should (lsp-workspaces))
     (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

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
    (lsp-test-wait-for-diagnostic-update)


    ;; Now the diagnostic is correct again
    (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))
    ;; Clean up diagnostics
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "non-existing")))

(ert-deftest lsp-mock-server-updates-diags-clears-up ()
  "Test ensuring diagnostics are cleared after a change."
  (let ((lsp-diagnostic-clean-after-change t))
    (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
      ;; There are no diagnostics at first
      (should (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 0))

      ;; Server found diagnostic
      (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
      (lsp-test-sync-wait
       4 "LSP mode to receive initial diagnostics"
       (should (lsp-workspaces))
       (eq (length (gethash lsp-test-sample-file (lsp-diagnostics t))) 1))

      ;; The diagnostic is properly received
      (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                     (lsp-test-range-make (buffer-string)
                                          "line 1 unique word broming + common"
                                          "                   ^^^^^^^         ")))

      ;; Change the text: remove the first line
      (goto-char (point-min))
      (kill-line 1)

      ;; After a short while, diagnostics are cleared up
      (lsp-test-sync-wait 4 "LSP mode to clear up diagnostics"
                          (should (lsp-workspaces))
                          (null (gethash lsp-test-sample-file (lsp-diagnostics t))))

      ;; Server sent an update
      (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
      (lsp-test-wait-for-diagnostic-update)


      ;; Now the diagnostic is correct again
      (should (equal (lsp-test-diag-get (car (gethash lsp-test-sample-file (lsp-diagnostics t))))
                     (lsp-test-range-make (buffer-string)
                                          "line 1 unique word broming + common"
                                          "                   ^^^^^^^         ")))
      ;; Clean up diagnostics
      (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "non-existing"))))

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

(defun lsp-test-schedule-response (method response)
  "Schedule a RESPONSE to be sent in response to METHOD."
  (lsp-test-send-command-to-mock-server
   (format "(schedule-response %S %S)" method response)))

(ert-deftest lsp-mock-server-provides-references ()
  "Test ensuring that lsp-mode accepts correct locations for references."
  (let* (found-xrefs
         (xref-show-xrefs-function (lambda (fetcher &rest _params)
                                     (setq found-xrefs (funcall fetcher)))))
    (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
      (lsp-test-schedule-response "textDocument/references"
                                  (lsp-test-make-references
                                   lsp-test-sample-file (buffer-string) "unique"))
      (lsp-find-references)
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

(ert-deftest lsp-mock-server-provides-folding-ranges ()
  "Test ensuring that lsp-mode accepts correct locations for folding ranges."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (lsp-test-schedule-response
     "textDocument/foldingRange"
     [(:kind "region" :startLine 0 :startCharacter 10 :endLine 1)
      (:kind "region" :startLine 1 :startCharacter 5 :endLine 2)])

    (let ((folding-ranges (lsp--get-folding-ranges)))
      (should (eq (length folding-ranges) 2))
      ;; LSP line numbers are 0-based, Emacs line numbers are 1-based
      ;; henace the +1
      (should (equal (line-number-at-pos
                      (lsp--folding-range-beg (nth 0 folding-ranges)))
                     1))
      (should (equal (line-number-at-pos
                      (lsp--folding-range-end (nth 0 folding-ranges)))
                     2))
      (should (equal (line-number-at-pos
                      (lsp--folding-range-beg (nth 1 folding-ranges)))
                     2))
      (should (equal (line-number-at-pos
                      (lsp--folding-range-end (nth 1 folding-ranges)))
                     3)))))

(ert-deftest lsp-mock-server-lsp-caches-folding-ranges ()
  "Test ensuring that lsp-mode accepts correct locations for folding ranges."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (should (eq (lsp--get-folding-ranges) nil))
    (lsp-test-schedule-response
     "textDocument/foldingRange"
     [(:kind "region" :startLine 0 :startCharacter 10 :endLine 1)])
    ;; Folding ranges are cached from the first request
    (should (eq (lsp--get-folding-ranges) nil))))

(defun lsp-test-all-overlays (tag)
  "Return all overlays tagged TAG in the current buffer."
  (let ((overlays (overlays-in (point-min) (point-max))))
    (seq-filter (lambda (overlay)
                  (overlay-get overlay tag))
                overlays)))

(defun lsp-test-all-overlays-as-ranges (tag)
  "Return all overlays tagged TAG in the current buffer as ranges.

Tagged overlays have the property TAG set to t."
  (let ((to-range
         (lambda (overlay)
           (let* ((beg (overlay-start overlay))
                  (end (overlay-end overlay))
                  (beg-line (line-number-at-pos beg))
                  (end-line (line-number-at-pos end))
                  (beg-col (progn (goto-char beg) (current-column)))
                  (end-col (progn (goto-char end) (current-column))))
             (should (equal beg-line end-line))
             (list :line (- beg-line 1) :from beg-col :to end-col)))))
    (save-excursion
      (mapcar to-range (lsp-test-all-overlays tag)))))

(defun lsp-test-make-highlights (contents word)
  "Come up with a list of highlights of WORD in CONTENTS.

Scan CONTENTS for all occurences of WORD and compose a list of highlights."
  (let ((add-uri (lambda (range) `(:kind 1 :range ,range))))
    (vconcat (mapcar add-uri (lsp-test-find-all-words contents word)))))

(defun lsp-mock-with-temp-window (buffer-name test-fn)
  "Create a temporary window displaying BUFFER-NAME and call TEST-FN.
BUFFER-NAME is the name of the buffer to display.
TEST-FN is a function to call with the temporary window."
  (let ((original-window (selected-window))
        (temp-window (split-window)))
    (unwind-protect
        (progn
          ;; Display the buffer in the temporary window
          (set-window-buffer temp-window buffer-name)
          ;; Switch to the temporary window
          (select-window temp-window)
          ;; Call the test function
          (funcall test-fn))
      ;; Clean up: Delete the temporary window and select the original window
      (delete-window temp-window)
      (select-window original-window))))

(defun lsp-test-sort-ranges (ranges)
  "Sort RANGES in natural order."
  (sort
   ranges
   (lambda (a b)
     (or (< (plist-get a :line) (plist-get b :line))
         (and (= (plist-get a :line) (plist-get b :line))
              (or (< (plist-get a :from) (plist-get b :from))
                  (and (= (plist-get a :from) (plist-get b :from))
                       (< (plist-get a :to) (plist-get b :to)))))))))

(ert-deftest lsp-mock-server-provides-symbol-highlights ()
  "Test ensuring that lsp-mode accepts correct locations for highlights."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (lsp-test-schedule-response
     "textDocument/documentHighlight"
     (lsp-test-make-highlights (buffer-string) "here"))
    ;; The highlight overlays are created only if visible in a window
    (lsp-mock-with-temp-window
     (current-buffer)
     (lambda ()
       (lsp-document-highlight)

       (let ((highlights (lsp-test-sort-ranges
                          (lsp-test-sync-wait
                           4 "LSP mode to receive highlights"
                           (should (lsp-workspaces))
                           (lsp-test-all-overlays-as-ranges 'lsp-highlight)))))
         (should (eq (length highlights) 3))
         (should (equal (nth 0 highlights)
                        (lsp-test-range-make (buffer-string)
                                             "line 2 unique word normalw common here"
                                             "                                  ^^^^")))
         (should (equal (nth 1 highlights)
                        (lsp-test-range-make (buffer-string)
                                             "line 3 words here and here"
                                             "             ^^^^         ")))
         (should (equal (nth 2 highlights)
                        (lsp-test-range-make (buffer-string)
                                             "line 3 words here and here"
                                             "                      ^^^^"))))
       ;; Remove highlights
       (lsp-test-schedule-response "textDocument/documentHighlight" [])
       (lsp-document-highlight)))))

(defun lsp-test-index-to-pos (idx)
  "Convert 0-based integer IDX to a position in the corrent buffer.

Retruns the position p-list."
  (lsp-point-to-position (1+ idx)))

(defun lsp-test-make-edits (marked-up)
  "Create a list of edits to transform current buffer according to MARKED-UP.

MARKED-UP string uses a simple markup syntax to indicate
insertions and deletions. The function returns a list of edits
each in the form `(:range .. :newText ..)'

The markup syntax is as follows:
- <word> - indicates an insertion of the text `word'
- ####### - indicates a deletion of the text that was in place of each `#'

All edits must be single line: deletion must not cross a line break
and insertion must not contain a line break."
  (let ((edits nil)
        (original (buffer-string))
        (orig-idx 0)
        (marked-idx 0))
    (while (and (< orig-idx (length original))
                (< marked-idx (length marked-up)))
      (let ((orig-char (aref original orig-idx))
            (marked-char (aref marked-up marked-idx)))
        (cond
         ((eq marked-char ?<) ; Insertion
          (let ((marked-idx-start marked-idx))
            (while (and (< marked-idx (length marked-up))
                        (not (eq (aref marked-up marked-idx) ?>)))
              (setq marked-idx (1+ marked-idx)))
            (should (< marked-idx (length marked-up)))
            (push `(:range (:start ,(lsp-test-index-to-pos orig-idx)
                            :end ,(lsp-test-index-to-pos orig-idx))
                    :newText ,(substring marked-up (1+ marked-idx-start) marked-idx))
                  edits)
            (setq marked-idx (1+ marked-idx)) ; Skip the closing >
            ))
         ((eq marked-char ?#) ; Deletion
          (let ((orig-idx-start orig-idx))
            (while (and (< marked-idx (length marked-up))
                        (< orig-idx (length original))
                        (eq (aref marked-up marked-idx) ?#))
              (setq orig-idx (1+ orig-idx))
              (setq marked-idx (1+ marked-idx)))
            (should (and (< marked-idx (length marked-up))
                         (< orig-idx (length original))))
            (push `(:range (:start ,(lsp-test-index-to-pos orig-idx-start)
                            :end ,(lsp-test-index-to-pos orig-idx))
                    :newText "")
                  edits)))
         (t (should (eq orig-char marked-char))
            (setq orig-idx (1+ orig-idx))
            (setq marked-idx (1+ marked-idx))))))
    (should (and (= orig-idx (length original))
                 (= marked-idx (length marked-up))))
    (vconcat (reverse edits))))

(ert-deftest lsp-mock-make-edits-sane ()
  "Check the test-utility function `lsp-mock-make-edits'."
  (with-temp-buffer
    (insert "line 0 common deleted common")
    (should (equal (lsp-test-make-edits
                    "<inserted>line 0 common deleted common")
                   [(:range (:start (:line 0 :character 0)
                             :end (:line 0 :character 0))
                            :newText "inserted")
                    ]))
    (should (equal (lsp-test-make-edits
                    "l<inserted>ine 0 common deleted common")
                   [(:range (:start (:line 0 :character 1)
                             :end (:line 0 :character 1))
                            :newText "inserted")
                    ]))
    (should (equal (lsp-test-make-edits
                    "line 0 <inserted>common ####### common")
                   [(:range (:start (:line 0 :character 7)
                             :end (:line 0 :character 7))
                            :newText "inserted")
                    (:range (:start (:line 0 :character 14)
                             :end (:line 0 :character 21))
                            :newText "")]))))

(ert-deftest lsp-mock-server-formats-with-edits ()
  "Test ensuring that lsp-mode requests and applies formatting correctly."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (lsp-test-schedule-response
     "textDocument/formatting"
     (lsp-test-make-edits
      "Line 0 ###### word fegam and common
line 1 unique <double>word ######### common
line 2 unique word #ormalw common here
line 3 words here and here
"))
    (lsp-format-buffer)
    (should (equal (buffer-string)
                   "Line 0  word fegam and common
line 1 unique doubleword  common
line 2 unique word ormalw common here
line 3 words here and here
"))))

(ert-deftest lsp-mock-server-suggests-action-with-simple-changes ()
  "Test ensuring that lsp-mode applies code action simple edits correctly."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (lsp-test-schedule-response
     "textDocument/codeAction"
     (vconcat (list `(:title "Some edits"
                      :kind "quickfix"
                      :isPreferred t
                      :edit
                      (:changes
                       ((,(concat "file://" lsp-test-sample-file)
                         .
                         ,(lsp-test-make-edits
                           "Line 0 unique word ######### common
line # unique word broming + common
line # unique word normalw common here
line #<81> words here and here
"))))))))
    (lsp-execute-code-action-by-kind "quickfix")
    (should (equal (buffer-string)
                   "Line 0 unique word  common
line  unique word broming + common
line  unique word normalw common here
line 81 words here and here
"))))

(ert-deftest lsp-mock-server-suggests-action-with-doc-changes ()
  "Test ensuring that lsp-mode applies code action document edits correctly."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (let ((docChanges
           (vconcat (list `(:textDocument
                            (:version 0 ; document was never changed
                             :uri ,(concat "file://" lsp-test-sample-file))
                            :edits
                            ,(lsp-test-make-edits
                              "Line 0 ########### ######### common
line 1<00> unique word broming + common
line # ###### word normalw common here
line #<81> words here and here
"))))))
      (lsp-test-schedule-response
       "textDocument/codeAction"
       (vconcat (list `(:title "Some edits"
                        :kind "quickfix"
                        :isPreferred t
                        :edit
                        (:changes #s(hash-table data ()) ; empty obj
                         :documentChanges ,docChanges)))))
      (lsp-execute-code-action-by-kind "quickfix")
      (should (equal (buffer-string)
                     "Line 0   common
line 100 unique word broming + common
line   word normalw common here
line 81 words here and here
")))))

(ert-deftest lsp-mock-doc-changes-wrong-version ()
  "Test ensuring that lsp-mode applies code action document edits correctly."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (let ((docChanges
           (vconcat (list `(:textDocument
                            (:version 1 ; This version does not exist
                             :uri ,(concat "file://" lsp-test-sample-file))
                            :edits [])))))
      (lsp-test-schedule-response
       "textDocument/codeAction"
       (vconcat (list `(:title "Some edits"
                        :kind "quickfix"
                        :isPreferred t
                        :edit
                        (:changes #s(hash-table data ()) ; empty obj
                         :documentChanges ,docChanges)))))
      (should-error (lsp-execute-code-action-by-kind "quickfix")))))

;; Some actions are executed partially by the server:
;; after the user selects the action, lsp-mode sends a request
;; to exute the associated command.
;; Only after that, server sends a request to perform edits
;; in the editor.
;; This test simulates only the last bit.
(ert-deftest lsp-mock-server-request-edits ()
  "Test ensuring that lsp-mode honors server's request for edits."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (let ((initial-content (buffer-string)))
      (lsp-test-send-command-to-mock-server
       (format "(princ (json-rpc-string '(:id 1 :method \"workspace/applyEdit\"
                                      :params (:edit
                                               (:changes
                                                ((%S . %S)))))))"
               (concat "file://" lsp-test-sample-file)
               (lsp-test-make-edits
                "#### <8>0 unique word fegam and common
line 1 unique word broming + common
line 2 unique word normalw common here
line 3 words here and here
")))
      (lsp-test-sync-wait 4 "LSP mode changes code"
                          (should (lsp-workspaces))
                          (not (equal initial-content (buffer-string))))
      (should (equal (buffer-string)
                     " 80 unique word fegam and common
line 1 unique word broming + common
line 2 unique word normalw common here
line 3 words here and here
")))))

(ert-deftest lsp-mock-server-no-declaration-found ()
  "Test checking that lsp-mode reports when server returns no declaration."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (should (string-match-p "not found" (lsp-find-declaration)))))

(ert-deftest lsp-mock-server-goto-declaration ()
  "Test checking that lsp-mode can follow the symbol declaration."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (let ((decl-range (lsp-test-range-make
                       (buffer-string)
                       "line 1 unique word broming + common"
                       "                   ^^^^^^^         ")))
      (lsp-test-schedule-response
       "textDocument/declaration"
       (vconcat (list `(:uri ,(concat "file://" lsp-test-sample-file)
                        :range ,(lsp-test-full-range decl-range)))))
      (lsp-find-declaration)
      ;; 1+ to convert 0-based LSP line number to 1-based Emacs line number
      (should (equal (1+ (plist-get decl-range :line)) (line-number-at-pos)))
      (should (equal (plist-get decl-range :from) (current-column))))))

(ert-deftest lsp-mock-server-goto-definition ()
  "Test checking that lsp-mode can follow the symbol definition."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    (let ((decl-range (lsp-test-range-make
                       (buffer-string)
                       "line 3 words here and here"
                       "     ^^^^^^^              ")))
      (lsp-test-schedule-response
       "textDocument/definition"
       (vconcat (list `(:uri ,(concat "file://" lsp-test-sample-file)
                        :range ,(lsp-test-full-range decl-range)))))
      (lsp-find-definition)
      ;; 1+ to convert 0-based LSP line number to 1-based Emacs line number
      (should (equal (1+ (plist-get decl-range :line)) (line-number-at-pos)))
      (should (equal (plist-get decl-range :from) (current-column))))))

(ert-deftest lsp-mock-server-provides-inlay-hints ()
  "lsp-mode accepts inlay hints from the server and displays them."
  (let ((lsp-inlay-hint-enable t)
        (hint-line 2)
        (hint-col 10))
    (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
      (lsp-mock-with-temp-window
       (current-buffer)
       (lambda ()
         (lsp-test-schedule-response
          "textDocument/inlayHint"
          (vconcat (list `(:kind 2
                           :position (:line ,hint-line :character ,hint-col)
                           :paddingLeft ()
                           :label "my hint"))))
         ;; Lsp will update inlay hints on idling
         (run-hooks 'lsp-on-idle-hook)
         (lsp-test-sync-wait 4 "LSP mode inserts inlay hints"
                             (should (lsp-workspaces))
                             (lsp-test-all-overlays 'lsp-inlay-hint))
         (let ((hints (lsp-test-all-overlays 'lsp-inlay-hint)))
           (should (eq (length hints) 1))
           (should (equal (overlay-get (car hints) 'before-string) "my hint"))
           (goto-char (overlay-start (car hints)))
                                        ; 1+ to convert 0-based LSP line number to 1-based Emacs line number
           (should (equal (line-number-at-pos) (1+ hint-line)))
           (should (equal (current-column) hint-col)))
         ;; Disable inlay hints to remove overlays
         (lsp-test-schedule-response "textDocument/inlayHint" [])
         (run-hooks 'lsp-on-idle-hook))))))

(ert-deftest lsp-mock-server-provides-code-lens ()
  "lsp-mode accepts code lenses from the server and displays them."
  (let ((line 2))
    (lsp-test-schedule-response
     "textDocument/codeLens"
     (vconcat (list `(:range (:start (:line ,line :character 0)
                              :end (:line ,line :character 1))
                      :command (:title "My command"
                                :command "myCommand")))))
    (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
      (lsp-test-sync-wait 4 "LSP server inserts lenses"
                          (lsp-test-all-overlays 'lsp-lens))
      (let ((lenses (lsp-test-all-overlays 'lsp-lens)))
        (should (eq (length lenses) 1))
        (should (string-match-p "My command"
                                (overlay-get (car lenses) 'after-string)))
        (goto-char (overlay-start (car lenses)))
        (should (equal (line-number-at-pos) (- line 1))))
      ;; Remove lens overlays
      (lsp-lens-hide))))

(defun lsp-test-overlay-ranges (tag)
  "Return all overlays tagged TAG in the current buffer as ranges."
  (save-excursion
    (seq-map (lambda (ovl)
               (goto-char (overlay-start ovl))
               (let ((from-char (current-column))
                     (from-line (line-number-at-pos)))
                 (goto-char (overlay-end ovl))
                 (should (equal from-line (line-number-at-pos)))
                 (list :line (- from-line 1) ;; 1-based to 0-based
                       :from from-char
                       :to (current-column))))
             (seq-filter (lambda (ovl) (overlay-get ovl tag))
                         (overlays-in (point-min) (point-max))))))

(defun lsp-test-flycheck-diags ()
  "Get all diags displayed by flycheck."
  (lsp-test-overlay-ranges 'flycheck-overlay))

(ert-deftest lsp-mock-server-flycheck-updates-diagnostics ()
  "Test that mock server can update diagnostics and lsp-mode reflects that."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    ;; There are no diagnostics at first
    (should (null (lsp-test-flycheck-diags)))

    ;; Server found a diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")

    ;; For some reason, flycheck refuses to call lsp-diagnostics--flycheck-start
    (lsp-test-sync-wait 4 "Flycheck inserts diagnostics"
                        (should (lsp-workspaces))
                        (flycheck-buffer)
                        (eq (length (lsp-test-flycheck-diags)) 1))

    ;; The diagnostic is properly received
    (should (equal (car (lsp-test-flycheck-diags))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Server found a different diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "fegam")
    (lsp-test-wait-for-diagnostic-update)
    (flycheck-buffer)

    ;; The new diagnostics is properly displayed instead of the old one
    (should (eq (length (lsp-test-flycheck-diags)) 1))
    (should (equal (car (lsp-test-flycheck-diags))
                   (lsp-test-range-make (buffer-string)
                                        "Line 0 unique word fegam and common"
                                        "                   ^^^^^           ")))
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "nonexistent")

    (lsp-test-sync-wait 3 "Flycheck diags dissipate"
                        (should (lsp-workspaces))
                        (flycheck-buffer)
                        (null (lsp-test-flycheck-diags)))))

(defun lsp-test-wait-for-diagnostic-update ()
  "Wait until LSP receives updated diagnostics from the mock server."
  (let ((diags-updated nil))
    (cl-flet ((on-diags-updated (lambda (&rest _args) (setq diags-updated t))))
      (add-hook 'lsp-diagnostics-updated-hook #'on-diags-updated)
      (unwind-protect
          (lsp-test-sync-wait 4 "LSP mode to receive updated diagnostics"
            (should (lsp-workspaces))
            diags-updated))
      (remove-hook 'lsp-diagnostics-updated-hook #'on-diags-updated))))

(ert-deftest lsp-mock-server-flycheck-updates-diags-with-delay ()
  "Test demonstrating delay in the diagnostics update.

If server takes noticeable time to update diagnostics after a
document change, and `lsp-diagnostic-clean-after-change' is
nil (default), diagnostic ranges will be off until server
publishes the update. This test demonstrates this behavior."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flycheck
    ;; There are no diagnostics at first
    (should (null (lsp-test-flycheck-diags)))

    ;; Server found diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
    (lsp-test-sync-wait
     4 "LSP mode to receive initial diagnostic"
     (should (lsp-workspaces))
     (flycheck-buffer)
     (eq (length (lsp-test-flycheck-diags)) 1))

    ;; The diagnostic is properly received
    (should (equal (car (lsp-test-flycheck-diags))
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
    (flycheck-buffer)
    ;; The diagnostic range is adjusted automatically
    (should (equal (car (lsp-test-flycheck-diags))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Server sent an update
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")

    (lsp-test-wait-for-diagnostic-update)
    (flycheck-buffer)

    ;; The diagnostic is still in a correct position
    (should (equal (car (lsp-test-flycheck-diags))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Remove diagnostics
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "nonexistent")
    (lsp-test-sync-wait 3 "Flycheck diags dissipate"
                        (should (lsp-workspaces))
                        (flycheck-buffer)
                        (null (lsp-test-flycheck-diags)))))

(defun lsp-test-flymake-diags ()
  "Get all diags displayed by flymake."
  ;; The first property flymake sets on a diagnostic overlay is 'category
  (lsp-test-overlay-ranges 'category))

(ert-deftest lsp-mock-server-flymake-maintains-diags-with-doc-changes ()
  "Test demonstrating delay in the diagnostics update.

If server takes noticeable time to update diagnostics after a
document change, and `lsp-diagnostic-clean-after-change' is
nil (default), diagnostic positions in (lsp-diagnostics) will be
off until server publishes the update. However, flymake keeps the
overlays it created until an update comes, and overlays are
automatically adjusted on every edit, so diagnostic ranges remain
correct."
  (lsp-mock-run-with-mock-server lsp-test-sample-file :flymake
    ;; There are no diagnostics at first
    (should (null (lsp-test-flymake-diags)))

    ;; Server found diagnostic
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")
    (lsp-test-sync-wait
     4 "LSP mode to receive initial diagnostic"
     (should (lsp-workspaces))
     (flymake-start)
     (eq (length (lsp-test-flymake-diags)) 1))

    ;; The diagnostic is properly received
    (should (equal (car (lsp-test-flymake-diags))
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
    (flymake-start)
    ;; Unlike flycheck, flymake keeps the old diagnostics as overlays until it
    ;; gets an update. So the range of the diagnostic is preserved properly.
    (should (equal (car (lsp-test-flymake-diags))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Server sent an update
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "broming")

    (lsp-test-wait-for-diagnostic-update)
    (flymake-start)

    ;; Upon reception, flymake replaces the old overlays with the
    ;; new ones placed on the reported position, which happend to
    ;; be the same.
    (should (equal (car (lsp-test-flymake-diags))
                   (lsp-test-range-make (buffer-string)
                                        "line 1 unique word broming + common"
                                        "                   ^^^^^^^         ")))

    ;; Remove diagnostics
    (lsp-test-command-send-diags lsp-test-sample-file (buffer-string) "nonexistent")
    (lsp-test-sync-wait 3 "Flymake diags dissipate"
                        (should (lsp-workspaces))
                        (flymake-start)
                        (null (lsp-test-flymake-diags)))))

(defun lsp-mock-edit-org-buffer ()
  "Edit the org buffer: remove 2 lines and undent code block."
  ;; Change the text: remove the first line in the org doc itself
  (goto-char (point-min))
  (kill-line 1)
  ;; And in the code block
  (search-forward "Line 0 unique")
  (goto-char (line-beginning-position))
  (kill-line 1)
  ;; And unindent the block by 2 characters
  (forward-line -1)
  (dotimes (_ 5) ;; 3 lines in the code block + begin_src + end_src lines
    (goto-char (line-beginning-position))
    (kill-forward-chars 2)
    (forward-line 1))
  (forward-line -1)
  (should (string-equal (buffer-string)
                        "
** Header to a section with some indentation
     language =prog= will resolve into the =prog-mode= - mode for the mock LSP server
   #+begin_src prog :tangle \"sample.txt\"
   line 1 unique word broming + common
   line 2 unique word normalw common here
   line 3 words here and here
   #+end_src
")))

(defun lsp-test-org-code-block-contents ()
  "Return the contents of the sample file."
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+begin_src prog")
    (forward-line 1)
    (let* ((beg (point))
           (end (progn (search-forward "#+end_src") (line-beginning-position)))
           (contents (buffer-substring-no-properties beg end))
           (lines (split-string contents "\n"))
           (trimmed-lines (mapcar (lambda (line) (string-trim-left line)) lines)))
      (string-join trimmed-lines "\n"))))


(ert-deftest lsp-mock-server-org-flycheck-updates-diags-with-delay ()
  :tags '(org)
  "Test ensuring flycheck shows correct diagnostic locations in org-mode."
  (let ((org-file (f-join lsp-test-location "fixtures/SamplesForMock/embedded-snippet.org"))
        (snippet-file lsp-test-sample-file))
    (lsp-mock-run-with-mock-server org-file :flycheck
      (goto-char (point-min))
      (search-forward "Line 0 unique")
      (lsp-org)
                                        ;(should (eq (lsp-test-total-folder-count) (1+ initial-server-count)))
      (lsp-test-sync-wait 4 "LSP workspace initialization"
        (eq 'initialized
            (lsp--workspace-status (cl-first (lsp-workspaces)))))
      (should (null (lsp-test-flycheck-diags)))
      (lsp-test-command-send-diags
       snippet-file (lsp-test-org-code-block-contents) "broming")
      (lsp-test-sync-wait 4 "lsp mode to get the diagnostic"
        (should (lsp-workspaces))
        (flycheck-buffer)
        (lsp-test-flycheck-diags))
      (should (equal (car (lsp-test-flycheck-diags))
                     (lsp-test-range-make (buffer-string)
                                          "     line 1 unique word broming + common"
                                          "                        ^^^^^^^         ")))
      (lsp-mock-edit-org-buffer)
      ;; Give it some time to update
      (sleep-for 0.5)
      (flycheck-buffer)
      ;; The diagnostic position is properly adjusted after the change
      (should (equal (car (lsp-test-flycheck-diags))
                     (lsp-test-range-make (buffer-string)
                                          "   line 1 unique word broming + common"
                                          "                      ^^^^^^^         ")))
      ;; Server sent an update
      (lsp-test-command-send-diags
       snippet-file (lsp-test-org-code-block-contents) "broming")
      (lsp-test-wait-for-diagnostic-update)
      (flycheck-buffer)

      ;; Now the line number is still again
      ;; The columns are shifted because lsp-org does
      ;; not adjust for changed indentation.
      (should (equal (car (lsp-test-flycheck-diags))
                     (lsp-test-range-make (buffer-string)
                                          "   line 1 unique word broming + common"
                                          "                        ^^^^^^^       ")))

      (lsp-test-command-send-diags
       lsp-test-sample-file (lsp-test-org-code-block-contents) "nonexistent")
      (lsp-test-sync-wait 3 "Flycheck diags dissipate"
        (should (lsp-workspaces))
        (flycheck-buffer)
        (null (lsp-test-flycheck-diags))))))


;;; lsp-mock-server-test.el ends here
