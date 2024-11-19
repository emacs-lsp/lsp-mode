;;; mock-lsp-server.el --- Mock LSP server                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024 emacs-lsp maintainers

;; Author: Arseniy Zaostrovnykh
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0
;; License: GPL-3.0-or-later

;; URL: https://github.com/emacs-lsp/lsp-mode
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A mock implementation of a Language Server Protocol server for testing
;; of the LSP client in lsp-mode.
;;
;; The server reads commands from a file `mock-server-commands.el` in the same
;; directory as this file. The commands are elisp code that is loaded and
;; executed by the server. It deletes the file after executing the command
;; indicating readiness for the next one.
;;
;; Due to `emacs --script` limitations, the server cannot watch two inputs
;; concurrently:
;; - stdin for the client input
;; - the mock-server-commands.el file for the commands
;;
;; Therefore, the server alternates between the two. It waits for the next
;; client input, and only after processing one can take and execute a command.
;; As a consequence, you should make sure to send a notification from the client
;; once you have created the command file with a command.

;;; Code:

(require 'json)

;; To ease debugging, print the stack trace on failure
(setq debug-on-error t)

(defconst command-file
  (expand-file-name "mock-server-commands.el"
                    (file-name-directory load-file-name))
  "Path to the file where the server expects external commands.")

(defun run-command-from-file-if-any ()
  "If there is the `command-file', execute and delete it."
  (if (file-exists-p command-file)
      (progn
        (load command-file)
        (delete-file command-file))))

(defun json-rpc-string (body)
  "Format BODY p-list as a JSON RPC message suitable for LSP."
  ;; 1+ - extra new-line at the end
  (let* ((encoded-body (json-encode `(:jsonrpc "2.0" ,@body)))
         (content-length-header
         (format "Content-Length: %d" (1+ (string-bytes encoded-body))))
        (content-type-header
         "Content-Type: application/vscode-jsonrpc; charset=utf8"))
    (concat content-length-header "\r\n"
            content-type-header "\r\n\r\n"
            encoded-body "\n")))

(defconst server-info
  '(:name "mockS" :version "0.1.0")
  "Basic server information: name and version.")


(defconst server-capabilities '(:referencesProvider t
                                :foldingRangeProvider t
                                :documentHighlightProvider t
                                :documentFormattingProvider t
                                :codeActionProvider t
                                :declarationProvider t
                                :definitionProvider t
                                :inlayHintProvider t
                                :codeLensProvider (:resolveProvider ()))
  "Capabilities of the server.")

(defun greeting (id)
  "Compose the greeting message in response to `initialize' request with id ID."
  (json-rpc-string `(:id ,id :result (:capabilities ,server-capabilities
                                      :serverInfo ,server-info))))

(defun respond (id result)
  "Acknowledge a request with id ID."
  (json-rpc-string `(:id ,id :result ,result)))

(defun publish-diagnostics (diagnostics)
  "Send JSON RPC message textDocument/PublishDiagnostics with DAGNOSTICS.

DIAGNOSICS must be a p-list (:path PATH :diags DIAGS),
where DIAGS is a list of p-lists in the form
(:source .. :code .. :range .. :message .. :severity ..)."
  (princ
   (json-rpc-string `(:method "textDocument/publishDiagnostics"
                      :params ,diagnostics))))

(defun get-id (input)
  "Extract request id from INPUT JSON message."
  (when (string-match "\"id\":\\([0-9]+\\)" input)
    (string-to-number (match-string 1 input))))

(defconst notification-methods
  '("\"method\":\"initialized\""
    "\"method\":\"textDocument/didOpen\""
    "\"method\":\"textDocument/didClose\""
    "\"method\":\"$/setTrace\""
    "\"method\":\"workspace/didChangeConfiguration\"")
  "Expected notification methods that require no acknowledgement.")

(defun is-notification (input)
  "Check if INPUT is a notification message.

See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage"
  (catch 'found
    (dolist (n notification-methods)
      (when (string-match-p n input)
        (throw 'found t)))
    nil))

(defvar scheduled-responses (make-hash-table :test 'equal)
  "Keep the planned response for the requiest of the given method.

Can contain only one planned response per method.
Key is the method, and value is the `result' field in the response.")

(defun schedule-response (method result)
  "Next time request of METHOD comes respond with `result' RESULT.

This function is useful for external commands,
allowing control over the server responses.

You can schedule only one response for a method for the entire session."
  (when (gethash method scheduled-responses)
    (error "Response for method %s is already scheduled" method))
  (puthash method result scheduled-responses))

(defun get-method (input)
  "Retrieve the method of the request in INPUT.

Returns nil if no method is found."
  (when (string-match "\"method\":\"\\([^\"]+\\)\"" input)
    (match-string 1 input)))

(defun get-response-for-request (method)
  "Find the scheduled response for METHOD request.

Returns empty array if not found:
 empty array is the usual representation of empty result.

The response is not removed to cover for potential plural requests."
  (if-let* ((response (gethash method scheduled-responses)))
      response
    []))

(defun handle-lsp-client-input ()
  "Read and handle one line of te input from the LSP client."
  (let ((line (read-string "")))
    (cond
     ((string-match "method\":\"initialize\"" line)
      (princ (greeting (get-id line))))
     ((string-match "method\":\"exit" line)
      (kill-emacs 0))
     ((string-match "method\":\"shutdown" line)
      (princ (respond (get-id line) nil)))
     ((is-notification line)
      ;; No need to acknowledge a notification
      )
     ((get-id line)
      ;; It has an id, probably some request
      ;; Acknowledge that it is received
      (princ (respond
              (get-id line)
              (get-response-for-request (get-method line)))))
     ((or (string-match "Content-Length" line)
          (string-match "Content-Type" line))
      ;; Ignore header
      )
     ((or (string-match "^\r$" line)
          (string-match "^$" line))
      ;; Ignore empty lines and header/content separators
      )
     (t (error "unexpected input '%s'" line)))))

;; Keep alternating from executing a command to handling client input.
;; If emacs --script had concurrency support,
;; it would have been executed concurrently.
(while t
  (run-command-from-file-if-any)
  (handle-lsp-client-input))

;;; mock-lsp-server.el ends here
