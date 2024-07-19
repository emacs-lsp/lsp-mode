;;; mock-lsp-server.el --- Mock LSP server                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024 emacs-lsp maintainers

;; Author: Arseniy Zaostrovnykh
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1
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
;; The server reads commands from a file `mock-server-commands.el` in the
;; same directory as this file. The commands are elisp code that is loaded
;; and executed by the server.

;;; Code:

(setq debug-on-error t)

(defconst command-file
  (expand-file-name "mock-server-commands.el"
                    (file-name-directory load-file-name)))

(defun run-command-from-file-if-any ()
  (if (file-exists-p command-file)
      (progn
        (load command-file)
        (delete-file command-file))))

(defun json-rpc-string (body)
  ;; 1+ - extra new-line at the end
  (format "Content-Length: %d\r\nContent-Type: application/vscode-jsonrpc; charset=utf8\r\n\r\n%s\n" (1+ (string-bytes body)) body))

;; TODO mock:
;; - codeActionProvider
;; - codeLensProvider
;; - document(Range)FormattingProvider?
;; - documentHighlightProvider
;; - referencesProvider?
;; - foldingRangeProvider
(defun greeting (id)
  (json-rpc-string
   (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"serverInfo\":{\"name\":\"mockS\",\"version\":\"0.0.1\"}}}"
           id)))

(defun ack (id)
  (json-rpc-string (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":[]}" id)))

(defun shutdown-ack (id)
  (json-rpc-string (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":null}" id)))

(defun loc-to-json (loc)
  (format "{\"line\":%d,\"character\":%d}" (plist-get loc :line) (plist-get loc :character)))

(defun range-to-json (range)
  (format "{\"start\":%s,\"end\":%s}"
          (loc-to-json (plist-get range :start))
          (loc-to-json (plist-get range :end))))

(defun diagnostic-to-json (diagnostic)
        (format "{\"source\":\"%s\",\"code\":\"%s\",\"range\":%s,\"message\":\"%s\",\"severity\":%d}"
                (plist-get diagnostic :source)
                (plist-get diagnostic :code)
                (range-to-json (plist-get diagnostic :range))
                (plist-get diagnostic :message)
                (plist-get diagnostic :severity)))

(defun publish-diagnostics (diagnostics)
  (princ
   (json-rpc-string
    (format "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument\\/publishDiagnostics\",\"params\":{\"uri\":\"file:\\/\\/%s\",\"diagnostics\":[%s]}}"
            (plist-get diagnostics :path)
            (mapconcat #'diagnostic-to-json (plist-get diagnostics :diags) ",")))))

(defun get-id (input)
  (if (string-match "\"id\":\\([0-9]+\\)" input)
      (string-to-number (match-string 1 input))
    nil))

(defun get-file-path (input)
  (if (string-match "\"uri\":\"file:\\/\\/\\([^,]+\\)\"," input)
      (match-string 1 input)
    nil))

(defconst notification-methods '("\"method\":\"initialized\""
                                 "\"method\":\"textDocument/didOpen\""
                                 "\"method\":\"textDocument/didClose\""
                                 "\"method\":\"$/setTrace\""
                                 "\"method\":\"workspace/didChangeConfiguration\""))

(defun is-notification (input)
  (catch 'found
    (dolist (n notification-methods)
      (when (string-match-p n input)
        (throw 'found t)))
    nil))

(defun handle-lsp-client-input ()
  (let ((line (read-string "")))
    (cond
     ((string-match "method\":\"initialize\"" line)
      (princ (greeting (get-id line))))
     ((string-match "method\":\"exit" line)
      (kill-emacs 0))
     ((string-match "method\":\"shutdown" line)
      (princ (shutdown-ack (get-id line))))
     ((is-notification line)
      ;; No need to acknowledge a notification
      )
     ((get-id line)
      ;; It has an id, probably some request
      ;; Acknowledge that it is received
      (princ (ack (get-id line))))
     ((or (string-match "Content-Length" line)
          (string-match "Content-Type" line))
      ;; Ignore header
      )
     ((or (string-match "^$" line)
          (string-match "^$" line))
      ;; Ignore other empty lines
      )
     (t (error "unexpected input '%s'" line)))))

(while t
  (run-command-from-file-if-any)
  (handle-lsp-client-input))

;;; mock-lsp-server.el ends here
