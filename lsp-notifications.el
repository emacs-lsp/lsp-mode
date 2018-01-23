;; Copyright (C) 2016-2017  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

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

;;; Code:

(require 'lsp-common)
(require 'cl-lib)
(require 'subr-x)

(defcustom lsp-inhibit-message nil
  "If non-nil, inhibit the message echo via `inhibit-message'."
  :type 'boolean
  :group 'lsp-mode)

(defun lsp--window-show-message (params)
  "Send the server's messages to message, inhibit if `lsp-inhibit-message' is set."
  (let* ((inhibit-message (or inhibit-message lsp-inhibit-message)))
    (message "%s" (lsp--propertize (gethash "message" params)
                    (gethash "type" params)))))

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received from the language
server and put in `lsp--diagnostics'."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--diagnostics (make-hash-table :test 'equal)
  "Hash table storing the diagnostics per file.")

(cl-defstruct lsp-diagnostic
  (range nil :read-only t)
  ;; range has the form (:start (:line N :column N) :end (:line N :column N) )
  ;; where N are zero-indexed numbers
  (line nil :read-only t)
  (column nil :read-only t)
  (severity nil :read-only t) ;; 1 - error, 2 - warning, 3 - information, 4 - hint
  (code nil :read-only t) ;; the diagnostic's code
  (source nil :read-only t) ;;
  (message nil :read-only t) ;; diagnostic's message
  (original nil :read-only t) ;; original diagnostic from LSP, kept for when it
  ;; needs to be sent back in e.g. codeAction
  ;; context.
  )

(defun lsp--make-diag (diag)
  "Make a `lsp-diagnostic' from DIAG."
  (let* ((range (gethash "range" diag))
         (start (gethash "start" range))
         (end (gethash "end" range))
         (message (gethash "message" diag))
         (source (gethash "source" diag)))
    (make-lsp-diagnostic
     :range (list :start (list :line   (gethash "line" start)
                               :column (gethash "character" start))
                  :end   (list :line   (gethash "line" end)
                               :column (gethash "character" end)))
     :line (gethash "line" start)
     :column (gethash "character" start)
     :severity (gethash "severity" diag)
     :code (gethash "code" diag)
     :source (gethash "source" diag)
     :message (if source (format "%s: %s" source message) message)
     :original diag)))

(defun lsp--equal-files (f1 f2)
  (and f1 f2
    (string-equal (expand-file-name f1) (expand-file-name f2))))

(defun lsp--on-diagnostics (params workspace)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}"
  (let ((file (lsp--uri-to-path (gethash "uri" params)))
        (diagnostics (gethash "diagnostics" params)) buffer)
    (puthash file (mapcar #'lsp--make-diag diagnostics) lsp--diagnostics)
    (setq buffer (cl-loop for buffer in (lsp--workspace-buffers workspace)
                          when (lsp--equal-files (buffer-file-name buffer) file)
                          return buffer
                          finally return nil))
    (when buffer
      (with-current-buffer buffer
        (run-hooks 'lsp-after-diagnostics-hook)))))

(declare-function lsp--workspace-buffers "lsp-methods" (workspace))

(provide 'lsp-notifications)
;;; lsp-notifications.el ends here
