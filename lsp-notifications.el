;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>

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

(require 'lsp-common)

(require 'cl-lib)

(defun lsp--window-show-message (params)
  (message "%s" (lsp--propertize (gethash "message" params)
				 (gethash "type" params))))

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received from the language
server and put in `lsp--diagnostics'."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--diagnostics (make-hash-table :test 'equal)
  "Hash table storing the diagnostics per file.")

(cl-defstruct lsp-diagnostic
  (range nil :read-only t) ;; of the form (number . number), both are points
  (line nil :read-only t)
  (column nil :read-only t)
  (severity nil :read-only t) ;; 1 - error, 2 - warning, 3 - information, 4 - hint
  (code nil :read-only t) ;; the diagnostic's code
  (source nil :read-only t) ;;
  (message nil :read-only t) ;; diagnostic's message
  )

(defun lsp--make-diag (diag)
  "Make a `lsp-diagnostic' from DIAG."
  (let* ((range (gethash "range" diag))
          (start (gethash "start" range))
          (message (gethash "message" diag))
          (source (gethash "source" diag)))
    (make-lsp-diagnostic
     :range `(,(lsp--position-to-point start)
              ,(lsp--position-to-point (gethash "end" range)))
     :line (gethash "line" start)
     :column (gethash "character" start)
     :severity (gethash "severity" diag)
     :code (gethash "code" diag)
     :source (gethash "source" diag)
     :message (if source (format "%s: %s" source message) message))))

(defun lsp--on-diagnostics (params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}"
  (let ((file (string-remove-prefix "file://" (gethash "uri" params)))
        (diagnostics (gethash "diagnostics" params)))
    (puthash file (mapcar #'lsp--make-diag diagnostics) lsp--diagnostics)
    (run-hooks 'lsp-after-diagnostics-hook)))

(provide 'lsp-notifications)
