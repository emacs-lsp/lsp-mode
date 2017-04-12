;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com> -*- lexical-binding: t -*-

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

(require 'json)
(require 'cl-lib)
(require 'lsp-common)
(require 'lsp-notifications)

(cl-defstruct lsp--parser
  (waiting-for-response nil)
  (response-result nil)
  (cur-token nil) ;; the current token being parsed
  (raw "") ;; raw data received from the server, for debugging purposes
  (headers '()) ;; alist of headers
  (body nil) ;; message body
  (reading-body nil) ;; If non-nil, reading body
  (last-terminate nil) ;; If non-nil, last token was '\r\n'
  (prev-char nil)

  (queued-notifications nil)

  (workspace nil) ;; the workspace
  (method-handlers nil :read-only t))

(defun lsp--get-message-type (params)
  "Get the message type from PARAMS."
  (when (not (string= (gethash "jsonrpc" params "") "2.0"))
    (error "JSON-RPC version is not 2.0"))
  (if (gethash "id" params nil)
    (if (gethash "error" params nil)
      'response-error
      'response)
    (if (gethash "method" params nil)
      'notification
      (error "Couldn't guess message type from params"))))

(defun lsp--flush-notifications (p)
  "Flush any notifications that were queued while processing the last response."
  (dolist (el (nreverse (lsp--parser-queued-notifications p)))
    (lsp--on-notification p el t))
  (setf (lsp--parser-queued-notifications p) nil))

(defun lsp--on-notification (p notification &optional dont-queue)
  "If response queue is empty, call the appropriate handler for NOTIFICATION.
Else it is queued (unless DONT-QUEUE is non-nil)"
  (let ((params (gethash "params" notification))
         handler)
    (if (and (not dont-queue) (lsp--parser-response-result p))
      (push (lsp--parser-queued-notifications p) notification)
      ;; else, call the appropriate handler
      (pcase (gethash "method" notification)
        ("window/showMessage" (lsp--window-show-message params))
        ("window/logMessage" (lsp--window-show-message params)) ;; Treat as showMessage for now
        ("textDocument/publishDiagnostics" (lsp--on-diagnostics params))
        ("textDocument/diagnosticsEnd")
        ("textDocument/diagnosticsBegin")
        (other
          (setq handler (gethash other (lsp--parser-method-handlers p) nil))
          (if (not handler)
            (message "Unkown method: %s" other)
            (funcall handler (lsp--parser-workspace p) params)))))))

(defconst lsp--errors
  '((-32700 "Parse Error")
     (-32600 "Invalid Request")
     (-32601 "Method not Found")
     (-32602 "Invalid Parameters")
     (-32603 "Internal Error")
     (-32099 "Server Start Error")
     (-32000 "Server End Error")))

(defun lsp--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
         (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
	    message
	    (or (car (alist-get code lsp--errors)) "Unknown error"))))

(defun lsp--parser-length-header (p)
  (string-to-number (cdr (assoc "Content-Length" (lsp--parser-headers p)))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
         key val)
    (unless pos
      (error "Invalid header string"))
    (setq key (substring s 0 pos)
      val (substring s (+ 2 pos)))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c being the elements of val
                   when (or (> c ?9) (< c ?0)) return nil
                   finally return t)
        nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun lsp--flush-header (p)
  (push (lsp--parse-header (substring (lsp--parser-cur-token p) 0
                             (1- (length (lsp--parser-cur-token p)))))
    (lsp--parser-headers p))
  (setf (lsp--parser-cur-token p) nil))

(defun lsp--cur-body-length (p)
  (string-bytes (lsp--parser-body p)))

(defun lsp--parser-reset (p)
  (setf (lsp--parser-cur-token p) nil
    (lsp--parser-raw p) ""
    (lsp--parser-headers p) '()
    (lsp--parser-body p) nil
    (lsp--parser-reading-body p) nil
    (lsp--parser-last-terminate p) nil
    (lsp--parser-prev-char p) nil))

(defun lsp--parser-on-message (p)
  "Called when the parser reads a complete message from the server."
  (let* ((json-array-type 'list)
          (json-object-type 'hash-table)
          (json-false nil)
          (json-data (json-read-from-string (lsp--parser-body p))))
    (pcase (lsp--get-message-type json-data)
      ('response (setf (lsp--parser-response-result p)
                   (and json-data (gethash "result" json-data nil))
                   (lsp--parser-waiting-for-response p) nil))
      ('response-error (setf (lsp--parser-response-result p) nil)
        (when json-data
          (message (lsp--error-string (gethash "error" json-data nil))))
        (setf (lsp--parser-response-result p) nil
          (lsp--parser-waiting-for-response p) nil))
      ('notification (lsp--on-notification p json-data))))
  (lsp--parser-reset p))

(defun lsp--parser-read (p output)
  (cl-assert (lsp--parser-workspace p) nil "Parser workspace cannot be nil.")
  (cl-loop for c being the elements of output do
    (if (eq c ?\n)
      (when (eq ?\r (lsp--parser-prev-char p))
        (unless (setf (lsp--parser-reading-body p)
                  (lsp--parser-last-terminate p))
          (lsp--flush-header p))
        (setf (lsp--parser-last-terminate p) t))
      (unless (eq c ?\r)
        (setf (lsp--parser-last-terminate p) nil))
      (if (lsp--parser-reading-body p)
        (progn (setf (lsp--parser-body p)
                 (concat (lsp--parser-body p) (list c)))
          (when (= (lsp--parser-length-header p)
                  (lsp--cur-body-length p))
            (when lsp-print-io
              (message "Output from language server: %s"
                (lsp--parser-body p)))
            (lsp--parser-on-message p)))
        (setf (lsp--parser-cur-token p)
          (concat (lsp--parser-cur-token p) (list c)))))
    (setf (lsp--parser-raw p)
      (concat (lsp--parser-raw p) (list c))
      (lsp--parser-prev-char p) c)))

(defun lsp--parser-make-filter (p ignore-regexps)
  #'(lambda (proc output)
      (when (cl-loop for r in ignore-regexps
              ;; check if the output is to be ignored or not
              ;; TODO: Would this ever result in false positives?
              when (string-match r output) return nil
              finally return t)
        (condition-case err
          (lsp--parser-read p output)
          (error
            (progn (lsp--parser-reset p)
              (setf (lsp--parser-response-result p) nil
                (lsp--parser-waiting-for-response p) nil)
              (error "Error parsing language server output: %s" err)))))
      (when (lsp--parser-waiting-for-response p)
        (with-local-quit (accept-process-output proc)))))

(provide 'lsp-receive)
;;; lsp-receive.el ends here
