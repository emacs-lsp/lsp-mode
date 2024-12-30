;;; lsp-lisp.el --- LSP client for Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP client for Lisp.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-lisp nil
  "LSP support for Lisp."
  :group 'lsp-mode
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-lisp-active-modes
  '( lisp-mode)
  "List of major mode that work with lisp."
  :type '(repeat function)
  :group 'lsp-lisp)

(defcustom lsp-lisp-alive-port 8006
  "Port to connect server to."
  :type 'integer
  :group 'lsp-lisp)

;;
;;; Server

;;;###autoload
(defun lsp-lisp-alive-start-ls ()
  "Start the alive-lsp."
  (interactive)
  (when-let* ((exe (executable-find "sbcl"))
             ((lsp--port-available "localhost" lsp-lisp-alive-port)))
    (lsp-async-start-process #'ignore #'ignore
                             exe
                             "--noinform"
                             "--eval"
                             "(ql:quickload \"alive-lsp\")"
                             "--eval"
                             (format "(alive/server::start :port %s)"
                                     lsp-lisp-alive-port))))

;;
;;; Core

(defun lsp-lisp-alive--tcp-connect-to-port ()
  "Define a TCP connection to language server."
  (list
   :connect
   (lambda (filter sentinel name _environment-fn _workspace)
     (let* ((host "localhost")
            (port lsp-lisp-alive-port)
            (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

       ;; TODO: Same :noquery issue (see above)
       (set-process-query-on-exit-flag tcp-proc nil)
       (set-process-filter tcp-proc filter)
       (set-process-sentinel tcp-proc sentinel)
       (cons tcp-proc tcp-proc)))
   :test? (lambda () t)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-lisp-alive--tcp-connect-to-port)
  :major-modes lsp-lisp-active-modes
  :priority -1
  :server-id 'alive-lsp))

(lsp-consistency-check lsp-lisp)

(provide 'lsp-lisp)
;;; lsp-lisp.el ends here
