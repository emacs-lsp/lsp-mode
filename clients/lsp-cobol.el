;;; lsp-cobol.el --- COBOL support  -*- lexical-binding: t; -*-

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
;; COBOL support.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-cobol nil
  "LSP support for COBOL."
  :group 'lsp-mode
  :link '(url-link "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol")
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-cobol-port 1044
  "Port to connect server to."
  :type 'integer
  :group 'lsp-cobol)

(defun lsp-cobol--tcp-connect-to-port ()
  "Define a TCP connection to language server."
  (list
   :connect (lambda (filter sentinel name _environment-fn _workspace)
              (let* ((host "localhost")
                     (port lsp-cobol-port)
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (set-process-sentinel tcp-proc sentinel)
                (cons tcp-proc tcp-proc)))
   :test? (lambda () t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-cobol--tcp-connect-to-port)
                  :activation-fn (lsp-activate-on "cobol")
                  :server-id 'cobol))

(lsp-consistency-check lsp-cobol)

(provide 'lsp-cobol)
;;; lsp-cobol.el ends here
