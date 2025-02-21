;;; lsp-c3.el --- C3 Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bruno Dias

;; Author: Bruno Dias
;; Keywords: c3 lsp

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

;; lsp-c3 client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-c3 nil
  "LSP support for the C3 programming language, using the server from https://github.com/pherrymason/c3-lsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/pherrymason/c3-lsp"))

(defcustom lsp-c3-c3-language-server-path nil
  "Path for c3-language-server.
Prefer to build by hand for now."
  :group 'lsp-c3
  :risky t
  :type 'file)

(defcustom lsp-c3-c3-path ""
  "The path to your C3 executable."
  :type 'file
  :group 'lsp-c3)

(defcustom lsp-c3-c3-format-path ""
  "The path to your c3-format executable.

There is an effort to bring formatting/linting to c3."
  :type 'file
  :group 'lsp-c3)

(defcustom lsp-c3-server-args '()
  "Arguments to pass to the server."
  :type '(repeat string)
  :group 'lsp-c3)

(defun lsp-c3--c3-language-server-command ()
  "Generate LSP startup command for the C3 Language Server."
  (cons
   (or lsp-c3-c3-language-server-path
       (lsp-package-path 'c3-language-server))
   lsp-c3-server-args))

(defun lsp-clients-c3--make-init-options ()
  "Init options for C3-language-server."
  `(:c3Path ,lsp-c3-c3-path
    :c3FormatPath ,lsp-c3-c3-format-path))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-c3--c3-language-server-command)
  :language-id "c3-ts"
  :major-modes '(c3-ts-mode)
  :priority 0
  :initialization-options #'lsp-clients-c3--make-init-options
  :server-id 'c3lsp
  :download-server-fn (lambda (_client callback _error-callback _update?)
                        (message "To use this lsp, you need to build it
and use `lsp-c3-c3-language-server-path` to define where the executable is located.

Hope soon, we integrate with the lsp installation system.")
                        (funcall callback))))

(lsp-consistency-check lsp-c3)

(provide 'lsp-c3)
;;; lsp-c3.el ends here
