;;; lsp-purescript.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, purescript

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

;; LSP Clients for the PureScript Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-purescript nil
  "LSP support for PureScript, using purescript-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nwolverson/purescript-language-server"))

(defcustom lsp-purescript-server-executable nil
  "Path to server executable."
  :type 'string
  :risky t
  :group 'lsp-purescript)

(defcustom lsp-purescript-server-args
  '("--stdio")
  "Arguments to pass to the server."
  :type '(repeat string)
  :risky t
  :group 'lsp-purescript)

(defun lsp-purescript--server-command ()
  "Generate LSP startup command for purescript-language-server."
  (cons (or lsp-purescript-server-executable
            (lsp-package-path 'purescript-language-server))
        lsp-purescript-server-args))

(lsp-dependency 'purescript-language-server
                '(:system "purescript-language-server")
                '(:npm :package "purescript-language-server"
                       :path "purescript-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-purescript--server-command)
  :major-modes '(purescript-mode)
  :priority -1
  :server-id 'pursls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'purescript-language-server callback error-callback))))


(provide 'lsp-purescript)
;;; lsp-purescript.el ends here
