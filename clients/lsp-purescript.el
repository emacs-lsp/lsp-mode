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

(defcustom-lsp lsp-purescript-add-spago-sources t
  "Whether to add spago sources to the globs.
Passed to the IDE server for source locations."
  :type 'boolean
  :group 'lsp-purescript
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "purescript.addSpagoSources")

(defcustom-lsp lsp-purescript-add-npm-path nil
  "Whether to add the local npm bin directory to the PATH."
  :type 'boolean
  :group 'lsp-purescript
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "purescript.addNpmPath")

(defcustom-lsp lsp-purescript-formatter "purty"
  "Tool to use to for formatting.
Must be installed and on PATH (or npm installed with addNpmPath set)"
  :type '(choice (:tag none purty purs-tidy pose))
  :group 'lsp-purescript
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "purescript.formatter")

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


(lsp-consistency-check lsp-purescript)

(provide 'lsp-purescript)
;;; lsp-purescript.el ends here
