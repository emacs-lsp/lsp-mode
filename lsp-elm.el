;;; lsp-elm.el --- Elm Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Daniel V

;; Author: Daniel V
;; Keywords: elm lsp

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

;; lsp-elm client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-elm nil
  "LSP support for the Elm programming language, using the server from https://github.com/elm-tooling/elm-language-server"
  :group 'lsp-mode
  :link '(url-link "https://github.com/elm-tooling/elm-language-server"))

(defcustom lsp-elm-server-install-dir
  (locate-user-emacs-file "elm-language-server")
  "Install directory for elm-language-server. This directory should contain a clone of elm-tooling/elm-language-server from github."
  :group 'lsp-elm
  :risky t
  :type 'directory)

(defun lsp-elm--elm-command ()
  "Generate LSP startup command for the Elm Language Server."
  `("node" ,(f-join (file-truename lsp-elm-server-install-dir)
                    "out/index.js")
    "--stdio"))

(defun lsp-clients-elm--make-init-options ()
  "Init options for elm-language-server."
  '(
    :runtime "node" ;; Runtime for tree-sitter module, can be either node or electron
    ))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-elm--elm-command)
                  :major-modes '(elm-mode)
                  :priority -1
                  :initialization-options #'lsp-clients-elm--make-init-options
                  :server-id 'elm-ls))

(provide 'lsp-elm)
;;; lsp-elm.el ends here
