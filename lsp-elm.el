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

(defcustom lsp-elm-elm-language-server-path
  "elm-language-server"
  "Path for elm-language-server.
Can be installed globally with npm -i -g @elm-tooling/elm-language-server,
or manually by cloning the repo and following the installing instructions."
  :group 'lsp-elm
  :risky t
  :type 'file)

(defcustom lsp-elm-trace-server
  nil
  "Enable/disable trace logging of client and server communication."
  :type 'boolean
  :group 'lsp-elm)

(defcustom lsp-elm-elm-path
  "elm"
  "Path to your elm executable."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-elm-format-path
  "elm-format"
  "Path to your elm executable."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-elm-test-path
  "elm-test"
  "Path to your elm executable."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-diagnostics-on-save-only
  nil
  "Determines whether or not diagnostic updates are triggered only on save."
  :type 'boolean
  :group 'lsp-elm)

(defcustom lsp-elm-server-args
  '("--stdio")
  "Arguments to pass to the server."
  :type '(repeat string)
  :group 'lsp-elm)

(defun lsp-elm--elm-language-server-command ()
  "Generate LSP startup command for the Elm Language Server."
  (cons
   lsp-elm-elm-language-server-path
   lsp-elm-server-args))

(defun lsp-clients-elm--make-init-options ()
  "Init options for elm-language-server."
  `(
    :elmPath ,lsp-elm-elm-path
    :elmFormatPath ,lsp-elm-elm-format-path
    :elmTestPath ,lsp-elm-elm-test-path
    :diagnosticsOnSaveOnly ,(lsp-json-bool lsp-elm-diagnostics-on-save-only)
    :trace.server ,(lsp-json-bool lsp-elm-trace-server)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-elm--elm-language-server-command)
                  :major-modes '(elm-mode)
                  :priority -1
                  :initialization-options #'lsp-clients-elm--make-init-options
                  :server-id 'elm-ls))

(provide 'lsp-elm)
;;; lsp-elm.el ends here
