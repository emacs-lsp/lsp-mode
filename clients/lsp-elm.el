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

(defcustom lsp-elm-elm-language-server-path nil
  "Path for elm-language-server.
Can be installed globally with: npm i -g @elm-tooling/elm-language-server,
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
  ""
  "The path to your elm executable.

Should be empty by default, in that case it will assume the name and try
to first get it from a local npm installation or a global one.  If you
set it manually it will not try to load from the npm folder."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-elm-format-path
  ""
  "The path to your elm-format executable.

Should be empty by default, in that case it will assume the name and try
to first get it from a local npm installation or a global one.  If you
set it manually it will not try to load from the npm folder."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-elm-test-path
  ""
  "The path to your elm-test executable.

Should be empty by default, in that case it will assume the name and try
to first get it from a local npm installation or a global one.  If you
set it manually it will not try to load from the npm folder."
  :type 'file
  :group 'lsp-elm)

(defcustom lsp-elm-disable-elmls-diagnostics
  nil
  "Enable/Disable linting diagnostics from the language server."
  :type 'boolean
  :group 'lsp-elm)

(defcustom lsp-elm-only-update-diagnostics-on-save
  nil
  "Only update compiler diagnostics on save, not on document change."
  :type 'boolean
  :group 'lsp-elm)

(defcustom lsp-elm-skip-install-package-confirmation
  nil
  "Skip confirmation for the Install Package code action."
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
   (or lsp-elm-elm-language-server-path
       (lsp-package-path 'elm-language-server))
   lsp-elm-server-args))

(defun lsp-clients-elm--make-init-options ()
  "Init options for elm-language-server."
  `(:elmPath ,lsp-elm-elm-path
    :elmFormatPath ,lsp-elm-elm-format-path
    :elmTestPath ,lsp-elm-elm-test-path
    :disableElmLSDiagnostics ,(lsp-json-bool lsp-elm-disable-elmls-diagnostics)
    :onlyUpdateDiagnosticsOnSave ,(lsp-json-bool lsp-elm-only-update-diagnostics-on-save)
    :skipInstallPackageConfirmation ,(lsp-json-bool lsp-elm-skip-install-package-confirmation)
    :trace.server ,(lsp-json-bool lsp-elm-trace-server)))

(lsp-dependency 'elm-language-server
                '(:system "elm-language-server")
                '(:npm :package "@elm-tooling/elm-language-server"
                       :path "elm-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-elm--elm-language-server-command)
  :major-modes '(elm-mode)
  :priority -1
  :initialization-options #'lsp-clients-elm--make-init-options
  :server-id 'elm-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'elm-language-server callback error-callback))))

(lsp-consistency-check lsp-elm)

(provide 'lsp-elm)
;;; lsp-elm.el ends here
