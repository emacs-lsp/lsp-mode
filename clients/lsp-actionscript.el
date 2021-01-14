;;; lsp-actionscript.el --- ActionScript Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jen-Chieh Shen

;; Author: Jen-Chieh Shen
;; Keywords: actionscript lsp

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

;; LSP client for ActionScript

;;; Code:

(require 'lsp-mode)

(defgroup lsp-actionscript nil
  "LSP support for ActionScript."
  :group 'lsp-mode
  :link '(url-link "https://github.com/BowlerHatLLC/vscode-as3mxml")
  :package-version `(lsp-mode . "7.1.0"))

(defcustom lsp-clients-actionscript-language-server-command
  '("actionscript"
    "ActionScript & MXML Language Server")
  "The command that starts the actionscript language server."
  :group 'lsp-actionscript
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values" string))
  :package-version `(lsp-mode . "7.1.0"))

(defun lsp-actioscript--startup-command ()
  "ActionScript startup command."
  ;; TODO
  )

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-actioscript--startup-command)
  :major-modes '(actionscript-mode)
  :priority -1
  :server-id 'as-ls))

(provide 'lsp-actionscript)
;;; lsp-actionscript.el ends here
