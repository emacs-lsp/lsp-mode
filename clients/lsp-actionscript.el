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
  "LSP support for Vala, using vala-language-server"
  :group 'lsp-mode
  :link '(url-link "https://github.com/BowlerHatLLC/vscode-as3mxml")
  :package-version `(lsp-mode . "7.1.0"))

(defcustom lsp-clients-actionscript-ls-executable "actionscript-language-server"
  "Path to the `actionscript-language-server' binary."
  :group 'lsp-actionscript
  :risky t
  :type 'file
  :package-version `(lsp-mode . "7.1.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-actionscript-ls-executable))
                  :major-modes '(actionscript-mode)
                  :priority -1
                  :server-id 'as-ls))

(provide 'lsp-actionscript)
;;; lsp-actionscript.el ends here
