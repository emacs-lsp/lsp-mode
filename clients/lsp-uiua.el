;;; lsp-uiua.el --- lsp-mode Uiua integration        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Tomas Fabrizio Orsi

;; Author: Tomas Fabrizio Orsi <torsi@fi.uba.ar>
;; Keywords: lsp, uiua

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

;; Client for uiua using the built-in language server.

;;; Code:

(defgroup lsp-tex nil
  "LSP support for the Uiua stack-based, array-oriented programming language."
  :group 'lsp-mode
  :link '(url-link "https://www.uiua.org/#language-server"))

(defcustom lsp-uiua-executable '("uiua" "lsp")
  "Command to start the uiua language server."
  :group 'lsp-uiua
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-uiua-executable)
  :activation-fn (lsp-activate-on "uiua")
  :server-id 'uiua))

(lsp-consistency-check lsp-uiua)

(provide 'lsp-uiua)
;;; lsp-uiua.el ends here
