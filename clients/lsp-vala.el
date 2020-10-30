;;; lsp-vala.el --- Vala Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daniel Svensson

;; Author: Daniel Svensson
;; Keywords: vala lsp

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

;; LSP client for Vala

;;; Code:

(require 'lsp-mode)

(defgroup lsp-vala nil
  "LSP support for Vala, using vala-language-server"
  :group 'lsp-mode
  :link '(url-link "https://github.com/benwaffle/vala-language-server")
  :package-version `(lsp-mode . "7.1.0"))

(defcustom lsp-clients-vala-ls-executable "vala-language-server"
  "Path to the `vala-language-server' binary."
  :group 'lsp-vala
  :risky t
  :type 'file
  :package-version `(lsp-mode . "7.1.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-vala-ls-executable))
                  :major-modes '(vala-mode)
                  :priority -1
                  :server-id 'valals))

(provide 'lsp-vala)
;;; lsp-vala.el ends here
