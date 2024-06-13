;;; lsp-gleam.el --- gleam client -*- lexical-binding: t; -*-

;; Copyright (C) 2022 emacs-lsp maintainers

;; Author: Jonathan Arnett
;; Keywords: lsp gleam

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

;; LSP client for the Gleam Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-gleam nil
  "LSP support for Gleam."
  :group 'lsp-mode
  :link '(url-link "https://gleam.run"))

(defcustom lsp-gleam-executable '("gleam" "lsp")
  "Command to run the Gleam LSP server."
  :group 'lsp-gleam
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-gleam-executable)
  :major-modes '(gleam-mode gleam-ts-mode)
  :priority -1
  :server-id 'gleam-lsp))

(provide 'lsp-gleam)
;;; lsp-gleam.el ends here
