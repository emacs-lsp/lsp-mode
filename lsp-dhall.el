;;; lsp-dhall.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, dhall

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

;; LSP Clients for the Dhall Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-dhall nil
  "LSP support for Dhall, using dhall-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/dhall-lang/dhall-haskell"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "dhall-lsp-server")
                  :major-modes '(dhall-mode)
                  :priority -1
                  :server-id 'dhallls))

(provide 'lsp-dhall)
;;; lsp-dhall.el ends here
