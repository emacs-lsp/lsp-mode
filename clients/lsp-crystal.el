;;; lsp-crystal.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, crystal

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

;; LSP Clients for the Crystal Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-scry nil
  "LSP support for Crystal via scry."
  :group 'lsp-mode
  :link '(url-link "https://github.com/crystal-lang-tools/scry"))

(defcustom lsp-clients-crystal-executable '("scry" "--stdio")
  "Command to start the scry language server."
  :group 'lsp-scry
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-crystal-executable)
                  :major-modes '(crystal-mode)
                  :server-id 'scry))

(lsp-consistency-check lsp-crystal)

(provide 'lsp-crystal)
;;; lsp-crystal.el ends here
