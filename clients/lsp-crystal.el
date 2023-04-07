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

(defgroup lsp-crystalline nil
  "LSP support for Crystal via crystalline."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elbywan/crystalline"))

(defcustom lsp-clients-crystal-executable '("crystalline" "--stdio")
  "Command to start the crystalline language server."
  :group 'lsp-crystalline
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-crystal-executable)
                  :major-modes '(crystal-mode)
                  :server-id 'crystalline))

(lsp-consistency-check lsp-crystal)

(provide 'lsp-crystal)
;;; lsp-crystal.el ends here
