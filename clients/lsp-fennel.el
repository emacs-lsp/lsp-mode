;;; lsp-fennel.el --- lsp-mode for the fennel-ls -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Merrick Luo

;; Author: Merrick Luo
;; Keywords: languages

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

;; LSP client for fennel-ls - an language server for fennel.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-fennel nil
  "LSP support for the fennel-ls language server."
  :group 'lsp-mode
  :link '(url-link "https://git.sr.ht/~xerool/fennel-ls"))

;; TODO: consider find in luarocks install location
(defun lsp-fennel--ls-command ()
  (executable-find "fennel-ls"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-fennel--ls-command)
  :activation-fn (lsp-activate-on "fennel")
  :priority -2
  :server-id 'fennel-ls))

(lsp-consistency-check lsp-fennel)

(provide 'lsp-fennel)
;;; lsp-fennel.el ends here
