;;; lsp-roc.el --- lsp-mode roc integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 lsp-mode maintainers

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

;; Client for the Roc language server.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-roc nil
  "LSP support for Roc, using roc_language_server"
  :group 'lsp-mode
  :link '(url-link "https://github.com/roc-lang/roc/tree/main/crates/language_server")
  :package-version `(lsp-mode . "9.0.1"))

(lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_language_server")
                                      :activation-fn (lsp-activate-on "roc")
                                      :major-modes '(roc-ts-mode)
                                      :server-id 'roc_language_server))
(lsp-consistency-check lsp-roc)

(provide 'lsp-roc)
;;; lsp-futhark.el ends here
