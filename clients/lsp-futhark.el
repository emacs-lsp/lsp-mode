;;; lsp-futhark.el --- lsp-mode futhark integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 lsp-mode maintainers

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

;; Client for the futhark language server.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-futhark nil
  "LSP support for Futhark, using futhark lsp"
  :group 'lsp-mode
  :link '(url-link "https://github.com/diku-dk/futhark/tree/master/src/Futhark/LSP")
  :package-version `(lsp-mode . "9.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("futhark" "lsp"))
                  :activation-fn (lsp-activate-on "futhark")
                  :server-id 'futhark))

(lsp-consistency-check lsp-futhark)

(provide 'lsp-futhark)
;;; lsp-futhark.el ends here
