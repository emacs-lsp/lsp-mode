;;; lsp-mojo.el --- lsp-mode Mojo integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Adam Liter
;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: Adam Liter <io@adamliter.org>
;; Keywords: languages,tools

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

;;  client for Mojo 🔥

;;; Code:

(require 'lsp-mode)

(defgroup lsp-mojo nil
  "LSP support for Mojo 🔥, using mojo-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/modularml/mojo"))

(defcustom lsp-mojo-executable "mojo-lsp-server"
  "The Mojo 🔥 LSP executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-mojo
  :type 'string)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-mojo-executable))
  :activation-fn (lsp-activate-on "mojo")
  :server-id 'mojo))

(lsp-consistency-check lsp-mojo)

(provide 'lsp-mojo)
;;; lsp-mojo.el ends here
