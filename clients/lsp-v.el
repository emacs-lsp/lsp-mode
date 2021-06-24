;;; lsp-v.el --- lsp-mode V integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 remimimimi

;; Author: remimimimi
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

;;  client for vls, the V language server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-v nil
  "LSP support for V via vls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vlang/vls/tree/master"))

(defcustom lsp-v-vls-executable "vls"
  "The vls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-v
  :type 'string)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-v-vls-executable))
  :activation-fn (lsp-activate-on "V")
  :server-id 'v-ls))

(lsp-consistency-check lsp-v)

(provide 'lsp-v)
;;; lsp-v.el ends here
