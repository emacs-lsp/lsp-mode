;;; lsp-jsonnet.el --- lsp client for jsonnet -*- lexical-binding: t; -*-

;; Copyright (C) 2023 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, jsonnet

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
;;
;; LSP client for jsonnet language.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-jsonnet nil
  "LSP support for jsonnet."
  :group 'lsp-mode
  :link '(url-link "https://github.com/grafana/jsonnet-language-server"))

(defcustom lsp-clients-jsonnet-server-executable '("jsonnet-language-server")
  "The jsonnet language server executable to use."
  :group 'lsp-jsonnet
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-clients-jsonnet-server-executable))
  :activation-fn (lsp-activate-on "jsonnet")
  :priority -1
  :major-modes '(jsonnet-mode)
  :server-id 'jsonnet-lsp))

(lsp-consistency-check lsp-jsonnet)

(provide 'lsp-jsonnet)
;;; lsp-jsonnet.el ends here
