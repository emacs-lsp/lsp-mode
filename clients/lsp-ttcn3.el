;;; lsp-ttcn3.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, ttcn3

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

;; LSP Clients for the TTCN3 Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ttcn3 nil
  "LSP support for TTCN3, using ntt-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nokia/ntt"))

(defcustom lsp-ttcn3-lsp-server-command
  '("ntt" "langserver")
  "Command to start ttcn3-language-server."
  :group 'lsp-ttcn3
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-ttcn3-lsp-server-command)
                  :activation-fn (lsp-activate-on "ttcn3")
                  :priority -1
                  :server-id 'ntt))

(provide 'lsp-ttcn3)
;;; lsp-ttcn3.el ends here

