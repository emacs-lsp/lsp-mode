;;; lsp-python-ty.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, python

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

;; LSP Clients for the Python(ty) Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-python-ty nil
  "LSP support for Python(ty)."
  :group 'lsp-mode
  :link '(url-link "https://github.com/astral-sh/ty"))

(defcustom lsp-python-ty-clients-server-command '("ty" "server")
  "Command to start the python-ty language server."
  :group 'lsp-python-ty
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-python-ty-clients-server-command))
                  :activation-fn (lsp-activate-on "python")
                  :priority -1
                  :add-on? t
                  :server-id 'ty-ls
                  :initialized-fn (lambda (workspace)
                                    (let ((caps (lsp--workspace-server-capabilities workspace)))
                                      (unless (lsp-get caps :inlayHintProvider)
                                        (lsp:set-server-capabilities-inlay-hint-provider? caps t))))))

(lsp-consistency-check lsp-python-ty)

(provide 'lsp-python-ty)
;;; lsp-python-ty.el ends here
