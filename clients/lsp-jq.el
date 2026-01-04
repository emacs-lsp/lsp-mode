;;; lsp-jq.el --- lsp client for jq -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, jq

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
;; LSP client for jq language.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-jq nil
  "LSP support for Jq."
  :group 'lsp-mode
  :link '(url-link "https://github.com/wader/jq-lsp"))

(defcustom lsp-clients-jq-server-executable '("jq-lsp")
  "The jq language server executable to use."
  :group 'lsp-jq
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-clients-jq-server-executable))
  :activation-fn (lsp-activate-on "jq")
  :priority -1
  :major-modes '(jq-mode jq-ts-mode)
  :server-id 'jq-lsp))

(lsp-consistency-check lsp-jq)

(provide 'lsp-jq)
;;; lsp-jq.el ends here
