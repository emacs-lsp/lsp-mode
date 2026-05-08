;;; lsp-toml-tombi.el --- lsp-mode TOML integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Sibi Prabakaran

;; Author: Sibi Prabakaran <sibi@psibi.in>
;; Keywords: lsp, toml

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

;; Client for tombi

;;; Code:

(require 'lsp-mode)

(defgroup lsp-tombi-toml nil
  "LSP support for TOML, using Tombi."
  :group 'lsp-mode
  :link '(url-link "https://github.com/tombi-toml/tombi"))

(defcustom lsp-tombi-toml-command "tombi"
  "Path to tombi command."
  :type 'string
  :group 'lsp-tombi-toml
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-tombi-toml--check-enabled (_file-name _mode)
  "Check if the tombi language server should be enabled in this buffer."
  (when (string= (lsp-buffer-language) "toml")
    t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (list lsp-tombi-toml-command "lsp")))
  :activation-fn #'lsp-tombi-toml--check-enabled
  :multi-root t
  :server-id 'tombi
  :priority -2))

(lsp-consistency-check lsp-tombi-toml)

(provide 'lsp-toml-tombi)
;;; lsp-toml-tombi.el ends here
