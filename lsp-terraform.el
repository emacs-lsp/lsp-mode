;;; lsp-terraform.el --- Terraform Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ross Donaldson

;; Author: Ross Donaldson
;; Keywords: terraform lsp

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

;; LSP client for Terraform

;;; Code:

(require 'lsp-mode)

(defgroup lsp-terraform nil
  "LSP support for Terraform, using terraform-lsp"
  :group 'lsp-mode
  :link '(url-link "https://github.com/juliosueiras/terraform-lsp"))

(defcustom lsp-terraform-server "terraform-lsp"
  "Path to the `terraform-lsp' binary."
  :group 'lsp-terraform
  :risky t
  :type 'file)

(defun lsp-terraform-server-start ()
  ())

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-terraform-server-start)
                  :major-modes '(terraform-mode)
                  :priority -1
                  :server-id 'tfls))

(provide 'lsp-terraform)
;;; lsp-terraform.el ends here
