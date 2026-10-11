;;; lsp-epher.el --- lsp-mode Epher integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 lsp-mode maintainers

;; Author: lsp-mode maintainers
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

;;  Client for Epher, a calculator language with inline answers and
;;  unit conversion (https://github.com/upyesp/epher).  The language
;;  server, epher-lsp, ships with the epher releases.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-epher nil
  "Settings for the Epher Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/upyesp/epher")
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-epher-executable '("epher-lsp")
  "Command to run the Epher language server."
  :group 'lsp-epher
  :risky t
  :type '(repeat string))

(lsp-dependency 'epher-language-server
                '(:system "epher-lsp"))

(add-to-list 'lsp-language-id-configuration '(epher-mode . "epher"))

(lsp-register-client
 (make-lsp-client :server-id 'epher-lsp
                  :new-connection (lsp-stdio-connection (lambda () lsp-epher-executable))
                  :major-modes '(epher-mode)))

(lsp-consistency-check lsp-epher)

(provide 'lsp-epher)
;;; lsp-epher.el ends here
