;;; lsp-sysml.el --- lsp client for SysML v2 and KerML -*- lexical-binding: t; -*-

;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, sysml, kerml

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
;; LSP client for SysML v2 and KerML using the OpenSysML language server
;; (sysml-lsp), see https://github.com/Open-MBEE/OpenSysML.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-sysml nil
  "LSP support for SysML v2 and KerML, using sysml-lsp from OpenSysML."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Open-MBEE/OpenSysML")
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-sysml-server-command '("sysml-lsp" "--stdio")
  "Command to start the OpenSysML language server."
  :group 'lsp-sysml
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "10.0.1"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-sysml-server-command))
  :activation-fn (lsp-activate-on "sysml" "kerml")
  :priority -1
  :major-modes '(sysml-mode kerml-mode)
  :server-id 'sysml-lsp))

(lsp-consistency-check lsp-sysml)

(provide 'lsp-sysml)
;;; lsp-sysml.el ends here
