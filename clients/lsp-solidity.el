;;; lsp-solidity.el --- solidity LSP                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords: tools

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

;;; Code:

(require 'lsp-mode)

(lsp-dependency 'solidity-language-server
                '(:system "nomicfoundation-solidity-language-server")
                '(:npm :package "@nomicfoundation/solidity-language-server"
                       :path "nomicfoundation-solidity-language-server"))


(defun lsp-client--solidity-ls-server-command ()
  "Startup command for Solidity language server."
  (list (lsp-package-path 'solidity-language-server) "--stdio"))


(defgroup lsp-solidity nil
  "LSP support for Solidity."
  :group 'lsp-mode
  :link '(url-link "https://github.com/NomicFoundation/hardhat-vscode/blob/development/server/README.md"))


(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-client--solidity-ls-server-command)
                  :activation-fn (lsp-activate-on "solidity" "sol")
                  :server-id 'solidity
                  :notification-handlers
                  (ht ("custom/validation-job-status"
                       #'lsp-client--solidity-validation-job-status))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'solidity-language-server callback error-callback))))

(defun lsp-client--solidity-validation-job-status (_workspace _params)
  ;; noop until I find out what to do with this
  )

(lsp-consistency-check lsp-solidity)

(provide 'lsp-solidity)
;;; lsp-solidity.el ends here
