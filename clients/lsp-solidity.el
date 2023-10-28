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

(defgroup lsp-solidity nil
  "LSP support for Solidity."
  :group 'lsp-mode
  :link '(url-link "https://github.com/NomicFoundation/hardhat-vscode/blob/development/server/README.md"))

(defcustom lsp-clients-solidity-server-command "npx @nomicfoundation/solidity-language-server --stdio"
  "Command to run the server.

You can also install nomicfoundation-solidity-language-server
globally with

npm install @nomicfoundation/solidity-language-server -g

and provide the command npx nomicfoundation-solidity-language-server --stdio

Or use

solc --lsp

npx solidity-ls --stdio
"
  :group 'lsp-solidity
  :type 'string)


(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (string-split lsp-clients-solidity-server-command))
                  :activation-fn (lsp-activate-on "solidity" "sol")
                  :server-id 'solidity
                  :notification-handlers
                  (ht ("custom/validation-job-status"
                       #'lsp-client--solidity-validation-job-status))
                  ))

(defun lsp-client--solidity-validation-job-status (_workspace params)
  ;; noop until I find out what to do with this
  )

(provide 'lsp-solidity)
;;; lsp-solidity.el ends here
