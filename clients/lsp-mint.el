;;; lsp-mint.el --- Mint Language Server configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;; Code:

(require 'lsp-mode)

(defgroup lsp-mint nil
  "LSP support for mint-lang."
  :group 'lsp-mode
  :link '(url-link "https://github.com/mint-lang/mint")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-clients-mint-executable '("mint" "ls")
  "Command to start the mint language server."
  :group 'lsp-mint
  :risky t
  :type 'file)

;; Mint
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-mint-executable)
                  :major-modes '(mint-mode)
                  :server-id 'mint-ls))

(provide 'lsp-mint)
;;; lsp-mint.el ends here
