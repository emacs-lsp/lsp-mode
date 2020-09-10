;;; lsp-prolog.el --- Prolog Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2020  James Cash

;; Author: James Cash <james.nvc@gmail.com>
;; Keywords: languages,tools

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

;; lsp-prolog client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-prolog nil
  "LSP support for Prolog."
  :link '(url-link "https://github.com/jamesnvc/lsp_server")
  :group 'lsp-mode
  :tag "Lsp Prolog")

(defcustom lsp-prolog-server-command '("swipl"
                                       "-g" "use_module(library(lsp_server))."
                                       "-g" "lsp_server:main"
                                       "-t" "halt"
                                       "--" "stdio")
  "The prolog-lsp server command."
  :group 'lsp-prolog
  :risky t
  :type 'list)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-prolog-server-command))
  :major-modes '(prolog-mode)
  :multi-root t
  :server-id 'prolog-lsp))

(provide 'lsp-prolog)
;;; lsp-prolog.el ends here
