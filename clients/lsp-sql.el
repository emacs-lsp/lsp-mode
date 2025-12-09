;;; lsp-sql.el --- SQL Client settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP client for SQL.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-sql nil
  "LSP support for SQL, using sql-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/joe-re/sql-language-server")
  :package-version `(lsp-mode . "9.0.1"))

(defcustom lsp-sql-server-path nil
  "Path points for SQL language server.

This is only for development use."
  :type 'string
  :group 'lsp-sql)

(defun lsp-sql--server-command ()
  "Generate startup command for SQL language server."
  (list (or lsp-sql-server-path
            (lsp-package-path 'sql-ls))
        "up" "--method" "stdio"))

(lsp-dependency 'sql-ls
                '(:system "sql-ls")
                '(:npm :package "sql-language-server"
                       :path "sql-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-sql--server-command)
  :major-modes '(sql-mode)
  :priority -1
  :multi-root t
  :server-id 'sql-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'sql-ls callback error-callback))))

(lsp-consistency-check lsp-sql)

(provide 'lsp-sql)
;;; lsp-sql.el ends here
