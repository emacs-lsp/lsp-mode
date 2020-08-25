;;; lsp-erlang.el --- Erlang Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Roberto Aloi

;; Author: Roberto Aloi
;; Keywords: erlang lsp

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

;; lsp-erlang client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-erlang nil
  "LSP support for the Erlang programming language, using erlang-ls"
  :group 'lsp-mode
  :link '(url-link "https://github.com/erlang-ls/erlang_ls"))

(defcustom lsp-erlang-server-path
  "erlang_ls"
  "Path to the Erlang Language Server binary."
  :group 'lsp-erlang
  :risky t
  :type 'file)

(defcustom lsp-erlang-server-connection-type
  'stdio
  "Type of connection to use with the Erlang Language Server: tcp or stdio"
  :group 'lsp-erlang
  :risky t
  :type 'symbol)

(defun lsp-erlang-server-start-fun (port)
  `(,lsp-erlang-server-path
    "--transport" "tcp"
    "--port" ,(number-to-string port)))

(defun lsp-erlang-server-connection ()
  (if (eq lsp-erlang-server-connection-type 'tcp)
      (lsp-tcp-connection 'lsp-erlang-server-start-fun)
    (lsp-stdio-connection `(,lsp-erlang-server-path "--transport" "stdio"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-erlang-server-connection)
                  :major-modes '(erlang-mode)
                  :priority -1
                  :server-id 'erlang-ls))

(provide 'lsp-erlang)
;;; lsp-erlang.el ends here
