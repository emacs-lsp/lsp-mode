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

(defcustom lsp-erlang-server-install-dir
  "."
  "Path to the Erlang Language Server installation dir."
  :group 'lsp-erlang
  :risky t
  :type 'directory)

(defun lsp-erlang-server-start-fun (port)
  `(,(concat lsp-erlang-server-install-dir "/_build/default/bin/erlang_ls")
    ,(number-to-string port)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection 'lsp-erlang-server-start-fun)
                  :major-modes '(erlang-mode)
                  :priority -1
                  :server-id 'erlang-ls))

(provide 'lsp-erlang)
;;; lsp-erlang.el ends here
