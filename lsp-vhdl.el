;;; lsp-vhdl.el --- VHDL Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Christian Birk Sørensen

;; Author: Christian Birk Sørensen <chrbirks+emacs@gmail.com>
;; Created: 6 October 2019
;; Keywords: languages, lsp, vhdl

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

;; LSP support for VHDL using the language server from http://www.vhdltool.com/download.
;; Set the following symbol to specify the path of the language server:
;;
;; (setq lsp-vhdl-server-path "/path/to/server_binary")
;;
;; The server requires a project file list called vhdltool-config.yaml which must
;; be placed the the project root. An example file can be found here:
;; http://www.vhdltool.com/configuration

;;; Code:

(require 'lsp-mode)

(defgroup lsp-vhdl nil
  "LSP support for VHDL using the server from http://www.vhdltool.com"
  :group 'lsp-mode
  :link '(url-link "http://www.vhdltool.com"))

(defcustom lsp-vhdl-server-path
  "NOT_SET"
  "Path to binary server file downloaded from http://www.vhdltool.com/download"
  :group 'lsp-vhdl
  :risky t
  :type 'file)

(defun lsp-vhdl--create-connection ()
  "Returns lsp-stdio-connection or an error if server not found"
  (lsp-stdio-connection
   (lambda ()
     (list lsp-vhdl-server-path "lsp"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-vhdl--create-connection)
                  :major-modes '(vhdl-mode)
                  :language-id "VHDL"
		              :priority -1
                  :server-id 'lsp-vhdl-server)
 )

(provide 'lsp-vhdl)
;;; lsp-vhdl.el ends here
