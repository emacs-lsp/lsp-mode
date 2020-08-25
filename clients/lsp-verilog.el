;;; lsp-verilog.el --- Verilog Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Patrick Grogan

;; Author: Patrick Grogan <pogrogan@gmail.com>
;; Created: 7 December 2019
;; Keywords: languages, lsp, verilog

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
;; LSP client support for Verilog. Right now, the only supported LSP server
;; is HDL CHecker. See https://github.com/suoto/hdl_checker
;;
;; This file is based on the lsp-vhdl.el file.
;;
;; Set the lsp-verilog-server-path to the binary directory if it is not
;; in the User path;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-verilog nil
  "LSP support for Verilog/SystemVerilog."
  :group 'lsp-mode
  :link '(url-link "https://github.com/suoto/hdl_checker"))

(defcustom lsp-clients-verilog-executable '("hdl_checker" "--lsp")
  "Command to start the hdl_checker language server."
  :group 'lsp-verilog
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-verilog-executable)
                  :major-modes '(verilog-mode)
                  :language-id "verilog"
    	          :priority -1
                  :server-id 'lsp-verilog))

(provide 'lsp-verilog)
;;; lsp-verilog.el ends here
