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

;; LSP support for VHDL using using an external language server. Currently
;; the supported servers are:
;;
;; VHDL-tool. See http://www.vhdltool.com/configuration for setting up the
;; project file.
;;
;; HDL Checker. See https://github.com/suoto/hdl_checker/wiki/setting-up-a-project
;; for setting up the project file.
;;
;; VHDL LS. See https://github.com/kraigher/rust_hdl#configuration for setting
;; up the project file.
;;
;; GHDL LS. See https://github.com/ghdl/ghdl-language-server for setting up the
;; project file.
;;
;; Set the symbol lsp-vhdl-server to select the language server and set
;; lsp-vhdl-server-path if the binary is not in the user PATH.

;;; Code:

(require 'lsp-mode)

(defvar vhdl-tool-bin-name "vhdl-tool"
  "Name of the VHDL Tool binary.")

(defvar hdl-checker-bin-name "hdl_checker"
  "Name of HDL Checker binary.")

(defvar vhdl-ls-bin-name "vhdl_ls"
  "Name of the VHDL LS binary.")

(defvar ghdl-ls-bin-name "ghdl-ls"
  "Name of the GHDL LS binary.")

(defgroup lsp-vhdl nil
  "LSP support for VHDL. Set lsp-vhdl-server to select server. The default is to use VHDL-tool."
  :group 'lsp-mode)

(defcustom lsp-vhdl-server 'vhdl-tool
  "Select which server to use:
VHDL-tool: A syntax checking, type checking and linting tool
\(http://vhdltool.com).

HDL Checker: A wrapper for third party tools such as GHDL,
ModelSim, Vivado Simulator (https://github.com/suoto/hdl_checker).

VHDL LS: A complete VHDL language server protocol implementation
with diagnostics, navigate to symbol, find all references etc.
\(https://github.com/kraigher/rust_hdl)."
  :type '(choice (const :tag "VHDL-tool" vhdl-tool)
                 (const :tag "HDL Checker" hdl-checker)
                 (const :tag "VHDL LS" vhdl-ls)
                 (const :tag "GHDL LS" ghdl-ls))
  :group 'lsp-vhdl)

(defcustom lsp-vhdl-server-path nil
  "Path to binary server file."
  :group 'lsp-vhdl
  :risky t
  :type 'file)

(defvar lsp-vhdl--params nil)

(defun lsp-vhdl--create-connection ()
  "Return lsp-stdio-connection based on the selected server."
  (lsp-vhdl--set-server-path)
  (lsp-vhdl--set-server-args)
  (lsp-stdio-connection
    (lambda () (cons (plist-get lsp-vhdl--params 'server-path) (plist-get lsp-vhdl--params 'server-args)))
    (lambda () (executable-find (plist-get lsp-vhdl--params 'server-path)))))

(defun lsp-vhdl--set-server-path()
  "Set path to server binary based on selection in lsp-vhdl-server."
  (cond ((eq lsp-vhdl-server 'hdl-checker) (if (eq lsp-vhdl-server-path nil)
					       (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path hdl-checker-bin-name))
					     (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))
	((eq lsp-vhdl-server 'vhdl-tool) (if (eq lsp-vhdl-server-path nil)
					     (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path vhdl-tool-bin-name))
					   (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))
	((eq lsp-vhdl-server 'vhdl-ls) (if (eq lsp-vhdl-server-path nil)
					   (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path vhdl-ls-bin-name))
					 (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))
	((eq lsp-vhdl-server 'ghdl-ls) (if (eq lsp-vhdl-server-path nil)
					   (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path ghdl-ls-bin-name))
					 (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))))

(defun lsp-vhdl--set-server-args()
  "Set server arguments based on server selection."
  (cond ((eq lsp-vhdl-server 'hdl-checker) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args '("--lsp"))))
	((eq lsp-vhdl-server 'vhdl-tool) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args '("lsp"))))
	((eq lsp-vhdl-server 'vhdl-ls) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args '())))
	((eq lsp-vhdl-server 'ghdl-ls) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args '())))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-vhdl--create-connection)
                  :major-modes '(vhdl-mode)
                  :language-id "VHDL"
                  :priority -1
                  :server-id 'lsp-vhdl))

(lsp-consistency-check lsp-vhdl)

(provide 'lsp-vhdl)
;;; lsp-vhdl.el ends here
