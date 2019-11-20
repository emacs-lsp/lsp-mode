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
;; VHDL-tool from http://www.vhdltool.com/download
;; The server requires a project file list called vhdltool-config.yaml which must
;; be placed the the project root. An example file can be found here:
;; http://www.vhdltool.com/configuration
;;
;; HDL Checker from https://github.com/suoto/hdl_checker.
;; This server requires a config file described here
;; https://github.com/suoto/hdl_checker/wiki/setting-up-a-project
;;
;; Set the symbol lsp-vhdl-server to select the language server and set
;; lsp-vhdl-server-path if the binary is not in the user PATH.

;;; Code:

(require 'lsp-mode)

(defvar vhdl-tool-bin-name "vhdl-tool"
  "Name of the VHDL Tool binary.")

(defvar vhdl-tool-disp-name "lsp-vhdl-tool"
  "Display name for VHDL-tool.")

(defvar hdl-checker-bin-name "hdl_checker"
  "Name of HDL Checker binary.")

(defvar hdl-checker-disp-name "lsp-hdl-checker"
  "Display name for HDL Checker.")

(defgroup lsp-vhdl nil
  "LSP support for VHDL. Set lsp-vhdl-server to select server. The default is to use VHDL-tool."
  :group 'lsp-mode)

(defcustom lsp-vhdl-server 'vhdl-tool
  "Select which server to use:
VHDL-tool: A syntax checking, type checking and linting tool (http://vhdltool.com).
HDL Checker: A wrapper for third party tools such as GHDL, ModelSim, Vivado Simulator (https://github.com/suoto/hdl_checker)."
  :type '(choice (const :tag "VHDL-tool" vhdl-tool)
                 (const :tag "HDL Checker" hdl-checker))
  :group 'lsp-vhdl)

(defcustom lsp-vhdl-server-path nil
  "Path to binary server file."
  :group 'lsp-vhdl
  :risky t
  :type 'file)

(defvar lsp-vhdl--params nil)

(defun lsp-vhdl--create-connection ()
  "Returns lsp-stdio-connection based on the selected server"
  (lsp-vhdl--set-server-path)
  (lsp-vhdl--set-server-args)
  (plist-put
   (lsp-stdio-connection (lambda () (list (plist-get lsp-vhdl--params 'server-path) (plist-get lsp-vhdl--params 'server-args))))
   :test? (lambda () (f-executable? (plist-get lsp-vhdl--params 'server-path)))))

(defun lsp-vhdl--set-server-path()
  "Set path to server binary based on selection in lsp-vhdl-server."
  (cond ((eq lsp-vhdl-server 'hdl-checker) (if (eq lsp-vhdl-server-path nil)
                                               (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path hdl-checker-bin-name))
                                             (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))
        ((eq lsp-vhdl-server 'vhdl-tool) (if (eq lsp-vhdl-server-path nil)
                                             (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path vhdl-tool-bin-name))
                                           (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-path lsp-vhdl-server-path))))))

(defun lsp-vhdl--set-server-args()
  "Set server arguments based on server selection."
  (cond ((eq lsp-vhdl-server 'hdl-checker) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args "--lsp")))
        ((eq lsp-vhdl-server 'vhdl-tool) (setq lsp-vhdl--params (plist-put lsp-vhdl--params 'server-args "lsp")))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-vhdl--create-connection)
                  :major-modes '(vhdl-mode)
                  :language-id "VHDL"
		          :priority -1
                  :server-id 'lsp-vhdl))

(provide 'lsp-vhdl)
;;; lsp-vhdl.el ends here
