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

(defun lsp-vhdl--create-connection ()
  "Returns lsp-stdio-connection based on the selected server"
  (cond ((eq lsp-vhdl-server 'hdl-checker) (lsp-vhdl-hdl-checker-server))
        ((eq lsp-vhdl-server 'vhdl-tool) (lsp-vhdl-vhdl-tool-server))
        (t (user-error "Server selection \"%s\" not supported" lsp-vhdl-server))))

(defun lsp-vhdl-vhdl-tool-server()
  "Set up connection for VHDL-tool server."
  ; If server path is not explicitly set assume it is in PATH
  (cond ((eq lsp-vhdl-server-path nil) (setq server-path vhdl-tool-bin-name))
        (t (setq server-path lsp-vhdl-server-path)))
  ; Check that binary is executable
  (if (eq (file-executable-p server-path) nil)
      (user-error "Server \"%s\" does not exist or is not executable" server-path)
    (lsp-stdio-connection
     (lambda ()
       (list server-path "lsp")))))

(defun lsp-vhdl-hdl-checker-server ()
  "Set up connection for HDL Checker server."
  ;; If server path is not explicitly set assume it is in PATH
  (cond ((eq lsp-vhdl-server-path nil) (setq server-path hdl-checker-bin-name))
        (t (setq server-path lsp-vhdl-server-path)))
  (lsp-stdio-connection
   (lambda ()
     (list server-path "--lsp"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-vhdl--create-connection)
                  :major-modes '(vhdl-mode)
                  :language-id "VHDL"
		              :priority -1
                  :server-id 'lsp-vhdl))

(provide 'lsp-vhdl)
;;; lsp-vhdl.el ends here
