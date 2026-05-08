;;; lsp-verilog.el --- Verilog Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Patrick Grogan
;; Copyright (C) 2019-2026 lsp-mode maintainers

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
;; LSP client support for Verilog/SystemVerilog. The following language
;; servers are available:
;;   1) HDL Checker. See https://github.com/suoto/hdl_checker
;;   2) SVLangserver. See https://github.com/imc-trading/svlangserver
;;   3) Verible. See https://github.com/chipsalliance/verible
;;
;; This file is based on the lsp-vhdl.el file.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-svlangserver nil
  "Settings for the SystemVerilog language server client."
  :group 'lsp-mode
  :link '(url-link "https://github.com/imc-trading/svlangserver")
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-svlangserver-includeIndexing '["**/*.{sv,svh}"]
  "Files included for indexing (glob pattern)"
  :group 'lsp-svlangserver
  :type '(lsp-repeatable-vector string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-excludeIndexing '["test/**/*.{sv,svh}"]
  "Files excluded for indexing (glob pattern)"
  :group 'lsp-svlangserver
  :type '(lsp-repeatable-vector string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-defines nil
  "Defines needed for linting"
  :group 'lsp-svlangserver
  :type '(lsp-repeatable-vector string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall"
  "Verilator command used for linting"
  :group 'lsp-svlangserver
  :type 'string
  :safe (lambda (x) (stringp x)))

(defcustom lsp-clients-svlangserver-lintOnUnsaved t
  "Enable linting on unsaved files"
  :group 'lsp-svlangserver
  :type 'boolean
  :safe (lambda (x) (booleanp x)))

(defcustom lsp-clients-svlangserver-formatCommand "verible-verilog-format"
  "Verible verilog format command"
  :group 'lsp-svlangserver
  :type 'string
  :safe (lambda (x) (stringp x)))

(defcustom lsp-clients-svlangserver-disableCompletionProvider nil
  "Disable auto completion provided by the language server"
  :group 'lsp-svlangserver
  :type 'boolean
  :safe (lambda (x) (booleanp x)))

(defcustom lsp-clients-svlangserver-disableHoverProvider nil
  "Disable hover over help provided by the language server"
  :group 'lsp-svlangserver
  :type 'boolean
  :safe (lambda (x) (booleanp x)))

(defcustom lsp-clients-svlangserver-disableSignatureHelpProvider nil
  "Disable signature help provided by the language server"
  :group 'lsp-svlangserver
  :type 'boolean
  :safe (lambda (x) (booleanp x)))

(defcustom lsp-clients-svlangserver-disableLinting nil
  "Disable verilator linting"
  :group 'lsp-svlangserver
  :type 'boolean
  :safe (lambda (x) (booleanp x)))

(defcustom lsp-clients-svlangserver-workspace-additional-dirs nil
  "Additional directories to be managed by this instance of svlangserver"
  :group 'lsp-svlangserver
  :type '(lsp-repeatable-vector string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-bin-path "svlangserver"
  "svlangserver binary path"
  :group 'lsp-svlangserver
  :type 'string
  :safe (lambda (x) (stringp x)))

(defcustom lsp-clients-svlangserver-bin-args nil
  "command line arguments for svlangserver binary"
  :group 'lsp-svlangserver
  :type '(lsp-repeatable-vector string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-node-command "node"
  "node binary path"
  :group 'lsp-svlangserver
  :type 'string
  :safe (lambda (x) (stringp x)))

(defcustom lsp-clients-svlangserver-module-path "svlangserver.js"
  "svlangserver module path"
  :group 'lsp-svlangserver
  :type 'string
  :safe (lambda (x) (stringp x)))

(defun lsp-clients-svlangserver-build-index ()
  (interactive)
  (lsp-send-execute-command "systemverilog.build_index"))

(defun lsp-clients-svlangserver-report-hierarchy (container-name)
  (interactive (list (read-string "Module/interface: " (cond ((use-region-p) (buffer-substring (region-beginning) (region-end))) (t "")))))
  (lsp-send-execute-command "systemverilog.report_hierarchy" (vector container-name)))

(lsp-dependency 'svlangserver
                '(:system "svlangserver"))

(defun lsp-clients-svlangserver-get-workspace-additional-dirs (_workspace)
  lsp-clients-svlangserver-workspace-additional-dirs)

(defun lsp-clients-svlangserver-command ()
  (let ((svlangserver-bin-path (lsp-package-path 'svlangserver)))
    (if svlangserver-bin-path
      (cons svlangserver-bin-path lsp-clients-svlangserver-bin-args)
      (if (file-exists-p lsp-clients-svlangserver-bin-path)
        (cons lsp-clients-svlangserver-bin-path lsp-clients-svlangserver-bin-args)
        (if (file-exists-p lsp-clients-svlangserver-module-path)
          `(,lsp-clients-svlangserver-node-command ,lsp-clients-svlangserver-module-path ,"--stdio")
          `(,"svlangserver"))))))

(lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-clients-svlangserver-command)
                     :major-modes '(verilog-mode)
                     :priority -1
                     :library-folders-fn 'lsp-clients-svlangserver-get-workspace-additional-dirs
                     :server-id 'svlangserver))

(lsp-register-custom-settings '(("systemverilog.includeIndexing" lsp-clients-svlangserver-includeIndexing)
                                ("systemverilog.excludeIndexing" lsp-clients-svlangserver-excludeIndexing)
                                ("systemverilog.defines" lsp-clients-svlangserver-defines)
                                ("systemverilog.launchConfiguration" lsp-clients-svlangserver-launchConfiguration)
                                ("systemverilog.lintOnUnsaved" lsp-clients-svlangserver-lintOnUnsaved)
                                ("systemverilog.formatCommand" lsp-clients-svlangserver-formatCommand)
                                ("systemverilog.disableCompletionProvider" lsp-clients-svlangserver-disableCompletionProvider)
                                ("systemverilog.disableHoverProvider" lsp-clients-svlangserver-disableHoverProvider)
                                ("systemverilog.disableSignatureHelpProvider" lsp-clients-svlangserver-disableSignatureHelpProvider)
                                ("systemverilog.disableLinting" lsp-clients-svlangserver-disableLinting)))

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
    	          :priority -2
                  :server-id 'lsp-verilog))

(lsp-consistency-check lsp-verilog)

(defgroup lsp-verible nil
  "LSP support for Verilog/SystemVerilog using the Verible suite."
  :group 'lsp-mode
  :link '(url-link "https://github.com/chipsalliance/verible"))

(defcustom lsp-clients-verible-executable '("verible-verilog-ls")
  "Command to start the Verible Verilog language server."
  :group 'lsp-verible
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-verible-executable)
                  :major-modes '(verilog-mode)
                  :language-id "verilog"
                  :priority -2
                  :server-id 'lsp-verilog-verible))

(provide 'lsp-verilog)
;;; lsp-verilog.el ends here
