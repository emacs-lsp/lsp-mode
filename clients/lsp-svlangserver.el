;;; lsp-svlangserver.el --- lsp-mode SystemVerilog integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 lsp-mode maintainers

;; Author: lsp-mode maintainers
;; Keywords: languages

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

;;  LSP client for svlangserver

;;; Code:

(require 'lsp-mode)

(defgroup lsp-svlangserver nil
  "Settings for the SystemVerilog language server client."
  :group 'lsp-mode
  :tag "Language Server"
  :package-version '(lsp-mode . "7.1"))

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

(defcustom lsp-clients-svlangserver-workspace-additional-dirs nil
  "Additional directories to be managed by this instance of svlangserver"
  :group 'lsp-svlangserver
  :type '(repeat string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-includeIndexing '("**/*.{sv,svh}")
  "Files included for indexing (glob pattern)"
  :group 'lsp-svlangserver
  :type '(repeat string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-excludeIndexing '("test/**/*.{sv,svh}")
  "Files excluded for indexing (glob pattern)"
  :group 'lsp-svlangserver
  :type '(repeat string)
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom lsp-clients-svlangserver-defines '()
  "Defines needed for linting"
  :group 'lsp-svlangserver
  :type '(repeat string)
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

(defun lsp-clients-svlangserver-command ()
  (list lsp-clients-svlangserver-node-command lsp-clients-svlangserver-module-path "--stdio"))

(defun lsp-clients-svlangserver-get-workspace-additional-dirs (_workspace)
  lsp-clients-svlangserver-workspace-additional-dirs)

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

(provide 'lsp-svlangserver)
