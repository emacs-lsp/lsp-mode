;;; lsp-elixir.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, elixir

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

;; LSP Clients for the Elixir Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'ht)

(defcustom lsp-elixir-dialyzer-enabled t
  "Run ElixirLS's rapid Dialyzer when code is saved."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-dialyzer-warn-opts '()
  "Dialyzer options to enable or disable warnings.

See Dialyzer's documentation for options. Note that the \"race_conditions\" option is unsupported"
  :type '(repeat string)
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-dialyzer-format "dialyxir_long"
  "Formatter to use for Dialyzer warnings."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-mix-env "test"
  "Mix environment to use for compilation."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-mix-target nil
  "Mix target to use for compilation (requires Elixir >= 1.8)."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-project-dir nil
  "Subdirectory containing Mix project if not in the project root.

 If value is `\"\"` then defaults to the workspace rootUri."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-fetch-deps t
  "Automatically fetch project dependencies when compiling."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-suggest-specs t
  "Suggest @spec annotations inline using Dialyzer's inferred success typings (Requires Dialyzer)."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-elixir-signature-after-complete t
  "Show signature help after confirming autocomplete."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "7.1"))

(defgroup lsp-elixir nil
  "LSP support for Elixir, using elixir-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elixir-lsp/elixir-ls"))

(defcustom lsp-clients-elixir-server-executable
  (if (equal system-type 'windows-nt)
      "language_server.bat"
    "language_server.sh")
  "The elixir-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-elixir
  :type 'file)

(lsp-register-custom-settings
 '(("elixirLS.dialyzerEnabled" lsp-elixir-dialyzer-enabled t)
   ("elixirLS.dialyzerWarnOpts" lsp-elixir-dialyzer-warn-opts)
   ("elixirLS.dialyzerFormat" lsp-elixir-dialyzer-format)
   ("elixirLS.mixEnv" lsp-elixir-mix-env)
   ("elixirLS.mixTarget" lsp-elixir-mix-target)
   ("elixirLS.projectDir" lsp-elixir-project-dir)
   ("elixirLS.fetchDeps" lsp-elixir-fetch-deps t)
   ("elixirLS.suggestSpecs" lsp-elixir-suggest-specs t)
   ("elixirLS.signatureAfterComplete" lsp-elixir-signature-after-complete t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () `(,lsp-clients-elixir-server-executable)))
                  :major-modes '(elixir-mode)
                  :priority -1
                  :server-id 'elixir-ls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "elixirLS")))
                                    (puthash
                                     "textDocumentSync"
                                     (ht ("save" t)
                                         ("change" 2))
                                     (lsp--workspace-server-capabilities workspace)))))

(provide 'lsp-elixir)
;;; lsp-elixir.el ends here
