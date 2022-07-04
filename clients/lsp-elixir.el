;;; lsp-elixir.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2021 emacs-lsp maintainers

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
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-dialyzer-warn-opts '()
  "Dialyzer options to enable or disable warnings.

See Dialyzer's documentation for options. Note that the \"race_conditions\"
option is unsupported"
  :type '(repeat string)
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-dialyzer-format "dialyxir_long"
  "Formatter to use for Dialyzer warnings."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-mix-env "test"
  "Mix environment to use for compilation."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-mix-target nil
  "Mix target to use for compilation (requires Elixir >= 1.8)."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-project-dir nil
  "Subdirectory containing Mix project if not in the project root.

If value is `\"\"` then defaults to the workspace rootUri."
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-fetch-deps t
  "Automatically fetch project dependencies when compiling."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-suggest-specs t
  "Suggest @spec annotations inline using Dialyzer's inferred success typings.
This requires Dialyzer."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-signature-after-complete t
  "Show signature help after confirming autocomplete."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defgroup lsp-elixir nil
  "LSP support for Elixir, using elixir-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elixir-lsp/elixir-ls"))

(define-obsolete-variable-alias 'lsp-clients-elixir-server-executable 'lsp-elixir-server-command "2021-04-05")

(defcustom lsp-elixir-server-command
  (if (equal system-type 'windows-nt)
      '("language_server.bat")
    '("language_server.sh"))
  "Command to start elixir-ls.

Leave as default to let `executable-find' search for it."
  :group 'lsp-elixir
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-ls-version "v0.9.0"
  "Elixir-Ls version to download.
It has to be set before `lsp-elixir.el' is loaded and it has to
be available here: https://github.com/elixir-lsp/elixir-ls/releases/"
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-elixir-ls-download-url
  (format "https://github.com/elixir-lsp/elixir-ls/releases/download/%s/elixir-ls.zip"
          lsp-elixir-ls-version)
  "Automatic download url for elixir-ls"
  :type 'string
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.1"))


(defconst lsp-elixir-ls-server-dir
  (f-join lsp-server-install-dir "elixir-ls")
  "Elixir-ls local server Directory.")

(defcustom lsp-elixir-local-server-command
  (f-join lsp-elixir-ls-server-dir
          (cl-first lsp-elixir-server-command))
  "Command to start local elixir-ls binary."
  :group 'lsp-elixir
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-elixir-enable-test-lenses t
  "Suggest Tests."
  :type 'boolean
  :group 'lsp-elixir
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-elixir--build-test-command (argument)
  "Builds the test command from the ARGUMENT."
  (let ((test-name (lsp-get argument :testName))
        (module (lsp-get argument :module))
        (describe (lsp-get argument :describe)))
    (cond (module (concat "\"" "module:" module "\""))
          ((not test-name) (concat "\"" "describe:" describe "\""))
          (describe (concat "\"" "test:test " describe " " test-name "\"" ))
          (t (concat "\"" "test:test " test-name "\"" )))))

(lsp-defun lsp-elixir--run-test ((&Command :arguments?))
  "Runs tests."
  (let* ((argument (lsp-seq-first arguments?))
         (file-path (lsp-get argument :filePath))
         (test-command (lsp-elixir--build-test-command argument)))
    (compile
     (concat "cd " (lsp-workspace-root file-path) " && "
             "mix test --exclude test --include " test-command " " file-path
             " --no-color"))
    file-path))

(lsp-dependency
 'elixir-ls
 `(:download :url lsp-elixir-ls-download-url
             :decompress :zip
             :store-path ,(f-join lsp-server-install-dir "elixir-ls" "elixir-ls.zip")
             :binary-path lsp-elixir-server-command
             :set-executable? t))

(lsp-register-custom-settings
 '(("elixirLS.dialyzerEnabled" lsp-elixir-dialyzer-enabled t)
   ("elixirLS.dialyzerWarnOpts" lsp-elixir-dialyzer-warn-opts)
   ("elixirLS.dialyzerFormat" lsp-elixir-dialyzer-format)
   ("elixirLS.mixEnv" lsp-elixir-mix-env)
   ("elixirLS.mixTarget" lsp-elixir-mix-target)
   ("elixirLS.projectDir" lsp-elixir-project-dir)
   ("elixirLS.fetchDeps" lsp-elixir-fetch-deps t)
   ("elixirLS.suggestSpecs" lsp-elixir-suggest-specs t)
   ("elixirLS.signatureAfterComplete" lsp-elixir-signature-after-complete t)
   ("elixirLS.enableTestLenses" lsp-elixir-enable-test-lenses t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (when (f-exists? lsp-elixir-local-server-command)
                                              lsp-elixir-local-server-command)
                                            (or (executable-find
                                                 (cl-first lsp-elixir-server-command))
                                                (lsp-package-path 'elixir-ls))
                                            "language_server.bat")
                                       ,@(cl-rest lsp-elixir-server-command))))
                  :activation-fn (lsp-activate-on "elixir")
                  :priority -1
                  :server-id 'elixir-ls
                  :action-handlers (ht ("elixir.lens.test.run" 'lsp-elixir--run-test))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'elixir-ls callback error-callback))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "elixirLS")))
                                    (lsp-put
                                     (lsp--workspace-server-capabilities workspace)
                                     :textDocumentSync
                                     (lsp-make-text-document-sync-options
                                      :save t
                                      :change 2)))))

(lsp-consistency-check lsp-elixir)

(provide 'lsp-elixir)
;;; lsp-elixir.el ends here
