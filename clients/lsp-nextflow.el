;;; .local/straight/repos/lsp-mode/clients/lsp-nextflow.el -*- lexical-binding: t; -*-


;; Copyright (C) 2024 Edmund Miller

;; Author: Edmund Miller
;; Keywords: lsp, nextflow, groovy

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

;; LSP Clients for the Nextflow Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-nextflow nil
  "LSP support for nextflow, using nextflow-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nextflow-io/vscode-language-nextflow")
  :package-version `(lsp-mode . "8.0.0"))

(defcustom lsp-nextflow-java-path "java"
  "Path of the java executable."
  :group 'lsp-nextflow
  :type 'string)

(defcustom lsp-nextflow-version "1.0.0"
  "Version of Nextflow language server."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-nextflow-extension-name
  (format "nextflow-%s.vsix" lsp-nextflow-version)
  "File name of the extension file from language server."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-nextflow-server-download-url
  (format "https://github.com/nextflow-io/vscode-language-nextflow/releases/download/v%s/%s"
          lsp-nextflow-version lsp-nextflow-extension-name)
  "Automatic download url for lsp-nextflow."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-nextflow-server-store-path
  (f-join lsp-server-install-dir "nf-ls")
  "The path to the file in which `lsp-nextflow' will be stored."
  :type 'file
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-nextflow--extension-path ()
  "Return full path of the downloaded extension."
  (f-join lsp-nextflow-server-store-path lsp-nextflow-extension-name))

(defun lsp-nextflow--extension-dir ()
  "Return Nextflow extension path."
  (f-join lsp-nextflow-server-store-path "extension"))

(defun lsp-nextflow--server-command ()
  "Startup command for Nextflow language server."
  `("java" "-jar" (f-join lsp-nextflow-language-server-install-dir "extension/bin/language-server-all.jar")))

(lsp-dependency
 'nf-ls
 '(:system "nf-ls")
 `(:download :url lsp-nextflow-server-download-url
   :decompress :zip
   :store-path ,(f-no-ext (lsp-nextflow--extension-path))
   :set-executable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-nextflow--server-command
                   (lambda () (f-exists? (lsp-nextflow--extension-dir))))
  :major-modes '(nextflow-mode)
  :priority -1
  :server-id 'nextflow-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'nextflow-ls callback error-callback))))

(lsp-consistency-check lsp-nextflow)

;;
;;; Settings

;; (lsp-generate-settings "~/src/nf-core/vscode-language-nextflow/package.json" 'lsp-nextflow)

(lsp-defcustom lsp-nextflow-debug nil
  "Enable debug logging and debug information in hover hints."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.debug")

(lsp-defcustom lsp-nextflow-files-exclude [".git" ".nf-test" "work"]
  "Configure glob patterns for excluding folders from being searched for Nextflow scripts and configuration files."
  :type 'lsp-string-vector
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.files.exclude")

(lsp-defcustom lsp-nextflow-formatting-harshil-alignment nil
  "Use the [Harshil Alignment™️](https://nf-co.re/docs/contributing/code_editors_and_styling/harshil_alignment) when formatting Nextflow scripts and config files.

  *Note: not all rules are supported yet*"
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.formatting.harshilAlignment")

(lsp-defcustom lsp-nextflow-java-home nil
  "Specifies the folder path to the JDK. Use this setting if the extension cannot find Java automatically."
  :type '(choice (const :tag "Auto" nil)
          (directory :tag "Custom JDK path"))
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.java.home")

(lsp-defcustom lsp-nextflow-suppress-future-warnings t
  "Hide warnings for future changes, deprecations, and removals."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.suppressFutureWarnings")

(provide 'lsp-nextflow)
;;; lsp-nextflow.el ends here
