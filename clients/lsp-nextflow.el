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
  :link '(url-link "https://github.com/nextflow-io/vscode-language-nextflow"))

(defcustom lsp-nextflow-server-version "1.0.0"
  "The Nextflow language server version to install."
  :type 'string
  :group 'lsp-nextflow)

(defcustom lsp-nextflow-language-server-install-dir
  (f-join lsp-server-install-dir "nextflow-language-server/")
  "Installation directory for Nextflow Server."
  :group 'lsp-nextflow
  :type 'directory)

(defcustom lsp-nextflow-java-path "java"
  "Java Runtime binary location."
  :group 'lsp-nextflow
  :type 'file)

(defcustom lsp-nextflow-jar-path
  (f-join lsp-nextflow-language-server-install-dir "extension/bin/language-server-all.jar")
  "Nextflow language server jar file."
  :group 'lsp-nextflow
  :type 'file)

(defcustom lsp-nextflow-args '("-jar")
  "Arguments to the Nextflow Language server."
  :group 'lsp-nextflow
  :type '(repeat string))

(defcustom lsp-nextflow-command nil
  "Final command to call the Nextflow Language server."
  :group 'lsp-nextflow
  :type '(repeat string))

(defcustom lsp-nextflow-server-download-url
  "https://github.com/edmundmiller/vscode-language-nextflow/releases/download/1.0.0/nextflow-1.0.0.vsix"
  "Download URL for the Nextflow language server.")

(defun lsp-nextflow-test ()
  "Test the Nextflow language server binaries and files."
  (and (executable-find lsp-nextflow-java-path)
       (f-exists? lsp-nextflow-jar-path)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (or lsp-nextflow-command
                         `(,lsp-nextflow-java-path
                           ,@lsp-nextflow-args
                           ,lsp-nextflow-jar-path))))
  #'lsp-nextflow-test)
 :major-modes '(nextflow-mode)
 :activation-fn (lsp-activate-on "nextflow")
 ;; (add-to-list 'lsp-language-id-configuration '(".*\\.svelte$" . "svelte"))
 :server-id 'nextflow-ls
 :priority -1))
;; :notification-handlers (lsp-ht ("emmy/progressReport" #'ignore))))
;; :initialized-fn (lambda (workspace)
;;                   (with-lsp-workspace workspace
;;                     (lsp--set-configuration (lsp-configuration-section "nextflow"))))))

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
