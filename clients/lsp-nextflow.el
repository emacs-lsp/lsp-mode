;;; lsp-nextflow.el --- lsp-mode nextflow integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Edmund Miller
;; Copyright (C) 2024-2026 emacs-lsp maintainers

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
  :link '(url-link "https://github.com/nextflow-io/language-server"))

(defcustom lsp-nextflow-java-path "java"
  "Path of the java executable."
  :group 'lsp-nextflow
  :type 'string)

(lsp-defcustom lsp-nextflow-language-version "25.10"
  "Nextflow language version to be used by the language server."
  :type '(choice (const "26.04 (preview)") (const "25.10") (const "25.04") (const "24.10"))
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.languageVersion")

(defcustom lsp-nextflow-server-download-url
  (format "https://github.com/nextflow-io/language-server/releases/download/v%s/language-server-all.jar"
          lsp-nextflow-language-version)
  "Automatic download url for lsp-nextflow."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-nextflow-server-file
  (f-join lsp-server-install-dir "nextflow-language-server.jar")
  "The path to the file in which `lsp-nextflow' will be stored."
  :group 'lsp-nextflow
  :risky t
  :type 'file
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-nextflow-server-command ()
  "Startup command for Nextflow language server."
  `("java" "-jar" ,(expand-file-name lsp-nextflow-server-file)))

(lsp-dependency 'nextflow-lsp
                '(:system lsp-nextflow-server-file)
                `(:download :url lsp-nextflow-server-download-url
                  :store-path lsp-nextflow-server-file))

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
  "Folders that should be excluded when scanning the workspace for Nextflow files."
  :type 'lsp-string-vector
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.files.exclude")

(lsp-defcustom lsp-nextflow-formatting-harshil-alignment nil
  "Use the [Harshil Alignment™️](https://nf-co.re/docs/contributing/code_editors_and_styling/harshil_alignment) when formatting Nextflow scripts and config files.

*Note: not all rules are supported.*"
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.formatting.harshilAlignment")

(lsp-defcustom lsp-nextflow-java-home ""
  "Specify the folder path to the desired Java runtime.

Equivalent to the `JAVA_HOME` environment variable, i.e.
the Java binary should be located at `$JAVA_HOME/bin/java`.
Use this setting if the extension cannot find Java automatically."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.java.home")

(lsp-defcustom lsp-nextflow-suppress-future-warnings t
  "Hide warnings for future changes, deprecations, and removals."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.suppressFutureWarnings")

(lsp-defcustom lsp-nextflow-completion-extended nil
  "Provide auto-completions from outside the current script.

If an external completion is selected, it will be automatically
included into the current script."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.completion.extended")

(lsp-defcustom lsp-nextflow-completion-max-items 100
  "The maximum number of auto-completions to suggest at a time."
  :type 'number
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.completion.maxItems")

(lsp-defcustom lsp-nextflow-error-reporting-mode "warnings"
  "Set the desired level of error reporting."
  :type '(choice (const "off") (const "errors") (const "warnings") (const "paranoid"))
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.errorReportingMode")

(lsp-defcustom lsp-nextflow-formatting-mahesh-form nil
  "Place process outputs at the end of the process body when formatting Nextflow scripts."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.formatting.maheshForm")

(lsp-defcustom lsp-nextflow-formatting-sort-declarations nil
  "Sort script declarations when formatting Nextflow scripts."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.formatting.sortDeclarations")

(lsp-defcustom lsp-nextflow-telemetry-enabled nil
  "Enable usage data to be sent to Seqera.

See the welcome page for more information about what we do and do not collect."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.telemetry.enabled")

(lsp-defcustom lsp-nextflow-type-checking nil
  "Enable static type checking."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.typeChecking")

;;
;;; Client

(lsp-register-client
 (make-lsp-client
  ;; FIXME
  ;; :download-server-fn (lambda (_client callback error-callback _update?)
  ;;                       (lsp-package-ensure 'nextflow-lsp callback error-callback))
  :new-connection (lsp-stdio-connection #'lsp-nextflow-server-command)
  :major-modes '(nextflow-mode)
  :multi-root t
  :activation-fn (lsp-activate-on "nextflow")
  :priority -1
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "nextflow"))))
  ;; TODO Handle preview dag
  :server-id 'nextflow-lsp))

(lsp-consistency-check lsp-nextflow)

(provide 'lsp-nextflow)
;;; lsp-nextflow.el ends here
