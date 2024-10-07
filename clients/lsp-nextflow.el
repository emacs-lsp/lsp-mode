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

(defcustom lsp-nextflow-server-file (f-join lsp-server-install-dir "nextflow-language-server-all.jar")
  "JAR file path for nextflow-language-server-all.jar."
  :group 'lsp-nextflow
  :risky t
  :type 'file)

;;
;;; Installation

(defcustom lsp-nextflow-server-store-path
  (expand-file-name "nextflow/" lsp-server-install-dir)
  "The path to the file in which COBOL language service will be stored."
  :type 'file
  :group 'lsp-nextflow)

(defcustom lsp-nextflow-server-version "1.0.0"
  "The Nextflow language server version to install."
  :type 'file
  :group 'lsp-nextflow)

;; TODO Use  (lsp-vscode-extension-url "Nightrains" "robloxlsp" "1.5.11")
(defcustom lsp-nextflow-server-download-url
  "https://github.com/edmundmiller/vscode-language-nextflow/releases/download/1.0.0/nextflow-1.0.0.vsix"
  "Format to the download url link.")


;; TODO
;; (defun lsp-lua-roblox-language-server-test ()
;;   "Test Lua language server binaries and files."
;;   (and (f-exists? lsp-lua-roblox-language-server-main-location)
;;        (f-exists? lsp-lua-roblox-language-server-bin)))

(defun lsp-nextflow-language-server-install (_client callback error-callback _update?)
  "Download the latest version of nextflow-language-server and extract it"
  (lsp-download-install
   (lambda (&rest _)
     (set-file-modes lsp-nextflow-language-server-bin #o0700)
     (funcall callback))
   error-callback
   :url lsp-nextflow-server-download-url
   :store-path lsp-nextflow-server-store-path))

(defun lsp-nextflow--stored-executable ()
  "Return the stored Nextflow language service executable."
  (f-join lsp-cobol-server-store-path "extension/bin/language-server-all.jar"))

(defun lsp-nextflow--lsp-command ()
  "Generate LSP startup command."
  `("java" "-jar" ,(expand-file-name lsp-nextflow-server-file)))

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
  :type '(repeat string)
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.java.home")

(lsp-defcustom lsp-nextflow-suppress-future-warnings t
  "Hide warnings for future changes, deprecations, and removals."
  :type 'boolean
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "nextflow.suppressFutureWarnings")

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-nextflow--lsp-command)
                  :major-modes '(nextflow-mode)
                  :priority -1
                  :server-id 'nextflow-ls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "nextflow"))))))

(lsp-consistency-check lsp-nextflow)

(provide 'lsp-nextflow)
;;; lsp-nextflow.el ends here
