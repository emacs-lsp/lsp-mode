;;; lsp-nextflow.el --- lsp-mode nextflow integration -*- lexical-binding: t; -*-

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
;;
;; The language server JAR will be automatically downloaded from GitHub releases.

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

(defcustom lsp-nextflow-version "25.04.2"
  "Version of Nextflow language server."
  :type 'string
  :group 'lsp-nextflow
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-nextflow-server-download-url
  (format "https://github.com/nextflow-io/language-server/releases/download/v%s/language-server-all.jar"
          lsp-nextflow-version)
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

(defun lsp-nextflow--async-download (callback error-callback)
  "Asynchronously download Nextflow language server JAR file."
  (let ((download-buffer (url-retrieve 
                          lsp-nextflow-server-download-url
                          (lambda (status callback error-callback)
                            (if (plist-get status :error)
                                (progn
                                  (message "Nextflow LSP download failed: %s" (plist-get status :error))
                                  (funcall error-callback (plist-get status :error)))
                              (unwind-protect
                                  (progn
                                    (goto-char (point-min))
                                    (re-search-forward "\n\n" nil 'noerror)
                                    (let ((jar-content (buffer-substring (point) (point-max))))
                                      (mkdir (f-parent lsp-nextflow-server-file) t)
                                      (with-temp-file lsp-nextflow-server-file
                                        (set-buffer-file-coding-system 'binary)
                                        (insert jar-content))
                                      (message "Nextflow LSP download completed: %s" lsp-nextflow-server-file)
                                      (funcall callback)))
                                (kill-buffer (current-buffer)))))
                          (list callback error-callback))))
    (message "Downloading Nextflow LSP server from %s..." lsp-nextflow-server-download-url)))

(defun lsp-nextflow-server-command ()
  "Startup command for Nextflow language server."
  `(,lsp-nextflow-java-path "-jar" ,(expand-file-name lsp-nextflow-server-file)))

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
  "Configure glob patterns for excluding folders from being searched for
Nextflow scripts and configuration files."
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
  "Specifies the folder path to the JDK. Use this setting if the extension cannot
find Java automatically."
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

;;
;;; Client

(lsp-register-client
 (make-lsp-client
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-nextflow--async-download callback error-callback))
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (list
                      lsp-nextflow-java-path
                      "-jar"
                      (expand-file-name lsp-nextflow-server-file))))
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
