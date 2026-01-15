;;; lsp-dockerfile.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, dockerfile

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

;; LSP Clients for Dockerfile documents.

;;; Code:

(require 'lsp-mode)


;;; Dockerfile

(defgroup lsp-dockerfile nil
  "Dockerfile LSP client, provided by the Dockerfile Language Server."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/rcjsuen/dockerfile-language-server-nodejs"))

(defcustom lsp-dockerfile-language-server-command
  '("docker-langserver" "--stdio")
  "The command that starts the docker language server."
  :group 'lsp-dockerfile
  :type '(repeat :tag "List of string values" string))

;; Diagnostic settings
(lsp-defcustom lsp-dockerfile-deprecated-maintainer "warning"
  "Controls the diagnostic severity for the deprecated MAINTAINER instruction."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.deprecatedMaintainer")

(lsp-defcustom lsp-dockerfile-directive-casing "warning"
  "Controls the diagnostic severity for parser directive casing.
Parser directives include escape and syntax."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.directiveCasing")

(lsp-defcustom lsp-dockerfile-empty-continuation-line "ignore"
  "Controls the diagnostic severity for empty continuation lines."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.emptyContinuationLine")

(lsp-defcustom lsp-dockerfile-instruction-casing "warning"
  "Controls the diagnostic severity for instruction casing.
Instructions include FROM, RUN, COPY, etc."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.instructionCasing")

(lsp-defcustom lsp-dockerfile-instruction-cmd-multiple "warning"
  "Controls the diagnostic severity for multiple CMD instructions.
Only the last CMD instruction takes effect in a Dockerfile."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.instructionCmdMultiple")

(lsp-defcustom lsp-dockerfile-instruction-entrypoint-multiple "warning"
  "Controls the diagnostic severity for multiple ENTRYPOINT instructions.
Only the last ENTRYPOINT instruction takes effect in a Dockerfile."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.instructionEntrypointMultiple")

(lsp-defcustom lsp-dockerfile-instruction-healthcheck-multiple "warning"
  "Controls the diagnostic severity for multiple HEALTHCHECK instructions.
Only the last HEALTHCHECK instruction takes effect in a Dockerfile."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.instructionHealthcheckMultiple")

(lsp-defcustom lsp-dockerfile-instruction-json-single-quotes "warning"
  "Controls the diagnostic severity for JSON arrays using single quotes.
JSON arrays in Dockerfile instructions should use double quotes."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.diagnostics.instructionJSONInSingleQuotes")

;; Formatter settings
(lsp-defcustom lsp-dockerfile-format-ignore-multiline-instructions nil
  "Whether to ignore multiline instructions during formatting."
  :type 'boolean
  :group 'lsp-dockerfile
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "docker.languageserver.formatter.ignoreMultilineInstructions")

(lsp-register-custom-settings
 '(("docker.languageserver.diagnostics.deprecatedMaintainer"
    lsp-dockerfile-deprecated-maintainer)
   ("docker.languageserver.diagnostics.directiveCasing"
    lsp-dockerfile-directive-casing)
   ("docker.languageserver.diagnostics.emptyContinuationLine"
    lsp-dockerfile-empty-continuation-line)
   ("docker.languageserver.diagnostics.instructionCasing"
    lsp-dockerfile-instruction-casing)
   ("docker.languageserver.diagnostics.instructionCmdMultiple"
    lsp-dockerfile-instruction-cmd-multiple)
   ("docker.languageserver.diagnostics.instructionEntrypointMultiple"
    lsp-dockerfile-instruction-entrypoint-multiple)
   ("docker.languageserver.diagnostics.instructionHealthcheckMultiple"
    lsp-dockerfile-instruction-healthcheck-multiple)
   ("docker.languageserver.diagnostics.instructionJSONInSingleQuotes"
    lsp-dockerfile-instruction-json-single-quotes)
   ("docker.languageserver.formatter.ignoreMultilineInstructions"
    lsp-dockerfile-format-ignore-multiline-instructions t)))

(lsp-dependency 'docker-langserver
                '(:system "docker-langserver")
                '(:npm :package "dockerfile-language-server-nodejs"
                       :path "docker-langserver"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find
                                             (cl-first lsp-dockerfile-language-server-command))
                                            (lsp-package-path 'docker-langserver))
                                       ,@(cl-rest lsp-dockerfile-language-server-command))))
                  :activation-fn (lsp-activate-on "dockerfile")
                  :priority -1
                  :server-id 'dockerfile-ls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "docker"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'docker-langserver
                                                            callback error-callback))))

(lsp-consistency-check lsp-dockerfile)

(provide 'lsp-dockerfile)
;;; lsp-dockerfile.el ends here
