;;; lsp-groovy.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, groovy

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

;; LSP Clients for the Groovy Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-groovy nil
  "LSP support for Groovy, using groovy-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/GroovyLanguageServer/groovy-language-server"))

(defcustom lsp-groovy-server-file (f-join lsp-server-install-dir "groovy-language-server-all.jar")
  "JAR file path for groovy-language-server-all.jar."
  :group 'lsp-groovy
  :risky t
  :type 'file)

(defun lsp-groovy--lsp-command ()
  "Generate LSP startup command."
  `("java" "-jar" ,(expand-file-name lsp-groovy-server-file)))

(defcustom lsp-groovy-classpath ["/usr/local/opt/groovy/libexec/lib"]
  "List of paths to Groovy JARs."
  :group 'lsp-groovy
  :risky t
  :type 'lsp-string-vector)

(lsp-register-custom-settings
 '(("groovy.classpath" lsp-groovy-classpath)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-groovy--lsp-command)
                  :major-modes '(groovy-mode)
                  :priority -1
                  :server-id 'groovy-ls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "groovy"))))))

(lsp-consistency-check lsp-groovy)

(provide 'lsp-groovy)
;;; lsp-groovy.el ends here
