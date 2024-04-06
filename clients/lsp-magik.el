;;; lsp-magik.el --- Language server client for Magik  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Keronic

;; Author: <robin.putters@keronic.com>
;; Keywords: lsp, magik

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

;; LSP client for the Magik programming language
;; https://github.com/StevenLooman/magik-tools

;;; Code:

(require `lsp-mode)

(defgroup lsp-magik nil
  "LSP support for Magik."
  :link '(url-link "https://github.com/StevenLooman/magik-tools")
  :group 'lsp-mode
  :tag "Lsp Magik"
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-version "0.9.0"
  "Version of LSP server."
  :type `string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-download-url-lsp (format "https://github.com/StevenLooman/magik-tools/releases/download/%s/magik-language-server-%s.jar" lsp-magik-version lsp-magik-version)
  "URL of LSP server to download."
  :type `string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(lsp-dependency
 'magik-ls
 `(:download :url lsp-magik-download-url-lsp
             :store-path ,(f-join lsp-server-install-dir "magik-ls" (format "magik-language-server-%s.jar" lsp-magik-version))))

(defcustom lsp-magik-ls-path
  (f-join lsp-server-install-dir (format "magik-ls/magik-language-server-%s.jar" lsp-magik-version))
  "Path of the language server."
  :type 'string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-java-home nil
  "Path to Java Runtime, Java 11 minimum."
  :type `string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-smallworld-gis nil
  "Path to Smallworld Core."
  :type `string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-typing-type-database-paths []
  "Paths to type databases."
  :type `lsp-string-vector
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-typing-enable-checks nil
  "Enable typing checks."
  :type `boolean
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-trace-server "off"
  "Traces the communication between VS Code and the Magik language server."
  :type `(choice (const "off") (const "message") (const "verbose"))
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-java-path (lambda ()
                                 (cond ((eq system-type 'windows-nt)
                                        (or (lsp-resolve-value (executable-find (expand-file-name "bin/java" (getenv "JAVA_HOME"))))
                                            (lsp-resolve-value (executable-find "java"))))
                                       (t "java")))
  "Path of the java executable."
  :type 'string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-lint-override-config-file nil
  "Override path to magiklintrc.properties."
  :type 'string
  :group `lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-client
 (make-lsp-client
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'magik-ls callback error-callback))
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (list
                      (substitute-in-file-name (lsp-resolve-value lsp-magik-java-path))
                      "-jar"
                      (substitute-in-file-name lsp-magik-ls-path)
                      "--debug")))
  :activation-fn (lsp-activate-on "magik")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "magik"))))
  :server-id 'magik))

(lsp-register-custom-settings
 `(("magik.javaHome" lsp-magik-java-home)
   ("magik.smallworldGis" lsp-magik-smallworld-gis)
   ("magik.typing.typeDatabasePaths" lsp-magik-typing-type-database-paths)
   ("magik.typing.enableChecks" lsp-magik-typing-enable-checks)
   ("magik.trace.server" lsp-magik-trace-server)
   ("magik.lint.overrideConfigFile" lsp-magik-lint-override-config-file)))

(lsp-consistency-check lsp-magik)

(provide 'lsp-magik)
;;; lsp-magik.el ends here
