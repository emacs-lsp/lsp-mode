;;; lsp-kotlin.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, kotlin

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

;; LSP Clients for the Kotlin Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-kotlin nil
  "LSP support for Kotlin, using KotlinLanguageServer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/fwcd/KotlinLanguageServer"))

(define-obsolete-variable-alias
  'lsp-kotlin-language-server-path
  'lsp-clients-kotlin-server-executable
  "lsp-mode 6.4")

(defcustom lsp-clients-kotlin-server-executable
  (if (eq system-type 'windows-nt)
      "kotlin-language-server.bat"
    "kotlin-language-server")
  "The kotlin-language-server executable to use.
Leave as just the executable name to use the default behavior of finding the
executable with `exec-path'."
  :type 'string
  :group 'lsp-kotlin)

(defcustom lsp-kotlin-trace-server "off"
  "Traces the communication between VSCode and the Kotlin language server."
  :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-compiler-jvm-target "1.8"
  "Specifies the JVM target, e.g. \"1.6\" or \"1.8\"."
  :type 'string
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-linting-debounce-time 250
  "[DEBUG] Specifies the debounce time limit.
Lower to increase responsiveness at the cost of possible stability issues."
  :type 'number
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-completion-snippets-enabled t
  "Specifies whether code completion should provide snippets (true) or
plain-text items (false)."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-debug-adapter-enabled t
  "[Recommended] Specifies whether the debug adapter should be used.
When enabled a debugger for Kotlin will be available."
  :type 'boolean)

(defcustom lsp-kotlin-debug-adapter-path ""
  "Optionally a custom path to the debug adapter executable."
  :type 'string
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-external-sources-use-kls-scheme t
  "[Recommended] Specifies whether URIs inside JARs should be represented
using the 'kls'-scheme."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-external-sources-auto-convert-to-kotlin t
  "Specifies whether decompiled/external classes should be auto-converted
to Kotlin."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-server-download-url
  "https://github.com/fwcd/kotlin-language-server/releases/download/latest/server.zip"
  "The URL for the language server download."
  :type 'string
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-custom-settings
 '(("kotlin.externalSources.autoConvertToKotlin" lsp-kotlin-external-sources-auto-convert-to-kotlin t)
   ("kotlin.externalSources.useKlsScheme" lsp-kotlin-external-sources-use-kls-scheme t)
   ("kotlin.debugAdapter.path" lsp-kotlin-debug-adapter-path)
   ("kotlin.debugAdapter.enabled" lsp-kotlin-debug-adapter-enabled t)
   ("kotlin.completion.snippets.enabled" lsp-kotlin-completion-snippets-enabled t)
   ("kotlin.linting.debounceTime" lsp-kotlin-linting-debounce-time)
   ("kotlin.compiler.jvm.target" lsp-kotlin-compiler-jvm-target)
   ("kotlin.trace.server" lsp-kotlin-trace-server)
   ("kotlin.languageServer.path" lsp-clients-kotlin-server-executable)))

(defvar lsp-kotlin--language-server-path
  (f-join lsp-server-install-dir
          "kotlin" "server" "bin" (if (eq system-type 'windows-nt)
                                      "kotlin-language-server.bat"
                                    "kotlin-language-server"))
  "The path to store the language server at if necessary.")

(lsp-dependency
 'kotlin-language-server
 `(:system ,lsp-clients-kotlin-server-executable)
 `(:download :url lsp-kotlin-server-download-url
             :decompress :zip
             :store-path ,(f-join lsp-server-install-dir "kotlin" "kotlin-language-server")
             :binary-path lsp-kotlin--language-server-path
             :set-executable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-kotlin-server-executable)
  :major-modes '(kotlin-mode)
  :priority -1
  :server-id 'kotlin-ls
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "kotlin"))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'kotlin-language-server callback error-callback))))

(lsp-consistency-check lsp-kotlin)

(provide 'lsp-kotlin)
;;; lsp-kotlin.el ends here
