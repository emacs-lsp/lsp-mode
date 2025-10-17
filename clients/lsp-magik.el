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

(require 'lsp-mode)

(defgroup lsp-magik nil
  "LSP support for Magik."
  :link '(url-link "https://github.com/StevenLooman/magik-tools")
  :group 'lsp-mode
  :tag "Lsp Magik"
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-version "0.11.0"
  "Version of LSP server."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-download-url-lsp (format "https://github.com/StevenLooman/magik-tools/releases/download/%s/magik-language-server-%s.jar" lsp-magik-version lsp-magik-version)
  "URL of LSP server to download."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(lsp-dependency
 'magik-ls
 '(:download :url lsp-magik-download-url-lsp
             :store-path ,(f-join lsp-server-install-dir "magik-ls" (format "magik-language-server-%s.jar" lsp-magik-version))))

(defcustom lsp-magik-ls-path
  (f-join lsp-server-install-dir (format "magik-ls/magik-language-server-%s.jar" lsp-magik-version))
  "Path of the language server."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-magik-product-dirs []
  "Paths to (compiled, containing a libs/ directory) products."
  :type 'lsp-string-vector
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.productDirs")

(lsp-defcustom lsp-magik-lint-override-config-file nil
  "Override path to magiklintrc.properties."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "magik.lint.overrideConfigFile")

(lsp-defcustom lsp-magik-typing-type-database-paths []
  "Paths to type databases."
  :type 'lsp-string-vector
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "magik.typing.typeDatabasePaths")

(lsp-defcustom lsp-magik-typing-show-typing-inlay-hints nil
  "Show typing inlay hints."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.showTypingInlayHints")

(lsp-defcustom lsp-magik-typing-show-argument-inlay-hints nil
  "Show (certain) argument name inlay hints."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.showArgumentInlayHints")

(lsp-defcustom lsp-magik-typing-enable-checks nil
  "Enable typing checks."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "magik.typing.enableChecks")

(lsp-defcustom lsp-magik-typing-index-global-usages t
  "Enable indexing of usages of globals by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.indexGlobalUsages")

(lsp-defcustom lsp-magik-typing-index-method-usages nil
  "Enable indexing of usages of methods by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.indexMethodUsages")

(lsp-defcustom lsp-magik-typing-index-slot-usages t
  "Enable indexing of usages of slots by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.indexSlotUsages")

(lsp-defcustom lsp-magik-typing-index-condition-usages t
  "Enable indexing of usages of conditions by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.indexConditionUsages")

(lsp-defcustom lsp-magik-typing-cache-indexed-definitions-method-usages t
  "Store and load the indexed definitions in the workspace folders."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.typing.cacheIndexedDefinitions")

(lsp-defcustom lsp-magik-formatting-indent-char "tab"
  "Indent character, \"tab\" or \"space\"."
  :type '(choice (const "tab")
                 (const "space"))
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.indentChar")

(lsp-defcustom lsp-magik-formatting-indent-width 8
  "Indent width (tab size or number of spaces)."
  :type 'integer
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.indentWidth")

(lsp-defcustom lsp-magik-formatting-insert-final-newline t
  "Insert final newline."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.insertFinalNewline")

(lsp-defcustom lsp-magik-formatting-trim-trailing-whitespace t
  "Trim trailing whitespace."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.trimTrailingWhitespace")

(lsp-defcustom lsp-magik-formatting-trim-final-newlines t
  "Trim final newlines."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.trimFinalNewlines")

(lsp-defcustom lsp-magik-formatting-indent-strategy "null"
  "The strategy used for indentation, \"null\" or \"relative\"."
  :type '(choice (const "null")
                 (const "relative"))
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1")
  :lsp-path "magik.formatting.indentStrategy")

(defcustom lsp-magik-java-path (lambda ()
                                 (cond ((eq system-type 'windows-nt)
                                        (or (lsp-resolve-value (executable-find (expand-file-name "bin/java" (getenv "JAVA_HOME"))))
                                            (lsp-resolve-value (executable-find "java"))))
                                       (t "java")))
  "Path to Java Runtime, Java 11 minimum."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.1"))

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
  :activation-fn (lsp-activate-on "magik" "sw-product-def" "sw-module-def" "sw-load-list")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "magik"))))
  :server-id 'magik))

(lsp-consistency-check lsp-magik)

(provide 'lsp-magik)
;;; lsp-magik.el ends here
