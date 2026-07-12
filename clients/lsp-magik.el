;;; lsp-magik.el --- Language server client for Magik  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Keronic
;; Copyright (C) 2022-2026 emacs-lsp maintainers

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

(declare-function magik-utils-get-buffer-mode "magik-utils")
(declare-function barf-if-no-gis "magik-session")
(defvar magik-session-buffer)
(defvar magik-session-buffer-alist-prefix-function)

(defgroup lsp-magik nil
  "LSP support for Magik."
  :link '(url-link "https://github.com/StevenLooman/magik-tools")
  :group 'lsp-mode
  :tag "Lsp Magik"
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-version "0.12.0"
  "Version of LSP server."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-download-url-lsp (format "https://github.com/StevenLooman/magik-tools/releases/download/%s/magik-language-server-%s.jar" lsp-magik-version lsp-magik-version)
  "URL of LSP server to download."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-magik-ls-path
  (f-join lsp-server-install-dir "magik-ls" (format "magik-language-server-%s.jar" lsp-magik-version))
  "Path of the language server."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0"))

(lsp-dependency
 'magik-ls
 `(:download :url lsp-magik-download-url-lsp
             :store-path ,lsp-magik-ls-path))

(lsp-defcustom lsp-magik-product-dirs []
  "Paths to (compiled, containing a libs/ directory) products."
  :type 'lsp-string-vector
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.productDirs")

(lsp-defcustom lsp-magik-lint-override-config-file nil
  "Override path to magiklintrc.properties."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "magik.lint.overrideConfigFile")

(lsp-defcustom lsp-magik-lint-run-checks-on-change-delay 333
  "Run checks delayed after <value> milliseconds since last change.
The next update resets the delay."
  :type 'number
  :group 'lsp-magik
  :package-version '(lsp-mode . "11.0.0")
  :lsp-path "magik.lint.runChecksOnChangeDelay")

(lsp-defcustom lsp-magik-lint-run-checks-on-save nil
  "Run checks when saving the file, immediately."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "11.0.0")
  :lsp-path "magik.lint.runChecksOnSave")

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
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.showTypingInlayHints")

(lsp-defcustom lsp-magik-typing-show-argument-inlay-hints nil
  "Show (certain) argument name inlay hints."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
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
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.indexGlobalUsages")

(lsp-defcustom lsp-magik-typing-index-method-usages nil
  "Enable indexing of usages of methods by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.indexMethodUsages")

(lsp-defcustom lsp-magik-typing-index-slot-usages t
  "Enable indexing of usages of slots by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.indexSlotUsages")

(lsp-defcustom lsp-magik-typing-index-condition-usages t
  "Enable indexing of usages of conditions by methods."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.indexConditionUsages")

(lsp-defcustom lsp-magik-typing-cache-indexed-definitions-method-usages nil
  "Store and load the indexed definitions in the workspace folders."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.typing.cacheIndexedDefinitions")

(lsp-defcustom lsp-magik-formatting-indent-strategy "null"
  "The strategy used for indentation: \"null\", \"block\", or \"visual\"."
  :type '(choice (const "null")
                 (const "block")
                 (const "visual"))
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.indentStrategy")

(lsp-defcustom lsp-magik-formatting-indent-char "tab"
  "Indent character, \"tab\" or \"space\"."
  :type '(choice (const "tab")
                 (const "space"))
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.indentChar")

(lsp-defcustom lsp-magik-formatting-indent-width 8
  "Indent width (tab size or number of spaces)."
  :type 'integer
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.indentWidth")

(lsp-defcustom lsp-magik-formatting-insert-final-newline t
  "Insert final newline."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.insertFinalNewline")

(lsp-defcustom lsp-magik-formatting-trim-trailing-whitespace t
  "Trim trailing whitespace."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.trimTrailingWhitespace")

(lsp-defcustom lsp-magik-formatting-trim-final-newlines t
  "Trim final newlines."
  :type 'boolean
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0")
  :lsp-path "magik.formatting.trimFinalNewlines")

(defcustom lsp-magik-java-path (lambda ()
                                 (cond ((eq system-type 'windows-nt)
                                        (or (lsp-resolve-value (executable-find (expand-file-name "bin/java" (getenv "JAVA_HOME"))))
                                            (lsp-resolve-value (executable-find "java"))))
                                       (t "java")))
  "Path to Java Runtime, Java 11 minimum."
  :type 'string
  :group 'lsp-magik
  :package-version '(lsp-mode . "10.0.0"))

(defun lsp-magik--send-string (string msg)
  "Send STRING to the active GIS session process and show MSG."
  (unless (and (require 'magik-utils nil t) (require 'magik-session nil t))
    (user-error "The magik-mode is not available"))
  (let* ((gis (magik-utils-get-buffer-mode nil
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis)))
    (message "%s" msg)
    (when-let* ((window (display-buffer gis)))
      (set-window-point window (with-current-buffer gis (point-max))))
    (process-send-string process string)))

(lsp-defun lsp-magik--load-module ((&Command :arguments?))
  "Load the module in ARGUMENTS? into the GIS session."
  (when-let* ((module-name (lsp-seq-first arguments?)))
    (lsp-magik--send-string
     (format "sw_module_manager.load_module(\"%s\")\n$\n" module-name)
     (format "Adding module %s" module-name))))

(lsp-defun lsp-magik--add-product ((&Command :arguments?))
  "Add the product in ARGUMENTS? into the GIS session."
  (when-let* ((product-dir (lsp-seq-first arguments?)))
    (lsp-magik--send-string
     (format "smallworld_product.add_product(\"%s\")\n$\n" product-dir)
     (format "Adding product %s" product-dir))))

(lsp-defun lsp-magik--run-test ((&Command :arguments?))
  "Run the test identified by ARGUMENTS? in the active GIS session."
  (when-let* ((test-id (lsp-seq-first arguments?)))
    (cond
     ((string-prefix-p "method:" test-id)
      (let* ((rest (substring test-id 7))
             (dot-pos (string-match "\\." rest))
             (test-expr (format "%s.new(:|%s|)" (substring rest 0 dot-pos) (substring rest (1+ dot-pos)))))
        (lsp-magik--send-string
         (format (concat "_block\n"
                         "    _local suite << sw:test_suite.new()\n"
                         "    suite.add_test(%s)\n"
                         "    sw:test_runner.new().run_in_foreground(suite)\n"
                         "_endblock\n$\n")
                 test-expr)
         (format "Running test %s" rest))))
     ((string-prefix-p "test_case:" test-id)
      (lsp-magik--send-string
       (format "sw:test_runner.new().run_in_foreground(%s.suite())\n$\n"
               (substring test-id 10))
       (format "Running all tests in %s" (substring test-id 10)))))))

(defun lsp-magik-re-index ()
  "Trigger the Magik language server to re-index the workspace."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-send-execute-command "magik.reIndex"))

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
                      "--debug"))
                   (lambda ()
                     (and (executable-find (lsp-resolve-value lsp-magik-java-path) t)
                          (file-exists-p (substitute-in-file-name lsp-magik-ls-path)))))
  :activation-fn (lsp-activate-on "magik" "sw-product-def" "sw-module-def" "sw-load-list")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "magik"))))
  :server-id 'magik
  :action-handlers (ht ("magik.session.loadModule" #'lsp-magik--load-module)
                       ("magik.session.addProduct" #'lsp-magik--add-product)
                       ("magik.munit.runTest"      #'lsp-magik--run-test))))

(lsp-consistency-check lsp-magik)

(provide 'lsp-magik)
;;; lsp-magik.el ends here
