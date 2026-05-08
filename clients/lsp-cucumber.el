;;; lsp-cucumber.el --- LSP Clients for Cucumber  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh
;; Copyright (C) 2024-2026 emacs-lsp maintainers

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP server implementation for Cucumber
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-cucumber nil
  "LSP server implementation for Cucumber."
  :group 'lsp-mode
  :link '(url-link "https://github.com/cucumber/language-server"))

(defcustom lsp-cucumber-server-path nil
  "Path points for Cucumber language server.

This is only for development use."
  :type 'string
  :group 'lsp-cucumber)

(defcustom lsp-cucumber-active-modes
  '( feature-mode)
  "List of major mode that work with Cucumber language server."
  :type '(repeat function)
  :group 'lsp-cucumber)

(lsp-defcustom lsp-cucumber-features
  ["src/test/**/*.feature" "features/**/*.feature" "tests/**/*.feature" "*specs*/**/*.feature"]
  "Configure where the extension should look for .feature files."
  :type '(repeat string)
  :group 'lsp-cucumber
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "cucumber.features")

(lsp-defcustom lsp-cucumber-glue
  ["*specs*/**/*.cs" "features/**/*.js" "features/**/*.jsx" "features/**/*.php" "features/**/*.py" "features/**/*.rs" "features/**/*.rb" "features/**/*.ts" "features/**/*.tsx" "features/**/*_test.go" "**/*_test.go" "src/test/**/*.java" "tests/**/*.py" "tests/**/*.rs"]
  "Configure where the extension should look for source code where
step definitions and parameter types are defined."
  :type '(repeat string)
  :group 'lsp-cucumber
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "cucumber.glue")

(lsp-defcustom lsp-cucumber-parameter-types []
  "Configure parameters types to convert output parameters to your own types.

Details at https://github.com/cucumber/cucumber-expressions#custom-parameter-types.
Sample:
[(:name \"actor\"
  :regexp \"[A-Z][a-z]+\")]"
  :type '(lsp-repeatable-vector plist)
  :group 'lsp-cucumber
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "cucumber.parameterTypes")

(defun lsp-cucumber--server-command ()
  "Generate startup command for Cucumber language server."
  (or (and lsp-cucumber-server-path
           (list lsp-cucumber-server-path "--stdio"))
      (list (lsp-package-path 'cucumber-language-server) "--stdio")))

(lsp-dependency 'cucumber-language-server
                '(:system "cucumber-language-server")
                '(:npm :package "@cucumber/language-server"
                       :path "cucumber-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-cucumber--server-command)
  :major-modes lsp-cucumber-active-modes
  :priority -1
  :server-id 'cucumber-language-server
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'cucumber-language-server callback error-callback))))

(provide 'lsp-cucumber)
;;; lsp-cucumber.el ends here
