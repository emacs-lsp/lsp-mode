;;; lsp-css.el --- CSS language server configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-css nil
  "LSP support for CSS."
  :group 'lsp-mode
  :link '(url-link
          "https://github.com/microsoft/vscode/tree/main/extensions/css-language-features/server"))

(defcustom lsp-css-experimental-custom-data nil
  "A list of JSON file paths that define custom CSS data that
loads custom properties, at directives, pseudo classes /
elements."
  :type '(repeat string))

(defcustom lsp-css-completion-trigger-property-value-completion t
  "By default, VS Code triggers property value completion after
selecting a CSS property. Use this setting to disable this
behavior."
  :type 'boolean)

(defcustom lsp-css-validate t
  "Enables or disables all validations."
  :type 'boolean)

(defcustom lsp-css-lint-compatible-vendor-prefixes "ignore"
  "When using a vendor-specific prefix make sure to also include
all other vendor-specific properties."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-vendor-prefix "warning"
  "When using a vendor-specific prefix, also include the standard
property."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-duplicate-properties "ignore"
  "Do not use duplicate style definitions."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-empty-rules "warning"
  "Do not use empty rulesets."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-import-statement "ignore"
  "Import statements do not load in parallel."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-box-model "ignore"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-universal-selector "ignore"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-zero-units "ignore"
  "No unit for zero needed."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-font-face-properties "warning"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-hex-color-length "error"
  "Hex colors must consist of three or six hex numbers."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-arguments-in-color-function "error"
  "Invalid number of parameters."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-unknown-properties "warning"
  "Unknown property."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-valid-properties nil
  "A list of properties that are not validated against the
`unknownProperties` rule."
  :type '(repeat string))

(defcustom lsp-css-lint-ie-hack "ignore"
  "IE hacks are only necessary when supporting IE7 and older."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-unknown-vendor-specific-properties "ignore"
  "Unknown vendor specific property."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-property-ignored-due-to-display "warning"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-important "ignore"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-float "ignore"
  nil
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-id-selector "ignore"
  "Selectors should not contain IDs because these rules are too
tightly coupled with the HTML."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-lint-unknown-at-rules "warning"
  "Unknown at-rule."
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error")))

(defcustom lsp-css-trace-server "off"
  "Traces the communication between VS Code and the CSS language
server."
  :type '(choice
          (const "off")
          (const "messages")
          (const "verbose")))

(lsp-register-custom-settings
 '(("css.trace.server" lsp-css-trace-server)
   ("css.lint.unknownAtRules" lsp-css-lint-unknown-at-rules)
   ("css.lint.idSelector" lsp-css-lint-id-selector)
   ("css.lint.float" lsp-css-lint-float)
   ("css.lint.important" lsp-css-lint-important)
   ("css.lint.propertyIgnoredDueToDisplay" lsp-css-lint-property-ignored-due-to-display)
   ("css.lint.unknownVendorSpecificProperties" lsp-css-lint-unknown-vendor-specific-properties)
   ("css.lint.ieHack" lsp-css-lint-ie-hack)
   ("css.lint.validProperties" lsp-css-lint-valid-properties)
   ("css.lint.unknownProperties" lsp-css-lint-unknown-properties)
   ("css.lint.argumentsInColorFunction" lsp-css-lint-arguments-in-color-function)
   ("css.lint.hexColorLength" lsp-css-lint-hex-color-length)
   ("css.lint.fontFaceProperties" lsp-css-lint-font-face-properties)
   ("css.lint.zeroUnits" lsp-css-lint-zero-units)
   ("css.lint.universalSelector" lsp-css-lint-universal-selector)
   ("css.lint.boxModel" lsp-css-lint-box-model)
   ("css.lint.importStatement" lsp-css-lint-import-statement)
   ("css.lint.emptyRules" lsp-css-lint-empty-rules)
   ("css.lint.duplicateProperties" lsp-css-lint-duplicate-properties)
   ("css.lint.vendorPrefix" lsp-css-lint-vendor-prefix)
   ("css.lint.compatibleVendorPrefixes" lsp-css-lint-compatible-vendor-prefixes)
   ("css.validate" lsp-css-validate t)
   ("css.completion.triggerPropertyValueCompletion" lsp-css-completion-trigger-property-value-completion t)
   ("css.experimental.customData" lsp-css-experimental-custom-data)))

(defun lsp-css--server-command ()
  "Generate startup command for CSS language server."
  (list (lsp-package-path 'css-languageserver) "--stdio"))

;;; CSS
(lsp-defun lsp-css--apply-code-action ((&Command :arguments?))
  "Apply ACTION as workspace edit command."
  (lsp--apply-text-edits (cl-caddr arguments?) 'code-action))

(lsp-dependency 'css-languageserver
                '(:system "vscode-css-language-server")
                '(:npm :package "vscode-langservers-extracted"
                       :path "vscode-css-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-css--server-command)
  :activation-fn (lsp-activate-on "css" "scss" "sass" "less")
  :priority -1
  :action-handlers (lsp-ht ("_css.applyCodeAction" #'lsp-css--apply-code-action))
  :server-id 'css-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'css-languageserver callback error-callback))))

(lsp-consistency-check lsp-css)

(provide 'lsp-css)
;;; lsp-css.el ends here
