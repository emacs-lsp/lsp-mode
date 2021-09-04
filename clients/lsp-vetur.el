;;; lsp-vetur.el --- vls configuration                 -*- lexical-binding: t; -*-

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

;; VLS configuration

;;; Code:

(require 'lsp-mode)
(require 'lsp-html)

(defgroup lsp-vetur nil
  "LSP support for Vue, using the Vue Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vuejs/vetur/tree/master/server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-use-workspace-dependencies nil
  "Use dependencies from workspace. Currently only for
TypeScript."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-completion-auto-import t
  "Include completion for module export and auto import them"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-completion-use-scaffold-snippets t
  "Enable/disable Vetur's built-in scaffolding snippets"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-completion-tag-casing "kebab"
  "Casing conversion for tag completion"
  :type '(choice
          (const "initial")
          (const "kebab"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-grammar-custom-blocks '((docs . "md") (i18n . "json"))
  "Mapping from custom block tag name to language name. Used for
 generating grammar to support syntax highlighting for custom
 blocks."
  :type 'alist
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-validation-template t
  "Validate vue-html in <template> using eslint-plugin-vue"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-language-features-code-actions t
  "Enable/disable code actions."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-vetur-validation-style t
  "Validate css/scss/less/postcss in <style>"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-validation-script t
  "Validate js/ts in <script>"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-enable t
  "Enable/disable the Vetur document formatter."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-options-tab-size 2
  "Number of spaces per indentation level. Inherited by all formatters."
  :type 'number
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-options-use-tabs nil
  "Use tabs for indentation. Inherited by all formatters."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-html "prettyhtml"
  "Default formatter for <template> region"
  :type '(choice
          (const "none")
          (const "prettyhtml")
          (const "js-beautify-html")
          (const "prettier"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-css "prettier"
  "Default formatter for <style> region"
  :type '(choice
          (const "none")
          (const "prettier"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-postcss "prettier"
  "Default formatter for <style lang='postcss'> region"
  :type '(choice
          (const "none")
          (const "prettier"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-scss "prettier"
  "Default formatter for <style lang='scss'> region"
  :type '(choice
          (const "none")
          (const "prettier"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-less "prettier"
  "Default formatter for <style lang='less'> region"
  :type '(choice
          (const "none")
          (const "prettier"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-stylus "stylus-supremacy"
  "Default formatter for <style lang='stylus'> region"
  :type '(choice
          (const "none")
          (const "stylus-supremacy"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-js "prettier"
  "Default formatter for <script> region"
  :type '(choice
          (const "none")
          (const "prettier")
          (const "prettier-eslint")
          (const "vscode-typescript"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-ts "prettier"
  "Default formatter for <script> region"
  :type '(choice
          (const "none")
          (const "prettier")
          (const "vscode-typescript"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-default-formatter-options
  '((js-beautify-html (wrap_attributes . "force-expand-multiline"))
    (prettyhtml (printWidth . 100)
                (singleQuote . :json-false)
                (wrapAttributes . :json-false)
                (sortAttributes . :json-false)))
  "Options for all default formatters"
  :type 'alist
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-style-initial-indent nil
  "Whether to have initial indent for <style> region"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-format-script-initial-indent nil
  "Whether to have initial indent for <script> region"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-trace-server "off"
  "Traces the communication between VS Code and Vue Language Server."
  :type '(choice
          (const "off")
          (const "messages")
          (const "verbose"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-dev-vls-path ""
  "The vls path for development"
  :type 'string
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-vetur-dev-vls-port -1
  "The vls port for development"
  :type 'integer
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-vetur-dev-log-level "INFO"
  "The vls log level for development"
  :type '(choice
          (const "INFO")
          (const "DEBUG"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-vetur-experimental-template-interpolation-service nil
  "Whether to have template interpolation service"
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-typescript-tsdk nil
  "Specifies the folder path containing the tsserver and
lib*.d.ts files to use."
  :type '(repeat string)
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-disable-automatic-type-acquisition nil
  "Disables automatic type acquisition. Automatic type
acquisition fetches `@types` packages from npm to improve
IntelliSense for external libraries."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-npm nil
  "Specifies the path to the NPM executable used for Automatic
Type Acquisition. Requires using TypeScript 2.3.4 or newer in the
workspace."
  :type '(repeat string)
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-check-npm-is-installed t
  "Check if NPM is installed for Automatic Type Acquisition."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-references-code-lens-enabled nil
  "Enable/disable references CodeLens in JavaScript files."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-references-code-lens-enabled nil
  "Enable/disable references CodeLens in TypeScript files."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-implementations-code-lens-enabled nil
  "Enable/disable implementations CodeLens. This CodeLens shows
the implementers of an interface."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsserver-log "off"
  "Enables logging of the TS server to a file. This log can be
used to diagnose TS Server issues. The log may contain file
paths, source code, and other potentially sensitive information
from your project."
  :type '(choice
          (const "off")
          (const "terse")
          (const "normal")
          (const "verbose"))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsserver-plugin-paths nil
  "Additional paths to discover Typescript Language Service
plugins. Requires using TypeScript 2.3.0 or newer in the
workspace."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsserver-trace "off"
  "Enables tracing of messages sent to the TS server. This trace
can be used to diagnose TS Server issues. The trace may contain
file paths, source code, and other potentially sensitive
information from your project."
  :type '(choice
          (const "off")
          (const "messages")
          (const "verbose"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-complete-function-calls nil
  "Complete functions with their parameter signature."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-complete-function-calls nil
  "Complete functions with their parameter signature."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-report-style-checks-as-warnings t
  "Report style checks as warnings."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-validate-enable t
  "Enable/disable TypeScript validation."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-enable t
  "Enable/disable default TypeScript formatter."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-comma-delimiter t
  "Defines space handling after a comma delimiter."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-constructor nil
  "Defines space handling after the constructor keyword. Requires
using TypeScript 2.3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-semicolon-in-for-statements t
  "Defines space handling after a semicolon in a for statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-before-and-after-binary-operators t
  "Defines space handling after a binary operator."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-keywords-in-control-flow-statements t
  "Defines space handling after keywords in a control flow
statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions t
  "Defines space handling after function keyword for anonymous
functions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-before-function-parenthesis nil
  "Defines space handling before function argument parentheses."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
  "Defines space handling after opening and before closing
non-empty parenthesis."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
  "Defines space handling after opening and before closing
non-empty brackets."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
  "Defines space handling after opening and before closing
non-empty braces. Requires using TypeScript 2.3.0 or newer in the
workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
  "Defines space handling after opening and before closing
template string braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
  "Defines space handling after opening and before closing JSX
expression braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-type-assertion nil
  "Defines space handling after type assertions in TypeScript.
Requires using TypeScript 2.4 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-place-open-brace-on-new-line-for-functions nil
  "Defines whether an open brace is put onto a new line for
functions or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks nil
  "Defines whether an open brace is put onto a new line for
control blocks or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-validate-enable t
  "Enable/disable JavaScript validation."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-enable t
  "Enable/disable default JavaScript formatter."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-comma-delimiter t
  "Defines space handling after a comma delimiter."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-constructor nil
  "Defines space handling after the constructor keyword. Requires
using TypeScript 2.3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-semicolon-in-for-statements t
  "Defines space handling after a semicolon in a for statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-before-and-after-binary-operators t
  "Defines space handling after a binary operator."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-keywords-in-control-flow-statements t
  "Defines space handling after keywords in a control flow
statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions t
  "Defines space handling after function keyword for anonymous
functions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-before-function-parenthesis nil
  "Defines space handling before function argument parentheses."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
  "Defines space handling after opening and before closing
non-empty parenthesis."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
  "Defines space handling after opening and before closing
non-empty brackets."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
  "Defines space handling after opening and before closing
non-empty braces. Requires using TypeScript 2.3.0 or newer in the
workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
  "Defines space handling after opening and before closing
template string braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
  "Defines space handling after opening and before closing JSX
expression braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-place-open-brace-on-new-line-for-functions nil
  "Defines whether an open brace is put onto a new line for
functions or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-place-open-brace-on-new-line-for-control-blocks nil
  "Defines whether an open brace is put onto a new line for
control blocks or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-implicit-project-config-check-js nil
  "Enable/disable semantic checking of JavaScript files. Existing
jsconfig.json or tsconfig.json files override this setting.
Requires using TypeScript 2.3.1 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-implicit-project-config-experimental-decorators nil
  nil
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-names t
  "Enable/disable including unique names from the file in
JavaScript suggestions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsc-auto-detect "on"
  "Controls auto detection of tsc tasks."
  :type '(choice
          (const "on")
          (const "off")
          (const "build")
          (const "watch"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-paths t
  "Enable/disable suggestions for paths in import statements and
require calls."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-paths t
  "Enable/disable suggestions for paths in import statements and
require calls."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-auto-imports t
  "Enable/disable auto import suggestions. Requires using
TypeScript 2.6.1 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-auto-imports t
  "Enable/disable auto import suggestions. Requires using
TypeScript 2.6.1 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-complete-js-docs t
  "Enable/disable suggestion to complete JSDoc comments."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-complete-js-docs t
  "Enable/disable suggestion to complete JSDoc comments."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-locale nil
  nil
  :type '(choice
          (const "de")
          (const "es")
          (const "en")
          (const "fr")
          (const "it")
          (const "ja")
          (const "ko")
          (const "ru")
          (const "zh-CN")
          (const "zh-TW")
          nil)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggestion-actions-enabled t
  "Enable/disable suggestion diagnostics for JavaScript files in
the editor. Requires using TypeScript 2.8 or newer in the
workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggestion-actions-enabled t
  "Enable/disable suggestion diagnostics for TypeScript files in
the editor. Requires using TypeScript 2.8 or newer in the
workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-preferences-quote-style "auto" nil
  :type '(choice
          (const "auto")
          (const "single")
          (const "double"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-preferences-quote-style "auto" nil
  :type '(choice
          (const "auto")
          (const "single")
          (const "double"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-preferences-import-module-specifier "auto"
  "Preferred path style for auto imports."
  :type '(choice
          (const "auto")
          (const "relative")
          (const "non-relative"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-preferences-import-module-specifier "auto"
  "Infer the shortest path type."
  :type '(choice
          (const "auto")
          (const "relative")
          (const "non-relative"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-preferences-rename-shorthand-properties t
  "Enable/disable introducing aliases for object shorthand
properties during renames. Requires using TypeScript 3.4 or newer
in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-preferences-rename-shorthand-properties t
  "Enable/disable introducing aliases for object shorthand
properties during renames. Requires using TypeScript 3.4 or newer
in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-update-imports-on-file-move-enabled "prompt"
  "Enable/disable automatic updating of import paths when you
rename or move a file in VS Code. Requires using TypeScript 2.9
or newer in the workspace."
  :type '(choice
          (const "prompt")
          (const "always")
          (const "never"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-update-imports-on-file-move-enabled "prompt"
  "Prompt on each rename."
  :type '(choice
          (const "prompt")
          (const "always")
          (const "never"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-auto-closing-tags t
  "Enable/disable automatic closing of JSX tags. Requires using
TypeScript 3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-auto-closing-tags t
  "Enable/disable automatic closing of JSX tags. Requires using
TypeScript 3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-enabled t
  "Enabled/disable autocomplete suggestions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-enabled t
  "Enabled/disable autocomplete suggestions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-surveys-enabled t
  "Enabled/disable occasional surveys that help us improve VS
Code's JavaScript and TypeScript support."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-emmet "never"
  "Controls the Emmet suggestions that show up in the suggestion/completion list."
  :type  '(choice
           (const "never")
           (const "inMarkupAndStylesheetFilesOnly")
           (const "always" ))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings
 '(("javascript.autoClosingTags" lsp-javascript-auto-closing-tags t)
   ("javascript.format.enable" lsp-javascript-format-enable t)
   ("javascript.format.insertSpaceAfterCommaDelimiter" lsp-javascript-format-insert-space-after-comma-delimiter t)
   ("javascript.format.insertSpaceAfterConstructor" lsp-javascript-format-insert-space-after-constructor t)
   ("javascript.format.insertSpaceAfterFunctionKeywordForAnonymousFunctions" lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions t)
   ("javascript.format.insertSpaceAfterKeywordsInControlFlowStatements" lsp-javascript-format-insert-space-after-keywords-in-control-flow-statements t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-template-string-braces t)
   ("javascript.format.insertSpaceAfterSemicolonInForStatements" lsp-javascript-format-insert-space-after-semicolon-in-for-statements t)
   ("javascript.format.insertSpaceBeforeAndAfterBinaryOperators" lsp-javascript-format-insert-space-before-and-after-binary-operators t)
   ("javascript.format.insertSpaceBeforeFunctionParenthesis" lsp-javascript-format-insert-space-before-function-parenthesis t)
   ("javascript.format.placeOpenBraceOnNewLineForControlBlocks" lsp-javascript-format-place-open-brace-on-new-line-for-control-blocks t)
   ("javascript.format.placeOpenBraceOnNewLineForFunctions" lsp-javascript-format-place-open-brace-on-new-line-for-functions t)
   ("javascript.implicitProjectConfig.checkJs" lsp-javascript-implicit-project-config-check-js t)
   ("javascript.implicitProjectConfig.experimentalDecorators" lsp-javascript-implicit-project-config-experimental-decorators t)
   ("javascript.preferences.importModuleSpecifier" lsp-javascript-preferences-import-module-specifier)
   ("javascript.preferences.quoteStyle" lsp-javascript-preferences-quote-style)
   ("javascript.preferences.renameShorthandProperties" lsp-javascript-preferences-rename-shorthand-properties t)
   ("javascript.referencesCodeLens.enabled" lsp-javascript-references-code-lens-enabled t)
   ("javascript.suggest.autoImports" lsp-javascript-suggest-auto-imports t)
   ("javascript.suggest.completeFunctionCalls" lsp-javascript-suggest-complete-function-calls t)
   ("javascript.suggest.completeJSDocs" lsp-javascript-suggest-complete-js-docs t)
   ("javascript.suggest.enabled" lsp-javascript-suggest-enabled t)
   ("javascript.suggest.names" lsp-javascript-suggest-names t)
   ("javascript.suggest.paths" lsp-javascript-suggest-paths t)
   ("javascript.suggestionActions.enabled" lsp-javascript-suggestion-actions-enabled t)
   ("javascript.updateImportsOnFileMove.enabled" lsp-javascript-update-imports-on-file-move-enabled)
   ("javascript.validate.enable" lsp-javascript-validate-enable t)
   ("typescript.autoClosingTags" lsp-typescript-auto-closing-tags t)
   ("typescript.check.npmIsInstalled" lsp-typescript-check-npm-is-installed t)
   ("typescript.disableAutomaticTypeAcquisition" lsp-typescript-disable-automatic-type-acquisition t)
   ("typescript.format.enable" lsp-typescript-format-enable t)
   ("typescript.format.insertSpaceAfterCommaDelimiter" lsp-typescript-format-insert-space-after-comma-delimiter t)
   ("typescript.format.insertSpaceAfterConstructor" lsp-typescript-format-insert-space-after-constructor t)
   ("typescript.format.insertSpaceAfterFunctionKeywordForAnonymousFunctions" lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions t)
   ("typescript.format.insertSpaceAfterKeywordsInControlFlowStatements" lsp-typescript-format-insert-space-after-keywords-in-control-flow-statements t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces" lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets" lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-brackets t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis" lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces" lsp-typescript-format-insert-space-after-opening-and-before-closing-template-string-braces t)
   ("typescript.format.insertSpaceAfterSemicolonInForStatements" lsp-typescript-format-insert-space-after-semicolon-in-for-statements t)
   ("typescript.format.insertSpaceAfterTypeAssertion" lsp-typescript-format-insert-space-after-type-assertion t)
   ("typescript.format.insertSpaceBeforeAndAfterBinaryOperators" lsp-typescript-format-insert-space-before-and-after-binary-operators t)
   ("typescript.format.insertSpaceBeforeFunctionParenthesis" lsp-typescript-format-insert-space-before-function-parenthesis t)
   ("typescript.format.placeOpenBraceOnNewLineForControlBlocks" lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks t)
   ("typescript.format.placeOpenBraceOnNewLineForFunctions" lsp-typescript-format-place-open-brace-on-new-line-for-functions t)
   ("typescript.implementationsCodeLens.enabled" lsp-typescript-implementations-code-lens-enabled t)
   ("typescript.locale" lsp-typescript-locale)
   ("typescript.npm" lsp-typescript-npm)
   ("typescript.preferences.importModuleSpecifier" lsp-typescript-preferences-import-module-specifier)
   ("typescript.preferences.quoteStyle" lsp-typescript-preferences-quote-style)
   ("typescript.preferences.renameShorthandProperties" lsp-typescript-preferences-rename-shorthand-properties t)
   ("typescript.referencesCodeLens.enabled" lsp-typescript-references-code-lens-enabled t)
   ("typescript.reportStyleChecksAsWarnings" lsp-typescript-report-style-checks-as-warnings t)
   ("typescript.suggest.autoImports" lsp-typescript-suggest-auto-imports t)
   ("typescript.suggest.completeFunctionCalls" lsp-typescript-suggest-complete-function-calls t)
   ("typescript.suggest.completeJSDocs" lsp-typescript-suggest-complete-js-docs t)
   ("typescript.suggest.enabled" lsp-typescript-suggest-enabled t)
   ("typescript.suggest.paths" lsp-typescript-suggest-paths t)
   ("typescript.suggestionActions.enabled" lsp-typescript-suggestion-actions-enabled t)
   ("typescript.surveys.enabled" lsp-typescript-surveys-enabled t)
   ("typescript.tsc.autoDetect" lsp-typescript-tsc-auto-detect)
   ("typescript.tsdk" lsp-typescript-tsdk)
   ("typescript.tsserver.log" lsp-typescript-tsserver-log)
   ("typescript.tsserver.pluginPaths" lsp-typescript-tsserver-plugin-paths)
   ("typescript.tsserver.trace" lsp-typescript-tsserver-trace)
   ("typescript.updateImportsOnFileMove.enabled" lsp-typescript-update-imports-on-file-move-enabled)
   ("typescript.validate.enable" lsp-typescript-validate-enable t)
   ("vetur.trace.server" lsp-vetur-trace-server)
   ("vetur.format.scriptInitialIndent" lsp-vetur-format-script-initial-indent t)
   ("vetur.format.styleInitialIndent" lsp-vetur-format-style-initial-indent t)
   ("vetur.format.defaultFormatterOptions" lsp-vetur-format-default-formatter-options)
   ("vetur.format.defaultFormatter.ts" lsp-vetur-format-default-formatter-ts)
   ("vetur.format.defaultFormatter.js" lsp-vetur-format-default-formatter-js)
   ("vetur.format.defaultFormatter.stylus" lsp-vetur-format-default-formatter-stylus)
   ("vetur.format.defaultFormatter.less" lsp-vetur-format-default-formatter-less)
   ("vetur.format.defaultFormatter.scss" lsp-vetur-format-default-formatter-scss)
   ("vetur.format.defaultFormatter.postcss" lsp-vetur-format-default-formatter-postcss)
   ("vetur.format.defaultFormatter.css" lsp-vetur-format-default-formatter-css)
   ("vetur.format.defaultFormatter.html" lsp-vetur-format-default-formatter-html)
   ("vetur.format.options.useTabs" lsp-vetur-format-options-use-tabs t)
   ("vetur.format.options.tabSize" lsp-vetur-format-options-tab-size)
   ("vetur.format.enable" lsp-vetur-format-enable t)
   ("vetur.validation.script" lsp-vetur-validation-script t)
   ("vetur.validation.style" lsp-vetur-validation-style t)
   ("vetur.validation.template" lsp-vetur-validation-template t)
   ("vetur.languageFeatures.codeActions" lsp-vetur-language-features-code-actions t)
   ("vetur.grammar.customBlocks" lsp-vetur-grammar-custom-blocks)
   ("vetur.completion.tagCasing" lsp-vetur-completion-tag-casing)
   ("vetur.completion.useScaffoldSnippets" lsp-vetur-completion-use-scaffold-snippets t)
   ("vetur.completion.autoImport" lsp-vetur-completion-auto-import t)
   ("vetur.useWorkspaceDependencies" lsp-vetur-use-workspace-dependencies t)
   ("vetur.dev.vlsPath" lsp-vetur-dev-vls-path)
   ("vetur.dev.vlsPort" lsp-vetur-dev-vls-port)
   ("vetur.dev.logLevel" lsp-vetur-dev-log-level)
   ("vetur.experimental.templateInterpolationService" lsp-vetur-experimental-template-interpolation-service t)
   ("emmet.showExpandedAbbreviation" lsp-vetur-emmet)))

(define-obsolete-variable-alias
  'lsp-vetur-server
  'lsp-vetur-server-command
  "lsp-mode 6.1")

(defcustom lsp-vetur-global-snippets-dir (expand-file-name (locate-user-emacs-file ".snippets/vetur"))
  "Path to snippets dir."
  :type 'file
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-vetur-server-command '("vls")
  "Command to start vetur."
  :type '(repeat string)
  :risky t
  :package-version '(lsp-mode . "6.1"))

(lsp-dependency 'vetur-language-server
                '(:system "vls")
                '(:npm :package "vls" :path "vls"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find (cl-first lsp-vetur-server-command))
                                            (lsp-package-path 'vetur-language-server))
                                       ,@(cl-rest lsp-vetur-server-command))))
                  :activation-fn (lambda (filename _mode)
                                   (string= (file-name-extension filename) "vue"))
                  :priority -1
                  :multi-root t
                  :ignore-messages '("readFile .*? requested by Vue but content not available")
                  :server-id 'vls
                  :initialization-options (lambda () (ht-merge (lsp-configuration-section "vetur")
                                                               (lsp-configuration-section "html")
                                                               (lsp-configuration-section "javascript")
                                                               (lsp-configuration-section "typescript")
                                                               (lsp-configuration-section "emmet")
                                                               (ht ("globalSnippetDir" lsp-vetur-global-snippets-dir))))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (ht-merge (lsp-configuration-section "vetur")
                                                 (lsp-configuration-section "html")
                                                 (lsp-configuration-section "javascript")
                                                 (lsp-configuration-section "emmet")
                                                 (lsp-configuration-section "typescript")))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'vetur-language-server
                                                            callback error-callback))))

(lsp-consistency-check lsp-vetur)

(provide 'lsp-vetur)
;;; lsp-vetur.el ends here
