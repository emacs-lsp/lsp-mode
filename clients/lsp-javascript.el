;;; lsp-javascript.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp,

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

;; LSP Clients for the JavaScript and TypeScript Programming Languages.

;;; Code:

(require 'lsp-mode)

(lsp-dependency 'javascript-typescript-langserver
                '(:system "javascript-typescript-stdio")
                '(:npm :package "javascript-typescript-langserver"
                       :path "javascript-typescript-stdio"))

(defgroup lsp-typescript-javascript nil
  "Support for TypeScript/JavaScript, using Sourcegraph's JavaScript/TypeScript language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sourcegraph/javascript-typescript-langserver"))

;; Original name can be confused with initializationOptions. Preferences is just one option of initializationOptions.
(define-obsolete-variable-alias
  'lsp-clients-typescript-init-opts
  'lsp-clients-typescript-preferences
  "lsp-mode 8.0.1")

(defcustom lsp-clients-typescript-javascript-server-args '()
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript-javascript
  :risky t
  :type '(repeat string))

(defun lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
  "Check if the js-ts lsp server should be enabled based on FILENAME."
  (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
      (and (derived-mode-p 'js-mode 'typescript-mode 'typescript-ts-mode)
           (not (derived-mode-p 'json-mode)))))

;; Unmaintained sourcegraph server
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons (lsp-package-path 'javascript-typescript-langserver)
                                                                lsp-clients-typescript-javascript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -3
                  :completion-in-comments? t
                  :server-id 'jsts-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'javascript-typescript-langserver
                                         callback
                                         error-callback))
                  :initialized-fn (lambda (_workspace)
                                    (warn (concat "The javascript-typescript-langserver (jsts-ls) is unmaintained; "
                                                  "it is recommended to use ts-ls or deno-ls instead.")))))

(defgroup lsp-typescript nil
  "LSP support for TypeScript, using Theia/Typefox's TypeScript Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/theia-ide/typescript-language-server"))

(defcustom lsp-clients-typescript-tls-path "typescript-language-server"
  "Path to the typescript-language-server binary."
  :group 'lsp-typescript
  :risky t
  :type 'string)

(defcustom lsp-clients-typescript-server-args '("--stdio")
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-typescript-disable-automatic-typing-acquisition nil
  "Disable tsserver from automatically fetching missing type definitions.
\(@types packages) for external modules."
  :group 'lsp-typescript
  :type 'boolean)

(defcustom lsp-clients-typescript-log-verbosity "info"
  "The verbosity level of the information printed in the log by tsserver."
  :group 'lsp-typescript
  :type '(choice
          (const "off")
          (const "terse")
          (const "normal")
          (const "requesttime")
          (const "verbose")))

(defcustom lsp-clients-typescript-max-ts-server-memory nil
  "The maximum size of the V8's old memory section in megabytes.
\(for example 4096 means 4GB). The default value is dynamically configured
by Node so can differ per system. Increase for very big projects that
exceed allowed memory usage."
  :group 'lsp-typescript
  :type 'integer)

(defcustom lsp-clients-typescript-npm-location nil
  "Specifies the path to the NPM exec used for Automatic Type Acquisition."
  :group 'lsp-typescript
  :type 'string)

(defcustom lsp-clients-typescript-prefer-use-project-ts-server nil
  "When set, prefers using the tsserver.js from your project. This
can allow loading plugins configured in your tsconfig.json."
  :group 'lsp-typescript
  :type 'boolean)

(defcustom lsp-clients-typescript-plugins (vector)
  "The list of plugins to load.
It should be a vector of plist with keys `:location' and `:name'
where `:name' is the name of the package and `:location' is the
directory containing the package. Example:
\(vector
   \(list :name \"@vsintellicode/typescript-intellicode-plugin\"
         :location \"<path>.vscode/extensions/visualstudioexptteam.
                            vscodeintellicode-1.1.9/\"))"
  :group 'lsp-typescript
  :type  '(restricted-sexp :tag "Vector"
                           :match-alternatives
                           (lambda (xs)
                             (and (vectorp xs) (seq-every-p
                                                (-lambda ((&plist :name :location))
                                                  (and name location))
                                                xs)))))

(defcustom lsp-clients-typescript-preferences nil
  "Preferences passed to the Typescript (tsserver) process.
See https://github.com/typescript-language-server/typescript-language-server#initializationoptions for the list of preferences available in the latest version of TypeScript."
  :group 'lsp-typescript
  :type 'plist)

(defcustom lsp-clients-typescript-tsserver nil
  "Options related to the tsserver process. See below for more info.
See https://github.com/typescript-language-server/typescript-language-server#initializationoptions for the list of tsserver available in the latest version of TypeScript."
  :group 'lsp-typescript
  :type 'plist)

(defcustom lsp-typescript-tsdk nil
  "Specifies the folder path containing tsserver and lib*.d.ts files to use."
  :type '(repeat string)
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-disable-automatic-type-acquisition nil
  "Disables automatic type acquisition.
Automatic type acquisition fetches `@types` packages from npm to improve
IntelliSense for external libraries."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-npm nil
  "Specifies the path to the NPM exec used for Automatic Type Acquisition.
Requires using TypeScript 2.3.4 or newer in the
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
  "Enable/disable implementations CodeLens.
This CodeLens shows the implementers of an interface."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsserver-log "off"
  "Enables logging of the TS server to a file.
This log can be used to diagnose TS Server issues. The log may contain file
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
  "Additional paths to discover Typescript Language Service plugins.
Requires using TypeScript 2.3.0 or newer in the
workspace."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-tsserver-trace "off"
  "Enables tracing of messages sent to the TS server.
This trace can be used to diagnose TS Server issues. The trace may contain
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
  "Defines space handling after the constructor keyword.
Requires using TypeScript 2.3.0 or newer in the workspace."
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
  "Defines space handling after keywords in a control flow statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions t
  "Defines space handling after function keyword for anonymous functions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-before-function-parenthesis nil
  "Defines space handling before function argument parentheses."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-empty-braces nil
  "Defines space handling after opening/before closing empty braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
  "Defines space handling after opening/before closing non-empty parenthesis."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
  "Defines space handling after opening and before closing non-empty brackets."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
  "Defines space handling after opening and before closing non-empty braces.
Requires using TypeScript 2.3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
  "Defines space handling after opening/before closing template string braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
  "Defines space handling after opening/before closing JSX expression braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-insert-space-after-type-assertion nil
  "Defines space handling after type assertions in TypeScript.
Requires using TypeScript 2.4 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-place-open-brace-on-new-line-for-functions nil
  "Defines whether an open brace is put onto a new line for functions or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks nil
  "Defines whether an open brace is put onto a newline for control blocks."
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
  "Defines space handling after the constructor keyword.
Requires using TypeScript 2.3.0 or newer in the workspace."
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
  "Defines space handling after keywords in a control flow statement."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions t
  "Defines space handling after function keyword for anonymous functions."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-before-function-parenthesis nil
  "Defines space handling before function argument parentheses."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-empty-braces nil
  "Defines space handling after opening/before closing empty braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
  "Defines space handling after opening and before closing non-empty parenthesis."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
  "Defines space handling after opening and before closing non-empty brackets."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
  "Defines space handling after opening and before closing non-empty braces.
Requires using TypeScript 2.3.0 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
  "Defines space handling after opening/before closing template string braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
  "Defines space handling after opening/before closing JSX expression braces."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-place-open-brace-on-new-line-for-functions nil
  "Defines whether an open brace is put onto a new line for functions or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-format-place-open-brace-on-new-line-for-control-blocks nil
  "Defines whether an open brace is put onto a new line for control blocks or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-implicit-project-config-check-js nil
  "Enable/disable semantic checking of JavaScript files.
Existing jsconfig.json or tsconfig.json files override this setting.
Requires using TypeScript 2.3.1 or newer in the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-implicit-project-config-experimental-decorators nil
  nil
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-names t
  "Enable/disable including unique names from the file in JavaScript suggestions."
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
  "Enable/disable suggestions for paths in import statements and require calls."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-typescript-suggest-paths t
  "Enable/disable suggestions for paths in import statements and require calls."
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-javascript-suggest-auto-imports t
  "Enable/disable auto import suggestions.
Requires using TypeScript 2.6.1 or newer in the workspace."
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

(defcustom lsp-javascript-display-enum-member-value-hints nil
  "Show inlay hints for enum member values."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-return-type-hints nil
  "Show inlay hints for function return types."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-parameter-type-hints nil
  "Show inlay hints for function parameters."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-parameter-name-hints "none"
  "Level of hinting for parameter types."
  :type '(choice (const :tag "none" "none")
                 (const :tag "literals" "literals")
                 (const :tag "all" "all"))
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-parameter-name-hints-when-argument-matches-name nil
  "Show inlay hints for function parameters even when argument matches
name (e.g. `data' variable passed as `data' parameter)."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-property-declaration-type-hints nil
  "Show inlay hints for property declaration types."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-display-variable-type-hints nil
  "Show inlay hints for variable types."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-javascript-completions-complete-function-calls t
  "Complete function calls."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-custom-settings
 '(("javascript.autoClosingTags" lsp-javascript-auto-closing-tags t)
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
   ("javascript.format.enable" lsp-javascript-format-enable t)
   ("javascript.format.insertSpaceAfterCommaDelimiter" lsp-javascript-format-insert-space-after-comma-delimiter t)
   ("javascript.format.insertSpaceAfterConstructor" lsp-javascript-format-insert-space-after-constructor t)
   ("javascript.format.insertSpaceAfterFunctionKeywordForAnonymousFunctions" lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions t)
   ("javascript.format.insertSpaceAfterKeywordsInControlFlowStatements" lsp-javascript-format-insert-space-after-keywords-in-control-flow-statements t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingEmptyBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-empty-braces t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis" lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis t)
   ("javascript.format.insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces" lsp-javascript-format-insert-space-after-opening-and-before-closing-template-string-braces t)
   ("javascript.format.insertSpaceAfterSemicolonInForStatements" lsp-javascript-format-insert-space-after-semicolon-in-for-statements t)
   ("javascript.format.insertSpaceBeforeAndAfterBinaryOperators" lsp-javascript-format-insert-space-before-and-after-binary-operators t)
   ("javascript.format.insertSpaceBeforeFunctionParenthesis" lsp-javascript-format-insert-space-before-function-parenthesis t)
   ("javascript.format.placeOpenBraceOnNewLineForControlBlocks" lsp-javascript-format-place-open-brace-on-new-line-for-control-blocks t)
   ("javascript.format.placeOpenBraceOnNewLineForFunctions" lsp-javascript-format-place-open-brace-on-new-line-for-functions t)
   ("typescript.autoClosingTags" lsp-typescript-auto-closing-tags t)
   ("typescript.check.npmIsInstalled" lsp-typescript-check-npm-is-installed t)
   ("typescript.disableAutomaticTypeAcquisition" lsp-typescript-disable-automatic-type-acquisition t)
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
   ("typescript.format.enable" lsp-typescript-format-enable t)
   ("typescript.format.insertSpaceAfterCommaDelimiter" lsp-typescript-format-insert-space-after-comma-delimiter t)
   ("typescript.format.insertSpaceAfterConstructor" lsp-typescript-format-insert-space-after-constructor t)
   ("typescript.format.insertSpaceAfterFunctionKeywordForAnonymousFunctions" lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions t)
   ("typescript.format.insertSpaceAfterKeywordsInControlFlowStatements" lsp-typescript-format-insert-space-after-keywords-in-control-flow-statements t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces" lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t)
   ("typescript.format.insertSpaceAfterOpeningAndBeforeClosingEmptyBraces" lsp-typescript-format-insert-space-after-opening-and-before-closing-empty-braces t)
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
   ("typescript.inlayHints.includeInlayEnumMemberValueHints" lsp-javascript-display-enum-member-value-hints t)
   ("typescript.inlayHints.includeInlayFunctionLikeReturnTypeHints" lsp-javascript-display-return-type-hints t)
   ("typescript.inlayHints.includeInlayFunctionParameterTypeHints" lsp-javascript-display-parameter-type-hints t)
   ("typescript.inlayHints.includeInlayParameterNameHints" lsp-javascript-display-parameter-name-hints nil)
   ("typescript.inlayHints.includeInlayParameterNameHintsWhenArgumentMatchesName" lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
   ("typescript.inlayHints.includeInlayPropertyDeclarationTypeHints" lsp-javascript-display-property-declaration-type-hints t)
   ("typescript.inlayHints.includeInlayVariableTypeHints" lsp-javascript-display-variable-type-hints t)
   ("javascript.inlayHints.includeInlayEnumMemberValueHints" lsp-javascript-display-enum-member-value-hints t)
   ("javascript.inlayHints.includeInlayFunctionLikeReturnTypeHints" lsp-javascript-display-return-type-hints t)
   ("javascript.inlayHints.includeInlayFunctionParameterTypeHints" lsp-javascript-display-parameter-type-hints t)
   ("javascript.inlayHints.includeInlayParameterNameHints" lsp-javascript-display-parameter-name-hints nil)
   ("javascript.inlayHints.includeInlayParameterNameHintsWhenArgumentMatchesName" lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
   ("javascript.inlayHints.includeInlayPropertyDeclarationTypeHints" lsp-javascript-display-property-declaration-type-hints t)
   ("javascript.inlayHints.includeInlayVariableTypeHints" lsp-javascript-display-variable-type-hints t)
   ("completions.completeFunctionCalls" lsp-javascript-completions-complete-function-calls t)))

(lsp-dependency 'typescript-language-server
                '(:system lsp-clients-typescript-tls-path)
                '(:npm :package "typescript-language-server"
                       :path "typescript-language-server"))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(defun lsp-javascript--rename (_workspace args)
  (let ((path (lsp--uri-to-path (lsp-get (lsp-get args :textDocument) :uri))))
    (if (f-exists? path)
        (with-current-buffer (find-file path)
          (goto-char (lsp--position-to-point
                      (lsp-get args :position))))
      (error "There is no file %s" path)))
  (call-interactively #'lsp-rename)
  nil)

(defun lsp-javascript-rename-file ()
  "Rename current file and all it's references in other files."
  (interactive)
  (let* ((name (buffer-name))
         (old (buffer-file-name))
         (basename (file-name-nondirectory old)))
    (unless (and old (file-exists-p old))
      (error "Buffer '%s' is not visiting a file." name))
    (let ((new (read-file-name "New name: " (file-name-directory old) basename nil basename)))
      (when (get-file-buffer new)
        (error "A buffer named '%s' already exists." new))
      (when (file-exists-p new)
        (error "A file named '%s' already exists." new))
      (lsp--send-execute-command
       "_typescript.applyRenameFile"
       (vector (list :sourceUri (lsp--buffer-uri)
                     :targetUri (lsp--path-to-uri new))))
      (mkdir (file-name-directory new) t)
      (rename-file old new)
      (rename-buffer new)
      (set-visited-file-name new)
      (set-buffer-modified-p nil)
      (lsp-disconnect)
      (setq-local lsp-buffer-uri nil)
      (lsp)
      (lsp--info "Renamed '%s' to '%s'." name (file-name-nondirectory new)))))

(defun lsp-javascript-initialized? ()
  (when-let ((workspace (lsp-find-workspace 'ts-ls (buffer-file-name))))
    (eq 'initialized (lsp--workspace-status workspace))))

(defun lsp-clients-typescript-project-ts-server-path ()
  (f-join (lsp-workspace-root) "node_modules" "typescript" "lib" "tsserver.js"))

(defun lsp-clients-typescript-server-path ()
  (cond
   ((and
     lsp-clients-typescript-prefer-use-project-ts-server
     (f-exists? (lsp-clients-typescript-project-ts-server-path)))
    (lsp-clients-typescript-project-ts-server-path))
   (t
    (lsp-package-path 'typescript))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          `(,(lsp-package-path 'typescript-language-server)
                                                            "--tsserver-path"
                                                            ,(lsp-clients-typescript-server-path)
                                                            ,@lsp-clients-typescript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -2
                  :completion-in-comments? t
                  :initialization-options (lambda ()
                                            (append
                                             (when lsp-clients-typescript-disable-automatic-typing-acquisition
                                               (list :disableAutomaticTypingAcquisition lsp-clients-typescript-disable-automatic-typing-acquisition))
                                             (when lsp-clients-typescript-log-verbosity
                                               (list :logVerbosity lsp-clients-typescript-log-verbosity))
                                             (when lsp-clients-typescript-max-ts-server-memory
                                               (list :maxTsServerMemory lsp-clients-typescript-max-ts-server-memory))
                                             (when lsp-clients-typescript-npm-location
                                               (list :npmLocation lsp-clients-typescript-npm-location))
                                             (when lsp-clients-typescript-plugins
                                               (list :plugins lsp-clients-typescript-plugins))
                                             (when lsp-clients-typescript-preferences
                                               (list :preferences lsp-clients-typescript-preferences))
                                             (when lsp-clients-typescript-tsserver
                                               (list :tsserver lsp-clients-typescript-tsserver))))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (ht-merge (lsp-configuration-section "javascript")
                                                 (lsp-configuration-section "typescript")
                                                 (lsp-configuration-section "completions")
                                                 (lsp-configuration-section "diagnostics"))))
                                    (let ((caps (lsp--workspace-server-capabilities workspace))
                                          (format-enable (or lsp-javascript-format-enable lsp-typescript-format-enable)))
                                      (lsp:set-server-capabilities-document-formatting-provider? caps format-enable)
                                      (lsp:set-server-capabilities-document-range-formatting-provider? caps format-enable)))
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls
                  :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'typescript
                                         (-partial #'lsp-package-ensure
                                                   'typescript-language-server
                                                   callback
                                                   error-callback)
                                         error-callback))))


(defgroup lsp-flow nil
  "LSP support for the Flow Javascript type checker."
  :group 'lsp-mode
  :link '(url-link "https://flow.org"))

(defcustom lsp-clients-flow-server "flow"
  "The Flow executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-flow
  :risky t
  :type 'file)

(defcustom lsp-clients-flow-server-args '("lsp")
  "Extra arguments for starting the Flow language server."
  :group 'lsp-flow
  :risky t
  :type '(repeat string))

(defun lsp-clients-flow-tag-file-present-p (file-name)
  "Check if the '// @flow' or `/* @flow */' tag is present in
the contents of FILE-NAME."
  (if-let ((buffer (find-buffer-visiting file-name)))
      (with-current-buffer buffer
        (lsp-clients-flow-tag-string-present-p))
    (with-temp-buffer
      (insert-file-contents file-name)
      (lsp-clients-flow-tag-string-present-p))))

(defun lsp-clients-flow-tag-string-present-p ()
  "Helper for `lsp-clients-flow-tag-file-present-p' that works
with the file contents."
  (save-excursion
    (goto-char (point-min))
    (let (stop found)
      (while (not stop)
        (unless (re-search-forward "[^\n[:space:]]" nil t)
          (setq stop t))
        (if (= (point) (point-min)) (setq stop t) (backward-char))
        (cond ((or (looking-at "//+[ ]*@flow")
                   (looking-at "/\\**[ ]*@flow")
                   (looking-at "[ ]*\\*[ ]*@flow"))
               (setq found t) (setq stop t))
              ((or (looking-at "//") (looking-at "*"))
               (forward-line))
              ((looking-at "/\\*")
               (save-excursion
                 (unless (re-search-forward "*/" nil t) (setq stop t)))
               (forward-line))
              (t (setq stop t))))
      found)))

(defun lsp-clients-flow-project-p (file-name)
  "Check if FILE-NAME is part of a Flow project, that is, if
there is a .flowconfig file in the folder hierarchy."
  (locate-dominating-file file-name ".flowconfig"))

(defun lsp-clients-flow-activate-p (file-name _mode)
  "Check if the Flow language server should be enabled for a
particular FILE-NAME and MODE."
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (not (derived-mode-p 'json-mode))
       (or (lsp-clients-flow-project-p file-name)
           (lsp-clients-flow-tag-file-present-p file-name))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection (lambda ()
                                          (cons lsp-clients-flow-server
                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))

(defgroup lsp-deno nil
  "LSP support for the Deno language server."
  :group 'lsp-mode
  :link '(url-link "https://deno.land/"))

(defcustom lsp-clients-deno-server "deno"
  "The Deno executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-server-args '("lsp")
  "Extra arguments for starting the Deno language server."
  :group 'lsp-deno
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-enable-lint t
  "Controls if linting information will be provided by the Deno Language Server."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-enable-code-lens-references t
  "Enables or disables the display of code lens information."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-enable-code-lens-references-all-functions t
  "Enables or disables the display of code lens information for all functions.
Setting this variable to `non-nil' implicitly enables
`lsp-clients-deno-enable-code-lens-references'."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-enable-code-lens-implementations t
  "Enables or disables the display of code lens information for implementations."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-config nil
  "The file path to a tsconfig.json file.
The path can be either be relative to the workspace, or an
absolute path.

Examples: `./tsconfig.json',
`/path/to/tsconfig.json', `C:\\path\\to\\tsconfig.json'"
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-import-map nil
  "The file path to an import map.
Import maps provide a way to relocate modules based on their
specifiers.  The path can either be relative to the workspace, or
an absolute path.

Examples: `./import-map.json',
`/path/to/import-map.json', `C:\\path\\to\\import-map.json'."
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clients-deno-enable-unstable nil
  "Controls if code will be type checked with Deno's unstable APIs."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-clients-deno--make-init-options ()
  "Initialization options for the Deno language server."
  `(:enable t
    :config ,lsp-clients-deno-config
    :importMap ,lsp-clients-deno-import-map
    :lint ,(lsp-json-bool lsp-clients-deno-enable-lint)
    :unstable ,(lsp-json-bool lsp-clients-deno-enable-unstable)
    :codeLens (:implementations ,(lsp-json-bool lsp-clients-deno-enable-code-lens-implementations)
               :references ,(lsp-json-bool (or lsp-clients-deno-enable-code-lens-references
                                               lsp-clients-deno-enable-code-lens-references-all-functions))
               :referencesAllFunctions ,(lsp-json-bool lsp-clients-deno-enable-code-lens-references-all-functions))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection (lambda ()
                                          (cons lsp-clients-deno-server
                                                lsp-clients-deno-server-args)))
                  :initialization-options #'lsp-clients-deno--make-init-options
                  :priority -5
                  :activation-fn #'lsp-typescript-javascript-tsx-jsx-activate-p
                  :server-id 'deno-ls))

(lsp-consistency-check lsp-javascript)

(provide 'lsp-javascript)
;;; lsp-javascript.el ends here
