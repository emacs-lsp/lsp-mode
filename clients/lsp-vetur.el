;;; lsp-vetur.el --- vls configuration                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski
;; Copyright (C) 2019-2026 lsp-mode maintainers

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
;; vls shares the same format configurations with ts-ls.
(require 'lsp-javascript)

(defgroup lsp-vetur nil
  "LSP support for Vue, using the Vue Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vuejs/vetur/tree/master/server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-vetur-ignore-project-warning nil
  "Ignore projects without jsconfig.json or tsconfig.json warnings."
  :type 'boolean
  :group 'lsp-vetur
  :package-version '(lsp-mode . "9.0.0"))

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

(defcustom lsp-vetur-completion-scaffold-snippet-sources
  '((workspace . "(W)")
    (user . "(U)")
    (vetur . "(V)"))
  "Where Vetur source Scaffold Snippets from and how to indicate them.
- workspace: <WORKSPACE>/.vscode/vetur/snippets.
- user: <USER-DATA-DIR>/User/snippets/vetur.
- vetur: Bundled in Vetur.
The source value can be a string \"(User)\" or an emoji \"✌\".
Set a source to \"\" to disable it.
"
  :type 'alist
  :group 'lsp-vetur
  :link '(url-link "https://vuejs.github.io/vetur/guide/snippet.html")
  :package-version '(lsp-mode. "9.0.0"))

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

(defcustom lsp-vetur-emmet "never"
  "Controls the Emmet suggestions that show up in the suggestion/completion list."
  :type  '(choice
           (const "never")
           (const "inMarkupAndStylesheetFilesOnly")
           (const "always" ))
  :group 'lsp-vetur
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings
 '(("vetur.trace.server" lsp-vetur-trace-server)
   ("vetur.ignoreProjectWarning" lsp-vetur-ignore-project-warning t)
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
   ("vetur.completion.scaffoldSnippetSources" lsp-vetur-completion-scaffold-snippet-sources)
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
