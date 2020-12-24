;;; lsp-svelte.el --- LSP Svelte integration -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Stepan Lusnikov

;; Author: Stepan Lusnikov <endenwer@gmail.com>
;; Keywords: lsp svelte

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

;; LSP client for Svelte

;;; Code:

(require 'lsp-mode)

(defgroup lsp-svelte nil
  "LSP support for Svelte."
  :group 'lsp-mode
  :link '(url-link
          "https://github.com/sveltejs/language-tools"))

(lsp-dependency 'svelte-language-server
                '(:system "svelteserver")
                '(:npm :package "svelte-language-server"
                       :path "svelteserver"))

(defcustom lsp-svelte-plugin-typescript-enable t
  "Enable the TypeScript plugin"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-diagnostics-enable t
  "Enable diagnostic messages for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-hover-enable t
  "Enable hover info for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-document-symbols-enable t
  "Enable document symbols for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-completions-enable t
  "Enable completions for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-find-references-enable t
  "Enable find-references for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-definitions-enable t
  "Enable go to definition for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-code-actions-enable t
  "Enable code actions for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-selection-range-enable t
  "Enable selection range for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-signature-help-enable t
  "Enable signature help (parameter hints) for TypeScript"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-typescript-rename-enable t
  "Enable rename functionality for JS/TS variables inside Svelte files"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-enable t
  "Enable the CSS plugin"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-globals ""
  "Which css files should be checked for global variables (`--global-var: value;`). These variables are added to the css completions. String of comma-separated file paths or globs relative to workspace root."
  :type 'string
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-diagnostics-enable t
  "Enable diagnostic messages for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-hover-enable t
  "Enable hover info for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-completions-enable t
  "Enable auto completions for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-completions-emmet t
  "Enable emmet auto completions for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-document-colors-enable t
  "Enable document colors for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-color-presentations-enable t
  "Enable color picker for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-document-symbols-enable t
  "Enable document symbols for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-css-selection-range-enable t
  "Enable selection range for CSS"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-enable t
  "Enable the HTML plugin"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-hover-enable t
  "Enable hover info for HTML"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-completions-enable t
  "Enable auto completions for HTML"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-completions-emmet t
  "Enable emmet auto completions for HTML"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-tag-complete-enable t
  "Enable HTML tag auto closing"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-html-document-symbols-enable t
  "Enable document symbols for HTML"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-enable t
  "Enable the Svelte plugin"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-diagnostics-enable t
  "Enable diagnostic messages for Svelte"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-compiler-warnings nil
  "Svelte compiler warning codes to ignore or to treat as errors.
Example: '((css-unused-selector . ignore) (unused-export-let . error))"
  :type '(alist :key-type (symbol :tag "Warning code")
                :value-type (choice
                             (const :tag "Ignore" ignore)
                             (const :tag "Treat as error" error)))
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-format-enable t
  "Enable formatting for Svelte (includes css & js)"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-completions-enable t
  "Enable auto completions for Svelte"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-hover-enable t
  "Enable hover information for Svelte"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-code-actions-enable t
  "Enable Code Actions for Svelte"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-selection-range-enable t
  "Enable selection range for Svelte"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-svelte-plugin-svelte-rename-enable t
  "Enable rename/move Svelte files functionality"
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(lsp-register-custom-settings
 '(("svelte.plugin.svelte.rename.enable" lsp-svelte-plugin-svelte-rename-enable t)
   ("svelte.plugin.svelte.selectionRange.enable" lsp-svelte-plugin-svelte-selection-range-enable t)
   ("svelte.plugin.svelte.codeActions.enable" lsp-svelte-plugin-svelte-code-actions-enable t)
   ("svelte.plugin.svelte.hover.enable" lsp-svelte-plugin-svelte-hover-enable t)
   ("svelte.plugin.svelte.completions.enable" lsp-svelte-plugin-svelte-completions-enable t)
   ("svelte.plugin.svelte.format.enable" lsp-svelte-plugin-svelte-format-enable t)
   ("svelte.plugin.svelte.compilerWarnings" lsp-svelte-plugin-svelte-compiler-warnings)
   ("svelte.plugin.svelte.diagnostics.enable" lsp-svelte-plugin-svelte-diagnostics-enable t)
   ("svelte.plugin.svelte.enable" lsp-svelte-plugin-svelte-enable t)
   ("svelte.plugin.html.documentSymbols.enable" lsp-svelte-plugin-html-document-symbols-enable t)
   ("svelte.plugin.html.tagComplete.enable" lsp-svelte-plugin-html-tag-complete-enable t)
   ("svelte.plugin.html.completions.emmet" lsp-svelte-plugin-html-completions-emmet t)
   ("svelte.plugin.html.completions.enable" lsp-svelte-plugin-html-completions-enable t)
   ("svelte.plugin.html.hover.enable" lsp-svelte-plugin-html-hover-enable t)
   ("svelte.plugin.html.enable" lsp-svelte-plugin-html-enable t)
   ("svelte.plugin.css.selectionRange.enable" lsp-svelte-plugin-css-selection-range-enable t)
   ("svelte.plugin.css.documentSymbols.enable" lsp-svelte-plugin-css-document-symbols-enable t)
   ("svelte.plugin.css.colorPresentations.enable" lsp-svelte-plugin-css-color-presentations-enable t)
   ("svelte.plugin.css.documentColors.enable" lsp-svelte-plugin-css-document-colors-enable t)
   ("svelte.plugin.css.completions.emmet" lsp-svelte-plugin-css-completions-emmet t)
   ("svelte.plugin.css.completions.enable" lsp-svelte-plugin-css-completions-enable t)
   ("svelte.plugin.css.hover.enable" lsp-svelte-plugin-css-hover-enable t)
   ("svelte.plugin.css.diagnostics.enable" lsp-svelte-plugin-css-diagnostics-enable t)
   ("svelte.plugin.css.globals" lsp-svelte-plugin-css-globals)
   ("svelte.plugin.css.enable" lsp-svelte-plugin-css-enable t)
   ("svelte.plugin.typescript.rename.enable" lsp-svelte-plugin-typescript-rename-enable t)
   ("svelte.plugin.typescript.signatureHelp.enable" lsp-svelte-plugin-typescript-signature-help-enable t)
   ("svelte.plugin.typescript.selectionRange.enable" lsp-svelte-plugin-typescript-selection-range-enable t)
   ("svelte.plugin.typescript.codeActions.enable" lsp-svelte-plugin-typescript-code-actions-enable t)
   ("svelte.plugin.typescript.definitions.enable" lsp-svelte-plugin-typescript-definitions-enable t)
   ("svelte.plugin.typescript.findReferences.enable" lsp-svelte-plugin-typescript-find-references-enable t)
   ("svelte.plugin.typescript.completions.enable" lsp-svelte-plugin-typescript-completions-enable t)
   ("svelte.plugin.typescript.documentSymbols.enable" lsp-svelte-plugin-typescript-document-symbols-enable t)
   ("svelte.plugin.typescript.hover.enable" lsp-svelte-plugin-typescript-hover-enable t)
   ("svelte.plugin.typescript.diagnostics.enable" lsp-svelte-plugin-typescript-diagnostics-enable t)
   ("svelte.plugin.typescript.enable" lsp-svelte-plugin-typescript-enable t)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'svelte-language-server)
                       "--stdio")))
  :activation-fn (lambda (file-name _mode)
                   (string= (f-ext file-name)
                            "svelte"))
  :initialization-options
  (lambda ()
    ;; XXX: workaround for https://github.com/Wilfred/ht.el/issues/38
    ;; Use `ht-get*' instead of `lsp--ht-get' when
    ;; https://github.com/Wilfred/ht.el/pull/39 is merged
    (list :config (lsp--ht-get (lsp-configuration-section "svelte.plugin")
                               "svelte"
                               "plugin")
          :prettierConfig (lsp-configuration-section "prettier")
          :emmetConfig (lsp-configuration-section "emmet")
          :typescriptConfig: (list :typescript (lsp-configuration-section "typescript")
                                   :javascript (lsp-configuration-section "javascript"))
          :dontFilterIncompleteCompletions t))
  :server-id 'svelte-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'svelte-language-server callback error-callback))
  :initialized-fn
  (lambda (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration
       (ht-merge (lsp-configuration-section "svelte")
                 (lsp-configuration-section "javascript")
                 (lsp-configuration-section "typescript")))
      (lsp--server-register-capability
       (lsp-make-registration
        :id "js/ts/id"
        :method "workspace/didChangeWatchedFiles"
        :register-options? (lsp-make-did-change-watched-files-registration-options
                            :watchers
                            (vector (lsp-make-file-system-watcher :glob-pattern "**/*.js")
                                    (lsp-make-file-system-watcher :glob-pattern "**/*.ts")))))))))

(provide 'lsp-svelte)
;;; lsp-svelte.el ends here
