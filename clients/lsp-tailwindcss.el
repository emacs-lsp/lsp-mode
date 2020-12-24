;;; lsp-tailwindcss.el --- lsp-mode tailwindcss integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Takeshi Tsukamoto

;; Author: Takeshi Tsukamoto <dev@itome.team>
;; Keywords: languages

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


(defgroup lsp-tailwindcss nil
  "Tailwind CSS language server group."
  :group 'lsp-mode
  :link '(url-link "https://tailwindcss.com/docs/intellisense"))

(defcustom lsp-tailwindcss-unzipped-path (f-join lsp-server-install-dir "tailwindcss/unzipped")
  "The path to the file in which `tailwindcss' will be stored."
  :type 'file
  :group 'lsp-tailwindcss
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-enable t
  "Controls whether tailwindcss is enabled for JavaScript files or not."
  :type 'boolean
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-server-command `("node"
                                            ,(f-join lsp-tailwindcss-unzipped-path "extension/dist/server/index.js")
                                            "--stdio")
  "Command to start tailwindcss intellisense server."
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-include-languages '()
  "Enable features in languages that are not supported by default.
  Add a mapping here between the new language and an already supported language.
  E.g.: `{\"plaintext\": \"html\"}`"
  :type 'alist
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-emmet-completions nil
  "Enable completions when using Emmet-style syntax,
  for example div.bg-red-500.uppercase. Default: false"
  :type 'boolean
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-color-decorators "inherit"
  "Controls whether the editor should render inline color decorators
  for Tailwind CSS classes and helper functions."
  :type '(choice
          (const "inherit")
          (const "on")
          (const "off"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-validate t
  "Enable linting. Rules can be configured individually
  using the `tailwindcss.lint.*` settings"
  :type 'boolean
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-css-conflict "warning"
  "Class names on the same HTML element which apply the same CSS property or properties"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-invalid-apply "error"
  "Unsupported use of the @apply directive(https://tailwindcss.com/docs/functions-and-directives/#apply)"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-invalid-screen "error"
  "Unknown screen name used with the
  @screen directive(https://tailwindcss.com/docs/functions-and-directives/#screen)"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-invalid-variant "error"
  "Unknown variant name used with the
  @variants directive(https://tailwindcss.com/docs/functions-and-directives/#variants)"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-invalid-config-path "error"
  "Unknown or invalid path used with the
  theme helper(https://tailwindcss.com/docs/functions-and-directives/#theme)"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-lint-invalid-tailwind-directive "error"
  "Unknown value used with the
  @tailwind directive(https://tailwindcss.com/docs/functions-and-directives/#tailwind)"
  :type '(choice
          (const "ignore")
          (const "warning")
          (const "error"))
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-experimental-class-regex '()
  ""
  :type '(repeat string)
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-tailwindcss-experimental-show-pixel-values '()
  ""
  :type '(repeat string)
  :package-version '(lsp-mode . "7.1"))

(lsp-register-custom-settings
 '(("tailwindCSS.includeLanguages" lsp-tailwindcss-include-languages)
   ("tailwindCSS.emmetCompletions" lsp-tailwindcss-emmet-completions)
   ("tailwindCSS.colorDecorators" lsp-tailwindcss-color-decorators)
   ("tailwindCSS.validate" lsp-tailwindcss-validate)
   ("tailwindCSS.lint.cssConflict" lsp-tailwindcss-lint-css-conflict)
   ("tailwindCSS.lint.invalidApply" lsp-tailwindcss-lint-invalid-apply)
   ("tailwindCSS.lint.invalidScreen" lsp-tailwindcss-lint-invalid-screen)
   ("tailwindCSS.lint.invalidVariant" lsp-tailwindcss-lint-invalid-variant)
   ("tailwindCSS.lint.invalidConfigPath" lsp-tailwindcss-lint-invalid-config-path)
   ("tailwindCSS.lint.invalidTailwindDirective" lsp-tailwindcss-lint-invalid-tailwind-directive)
   ("tailwindCSS.experimental.classRegex" lsp-tailwindcss-experimental-class-regex)
   ("tailwindCSS.experimental.showPixelValues" lsp-tailwindcss-experimental-show-pixel-values)))

(defun lsp-tailwindcss--get-configuration (_workspace _)
  (ht-merge
   (ht ("tabSize" (symbol-value (lsp--get-indent-width major-mode))))
   (lsp-configuration-section "tailwindcss")))

(defun lsp-tailwindcss--server-exists? (tailwindcss-server-command)
  (let* ((command-name (f-base (f-filename (cl-first tailwindcss-server-command))))
         (first-argument (cl-second tailwindcss-server-command))
         (first-argument-exist (and first-argument (file-exists-p first-argument))))
    (if (equal command-name "node")
        first-argument-exist
      (executable-find (cl-first tailwindcss-server-command)))))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda () lsp-tailwindcss-server-command)
   (lambda () (lsp-tailwindcss--server-exists? lsp-tailwindcss-server-command)))
  :priority 0
  :add-on? t
  :server-id 'tailwindcss
  :activation-fn (lambda (filename &optional _)
                   (when lsp-tailwindcss-enable
                     (or (string-match-p (rx (one-or-more anything) "."
                                             (or "ts" "js" "jsx" "tsx" "html" "vue" "css")eos)
                                         filename)
                         (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode 'css-mode))))
  :notification-handlers (ht
                          ("tailwindcss/configUpdated" #'ignore)
                          ("tailwindcss/configError" #'ignore)
                          ("tailwindcss/getConfiguration" #'lsp-tailwindcss--get-configuration)
                          ("tailwindcss/getDocumentSymbols" #'lsp--get-document-symbols))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (let ((tmp-zip (make-temp-file "ext" nil ".zip")))
                          (delete-file tmp-zip)
                          (lsp-download-install
                           (lambda (&rest _)
                             (condition-case err
                                 (progn
                                   (lsp-unzip tmp-zip lsp-tailwindcss-unzipped-path)
                                   (funcall callback))
                               (error (funcall error-callback err))))
                           error-callback
                           :url (lsp-vscode-extension-url "bradlc" "vscode-tailwindcss" "0.5.6")
                           :store-path tmp-zip)))))

(provide 'lsp-tailwindcss)
;;; lsp-tailwindcss.el ends here
