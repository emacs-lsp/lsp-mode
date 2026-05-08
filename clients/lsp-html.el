;;; lsp-html.el --- vscode-html-languageserver configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Vibhav Pant
;; Copyright (C) 2019-2026 emacs-lsp maintainers

;; Author: Vibhav Pant <vibhavp@gmail.com>
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

;;; Code:

(require 'lsp-mode)

(defgroup lsp-html nil
  "LSP support for HTML, using vscode's built-in language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/microsoft/vscode/tree/main/extensions/html-language-features/server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-custom-data []
  "A list of JSON file paths that define custom tags, properties and other HTML
syntax constructs. Only workspace folder setting will be read.
All json file paths should be relative to your workspace folder."
  :type 'lsp-repeatable-vector
  :group 'lsp-html
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-html-format-enable t
  "Enable/disable default HTML formatter."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-wrap-line-length 120
  "Maximum amount of characters per line (0 = disable)."
  :type 'number
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-unformatted "wbr"
  nil
  :type '(choice (const nil) string)
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-content-unformatted "pre,code,textarea"
  nil
  :group 'lsp-html
  :type '(choice (const nil) string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-indent-inner-html nil
  nil
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-preserve-new-lines t
  "Controls whether existing line breaks before elements should be preserved.
Only works before elements, not inside tags or for text."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-max-preserve-new-lines nil
  nil
  :type '(choice (const nil) integer)
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-indent-handlebars nil nil
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-end-with-newline nil
  "End with a newline."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-extra-liners "head, body, /html"
  nil
  :type '(choice (const nil) string)
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-format-wrap-attributes "auto"
  "Wrap attributes."
  :type '(choice
          (const "auto")
          (const "force")
          (const "force-aligned")
          (const "force-expand-multiline")
          (const "aligned-multiple")
          (const "preserve")
          (const "preserve-aligned"))
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-suggest-html5 t
  "Controls whether the built-in HTML language support suggests HTML5 tags,
properties and values."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-validate-scripts t
  "Controls whether the built-in HTML language support validates embedded
scripts."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-validate-styles t
  "Controls whether the built-in HTML language support validates embedded
styles."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-auto-closing-tags t
  "Enable/disable autoclosing of HTML tags."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-html-hover-documentation t
  "Whether to show documentation strings on hover or not."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-html-hover-references t
  "Whether to show MDN references in documentation popups."
  :type 'boolean
  :group 'lsp-html
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-html-trace-server "off"
  "Traces the communication between VS Code and the HTML language server."
  :type '(choice
          (const "off")
          (const "messages")
          (const "verbose"))
  :group 'lsp-html
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings
 '(("html.trace.server" lsp-html-trace-server)
   ("html.autoClosingTags" lsp-html-auto-closing-tags t)
   ("html.validate.styles" lsp-html-validate-styles t)
   ("html.validate.scripts" lsp-html-validate-scripts t)
   ("html.suggest.html5" lsp-html-suggest-html5 t)
   ("html.format.wrapAttributes" lsp-html-format-wrap-attributes)
   ("html.format.extraLiners" lsp-html-format-extra-liners)
   ("html.format.endWithNewline" lsp-html-format-end-with-newline t)
   ("html.format.indentHandlebars" lsp-html-format-indent-handlebars t)
   ("html.format.maxPreserveNewLines" lsp-html-format-max-preserve-new-lines)
   ("html.format.preserveNewLines" lsp-html-format-preserve-new-lines t)
   ("html.format.indentInnerHtml" lsp-html-format-indent-inner-html t)
   ("html.format.contentUnformatted" lsp-html-format-content-unformatted)
   ("html.format.unformatted" lsp-html-format-unformatted)
   ("html.format.wrapLineLength" lsp-html-format-wrap-line-length)
   ("html.format.enable" lsp-html-format-enable t)
   ("html.hover.documentation" lsp-html-hover-documentation t)
   ("html.hover.references" lsp-html-hover-references t)
   ("html.customData" lsp-html-custom-data)))

(defcustom lsp-html-server-command-args '("--stdio")
  "Command to start html-languageserver."
  :type '(repeat string)
  :group 'lsp-html
  :package-version '(lsp-mode . "6.3"))

;; Caveat: uri seems to be sent as a single length vector.
(defun lsp-html--get-content (_workspace files callback)
  "Helper function for getting the content of a URI/filename."
  (let* ((filename (aref files 0))
         (uri (f-join (lsp-workspace-root) filename))
         (file-content (f-read-text uri)))
    (funcall callback file-content)))

(lsp-dependency 'html-language-server
                '(:system "vscode-html-language-server")
                '(:npm :package "vscode-langservers-extracted"
                       :path "vscode-html-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (lsp-package-path 'html-language-server)
                                           lsp-html-server-command-args)))
                  :activation-fn (lsp-activate-on "html")
                  :priority -4
                  :completion-in-comments? t
                  :server-id 'html-ls
                  :initialization-options (lambda ()
                                            (list :dataPaths lsp-html-custom-data))
                  :async-request-handlers (ht ("html/customDataContent" #'lsp-html--get-content))
                  :initialized-fn (lambda (w)
                                    (with-lsp-workspace w
                                      (lsp--set-configuration
                                       (lsp-configuration-section "html"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'html-language-server callback
                                         error-callback))))

(lsp-consistency-check lsp-html)

(provide 'lsp-html)
;;; lsp-html.el ends here
