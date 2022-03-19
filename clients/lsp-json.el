;;; lsp-json.el ---  vscode-json-languageserver integration -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: kien.n.quang at gmail.com
;; Keywords: lsp

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

(require 'lsp-mode)
(require 'ht)
(require 'url)
(require 'url-util)

(defgroup lsp-json nil
  "LSP support for JSON, using vscode's built-in language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/microsoft/vscode/tree/main/extensions/json-language-features/server")
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-json-schemas nil
  "Associate schemas to JSON files in the current project"
  :type '(repeat alist)
  :group 'lsp-json
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-http-proxy nil
  "The URL of the proxy server to use when fetching schema."
  :type 'string
  :group 'lsp-json
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-http-proxyStrictSSL t
  "The URL of the proxy server to use when fetching schema."
  :type 'boolean
  :group 'lsp-json
  :package-version '(lsp-mode . "6.3"))

(lsp-register-custom-settings
 '(("json.schemas" lsp-json-schemas)
   ("http.proxy" lsp-http-proxy)
   ("http.proxyStrictSSL" lsp-http-proxyStrictSSL)))

(defvar lsp-json--extra-init-params
  `(:provideFormatter t
    :handledSchemaProtocols ["file" "http" "https"]))

(defvar lsp-json--schema-associations
  `(:/*.css-data.json ["https://raw.githubusercontent.com/Microsoft/vscode-css-languageservice/master/docs/customData.schema.json"]
    :/package.json ["http://json.schemastore.org/package"]
    :/*.html-data.json ["https://raw.githubusercontent.com/Microsoft/vscode-html-languageservice/master/docs/customData.schema.json"]
    :/*.schema.json ["http://json-schema.org/draft-07/schema#"]
    :/bower.json ["http://json.schemastore.org/bower"]
    :/composer.json ["http://json.schemastore.org/composer"]
    :/tsconfig.json ["http://json.schemastore.org/tsconfig"]
    :/tsconfig.*.json ["http://json.schemastore.org/tsconfig"]
    :/typings.json ["http://json.schemastore.org/typings"]
    :/.bowerrc ["http://json.schemastore.org/bowerrc"]
    :/.babelrc ["http://json.schemastore.org/babelrc"]
    :/.babelrc.json ["http://json.schemastore.org/babelrc"]
    :/babel.config.json ["http://json.schemastore.org/babelrc"]
    :/jsconfig.json ["http://json.schemastore.org/jsconfig"]
    :/jsconfig.*.json ["http://json.schemastore.org/jsconfig"]
    :/project.json ["http://json.schemastore.org/project"]
    :/omnisharp.json ["http://json.schemastore.org/omnisharp"]
    :/.eslintrc.json ["http://json.schemastore.org/eslintrc"]
    :/.eslintrc ["http://json.schemastore.org/eslintrc"])
  "Default json schemas.")

(defun lsp-json--get-content (_workspace uri callback)
  "Get content from URI."
  (ignore-errors
    (url-retrieve uri
                  (lambda (_status callback)
                    (goto-char (point-min))
                    (re-search-forward "\n\n" nil 'noerror)
                    (funcall
                     callback
                     (decode-coding-string (buffer-substring (point) (point-max))
                                           'utf-8-unix)))
                  (list callback))))

(lsp-dependency 'vscode-json-languageserver
                '(:system "vscode-json-language-server")
                '(:npm :package "vscode-langservers-extracted"
                       :path "vscode-json-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda () (list (lsp-package-path 'vscode-json-languageserver) "--stdio")))
  :activation-fn (lsp-activate-on "json" "jsonc")
  :server-id 'json-ls
  :priority 0
  :multi-root t
  :completion-in-comments? t
  :initialization-options lsp-json--extra-init-params
  :async-request-handlers (ht ("vscode/content" #'lsp-json--get-content))
  :initialized-fn
  (lambda (w)
    (with-lsp-workspace w
      (lsp--set-configuration
       (ht-merge (lsp-configuration-section "json")
                 (lsp-configuration-section "http")))
      (lsp-notify "json/schemaAssociations" lsp-json--schema-associations)))
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'vscode-json-languageserver callback error-callback))))

(lsp-consistency-check lsp-json)

(provide 'lsp-json)
;;; lsp-json.el ends here
