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
  "LSP support for JSON, using vscode-json-languageserver."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vscode-langservers/vscode-json-languageserver")
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-json-schemas nil
  "Associate schemas to JSON files in the current project"
  :type '(repeat alist)
  :group 'lsp-json
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-json-format-enable t
  "Enable/disable default JSON formatter"
  :type 'boolean
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
 '(("json.format.enable" lsp-json-format-enable t)
   ("json.schemas" lsp-json-schemas)
   ("http.proxy" lsp-http-proxy)
   ("http.proxyStrictSSL" lsp-http-proxyStrictSSL)))

(defcustom lsp-json-server "vscode-json-languageserver"
  "Json language server executable."
  :type 'string
  :group 'lsp-json
  :package-version '(lsp-mode . "6.3"))

(defun lsp-json--command ()
  "Return the command to start server."
  ;; Install language server
  (unless (and lsp-json-server (executable-find lsp-json-server))
    (cond
     ((eq system-type 'windows-nt)
      (unless (executable-find "npm")
        (user-error "`npm' is not available in your PATH, please install it"))
      (shell-command "npm install -g vscode-json-languageserver"))
     (t (user-error (format "Cannot find `%s', please install it" lsp-json-server)))))
  `(,lsp-json-server "--stdio"))

(defvar lsp-json--extra-init-params
  `(:handledSchemaProtocols ["file" "http" "https"]))

(defvar lsp-json--major-modes '(json-mode jsonc-mode))

(defvar lsp-json--schema-associations
  (ht ("/*.css-data.json" ["https://raw.githubusercontent.com/Microsoft/vscode-css-languageservice/master/docs/customData.schema.json"])
      ("/package.json" ["http://json.schemastore.org/package"])
      ("/*.html-data.json" ["https://raw.githubusercontent.com/Microsoft/vscode-html-languageservice/master/docs/customData.schema.json"])
      ("/*.schema.json" ["http://json-schema.org/draft-07/schema#"])
      ("/bower.json" ["http://json.schemastore.org/bower"])
      ("/composer.json" ["http://json.schemastore.org/composer"])
      ("/tsconfig.json" ["http://json.schemastore.org/tsconfig"])
      ("/tsconfig.*.json" ["http://json.schemastore.org/tsconfig"])
      ("/typings.json" ["http://json.schemastore.org/typings"])
      ("/.bowerrc" ["http://json.schemastore.org/bowerrc"])
      ("/.babelrc" ["http://json.schemastore.org/babelrc"])
      ("/.babelrc.json" ["http://json.schemastore.org/babelrc"])
      ("/babel.config.json" ["http://json.schemastore.org/babelrc"])
      ("/jsconfig.json" ["http://json.schemastore.org/jsconfig"])
      ("/jsconfig.*.json" ["http://json.schemastore.org/jsconfig"])
      ("/project.json" ["http://json.schemastore.org/project"])
      ("/omnisharp.json" ["http://json.schemastore.org/omnisharp"]))
  "Default json schemas.")

(defun lsp-json--get-content (_workspace uri callback)
  "Get content from URI."
  (url-retrieve uri (lambda (_status callback)
                      (goto-char (point-min))
                      (re-search-forward "\n\n" nil 'noerror)
                      (funcall
                       callback
                       (decode-coding-string (buffer-substring (point) (point-max))
                                             'utf-8-unix)))
                (list callback)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-json--command)
  :major-modes lsp-json--major-modes
  :server-id 'json-ls
  :priority -1
  :multi-root t
  :completion-in-comments? t
  :initialization-options lsp-json--extra-init-params
  :async-request-handlers (ht ("vscode/content" #'lsp-json--get-content))
  :initialized-fn (lambda (w)
                    (with-lsp-workspace w
                      (lsp--set-configuration (lsp-configuration-section "json"))
                      (lsp-notify "json/schemaAssociations" lsp-json--schema-associations)))))

;; Compatibility
(with-eval-after-load 'company-lsp
  (advice-add 'company-tng--supress-post-completion
              :after-while
              (lambda (&rest _)
                (not (memq major-mode lsp-json--major-modes)))
              '((name . --force-post-completion-for-json))))

(provide 'lsp-json)
;;; lsp-json.el ends here
