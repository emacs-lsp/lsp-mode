;;; lsp-graphql.el --- lsp client for graphql        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Binbin Ye
;; Copyright (C) 2021-2026 emacs-lsp maintainers

;; Author: Binbin Ye
;; Keywords: lsp, graphql

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

;; Support for running graphql lsp.  Support multiple server running at the same time when editing tsx/jsx.

;;; Code:

(require 'lsp-mode)

(lsp-dependency 'graphql-language-service-cli
                '(:system "graphql-lsp")
                '(:npm :package "graphql-language-service-cli"
                       :path "graphql-lsp"))


(defgroup lsp-graphql nil
  "LSP support for the GraphQL, using the graphql-language-service-cli as language server."
  :link '(url-link "https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli#readme")
  :group 'lsp-mode)

(defcustom lsp-clients-graphql-server-args '("server" "--method=stream")
  "CLI arguments for graphql language server."
  :type '(repeat string)
  :risky t
  :group 'lsp-graphql)

(defcustom lsp-graphql-target-file-extensions '("ts" "js" "jsx" "tsx" "vue" "graphql" "graphqls" "gql")
  "List of target file extensions for the GraphQL language server."
  :type '(repeat string)
  :group 'lsp-graphql)

(defcustom lsp-graphql-activated-modes '(js-mode js2-mode typescript-mode typescript-ts-mode)
  "List of major modes that can activate the GraphQL language server.
When a buffer is in one of these modes, the GraphQL language server
may be activated if appropriate GraphQL content is detected."
  :type '(repeat symbol)
  :group 'lsp-graphql)

(defun lsp-graphql-activate-p (filename &optional _)
  "Check if the GraphQL language server should be enabled based on FILENAME."
  (let ((target-extensions (mapconcat 'identity lsp-graphql-target-file-extensions "\\|")))
    (or (string-match-p (format "\\.\\(?:%s\\)\\'" target-extensions) filename)
        (apply 'derived-mode-p lsp-graphql-activated-modes))))


(lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection (lambda()
                                                           (cons (lsp-package-path 'graphql-language-service-cli)
                                                                 lsp-clients-graphql-server-args)))
                   :major-modes '(graphql-mode)
                   :language-id "graphql"
                   :server-id 'graphql-lsp
                   :priority -3
                   :add-on? t
                   :multi-root t
                   :activation-fn 'lsp-graphql-activate-p
                   :download-server-fn (lambda (_client callback error-callback _update?)
                                         (lsp-package-ensure
                                          'graphql-language-service-cli
                                          callback
                                          error-callback))))

(lsp-consistency-check lsp-graphql)

(provide 'lsp-graphql)
;;; lsp-graphql.el ends here
