;;; lsp-graphql.el --- lsp client for graphql        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Binbin Ye

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
                '(:npm :package "graphql-language-service-cli"
                       :path "graphql-lsp"))


(defgroup lsp-graphql nil
  "LSP support for the GraphQL, using the graphql-language-service-cli as language server."
  :link '(url-link "https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli#readme")
  :group 'lsp-mode)

(add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))

(defun lsp-graphql-activate-p (filename &optional _)
  "Check if the GraphQL language server should be enabled based on FILENAME."
  (or (string-match-p (rx (one-or-more anything) "."
                        (or "ts" "js" "jsx" "tsx" "vue" "graphql" "gql")eos)
        filename)
    (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode)
      (not (derived-mode-p 'json-mode)))))

(lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection (lambda()
                                                           `(,(lsp-package-path 'graphql-language-service-cli)
                                                              "server"
                                                              "--method=stream")))
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
