;;; lsp-cypher.el --- Cypher Client                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Gustav Hedengran
;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: Gustav Hedengran <gustav.hedengran@gmail.com>
;; Keywords: languages lsp cypher

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

(defgroup lsp-cypher nil
  "LSP support for Cypher."
  :group 'lsp-mode
  :link '(url-link "https://github.com/neo4j/cypher-language-support/blob/main/packages/language-server"))

(lsp-dependency 'cypher-language-server
                '(:system "cypher-language-server")
                '(:npm :package "@neo4j-cypher/language-server"
                       :path "cypher-language-server"))

(defun lsp-client--cypher-ls-server-command ()
  "Startup command for Cypher language server."
  (list (lsp-package-path 'cypher-language-server) "--stdio"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-client--cypher-ls-server-command)
                  :activation-fn (lsp-activate-on "cypher")
                  :language-id "cypher"
                  :server-id 'cypher-ls
                  :priority 0
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'cypher-language-server callback error-callback))))

(lsp-consistency-check lsp-cypher)

(provide 'lsp-cypher)
;;; lsp-cypher.el ends here
