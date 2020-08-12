;;; lsp-dockerfile.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, dockerfile

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

;; LSP Clients for Dockerfile documents.

;;; Code:

(require 'lsp-mode)


;;; Dockerfile

(defgroup lsp-dockerfile nil
  "Dockerfile LSP client, provided by the Dockerfile Language Server."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/rcjsuen/dockerfile-language-server-nodejs"))

(defcustom lsp-dockerfile-language-server-command
  '("docker-langserver" "--stdio")
  "The command that starts the docker language server."
  :group 'lsp-dockerfile
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const lsp-dockerfile-language-server-command))
                  :major-modes '(dockerfile-mode)
                  :priority -1
                  :server-id 'dockerfile-ls))

(provide 'lsp-dockerfile)
;;; lsp-dockerfile.el ends here
