;;; lsp-markdown.el --- lsp-mode markdown integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 lsp-mode maintainers

;; Author: lsp-mode maintainers
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

;;  LSP client for unified-language-server

;;; Code:

(require 'lsp-mode)

;;; Markdown
(defgroup lsp-markdown nil
  "Settings for the markdown language server client."
  :group 'lsp-mode
  :tag "Language Server"
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-markdown-server-command "unified-language-server"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-markdown
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-markdown-server-command-args '("--parser=remark-parse" "--stdio")
  "commandline-arguments for the markdown lsp server."
  :type '(repeat 'string)
  :group 'lsp-markdown
  :package-version '(lsp-mode . "7.1"))

(lsp-dependency 'unified-language-server
                '(:system "unified-language-server")
                '(:npm :package "unified-language-server"
                       :path "unified-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (or (executable-find lsp-markdown-server-command)
                                              (lsp-package-path 'unified-language-server))
                                           lsp-markdown-server-command-args)))
                  :activation-fn (lsp-activate-on "markdown")
                  :major-modes '(markdown-mode)
                  :server-id 'unified))

(provide 'lsp-markdown)
;;; lsp-markdown.el ends here
