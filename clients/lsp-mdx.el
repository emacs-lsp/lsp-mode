;;; lsp-mdx.el --- lsp-mode mdx integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 lsp-mode maintainers

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

;;  LSP client for mdx-analyzer language-server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-mdx nil
  "Settings for the mdx language server client."
  :group 'lsp-mode
  :link '(url-link "https://github.com/mdx-js/mdx-analyzer/tree/main/packages/language-server")
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-mdx-server-command "mdx-language-server"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-mdx
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-mdx-server-command-args '("--stdio")
  "Command-line arguments for the mdx lsp server."
  :type '(repeat string)
  :group 'lsp-mdx
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency 'mdx-language-server
                '(:system "mdx-language-server")
                '(:npm :package "@mdx-js/language-server"
                       :path "mdx-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (or (executable-find lsp-mdx-server-command)
                                               (lsp-package-path 'mdx-language-server))
                                           lsp-mdx-server-command-args)))
                  :activation-fn (lambda (&rest _args)
                                   (string-match-p "\\.mdx\\'" (buffer-file-name)))
                  :priority -2
                  :server-id 'mdx-analyzer
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'mdx-language-server callback error-callback))))

(provide 'lsp-mdx)
;;; lsp-mdx.el ends here
