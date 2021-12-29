;;; lsp-remark.el --- lsp-mode remark integration -*- lexical-binding: t; -*-

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

;;  LSP client for remark-language-server

;;; Code:

(require 'lsp-mode)

;;; Markdown
(defgroup lsp-remark nil
  "Settings for the markdown language server client."
  :group 'lsp-mode
  :link '(url-link "https://github.com/remarkjs/remark-language-server")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-remark-server-command "remark-language-server"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-remark
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-remark-server-command-args '("--stdio")
  "Command-line arguments for the markdown lsp server."
  :type '(repeat 'string)
  :group 'lsp-remark
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency 'remark-language-server
                '(:system "remark-language-server")
                '(:npm :package "remark-language-server"
                       :path "remark-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (or (executable-find lsp-remark-server-command)
                                               (lsp-package-path 'remark-language-server))
                                           lsp-remark-server-command-args)))
                  :activation-fn (lsp-activate-on "markdown")
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "remark-language-server"))))
                  :priority 0
                  :server-id 'remark))

(lsp-consistency-check lsp-remark)

(provide 'lsp-remark)
;;; lsp-remark.el ends here
