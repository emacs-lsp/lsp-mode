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
  :link '(url-link "https://github.com/unifiedjs/unified-language-server")
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-markdown-server-command "unified-language-server"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-markdown
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-markdown-server-command-args '("--parser=remark-parse" "--stdio")
  "Command-line arguments for the markdown lsp server."
  :type '(repeat 'string)
  :group 'lsp-markdown
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-markdown-remark-plugins [["#remark-preset-lint-markdown-style-guide"]]
  "The JSON configuration object for plugins.

For a complete list of plugins, check:
 https://github.com/unifiedjs/unified-language-server/blob/main/CONFIGURATION.md#re-using-settings"
  :type 'lsp-string-vector
  :group 'lsp-markdown
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-markdown-remark-check-text-with-setting "retext-english"
  "Configure `checkTextWith' subproperty.

For a complete list of plugins, check:
 https://github.com/unifiedjs/unified-language-server/blob/main/CONFIGURATION.md#re-using-settings"
  :type '(choice (
                  (const "retext-english")
                  (const "remark-parse")))
  :group 'lsp-markdown
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-markdown-remark-check-text-with-mutator ["#remark-retext" "#parse-latin"]
  "Vector of additional mutators.

For a complete list of plugins, check:
 https://github.com/unifiedjs/unified-language-server/blob/main/CONFIGURATION.md#re-using-settings"
  :type 'lsp-string-vector
  :group 'lsp-markdown
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency 'unified-language-server
                '(:system "unified-language-server")
                '(:npm :package "unified-language-server"
                       :path "unified-language-server"))

(lsp-register-custom-settings
 `(("unified-language-server.remark-parse.plugins" lsp-markdown-remark-plugins)
   ("unified-language-server.remark-parse.checkTextWith.setting" lsp-markdown-remark-check-text-with-setting)
   ("unified-language-server.remark-parse.checkTextWith.mutator" lsp-markdown-remark-check-text-with-mutator)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (or (executable-find lsp-markdown-server-command)
                                              (lsp-package-path 'unified-language-server))
                                           lsp-markdown-server-command-args)))
                  :activation-fn (lsp-activate-on "markdown")
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "unified-language-server"))))
                  :major-modes '(markdown-mode)
                  :priority -1
                  :major-modes '(markdown-mode)
                  :server-id 'unified))

(lsp-consistency-check lsp-markdown)

(provide 'lsp-markdown)
;;; lsp-markdown.el ends here
