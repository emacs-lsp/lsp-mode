;;; lsp-ruby-syntax-tree.el --- lsp-mode for the Ruby syntax_tree gem -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Geoffrey Lessel
;; Copyright (C) 2022-2026 lsp-mode maintainers

;; Author: Geoffrey Lessel
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

;; LSP client for the Ruby syntax_tree gem.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ruby-syntax-tree nil
  "LSP support for the Ruby syntax_tree gem."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ruby-syntax-tree/syntax_tree"))

(defcustom lsp-ruby-syntax-tree-use-bundler nil
  "Run stree (the syntax_tree executable) using bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-ruby-syntax-tree)

(defcustom lsp-ruby-syntax-tree-format-options nil
  "Options to pass to the stree lsp server."
  :type '(repeat string)
  :group 'lsp-ruby-syntax-tree)

(defun lsp-ruby-syntax-tree--build-command ()
  (append
   (if lsp-ruby-syntax-tree-use-bundler '("bundle" "exec"))
   '("stree" "lsp")
   lsp-ruby-syntax-tree-format-options))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-ruby-syntax-tree--build-command)
  :activation-fn (lsp-activate-on "ruby")
  :priority -4
  :server-id 'ruby-syntax-tree-ls))

(provide 'lsp-ruby-syntax-tree)
;;; lsp-ruby-syntax-tree.el ends here
