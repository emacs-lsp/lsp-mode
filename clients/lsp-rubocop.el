;;; lsp-rubocop.el --- lsp-mode for RuboCop  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Koichi Ito

;; Author: Koichi Ito <koic.ito@gmail.com>
;; Keywords: lsp, ruby, languages

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

;; LSP client for RuboCop which is a Ruby static code analyzer (a.k.a. linter)
;; and code formatter.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-rubocop nil
  "LSP support for RuboCop, using the RuboCop built-in language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rubocop/rubocop")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rubocop-use-bundler nil
  "Run RuboCop using Bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-rubocop
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rubocop-server-path nil
  "Path of the RuboCop built-in language server executable.
If specified, `lsp-rubocop-use-bundler' is ignored."
  :type 'file
  :group 'lsp-rubocop
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-rubocop--build-command ()
  "Build a command to start the RuboCop built-in language server."
  (append
   (if (and lsp-rubocop-use-bundler (not lsp-rubocop-server-path)) '("bundle" "exec"))
   (list (or lsp-rubocop-server-path "rubocop") "--lsp")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-rubocop--build-command)
  :activation-fn (lsp-activate-on "ruby")
  :priority -1
  :server-id 'rubocop-ls))

(lsp-consistency-check lsp-rubocop)

(provide 'lsp-rubocop)
;;; lsp-rubocop.el ends here
