;;; lsp-sorbet.el --- Sorbet server configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: Christopher Wilson <chris@sencjw.com>
;; Keywords:

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

;; lsp-sorbet client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-sorbet nil
  "LSP support for Ruby, using the Sorbet language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sorbet/sorbet")
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-sorbet-use-bundler nil
  "Run sorbet under bundler"
  :type 'boolean
  :group 'lsp-sorbet
  :package-version '(lsp-mode . "7.1.0"))

(defun lsp-sorbet--build-command ()
  "Build sorbet command"
  (let ((lsp-command '("srb" "typecheck" "--lsp" "--disable-watchman")))
    (if lsp-sorbet-use-bundler
              (append '("bundle" "exec") lsp-command)
            lsp-command)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-sorbet--build-command)
  :priority -2
  :major-modes '(ruby-mode enh-ruby-mode)
  :server-id 'sorbet-ls))

(provide 'lsp-sorbet)
;;; lsp-sorbet.el ends here
