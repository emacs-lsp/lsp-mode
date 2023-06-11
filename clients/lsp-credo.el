;;; lsp-credo.el --- lsp-mode Credo integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wilhelm H Kirschbaum

;; Author: Wilhelm H Kirschbaum
;; Keywords: lsp, elixir, credo

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

;; LSP Client for Elixir Credo

;;; Code:

(require 'lsp-mode)

(defgroup lsp-credo-ls nil
  "Settings for credo-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elixir-tools/credo-language-server")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-credo-ls-command '("credo-language-server" "--stdio=true")
  "The command that starts credo-ls."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-credo-ls
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
     `(,(or (executable-find (cl-first lsp-credo-ls-command))
            (lsp-package-path 'credo-ls))
       ,@(cl-rest lsp-credo-ls-command))))
  :activation-fn (lsp-activate-on "elixir")
  :priority -1
  :add-on? t
  :multi-root t
  :server-id 'credo-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'credo-ls callback error-callback))))

(lsp-consistency-check lsp-credo)

(provide 'lsp-credo)

;;; lsp-credo.el ends here
