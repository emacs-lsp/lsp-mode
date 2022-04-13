;;; lsp-nim.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, nim

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

;; LSP Clients for the Nim Programming Language.

;;; Code:

(require 'lsp-mode)

;; Nim
(defgroup lsp-nimlsp nil
  "LSP support for Nim, using nimlsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/PMunch/nimlsp"))

(defcustom-lsp lsp-nim-project-mapping []
  "Nimsuggest project mapping. Sample value

[(:projectFile \"root.nim\"
  :fileRegex \".*\\.nim\")]"

  :type '(lsp-repeatable-vector plist)
  :group 'lsp-nim
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "nim.projectMapping")

(defcustom-lsp lsp-nim-timeout 120000
  "Timeout for restarting `nimsuggest'"
  :type 'number
  :group 'lsp-nim
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "nim.timeout")

(defcustom-lsp lsp-nim-nimsuggest-path "nimsuggest"
  "Path to `nimsuggest' to use."
  :type 'number
  :group 'lsp-nim
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "nim.nimsuggestPath")

(defcustom lsp-nim-langserver "nimlangserver"
  "Path to `nimlangserver'"
  :type 'number
  :group 'lsp-nim
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
                  :activation-fn (lsp-activate-on "nim")
                  :priority -1
                  :server-id 'nimls))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-nim-langserver))
                  :activation-fn (lsp-activate-on "nim")
                  :server-id 'nimlangserver))

(lsp-consistency-check lsp-nim)

(provide 'lsp-nim)
;;; lsp-nim.el ends here
