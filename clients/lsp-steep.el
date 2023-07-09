;;; lsp-steep.el --- lsp-mode for Steep  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Masafumi Koba

;; Author: Masafumi Koba <ybiquitous@gmail.com>
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

;; LSP client for Steep which is a Ruby type checker.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-steep nil
  "LSP support for Steep, using the Steep language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/soutaro/steep"))

(defcustom lsp-steep-log-level "warn"
  "Log level of Steep."
  :type '(choice
          (const "fatal")
          (const "error")
          (const "warn")
          (const "info")
          (const "debug"))
  :group 'lsp-steep)

(defcustom lsp-steep-use-bundler nil
  "Run Steep using Bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-steep)

(defcustom lsp-steep-server-path nil
  "Path of the Steep language server executable.
If specified, `lsp-steep-use-bundler' is ignored."
  :type 'file
  :group 'lsp-steep
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-steep--build-command ()
  "Build a command to start the Steep language server."
  (append
   (if (and lsp-steep-use-bundler (not lsp-steep-server-path)) '("bundle" "exec"))
   (list (or lsp-steep-server-path "steep") "langserver" "--log-level" lsp-steep-log-level)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-steep--build-command)
  :activation-fn (lsp-activate-on "ruby")
  :priority -3
  :server-id 'steep-ls))

(lsp-consistency-check lsp-steep)

(provide 'lsp-steep)
;;; lsp-steep.el ends here
