;;; lsp-typeprof.el --- TypeProf server configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: lsp, ruby

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

;; Client for TypeProf.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-typeprof nil
  "LSP support for Ruby, using the TypeProf language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ruby/typeprof")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-typeprof-use-bundler nil
  "Run typeprof under bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-typeprof
  :package-version '(lsp-mode . "8.0.1"))

(defun lsp-typeprof--build-command ()
  "Build typeprof command."
  (let ((lsp-command '("typeprof" "--lsp" "--stdio")))
    (if lsp-typeprof-use-bundler
              (append '("bundle" "exec") lsp-command)
            lsp-command)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-typeprof--build-command)
  :priority -4
  :activation-fn (lsp-activate-on "ruby")
  :server-id 'typeprof-ls))

(lsp-consistency-check lsp-typeprof)

(provide 'lsp-typeprof)
;;; lsp-typeprof.el ends here
