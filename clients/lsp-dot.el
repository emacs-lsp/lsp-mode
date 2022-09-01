;;; lsp-dot.el --- LSP client for the DOT/Graphviz language -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Keywords: languages, tools

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

;; LSP client for the DOT/Graphviz language

;;; Code:

(require 'lsp-mode)

;;; DOT Language (Graphviz)
(defgroup lsp-dot nil
  "Settings for the DOT Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nikeee/dot-language-server")
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-dot--dot-ls-server-command ()
  "Startup command for the DOT language server."
  (list (lsp-package-path 'dot-language-server) "--stdio"))

(lsp-dependency 'dot-language-server
                '(:system "dot-language-server")
                '(:npm :package "dot-language-server"
                       :path "dot-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-dot--dot-ls-server-command)
  :priority -1
  :activation-fn (lsp-activate-on "dot")
  :server-id 'dot-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'dot-language-server callback error-callback))))

(lsp-consistency-check lsp-dot)

(provide 'lsp-dot)
;;; lsp-dot.el ends here
