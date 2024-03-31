;;; lsp-cucumber.el --- LSP Clients for Cucumber  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP server implementation for Cucumber
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-cucumber nil
  "LSP server implementation for Cucumber."
  :group 'lsp-mode
  :link '(url-link "https://github.com/cucumber/language-server"))

(defcustom lsp-cucumber-server-path nil
  "Path points for Cucumber language server.

This is only for development use."
  :type 'string
  :group 'lsp-cucumber)

(defcustom lsp-cucumber-active-modes
  '( feature-mode)
  "List of major mode that work with Cucumber language server."
  :type 'list
  :group 'lsp-cucumber)

(defun lsp-cucumber--server-command ()
  "Generate startup command for Cucumber language server."
  (or (and lsp-cucumber-server-path
           (list lsp-cucumber-server-path "--stdio"))
      (list (lsp-package-path 'cucumber-language-server) "--stdio")))

(lsp-dependency 'cucumber-language-server
                '(:system "cucumber-language-server")
                '(:npm :package "@cucumber/language-server"
                       :path "cucumber-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-cucumber--server-command)
  :major-modes lsp-cucumber-active-modes
  :priority -1
  :server-id 'cucumber-language-server
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'cucumber-language-server callback error-callback))))

(provide 'lsp-cucumber)
;;; lsp-cucumber.el ends here
