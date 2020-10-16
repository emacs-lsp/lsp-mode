;;; lsp-racket.el --- lsp-mode racket integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode maintainers

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

;; Client for the Racket language server.

;;; Code:

(require 'ht)
(require 'lsp-mode)


;; racket-langserver

(defgroup lsp-racket-langserver nil
  "LSP support for Racket, using racket-langserver"
  :group 'lsp-mode
  :link '(url-link "https://github.com/jeapostrophe/racket-langserver"))

(defcustom lsp-racket-langserver-command '("racket" "--lib" "racket-langserver")
  "Command to start the server."
  :type 'string
  :package-version '(lsp-mode . "7.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-racket-langserver-command))
                  :major-modes '(racket-mode)
                  :priority 1
                  :server-id 'racket-langserver))


;; Theia

(defgroup lsp-racket-language-server nil
  "LSP support for Racket, using racket-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/theia-ide/racket-language-server"))

(defcustom lsp-racket-language-server-path "racket-language-server"
  "Executable path for the server."
  :type 'string
  :package-version '(lsp-mode . "7.1"))

(defun lsp-racket-language-server-colorize-handler (&rest _args)
  "Handler for the colorize notification."
  ;; TODO:
  nil)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-racket-language-server-path))
                  :major-modes '(racket-mode)
                  :priority -1
                  :notification-handlers (ht ("racket/colorize" #'lsp-racket-language-server-colorize-handler))
                  :server-id 'racket-language-server))

(provide 'lsp-racket)
;;; lsp-racket.el ends here
