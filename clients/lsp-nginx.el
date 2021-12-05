;;; lsp-nginx.el --- Nginx Client settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: nginx lsp

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
;;
;; LSP client for Nginx
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-nginx nil
  "LSP support for Nginx."
  :group 'lsp-mode
  :link '(url-link "https://github.com/pappasam/nginx-language-server")
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-nginx-server-command '("nginx-language-server")
  "Command to start Nginx Language Server."
  :risky t
  :group 'lsp-nginx
  :type '(repeat string)
  :package-version `(lsp-mode . "8.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-nginx-server-command))
                  :major-modes '(nginx-mode)
                  :priority -1
                  :server-id 'nginx-ls))

(lsp-consistency-check lsp-nginx)

(provide 'lsp-nginx)
;;; lsp-nginx.el ends here
