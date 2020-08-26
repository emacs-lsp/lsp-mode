;;; lsp-nix.el --- lsp-mode nix integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode maintainers

;; Author: Seong Yong-ju <sei40kr@gmail.com>
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

;; Client for the rnix language server.

;;; Code:

(defgroup lsp-nix nil
  "LSP support for Nix, using rnix-lsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nix-community/rnix-lsp"))

(defcustom lsp-nix-server-path "rnix-lsp"
  "Executable path for the server."
  :type 'string
  :package-version '(lsp-mode . "7.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-server-path))
                  :major-modes '(nix-mode)
                  :server-id 'nix-ls))

(provide 'lsp-nix)
;;; lsp-nix.el ends here
