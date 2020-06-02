;;; lsp-serenata.el --- Serenata server configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

;; Author: Fermin Munoz <fmfs@posteo.net>
;; Keywords: php lsp

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

;; lsp-serenata client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-serenata nil
  "LSP support for the PHP programming language, using serenata"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/Serenata/Serenata")
  :package-version '(lsp-mode . "6.4"))

(defcustom lsp-serenata-server-path
  "serenata.phar"
  "Path to the Serenata Language Server phar file.
It can be downloaded from https://gitlab.com/Serenata/Serenata/-/releases."
  :group 'lsp-serenata
  :type 'file)

(defun lsp-serenata-server-start-fun (port)
  "Define serenata start function, it requires a PORT."
  `(,lsp-serenata-server-path
    "-u" ,(number-to-string port)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-serenata-server-start-fun)
  :major-modes '(php-mode)
  :priority -2
  :server-id 'serenata
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "serenata"))))))

(provide 'lsp-serenata)
;;; lsp-serenata.el ends here
