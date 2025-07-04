;;; lsp-postgres.el --- Postgres client settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shen, Jen-Chieh

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
;; LSP client for Postgres language server.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-postgres nil
  "LSP support for SQL, using Postgres language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/supabase-community/postgres-language-server")
  :package-version `(lsp-mode . "9.0.1"))

(defcustom lsp-postgres-server-path nil
  "Path points for Postgres language server.

This is only for development use."
  :type 'string
  :group 'lsp-postgres)

(defcustom lsp-postgres-server-store-path
  (expand-file-name "postgres-ls/" lsp-server-install-dir)
  "The path to the file in which Postgres ls will be stored."
  :type 'file
  :group 'lsp-postgres)

(defconst lsp-postgres-download-url-format
  "https://github.com/supabase-community/postgres-language-server/releases/latest/download/postgrestools_%s-%s"
  "Format to the download url link.")

(defun lsp-postgres--postgres-ls-url ()
  "Return Url points to the zls' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-postgres-download-url-format arch "pc-windows-msvc"))
      (darwin
       (format lsp-postgres-download-url-format arch "apple-darwin"))
      (gnu/linux
       (format lsp-postgres-download-url-format arch "unknown-linux-gnu")))))

(defun lsp-postgres--server-command ()
  "Generate startup command for Postgres language server."
  (list (or lsp-postgres-server-path
            (lsp-package-path 'postgres-ls))
        "lsp-proxy"))

(lsp-dependency
 'postgres-ls
 '(:system "postgrestools")
 `(:download :url ,(lsp-postgres--postgres-ls-url)
             :store-path ,(f-join lsp-postgres-server-store-path
                                  (pcase system-type
                                    ('windows-nt "postgrestools.exe")
                                    (_           "postgrestools")))
             :set-executable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-postgres--server-command)
  :major-modes '(sql-mode)
  :priority -2
  :multi-root t
  :server-id 'postgres-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'postgres-ls callback error-callback))))

(lsp-consistency-check lsp-postgres)

(provide 'lsp-postgres)
;;; lsp-postgres.el ends here
