;;; lsp-openscad.el --- openscad client         -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Len Trigg

;; Author: Len Trigg
;; Keywords: openscad lsp

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

;; lsp-openscad client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-openscad nil
  "LSP support for openscad."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Leathong/openscad-LSP"))

(defcustom lsp-openscad-server
  "openscad-language-server"
  "Path to the openscad language server."
  :group 'lsp-openscad
  :risky t
  :type 'file)

(defcustom lsp-openscad-server-connection-type
  'tcp
  "Type of connection to use with the OpenSCAD Language Server: tcp or stdio"
  :group 'lsp-openscad
  :risky t
  :type 'symbol)

(defun lsp-openscad-server-start-fun (port)
  `(,lsp-openscad-server "--port" ,(number-to-string port)))

(defun lsp-openscad-server-connection ()
  (if (eq lsp-openscad-server-connection-type 'tcp)
      (lsp-tcp-connection 'lsp-openscad-server-start-fun)
    (lsp-stdio-connection `(,lsp-openscad-server))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-openscad-server-connection)
                  :major-modes '(scad-mode)
                  :priority -1
                  :server-id 'openscad))

(provide 'lsp-openscad)
;;; lsp-openscad.el ends here
