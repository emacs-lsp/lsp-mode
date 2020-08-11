;;; lsp-ada.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 E. Alexander Barbosa

;; Author: E. Alexander Barbosa <elxbarbosa@outlook.com>
;; Keywords: lsp, ada

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

;; LSP Clients for the Ada Programming Language

;;; Code:

(require 'lsp-mode)

;;; Ada
(defgroup lsp-ada nil
  "LSP support for the Ada Programming Language, using the Ada Language Server."
  :group 'lsp-mode
  :tag "Language Server"
  :version "7.1")

(defcustom lsp-clients-ada-project-file "default.gpr"
  "Set the project file full path to configure the language server with.
  The ~ prefix (for the user home directory) is supported.
  See https://github.com/AdaCore/ada_language_server for a per-project
  configuration example."
  :group 'lsp-ada
  :version "7.1"
  :type 'string)

(defcustom lsp-clients-ada-option-charset "UTF-8"
  "The charset to use by the Ada Language server. Defaults to 'UTF-8'."
  :group 'lsp-ada
  :version "7.1"
  :type 'string)

(defcustom lsp-clients-ada-enable-diagnostics t
  "A boolean to disable diagnostics. Defaults to true."
  :group 'lsp-ada
  :version "7.1"
  :type 'boolean)

(lsp-register-custom-settings
 '(("ada.projectFile" lsp-clients-ada-project-file)
   ("ada.enableDiagnostics" lsp-clients-ada-enable-diagnostics)
   ("ada.defaultCharset" lsp-clients-ada-option-charset)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("ada_language_server"))
                  :major-modes '(ada-mode)
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "ada"))))
                  :server-id 'ada-lsp))

(provide 'lsp-ada)
;;; lsp-ada.el ends here
