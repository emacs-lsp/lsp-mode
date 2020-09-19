;;; lsp-ada.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
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

(defgroup lsp-ada nil
  "Settings for Ada Language Server."
  :group 'tools
  :tag "Language Server"
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-project-file "default.gpr"
  "Set the project file full path to configure the language server with.
  The ~ prefix (for the user home directory) is supported.
  See https://github.com/AdaCore/ada_language_server for a per-project
  configuration example."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-option-charset "UTF-8"
  "The charset to use by the Ada Language server. Defaults to 'UTF-8'."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-enable-diagnostics t
  "A boolean to disable diagnostics. Defaults to true."
  :type 'boolean
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(lsp-register-custom-settings
 '(("ada.projectFile" lsp-ada-project-file)
   ("ada.enableDiagnostics" lsp-ada-enable-diagnostics)
   ("ada.defaultCharset" lsp-ada-option-charset)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("ada_language_server"))
                  :major-modes '(ada-mode)
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "ada"))))
                  :server-id 'ada-ls))

(provide 'lsp-ada)
;;; lsp-ada.el ends here
