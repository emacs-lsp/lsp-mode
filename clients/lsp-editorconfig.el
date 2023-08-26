;;; lsp-editorconfig.el --- EditorConfig Client settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, editorconfig

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

;; LSP client for EditorConfig

;;; Code:

(require 'lsp-mode)

(defgroup lsp-editorconfig nil
  "Settings for the ecls Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Lilja/ecls")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-editorconfig-langserver-executable "ecls"
  "Command to start ecls language server."
  :type 'string
  :group 'lsp-editorconfig
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency 'ecls
                '(:system "ecls")
                '(:npm :package "ecls"
                       :path "ecls"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
     `(,lsp-editorconfig-langserver-executable "--stdio")))
  :priority -1
  :major-modes '(editorconfig-conf-mode)
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'ecls callback error-callback))
  :server-id 'ecls))

(provide 'lsp-editorconfig)
;;; lsp-editorconfig.el ends here
