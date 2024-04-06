;;; lsp-qml.el --- LSP Client for QML -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, qt, qml, gui,

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

;; LSP Client for QML (Qt Modeling Language).

;;; Code:
(require 'lsp-mode)

(defcustom lsp-qml-server-command "qmlls"
  "Server executable to use."
  :type 'string
  :group 'lsp-qml
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (executable-find lsp-qml-server-command)))
                  :activation-fn (lsp-activate-on "qml")
                  :priority -1
                  :server-id 'qml-ls))

(provide 'lsp-qml)
;;; lsp-qml.el ends here
