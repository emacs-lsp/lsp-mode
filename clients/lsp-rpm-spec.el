;;; lsp-rpm-spec.el --- lsp-mode integration for the rpm-spec-language-server -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, rpm-spec

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

;; LSP Client for RPM Spec files

;;; Code:

(require 'lsp-mode)

(defgroup lsp-rpm-spec nil
  "LSP support for RPM spec files using rpm-spec-language-server."
  :group 'lsp-mode
  :tag "Language Server"
  :link '(url-link "https://github.com/dcermak/rpm-spec-language-server"))

(defcustom lsp-rpm-spec-server-command '("python" "-m" "rpm_spec_language_server" "--stdio")
  "Command to start rpm-spec-language-server."
  :risky t
  :group 'lsp-rpm-spec
  :type '(repeat string))

(defun lsp-rpm-spec--install-server (_client callback error-callback update?)
  "Install the rpm-spec-language-server via pip.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
If UPDATE? is true, then pip will update the server."
  (lsp-async-start-process
   callback
   error-callback
   "pip" "install" "--user" "rpm-spec-language-server" (when update? "-U")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-rpm-spec-server-command))
                  :activation-fn (lsp-activate-on "rpm-spec")
                  :server-id 'rpm-spec-language-server))

(lsp-consistency-check lsp-rpm-spec)

(provide 'lsp-rpm-spec)

;;; lsp-rpm-spec.el ends here
