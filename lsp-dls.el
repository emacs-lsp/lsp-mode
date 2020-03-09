;;; lsp-dls.el --- dls configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode developers

;; Author: Hiroki Noda <kubo39@gmail.com>
;; Keywords:

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

;; dls client configuration

;;; Code:

(require 'lsp-mode)

(defgroup lsp-dls nil
  "LSP support for D, using https://github.com/d-language-server/dls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/d-language-server/dls")
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dls-path "dls"
  "Path to dls server binary."
  :type 'string
  :group 'lsp-dls
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dls-args nil
  "Add server initialization options for dls."
  :type '(repeat string)
  :group 'lsp-dls
  :package-version '(lsp-mode . "6.3"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (cons lsp-dls-path lsp-dls-args)))
                  :major-modes '(d-mode)
                  :priority -1
                  :server-id 'dls))

(provide 'lsp-dls)
;;; lsp-dls.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
