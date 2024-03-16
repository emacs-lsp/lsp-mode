;;; lsp-beancount.el --- Beancount Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2021 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, beancount

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

;; LSP client for Beancount

;;; Code:

(require 'lsp-mode)

(defgroup lsp-beancount nil
  "Settings for the Beancount Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/polarmutex/beancount-language-server")
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-beancount-langserver-executable "beancount-language-server"
  "Command to start Beancount language server."
  :type 'string
  :group 'lsp-beancount
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-beancount-journal-file nil
  "Path to Beancount journal file.

The path can be absolute, or relative to the currently opened file.
Use nil (the default) to use the current beancount buffer as the journal file."
  :type 'string
  :group 'lsp-beancount
  :package-version '(lsp-mode . "8.0.0"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
      `(,lsp-beancount-langserver-executable "--stdio")))
  :major-modes '(beancount-mode)
  :initialization-options
  `((journalFile . ,lsp-beancount-journal-file))
  :server-id 'beancount-ls))

(lsp-consistency-check lsp-beancount)

(provide 'lsp-beancount)
;;; lsp-beancount.el ends here
