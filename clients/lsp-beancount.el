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

(defcustom lsp-beancount-langserver-executable "beancount-langserver"
  "Command to start Beancount language server."
  :type 'string
  :group 'lsp-beancount
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-beancount-python-interpreter nil
  "Path to Python executable."
  :type 'string
  :group 'lsp-beancount
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-beancount-journal-file nil
  "Pathg to Beancount journal file."
  :type 'string
  :group 'lsp-beancount
  :package-version '(lsp-mode . "8.0.0"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
     (when (null lsp-beancount-python-interpreter)
       (setq lsp-beancount-python-interpreter (or (executable-find "python3")
                                                  (executable-find "python"))))
     `(,lsp-beancount-langserver-executable "--stdio")))
  :major-modes '(beancount-mode)
  :initialization-options
  `((journalFile . ,lsp-beancount-journal-file)
    (pythonPath . ,lsp-beancount-python-interpreter))
  :server-id 'beancount-ls))

(lsp-consistency-check lsp-beancount)

(provide 'lsp-beancount)
;;; lsp-beancount.el ends here
