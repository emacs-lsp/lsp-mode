;;; lsp-autotools.el --- Support configure.ac, Makefile.am, Makefile  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jen-Chieh Shen
;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: autotools lsp

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

;; Support configure.ac, Makefile.am, Makefile

;;; Code:

(require 'lsp-mode)

(defgroup lsp-autotools nil
  "LSP support for Autotools."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Freed-Wu/autotools-language-server")
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-autotools-active-modes
  '( autoconf-mode
     makefile-mode
     makefile-automake-mode
     makefile-gmake-mode
     makefile-makepp-mode
     makefile-bsdmake-mode
     makefile-imake-mode)
  "List of major mode that work with Autotools."
  :type '(repeat function)
  :group 'lsp-autotools)

(defun lsp-autotools--download-server (_client callback error-callback update?)
  "Install/update Autotools language server using `pip

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t."
  (lsp-async-start-process
   callback
   error-callback
   "pip" "install" "autotools-language-server" (when update? "-U")))

(defun lsp-autotools--server-command ()
  "Startup command for Autotools language server."
  (list "autotools-language-server"))

(defun lsp-autotools--test-present ()
  "Return non-nil if Autotools language server is installed globally."
  (executable-find "autotools-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-autotools--server-command
                   #'lsp-autotools--test-present)
  :major-modes lsp-autotools-active-modes
  :priority -1
  :server-id 'autotools-ls
  :download-server-fn #'lsp-autotools--download-server))

(lsp-consistency-check lsp-autotools)

(provide 'lsp-autotools)
;;; lsp-autotools.el ends here
