;;; lsp-asm.el --- Assembly Language Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jen-Chieh Shen

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: asm lsp

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

;; LSP client for Assembly Language

;;; Code:

(require 'lsp-mode)

(defgroup lsp-asm nil
  "LSP support for Assembly Language."
  :group 'lsp-mode
  :link '(url-link "https://github.com/bergercookie/asm-lsp")
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-asm-active-modes
  '( asm-mode fasm-mode masm-mode nasm-mode gas-mode)
  "List of major mode that work with asm-lsp."
  :type '(list symbol)
  :group 'lsp-asm)

(defcustom lsp-asm-home-url
  "https://github.com/bergercookie/asm-lsp"
  "Url we use to install asm-lsp."
  :type 'string
  :group 'lsp-asm
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-asm-store-path (f-join lsp-server-install-dir "asm")
  "The path to the file in which `asm-lsp' will be stored."
  :type 'file
  :group 'lsp-asm
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-asm--download-server (_client callback error-callback update?)
  "Install/update asm-ls language server using `cargo install'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t."
  (when update?
    (ignore-errors (delete-directory lsp-asm-store-path t)))
  (lsp-async-start-process
   callback
   error-callback
   "cargo" "install" "--git" lsp-asm-home-url "--root" lsp-asm-store-path))

(defun lsp-asm--executable ()
  "Return asm-lsp executable."
  (let ((local (f-join lsp-asm-store-path "bin"
                       (if (eq system-type 'windows-nt)
                           "asm-lsp.exe"
                         "asm-lsp"))))
    (or (and (f-exists? local) local)
        (executable-find "asm-lsp")
        (user-error "`asm-lsp' is not installed; for installation see %s for more information" lsp-asm-home-url))))

(defun lsp-asm--server-command ()
  "Startup command for Assembly language server."
  (list (lsp-asm--executable)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-asm--server-command
                   (lambda () (or (executable-find "asm-lsp")
                                  (f-exists? lsp-asm-store-path))))
  :major-modes lsp-asm-active-modes
  :priority -1
  :server-id 'asm-lsp
  :download-server-fn #'lsp-asm--download-server))

(lsp-consistency-check lsp-asm)

(provide 'lsp-asm)
;;; lsp-asm.el ends here
