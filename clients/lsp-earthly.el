;;; lsp-earthly.el --- earthlyls client         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel Loury

;; Author: Samuel Loury <konubinixweb@gmail.com>
;; Keywords: earthly lsp

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

;; LSP client for Earthfile

;;; Code:

(require 'lsp-mode)

(defgroup lsp-earthly nil
  "LSP support for Earthfile."
  :group 'lsp-mode
  :link '(url-link "https://github.com/glehmann/earthlyls")
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-earthly-active-modes
  '(earthfile-mode)
  "List of major mode that work with earthlyls."
  :type '(list symbol)
  :group 'lsp-earthly)

(defcustom lsp-earthly-home-url
  "https://github.com/glehmann/earthlyls"
  "Url we use to install earthlyls."
  :type 'string
  :group 'lsp-earthly
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-earthly-store-path (f-join lsp-server-install-dir "earthly")
  "The path to the file in which `earthlyls' will be stored."
  :type 'file
  :group 'lsp-earthly
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-earthly--download-server (_client callback error-callback update?)
  "Install/update earthly-ls language server using `cargo install'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t."
  (when update?
    (ignore-errors (delete-directory lsp-earthly-store-path t)))
  (lsp-async-start-process
   callback
   error-callback
   "cargo" "install" "--git" lsp-earthly-home-url "--root"
   lsp-earthly-store-path "earthlyls"))

(defun lsp-earthly--executable ()
  "Return earthlyls executable."
  (let ((local (f-join lsp-earthly-store-path "bin"
                       (if (eq system-type 'windows-nt)
                           "earthlyls.exe"
                         "earthlyls"))))
    (or (and (f-exists? local) local)
        (executable-find "earthlyls")
        (user-error "`earthlyls' is not installed; for installation see %s for more information" lsp-earthly-home-url))))

(defun lsp-earthly--server-command ()
  "Startup command for the earthlyls server."
  (list (lsp-earthly--executable)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-earthly--server-command
                   (lambda () (f-exists? lsp-earthly-store-path)))
  :major-modes lsp-earthly-active-modes
  :priority -1
  :server-id 'earthlyls
  :download-server-fn #'lsp-earthly--download-server))

(lsp-consistency-check lsp-earthly)

(provide 'lsp-earthly)
;;; lsp-earthly.el ends here
