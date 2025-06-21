;;; lsp-odin.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sam Precious
;;
;; Author: Sam Precious <samwdp@gmail.com>
;; Keywords: lsp, odin
;;
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
;;
;;; Commentary:
;;
;; LSP client for Odin using the ols language server
;;
;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-odin-ols nil
  "LSP support for Odin, using ols."
  :group 'lsp-mode
  :link '(url-link "https://github.com/DanielGavin/ols")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-odin-ols-download-url
  (concat "https://github.com/DanielGavin/ols/releases/download/nightly/"
          (cond ((eq system-type 'windows-nt)
                 (if (and (string-match "^x86_64-.*" system-configuration)
                          (version<= "26.4" emacs-version))
                     "ols-x86_64-pc-windows-msvc.zip"))

                ((eq system-type 'darwin)
                 (if (string-match "aarch64-.*" system-configuration)
                     "ols-arm64-darwin.zip"
                   "ols-x86_64-darwin.zip"))

                ((and (eq system-type 'gnu/linux)
                      (or (eq (string-match "^x86_64" system-configuration) 0)
                          (eq (string-match "^i[3-6]86" system-configuration) 0)))
                 "ols-x86_64-unknown-linux-gnu.zip")))
  "Automatic download url for ols language server."
  :group 'lsp-odin-ols
  :type 'string)


(defcustom lsp-odin-ols-server-install-dir
  (f-join lsp-server-install-dir "ols/")
  "Installation directory for ols server."
  :group 'lsp-odin-ols
  :type 'directory)

(defcustom lsp-odin-ols-store-path
  (f-join lsp-odin-ols-server-install-dir "latest" "ols.zip")
  "The path where ols .zip archive will be stored."
  :group 'lsp-odin-ols
  :type 'file)

(defcustom lsp-odin-ols-server-path
  nil
  "The path to ols language-server binary.
Set this if you have the binary installed or have it built yourself."
  :group 'lsp-odin-ols
  :type '(string :tag "Single string value or nil"))


(defcustom lsp-odin-ols-binary-path
  (f-join lsp-odin-ols-server-install-dir "latest" (if (eq system-type 'windows-nt)
                                                       "ols-x86_64-pc-windows-msvc.exe"
                                                     "ols-x86_64-unknown-linux-gnu"))
  "The path where ols binary after will be stored."
  :group 'lsp-odin-ols
  :type 'file)

(defcustom lsp-odin-ols-binary-path
  (f-join lsp-odin-ols-server-install-dir "latest" (cond ((eq system-type 'windows-nt)
                                                          "ols-x86_64-pc-windows-msvc.exe")

                                                         ((eq system-type 'darwin)
                                                          (if (string-match "aarch64-.*" system-configuration)
                                                              "ols-arm64-darwin"
                                                            "ols-x86_64-darwin"))

                                                         ((and (eq system-type 'gnu/linux)
                                                               (or (eq (string-match "^x86_64" system-configuration) 0)
                                                                   (eq (string-match "^i[3-6]86" system-configuration) 0)))
                                                          "ols-x86_64-unknown-linux-gnu")))
  "The path where ols binary after will be stored."
  :group 'lsp-odin-ols
  :type 'file)

(defcustom lsp-odin-ols-server-dir
  (f-join lsp-odin-ols-server-install-dir "latest" "ols")
  "The path where ols .zip archive will be extracted."
  :group 'lsp-odin-ols
  :type 'file)

(lsp-dependency
 'ols
 `(:download :url lsp-odin-ols-download-url
             :decompress :zip
             :store-path lsp-odin-ols-store-path
             :binary-path lsp-odin-ols-binary-path
             :set-executable? t)
 '(:system "ols"))

(defun lsp-odin--ols-download-server (_client callback error-callback _update?)
  "Download zip package for ols and install it.
Will invoke CALLBACK on success, ERROR-CALLBACK on error."
  (lsp-package-ensure 'ols callback error-callback))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-odin-ols-binary-path)
                  :major-modes '(odin-mode odin-ts-mode)
                  :language-id "odin"
                  :activation-fn (lsp-activate-on "odin")
                  :server-id 'ols
                  :multi-root t
                  :download-server-fn #'lsp-odin--ols-download-server))

(provide 'lsp-odin)
;;; lsp-odin.el ends here
