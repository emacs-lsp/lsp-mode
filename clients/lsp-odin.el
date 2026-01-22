;;; lsp-odin.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Sam Precious
;; Copyright (C) 2025-2026 emacs-lsp maintainers
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
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "arm64")))
    (format "https://github.com/DanielGavin/ols/releases/download/nightly/%s"
            (pcase system-type
              ('gnu/linux (format "ols-%s-unknown-linux-gnu.zip" arch))
              ('darwin (format "ols-%s-darwin.zip" arch))
              ('windows-nt (format "ols-%s-pc-windows-msvc.zip" arch)))))
  "Automatic download url for ols language server."
  :type 'string
  :group 'lsp-odin-ols
  :package-version '(lsp-mode . "9.0.0"))

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

(defcustom lsp-odin-ols-server-dir
  (f-join lsp-odin-ols-server-install-dir "latest" "ols")
  "The path where ols .zip archive will be extracted."
  :group 'lsp-odin-ols
  :type 'file)

(defcustom lsp-odin-ols-binary-path
  (f-join lsp-odin-ols-server-install-dir "latest"
          (pcase system-type
            ('windows-nt
             "ols-x86_64-pc-windows-msvc.exe")
            ('darwin
             (if (string-match "aarch64-.*" system-configuration)
                 "ols-arm64-darwin"
               "ols-x86_64-darwin"))
            (_
             "ols-x86_64-unknown-linux-gnu")))
  "The path where ols binary after will be stored."
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
