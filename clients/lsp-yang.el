;;; lsp-yang.el --- YANG Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Siddharth Sharma
;; Copyright (C) 2024-2026 lsp-mode maintainers

;; Author: Siddharth Sharma <siddharth.sharma@ericsson.com>
;; Keywords: languages, yang, lsp

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

;; LSP support for YANG using using an external language server. Currently
;; the supported server is:
;;
;; yang-lsp (yls).
;; See https://github.com/TypeFox/yang-lsp/blob/master/docs/Settings.md
;; for setting up the user/project/workspace files.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-yang nil
  "LSP support for the YANG data modeling language using yang-lsp server."
  :group 'lsp-yang
  :link '(url-link "https://github.com/TypeFox/yang-lsp"))

(defcustom lsp-yang-yls-version "0.7.6"
  "yang-lsp server version to download.

It has to be set before `lsp-yang.el' is loaded and it has to
be available here: https://github.com/TypeFox/yang-lsp/releases/"
  :type 'string
  :group 'lsp-yang
  :package-version '(lsp-mode . "9.0.0"))

(add-to-list 'auto-mode-alist '("^yang\\.settings$" . jsonc-mode))

(defcustom lsp-yang-yls-settings-schema-url
  (format "https://raw.githubusercontent.com/TypeFox/yang-lsp/v%s/schema/yang-lsp-settings-schema.json"
          lsp-yang-yls-version)
  "URL for yang-lsp server settings schema"
  :type 'string
  :group 'lsp-yang
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-yang-yls-executable "yang-language-server"
  "The yang-lsp server executable to use.

Leave as just the executable name to use the default behavior of finding the
executable with variable `exec-path'."
  :group 'lsp-yang
  :type 'string)

(defcustom lsp-yang-yls-download-url
  (format "https://github.com/TypeFox/yang-lsp/releases/download/v%s/yang-language-server_%s.zip"
          lsp-yang-yls-version
          lsp-yang-yls-version)
  "Automatic download url for yang-lsp server"
  :type 'string
  :group 'lsp-yang
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-yang-yls-store-path
  (f-join lsp-server-install-dir "yang-lsp" "yang-lsp")
  "The path to the file in which `yang-language-server' will be stored."
  :type 'file
  :group 'lsp-yang
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-yang-yls-binary-path
  (f-join lsp-server-install-dir (format "yang-lsp/yang-language-server-%s/bin"
                                         lsp-yang-yls-version)
          (pcase system-type
            ('windows-nt "yang-language-server.bat")
            (_ "yang-language-server")))
  "The path to `yang-language-server' binary."
  :type 'file
  :group 'lsp-yang
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-yang--stored-yls-executable ()
  "Return the stored yang-lsp server executable."
  (executable-find lsp-yang-yls-binary-path))

(lsp-dependency
  'yang-lsp
  `(:download :url lsp-yang-yls-download-url
              :decompress :zip
              :store-path lsp-yang-yls-store-path
              :binary-path lsp-yang-yls-binary-path
              :set-exectutable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (or (executable-find lsp-yang-yls-executable)
                                  (lsp-yang--stored-yls-executable)))
                   (lambda () (or (executable-find lsp-yang-yls-executable)
                                  (file-executable-p (lsp-yang--stored-yls-executable)))))
  :major-modes '(yang-mode)
  :language-id "YANG"
  :priority -1
  :server-id 'yls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'yang-lsp callback error-callback))))

(lsp-consistency-check lsp-yang)

(provide 'lsp-yang)
;;; lsp-yang.el ends here
