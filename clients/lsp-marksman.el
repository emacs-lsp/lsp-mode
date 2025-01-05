;;; lsp-marksman.el --- lsp-mode marksman integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 lsp-mode maintainers

;; Author: lsp-mode maintainers
;; Keywords: languages

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

;;  LSP client for marksman

;;; Code:

(require 'lsp-mode)

;;; Marksman
(defgroup lsp-marksman nil
  "Settings for the marksman language server client."
  :group 'lsp-mode
  :link '(url-link "https://github.com/artempyanykh/marksman")
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-marksman-server-command "marksman"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-marksman
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-marksman-server-command-args '()
  "Command-line arguments for the marksman lsp server. Not normally Needed."
  :type '(repeat string)
  :group 'lsp-marksman
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-marksman-download-url
  (format "https://github.com/artempyanykh/marksman/releases/latest/download/%s"
          (pcase system-type
            ('gnu/linux
             (if (string-match "^aarch64-.*" system-configuration)
                 "marksman-linux-arm64"
               "marksman-linux-x64"))
            ('darwin "marksman-macos")
            ('windows-nt "marksman.exe")))
  "Automatic download url for Marksman."
  :type 'string
  :group 'lsp-marksman
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-marksman-store-path (f-join lsp-server-install-dir
                                                "marksman"
                                                (if (eq system-type 'windows-nt)
                                                    "marksman.exe"
                                                  "marksman"))
  "The path to the file in which `marksman' will be stored."
  :type 'file
  :group 'lsp-marksman
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency 'marksman
                '(:system "marksman")
                `(:download :url lsp-marksman-download-url
                            :store-path lsp-marksman-store-path
                            :set-executable? t))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (or (executable-find lsp-marksman-server-command)
                                               (lsp-package-path 'marksman)
                                               "marksman")
                                           lsp-marksman-server-command-args)))
                  :activation-fn (lsp-activate-on "markdown")
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "marksman"))))
                  :priority -1
                  :server-id 'marksman
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'marksman callback error-callback))))

(lsp-consistency-check lsp-marksman)

(provide 'lsp-marksman)
;;; lsp-marksman.el ends here
