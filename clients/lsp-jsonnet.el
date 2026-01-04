;;; lsp-jsonnet.el --- lsp client for jsonnet -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, jsonnet

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
;;
;; LSP client for jsonnet language.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-jsonnet nil
  "LSP support for jsonnet."
  :group 'lsp-mode
  :link '(url-link "https://github.com/grafana/jsonnet-language-server"))

(defcustom lsp-clients-jsonnet-server-executable '("jsonnet-language-server")
  "The jsonnet language server executable to use."
  :group 'lsp-jsonnet
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-jsonnet-server-download-url
  (concat "https://github.com/grafana/jsonnet-language-server/releases/download/v0.16.0/"
          (cond ((and (eq system-type 'gnu/linux)
                      (string-match ".*x86_64.*" system-configuration))
                 "jsonnet-language-server_0.16.0_linux_amd64")
                ((and (eq system-type 'gnu/linux)
                      (string-match ".*arm64.*" system-configuration))
                 "jsonnet-language-server_0.16.0_linux_arm64")
                ((and (eq system-type 'darwin)
                      (string-match ".*arm.*" system-configuration))
                 "jsonnet-language-server_0.16.0_darwin_arm64")
                ((and (eq system-type 'darwin)
                      (string-match ".*x86_64.*" system-configuration))
                 "jsonnet-language-server_0.16.0_darwin_amd64")
                ((eq system-type 'windows-nt)
                 "jsonnet-language-server_0.16.0_windows_amd64.exe")))
  "The jsonnet language server download url."
  :group 'lsp-jsonnet
  :type 'string)

(lsp-dependency 'jsonnet-language-server
                `(:system ,(cl-first lsp-clients-jsonnet-server-executable))
                `(:download :url ,lsp-clients-jsonnet-server-download-url
                            :store-path ,(f-join user-emacs-directory ".cache" "lsp"
                                                 (if (eq system-type 'windows-nt)
                                                     (concat (cl-first lsp-clients-jsonnet-server-executable) ".exe")
                                                   (cl-first lsp-clients-jsonnet-server-executable)))
                            :set-executable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda ()
                                          `(,(or (executable-find
                                                  (cl-first lsp-clients-jsonnet-server-executable))
                                                 (lsp-package-path 'jsonnet-language-server)
                                                 (cl-first lsp-clients-jsonnet-server-executable)))))
  :activation-fn (lsp-activate-on "jsonnet")
  :priority -1
  :major-modes '(jsonnet-mode)
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'jsonnet-language-server callback error-callback))
  :server-id 'jsonnet-lsp))

(lsp-consistency-check lsp-jsonnet)

(provide 'lsp-jsonnet)
;;; lsp-jsonnet.el ends here
