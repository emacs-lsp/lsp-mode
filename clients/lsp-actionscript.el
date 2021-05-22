;;; lsp-actionscript.el --- ActionScript Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jen-Chieh Shen

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: actionscript lsp

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

;; LSP client for ActionScript

;;; Code:

(require 'lsp-mode)

(defgroup lsp-actionscript nil
  "LSP support for ActionScript."
  :group 'lsp-mode
  :link '(url-link "https://github.com/BowlerHatLLC/vscode-as3mxml")
  :package-version `(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-java-path "java"
  "Path of the java executable."
  :group 'lsp-actionscript
  :type 'string)

(defcustom lsp-actionscript-sdk-path ""
  "Path to supported SDK.
See https://github.com/BowlerHatLLC/vscode-as3mxml/wiki/Choose-an-ActionScript-SDK-for-the-current-workspace-in-Visual-Studio-Code."
  :type 'string
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-version "1.5.0"
  "Version of ActionScript language server."
  :type 'string
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-extension-name
  (format "vscode-nextgenas-%s.vsix" lsp-actionscript-version)
  "File name of the extension file from language server."
  :type 'string
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-server-download-url
  (format "https://github.com/BowlerHatLLC/vscode-as3mxml/releases/download/v%s/%s"
          lsp-actionscript-version lsp-actionscript-extension-name)
  "Automatic download url for lsp-actionscript."
  :type 'string
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-server-store-path
  (f-join lsp-server-install-dir "as3mxml")
  "The path to the file in which `lsp-actionscript' will be stored."
  :type 'file
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-actionscript-option-charset "UTF8"
  "The charset to use by the ActionScript Language server."
  :type 'string
  :group 'lsp-actionscript
  :package-version '(lsp-mode . "7.1.0"))

(defun lsp-actionscript--extension-root ()
  "The path that the downloaded extension will extract to."
  (f-join lsp-actionscript-server-store-path
          (format "vscode-nextgenas-%s" lsp-actionscript-version)))

(defun lsp-actionscript--extension-path ()
  "Return full path of the downloaded extension."
  (f-join lsp-actionscript-server-store-path lsp-actionscript-extension-name))

(defun lsp-actionscript--extension-dir ()
  "Return as3mxml extension path."
  (f-join (lsp-actionscript--extension-root) "extension"))

(defun lsp-actionscript--server-command ()
  "Startup command for ActionScript language server."
  (list lsp-actionscript-java-path
        (format "-Droyalelib=%s" lsp-actionscript-sdk-path)
        (format "-Dfile.encoding=%s" lsp-actionscript-option-charset)
        "-cp"
        (format "%s/bundled-compiler/*;%s/bin/*"
                (lsp-actionscript--extension-dir) (lsp-actionscript--extension-dir))
        "com.as3mxml.vscode.Main"))

(defun lsp-actionscript--extension-path-zip ()
  "Change extension path from .vsix to .zip."
  (concat (f-no-ext (lsp-actionscript--extension-path)) ".zip"))

(lsp-dependency
 'as3mxml
 '(:system "as3mxml")
 `(:download :url lsp-actionscript-server-download-url
             :store-path ,(lsp-actionscript--extension-path-zip)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-actionscript--server-command
                   (lambda () (f-exists? (lsp-actionscript--extension-path-zip))))
  :major-modes '(actionscript-mode)
  :priority -1
  :server-id 'as3mxml-ls
  :download-server-fn (lambda (_client _callback error-callback _update?)
                        (lsp-package-ensure
                         'as3mxml
                         (lambda ()
                           ;; TODO: Error handling when unzip failed
                           (lsp-unzip (lsp-actionscript--extension-path-zip)
                                      (lsp-actionscript--extension-root)))
                         error-callback))))

(lsp-consistency-check lsp-actionscript)

(provide 'lsp-actionscript)
;;; lsp-actionscript.el ends here
