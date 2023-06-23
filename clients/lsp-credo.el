;;; lsp-credo.el --- lsp-mode Credo integration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wilhelm H Kirschbaum

;; Author: Wilhelm H Kirschbaum
;; Keywords: lsp, elixir, credo

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

;; LSP Client for Elixir Credo

;;; Code:

(require 'lsp-mode)

(defgroup lsp-credo nil
  "Settings for credo language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elixir-tools/credo-language-server")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-credo-command '("credo-language-server" "--stdio=true")
  "The command that starts credo-language-server."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-credo
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-credo-version "0.1.2"
  "Credo language server version to download.
It has to be set before `lsp-credo.el' is loaded and it has to
be available here: https://github.com/elixir-tools/credo-language-server/releases."
  :type 'string
  :group 'lsp-credo
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-credo-download-url
  (format (concat "https://github.com/elixir-tools/credo-language-server"
                  "/releases/download/v%s/credo-language-server")
          lsp-credo-version)
  "Automatic download url for credo-language-server."
  :type 'string
  :group 'lsp-credo
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-credo-binary-path
  (f-join lsp-server-install-dir
          "credo-language-server"
          "credo-language-server")
  "The path to `credo-language-server' binary."
  :type 'file
  :group 'lsp-credo
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency
 'credo-language-server
 `(:download :url lsp-credo-download-url
             :store-path ,(f-join lsp-server-install-dir
                                  "credo-language-server"
                                  "credo-language-server")
             :binary-path lsp-credo-binary-path
             :set-executable? t))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
     `(,(or (executable-find (cl-first lsp-credo-command))
            (lsp-package-path 'credo-language-server))
       ,@(cl-rest lsp-credo-command))))
  :activation-fn (lsp-activate-on "elixir")
  :priority -1
  :add-on? t
  :multi-root nil
  :server-id 'credo-language-server
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'credo-language-server callback error-callback))))

(lsp-consistency-check lsp-credo)

(provide 'lsp-credo)

;;; lsp-credo.el ends here
