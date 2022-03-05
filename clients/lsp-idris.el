;;; lsp-idris.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 skykanin
;;
;; Author: skykanin <https://github.com/skykanin>
;; Keywords: idris lsp

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

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; LSP Client for the Idris2 Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)

(defgroup lsp-idris nil
  "LSP support for Idris."
  :link '(url-link "https://github.com/idris-community/idris2-lsp")
  :group 'lsp-mode
  :tag "Lsp Idirs"
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-idris2-lsp-path "idris2-lsp"
  "Command to start Idris 2 language server process."
  :group 'lsp-idris
  :type 'string
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-idris2-lsp-trace-server "off"
  "Traces the communication between VS Code and the language server."
  :group 'lsp-idris
  :type '(choice (:tag "off" "messages" "verbose"))
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-custom-settings
  '(("idris2-lsp.trace.server" lsp-idris2-lsp-trace-server)
    ("idris2-lsp.path" lsp-idris2-lsp-path)))

;; Register the client itself
(lsp-register-client
  (make-lsp-client
   :new-connection (lsp-stdio-connection lsp-idris2-lsp-path)
   ;; Activate lsp on idris or idris2 buffers
   :activation-fn (lsp-activate-on "idris" "idris2")
   ;; This should run under idris-mode and idris2-mode.
   :major-modes '(idris-mode idris2-mode)
   :language-id "idris"
   :server-id 'idris2-lsp))

(provide 'lsp-idris)
;;; lsp-idris.el ends here
