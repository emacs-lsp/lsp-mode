;;; lsp-gdscript.el --- LSP mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Oliver Frank

;; Author: Oliver Frank <oliverfrank321@gmail.com>
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

;; lsp-gdscript client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-gdscript nil
  "LSP support for GDScript, using godot's language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/godotengine/godot")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-gdscript-port 6005
  "Port to connect server to"
  :type 'integer
  :group 'lsp-gdscript)

(defun lsp-gdscript-tcp-connect-to-port ()
  "Define a TCP connection to language server."
  (list
   :connect (lambda (filter sentinel name _environment-fn _workspace)
              (let* ((host "localhost")
                     (port lsp-gdscript-port)
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (set-process-sentinel tcp-proc sentinel)
                (cons tcp-proc tcp-proc)))
   :test? (lambda () t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-gdscript-tcp-connect-to-port)
                  :activation-fn (lsp-activate-on "gdscript")
                  :server-id 'gdscript))

(lsp-consistency-check lsp-gdscript)

(provide 'lsp-gdscript)
;;; lsp-gdscript.el ends here
