;;; lsp-haxe.el --- Haxe Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Yannik Böttcher

;; Author: Yannik Böttcher <yannikboettcher@outlook.de>
;; Keywords:

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

;; lsp-haxe client

;;; Code:


;; adapted from clangd configuration in lsp-clients.el
(require 'lsp-mode)

(defgroup lsp-haxe nil
  "LSP support for Haxe using the language server provided by vshaxe"
  :group 'lsp-mode
  :link '(url-link "https://github.com/vshaxe/vshaxe"))


;; Haxe ls is executed via node
(defcustom lsp-clients--haxe-executable "node"
  "Haxe ls is executed via node."
  :group 'lsp-haxe
  :risky t
  :type 'file)

;; The server.js is being passed to node as an argument
(defcustom lsp-clients--haxe-server-path '""
  "The path to the server.js file."
  :group 'lsp-haxe
  :risky t
  :type 'file)

;; Build the actual Haxe ls command.
(defun lsp-clients--haxe-command ()
  "Haxe ls startup command."
  `(,lsp-clients--haxe-executable ,lsp-clients--haxe-server-path))

;; https://github.com/yyoncho/lsp-mode/commit/72186e1adc089d772c87ed8f287eb3333b66bfa7
;; This is to force the client to send a didChangeConfiguration Message. Without this, the server won't start, https://github.com/vshaxe/vshaxe/issues/328#issuecomment-471809093
(defcustom lsp-clients--haxe-settings (list :haxe.executable "haxe")
  "Lsp clients configuration settings."
  :group 'lsp-haxe
  :risky t
  :type '(repeat string))

;; https://github.com/emacs-lsp/lsp-mode/blob/150a933694349df960dc8fd7a15e04f5727e6433/lsp-rust.el#L251
(defun lsp-clients--haxe-processStart (_workspace params)
  "Handle processStart notification.  Just logs PARAMS."
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--haxe-command)
                  :major-modes '(haxe-mode)
                  ; force didChangeConfiguration message
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration lsp-clients--haxe-settings)))
                  :priority -1
                  :server-id 'haxe
                  :notification-handlers (lsp-ht ("haxe/progressStart" 'lsp-clients--haxe-processStart)
                                                 ("haxe/progressStop" 'ignore))))

(provide 'lsp-haxe)
;;; lsp-haxe.el ends here
