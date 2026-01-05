;;; lsp-ruff.el --- ruff lsp support             -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Freja Nordsiek
;; Copyright (C) 2023-2026 lsp-mode maintainers
;;
;; Author: Freja Nordsiek <fnordsie@posteo.net
;; Keywords: language tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ruff LSP Client for the Python programming language

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ruff nil
  "LSP support for Python, using ruff's Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/astral-sh/ruff"))

(defcustom lsp-ruff-server-command '("ruff" "server")
  "Command to start ruff lsp.
Previous ruff-lsp should change this to (\"ruff-lsp\")"
  :risky t
  :type '(repeat string)
  :group 'lsp-ruff)

(defcustom lsp-ruff-ruff-args '()
  "Arguments, passed to ruff."
  :risky t
  :type '(repeat string)
  :group 'lsp-ruff)

(defcustom lsp-ruff-log-level "error"
  "Tracing level."
  :type '(choice (const "debug")
                 (const "error")
                 (const "info")
                 (const "off")
                 (const "warn"))
  :group 'lsp-ruff)

(defcustom lsp-ruff-python-path "python3"
  "Path to the Python interpreter."
  :risky t
  :type 'string
  :group 'lsp-ruff)

(defcustom lsp-ruff-show-notifications "off"
  "When notifications are shown."
  :type '(choice (const "off")
                 (const "onError")
                 (const "onWarning")
                 (const "always"))
  :group 'lsp-ruff)

(defcustom lsp-ruff-advertize-organize-imports t
  "Whether to report ability to handle source.organizeImports actions."
  :type 'boolean
  :group 'lsp-ruff)

(defcustom lsp-ruff-advertize-fix-all t
  "Whether to report ability to handle source.fixAll actions."
  :type 'boolean
  :group 'lsp-ruff)

(defcustom lsp-ruff-import-strategy "fromEnvironment"
  "Where ruff is imported from if lsp-ruff-ruff-path is not set."
  :type '(choice (const "fromEnvironment")
                 (const "useBundled"))
  :group 'lsp-ruff)


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (append lsp-ruff-server-command lsp-ruff-ruff-args)))
  :activation-fn (lsp-activate-on "python")
  :server-id 'ruff
  :priority -2
  :add-on? t
  :multi-root t
  :initialization-options
  (lambda ()
    (list :settings
          (list :logLevel lsp-ruff-log-level
                :showNotifications lsp-ruff-show-notifications
                :organizeImports (lsp-json-bool lsp-ruff-advertize-organize-imports)
                :fixAll (lsp-json-bool lsp-ruff-advertize-fix-all)
                :importStrategy lsp-ruff-import-strategy)))))

(lsp-consistency-check lsp-ruff)

(provide 'lsp-ruff)
;;; lsp-ruff.el ends here
