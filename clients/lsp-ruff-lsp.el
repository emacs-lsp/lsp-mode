;;; lsp-ruff-lsp.el --- ruff-lsp support             -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Freja Nordsiek
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

;; ruff-lsp Client for the Python programming language

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ruff-lsp nil
  "LSP support for Python, using ruff-lsp's Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/charliermarsh/ruff-lsp"))

(defcustom lsp-ruff-lsp-server-command '("ruff-lsp")
  "Command to start ruff-lsp."
  :risky t
  :type '(repeat string)
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-ruff-path ["ruff"]
  "Paths to ruff to try, in order."
  :risky t
  :type 'lsp-string-vector
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-ruff-args []
  "Arguments, passed to ruff."
  :risky t
  :type 'lsp-string-vector
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-log-level "error"
  "Tracing level."
  :type '(choice (const "debug")
                 (const "error")
                 (const "info")
                 (const "off")
                 (const "warn"))
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-python-path "python3"
  "Path to the Python interpreter."
  :risky t
  :type 'string
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-show-notifications "off"
  "When notifications are shown."
  :type '(choice (const "off")
                 (const "onError")
                 (const "onWarning")
                 (const "always"))
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-advertize-organize-imports t
  "Whether to report ability to handle source.organizeImports actions."
  :type 'boolean
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-advertize-fix-all t
  "Whether to report ability to handle source.fixAll actions."
  :type 'boolean
  :group 'lsp-ruff-lsp)

(defcustom lsp-ruff-lsp-import-strategy "fromEnvironment"
  "Where ruff is imported from if lsp-ruff-lsp-ruff-path is not set."
  :type '(choice (const "fromEnvironment")
                 (const "useBundled"))
  :group 'lsp-ruff-lsp)


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () lsp-ruff-lsp-server-command))
  :activation-fn (lsp-activate-on "python")
  :server-id 'ruff-lsp
  :priority -2
  :add-on? t
  :initialization-options
  (lambda ()
    (list :settings
          (list :args lsp-ruff-lsp-ruff-args
                :logLevel lsp-ruff-lsp-log-level
                :path lsp-ruff-lsp-ruff-path
                :interpreter (vector lsp-ruff-lsp-python-path)
                :showNotifications lsp-ruff-lsp-show-notifications
                :organizeImports (lsp-json-bool lsp-ruff-lsp-advertize-organize-imports)
                :fixAll (lsp-json-bool lsp-ruff-lsp-advertize-fix-all)
                :importStrategy lsp-ruff-lsp-import-strategy)))))

(lsp-consistency-check lsp-ruff-lsp)

(provide 'lsp-ruff-lsp)
;;; lsp-ruff-lsp.el ends here
