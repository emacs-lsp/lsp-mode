;;; lsp-fortitude.el --- fortitude lsp support             -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Hill
;;
;; Author: Peter Hill <peter.hill@york.ac.uk>
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

;; Fortitude LSP Client for the Fortran programming language

;;; Code:

(require 'lsp-mode)

(defgroup lsp-fortitude nil
  "LSP support for Fortran, using fortitude's Fortran Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/PlasmaFAIR/fortitude"))

(defcustom lsp-fortitude-server-command '("fortitude" "server")
  "Command to start fortitude lsp."
  :risky t
  :type '(repeat string)
  :group 'lsp-fortitude)

(defcustom lsp-fortitude-fortitude-args '()
  "Arguments, passed to fortitude."
  :risky t
  :type '(repeat string)
  :group 'lsp-fortitude)

(defcustom lsp-fortitude-log-level "error"
  "Tracing level."
  :type '(choice (const "debug")
                 (const "error")
                 (const "info")
                 (const "off")
                 (const "warn"))
  :group 'lsp-fortitude)

(defcustom lsp-fortitude-advertize-fix-all t
  "Whether to report ability to handle source.fixAll actions."
  :type 'boolean
  :group 'lsp-fortitude)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (append lsp-fortitude-server-command lsp-fortitude-fortitude-args)))
  :activation-fn (lsp-activate-on "fortran")
  :server-id 'fortitude
  :priority -2
  :add-on? t
  :initialization-options
  (lambda ()
    (list :settings
          (list :logLevel lsp-fortitude-log-level
                :fixAll (lsp-json-bool lsp-fortitude-advertize-fix-all))))))

(lsp-consistency-check lsp-fortitude)

(provide 'lsp-fortitude)
;;; lsp-fortitude.el ends here
