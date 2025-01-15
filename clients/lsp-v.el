;;; lsp-v.el --- lsp-mode V integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 remimimimi
;; Copyright (C) 2024 niontrix

;; Author: remimimimi
;;         niontrix
;; Keywords: languages,tools

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

;;  Basic configuration for V LSP support

;;; Code:

(require 'lsp-mode)

(defgroup lsp-v nil
  "LSP support for V via vls. NOTICE!: `vls' is deprecated"
  :group 'lsp-mode
  :link '(url-link "https://github.com/vlang/vls/tree/master"))

(defgroup lsp-v-analyzer nil
  "LSP support for V, using v-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vlang/v-analyzer"))

(defcustom lsp-v-vls-executable "vls"
  "NOTICE!: vls is deprecated you should use `v-analyzer' instead.
The vls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'. "
  :group 'lsp-v
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-v-analyzer-path "v-analyzer"
  "Path to `v-analyzer'
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'. "
  :type 'string
  :group 'lsp-v
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-v-vls-executable))
                  :activation-fn (lsp-activate-on "V")
                  :priority -1
                  :server-id 'v-ls))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-v-analyzer-path))
                  :activation-fn (lsp-activate-on "v")
                  :notification-handlers
                  (ht ("experimental/serverStatus" #'ignore))
                  :language-id "v"
                  :priority 1
                  :server-id 'v-analyzer))

(defun lsp-v-analyzer-init ()
  "Runs the `v-analyzer init' command in the root folder of the current project.
After this `v-analyzer' can be further configured through the file
`.v-analyzer/config.toml'."
  (interactive)
  (let* ((project-root (lsp--suggest-project-root))
         (default-directory project-root)
         (v-analyzer-config ".v-analyzer/config.toml"))
    (when (and project-root
               (not (file-exists-p v-analyzer-config)))
      (message
       (shell-command-to-string (concat lsp-v-analyzer-path " init"))))))

(lsp-consistency-check lsp-v)

(provide 'lsp-v)
;;; lsp-v.el ends here
