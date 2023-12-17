;;; lsp-bash.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, bash, shell-script

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

;; LSP Clients for the Bash Programming Language

;;; Code:

(require 'lsp-mode)

;;; Bash
(defgroup lsp-bash nil
  "Settings for the Bash Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/bash-lsp/bash-language-server")
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-explainshell-endpoint nil
  "The endpoint to use explainshell.com to answer `onHover' queries.
See instructions at https://marketplace.visualstudio.com/items?itemName=mads-hartmann.bash-ide-vscode"
  :type 'string
  :risky t
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-highlight-parsing-errors nil
  "Consider parsing errors in scripts as `problems'."
  :type 'boolean
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-glob-pattern nil
  "Glob pattern used to find shell script files to parse."
  :type 'string
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.3"))

(defun lsp-bash--bash-ls-server-command ()
  "Startup command for Bash language server."
  (list (lsp-package-path 'bash-language-server) "start"))

(lsp-dependency 'bash-language-server
                '(:system "bash-language-server")
                '(:npm :package "bash-language-server"
                       :path "bash-language-server"))

(defvar sh-shell)

(defun lsp-bash-check-sh-shell (&rest _)
  "Check whether `sh-shell' is sh or bash.

This prevents the Bash server from being turned on in zsh files."
  (and (memq major-mode '(sh-mode bash-ts-mode ebuild-mode envrc-file-mode))
       (memq sh-shell '(sh bash))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-bash--bash-ls-server-command)
  :major-modes '(sh-mode bash-ts-mode ebuild-mode envrc-file-mode)
  :priority -1
  :activation-fn #'lsp-bash-check-sh-shell
  :environment-fn (lambda ()
                    '(("EXPLAINSHELL_ENDPOINT" . lsp-bash-explainshell-endpoint)
                      ("HIGHLIGHT_PARSING_ERRORS" . lsp-bash-highlight-parsing-errors)
                      ("GLOB_PATTERN" . lsp-bash-glob-pattern)))
  :server-id 'bash-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'bash-language-server callback error-callback))))

(lsp-consistency-check lsp-bash)

(provide 'lsp-bash)
;;; lsp-bash.el ends here
