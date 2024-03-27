;;; lsp-nushell.el --- lsp-mode ansible integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-lsp maintainers

;; Author: lsp-mode maintainers
;; Keywords: lsp, nushell

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

;; LSP Client for the nushell Language

;;; Code:

(require 'lsp-mode)

(defgroup lsp-nushell nil
  "LSP support for nushell."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nushell/nushell"))

(defcustom lsp-nushell-language-server-command
  '("nu" "--lsp")
  "The command that starts the nushell language server."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-nushell)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-nushell-language-server-command)
                  :activation-fn (lsp-activate-on "nushell")
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    ;; Nushell server returns an empty list of
                                    ;; completion options at initialization
                                    ;; so completionProvider capability is {}
                                    ;; When using plists, this value is parsed as
                                    ;; null/nil so we need to force it to "t"
                                    ;; to enable completion
                                    (let ((caps (lsp--workspace-server-capabilities workspace)))
                                      (unless (lsp-get caps :completionProvider)
                                        (lsp:set-server-capabilities-completion-provider? caps t))))
                  :server-id 'nushell-ls))

(lsp-consistency-check lsp-nushell)

(provide 'lsp-nushell)
;;; lsp-nushell.el ends here
