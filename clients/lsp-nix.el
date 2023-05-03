;;; lsp-nix.el --- lsp-mode nix integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode maintainers

;; Author: Seong Yong-ju <sei40kr@gmail.com>
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

;; Client for the rnix language server.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-nix-rnix nil
  "LSP support for Nix, using rnix-lsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nix-community/rnix-lsp"))

(defcustom lsp-nix-rnix-server-path "rnix-lsp"
  "Executable path for the server."
  :group 'lsp-nix-rnix
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-rnix-server-path))
                  :major-modes '(nix-mode)
                  :server-id 'rnix-lsp
                  :priority -1))

(defgroup lsp-nix-nil nil
  "LSP support for Nix, using nil."
  :group 'lsp-mode
  :link '(url-link "https://github.com/oxalica/nil"))

(defcustom lsp-nix-nil-server-path "nil"
  "Executable path for the server."
  :group 'lsp-nix-nil
  :type 'string
  :package-version '(lsp-mode . "8.0.1"))

(lsp-defcustom lsp-nix-nil-formatter nil
  "External formatter command with arguments.

  Example [nixpkgs-fmt]."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.formatting.command"
  :package-version '(lsp-mode . "8.0.1"))

(lsp-defcustom lsp-nix-nil-ignored-diagnostics nil
  "Ignored diagnostic kinds."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.diagnostics.ignored"
  :package-version '(lsp-mode . "8.0.1"))

(lsp-defcustom lsp-nix-nil-exclude-files-diagnostic nil
  "Files to exclude from showing diagnostics."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.diagnostics.excludedFiles"
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-nil-server-path))
                  :major-modes '(nix-mode)
                  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "nil"))))
                  :synchronize-sections '("nil")
                  :server-id 'nix-nil))

(lsp-consistency-check lsp-nix)

(provide 'lsp-nix)
;;; lsp-nix.el ends here
