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
                  :major-modes '(nix-mode nix-ts-mode)
                  :server-id 'rnix-lsp
                  :priority -2))

(defgroup lsp-nix-nixd nil
  "LSP support for Nix, using nixd language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nix-community/nixd"))

(defcustom lsp-nix-nixd-server-path "nixd"
  "Executable path for the server."
  :group 'lsp-nix-nixd
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-nix-nixd-server-arguments '()
  "Extra arguments for the server."
  :group 'lsp-nix-nixd
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-nix-nixd-formatting-command nil
  "External formatter command with arguments.

  Example: `[\"nixpkgs-fmt\"]`"
  :type 'lsp-string-vector
  :group 'lsp-nix-nixd
  :lsp-path "nixd.formatting.command"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-nix-nixd-nixpkgs-expr nil
  "This expression will be interpreted as \"nixpkgs\" toplevel
  Nixd provides package, lib completion/information from it.

  Resource Usage: Entries are lazily evaluated, entire nixpkgs takes 200~300MB
  for just \"names\". Package documentation, versions, are evaluated by-need.

  Example: `\"import <nixpkgs> { }\"`"
  :type 'string
  :group 'lsp-nix-nixd
  :lsp-path "nixd.nixpkgs.expr"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-nix-nixd-nixos-options-expr nil
  "Option set for NixOS option completion. If this is omitted, the default
  search path (`<nixpkgs>`) will be used.

  Example:
  `\"(builtins.getFlake \"/home/nb/nix\").nixosConfigurations.mnd.options\"`"
  :type 'string
  :group 'lsp-nix-nixd
  :lsp-path "nixd.options.nixos.expr"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-nix-nixd-home-manager-options-expr nil
  "Option set for home-manager option completion.

  Example:
  `\"(builtins.getFlake \"/home/nb/nix\").homeConfigurations.\"nb@mnd\".options\"`"
  :type 'string
  :group 'lsp-nix-nixd
  :lsp-path "nixd.options.home-manager.expr"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () (cons lsp-nix-nixd-server-path lsp-nix-nixd-server-arguments)))
                  :major-modes '(nix-mode nix-ts-mode)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "nixd"))))
                  :synchronize-sections '("nixd")
                  :server-id 'nixd-lsp
                  :priority -1))

(defgroup lsp-nix-nil nil
  "LSP support for Nix, using nil."
  :group 'lsp-mode
  :link '(url-link "https://github.com/oxalica/nil"))

(defcustom lsp-nix-nil-server-path "nil"
  "Executable path for the server."
  :group 'lsp-nix-nil
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nil-formatter nil
  "External formatter command with arguments.

  Example [nixpkgs-fmt]."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.formatting.command"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nil-ignored-diagnostics nil
  "Ignored diagnostic kinds."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.diagnostics.ignored"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nil-exclude-files-diagnostic nil
  "Files to exclude from showing diagnostics."
  :type 'lsp-string-vector
  :group 'lsp-nix-nil
  :lsp-path "nil.diagnostics.excludedFiles"
  :package-version '(lsp-mode . "9.0.0"))
(lsp-defcustom lsp-nix-nil-max-mem 10000
  "Max Memory MB"
  :type 'number
  :group 'lsp-nix-nil
  :lsp-path "nil.nix.maxMemoryMB"
  :package-version '(lsp-mode . "9.0.0"))
(lsp-defcustom lsp-nix-nil-auto-eval-inputs t
  "Auto Eval Inputs"
  :type 'boolean
  :group 'lsp-nix-nil
  :lsp-path "nil.nix.flake.autoEvalInputs"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-nil-server-path))
                  :major-modes '(nix-mode nix-ts-mode)
                  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "nil"))))
                  :synchronize-sections '("nil")
                  :server-id 'nix-nil))

(lsp-consistency-check lsp-nix)

(provide 'lsp-nix)
;;; lsp-nix.el ends here
