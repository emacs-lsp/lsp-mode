;;; lsp-pls.el --- PLS Integration for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Alexander Adolf

;; Author: Alexander Adolf <alexander.adolf@condition-alpha.com>
;; Maintainer: Alexander Adolf <alexander.adolf@condition-alpha.com>
;; Package-Requires: (lsp-mode)
;; Keywords: perl, lsp

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; PLS client
;; https://metacpan.org/pod/PLS

;;; Code:

(require 'lsp-mode)

(defgroup lsp-pls nil
  "LSP Mode support for PLS, the Perl Language Server."
  :group 'lsp-mode
  :link '(url-link "https://metacpan.org/pod/PLS")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-executable "pls"
  "Full path to the PLS executable."
  :type '(string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-arguments nil
  "Additional arguments needed to execute PLS."
  :type '(repeat 'string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-working-dir nil
  "Working directory to run PLS in.
Defaults to the workspace root when not configured."
  :type '(string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-include nil
  "Paths to be added to your @INC."
  :type '(repeat 'string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-perltidy-rc nil
  "Path to your .perltidyrc file.
Default is \"~/.perltidyrc\" when not configured."
  :type '(string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-perlcritic-rc nil
  "Path to your .perlcriticrc file.
Default is \"~/.perlcriticrc\" when not configured."
  :type '(string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-perlcritic-enabled t
  "Enable perlcritic checking."
  :type '(boolean)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-syntax-enabled t
  "Enable syntax checking."
  :type '(boolean)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-syntax-perl nil
  "Full path to an alternate perl used for syntax checking.
By default, the perl used to run PLS will be used."
  :type '(string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-pls-syntax-args nil
  "Additional arguments to pass to Perl when syntax checking.
This is useful if there is a BEGIN block in your code that
changes behavior depending on the contents of @ARGV."
  :type '(repeat 'string)
  :group 'lsp-pls
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-custom-settings
 '(("pls.cmd"                      lsp-pls-executable)
   ("pls.args"                     lsp-pls-arguments)
   ("pls.cwd"                      lsp-pls-working-dir)
   ("pls.inc"                      lsp-pls-include)
   ("pls.perltidy.perltidyrc"      lsp-pls-perltidy-rc)
   ("pls.perlcritic.perlcriticrc"  lsp-pls-perlcritic-rc)
   ("pls.perlcritic.enabled"       lsp-pls-perlcritic-enabled)
   ("pls.syntax.enabled"           lsp-pls-syntax-enabled)
   ("pls.syntax.perl"              lsp-pls-syntax-perl)
   ("pls.syntax.args"              lsp-pls-syntax-args)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (cons lsp-pls-executable lsp-pls-arguments)))
  :activation-fn (lsp-activate-on "perl")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "pls"))))
  :priority -1
  :server-id 'pls))

;; (lsp-consistency-check lsp-pls)

(provide 'lsp-pls)
;;; lsp-pls.el ends here
