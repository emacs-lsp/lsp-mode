;;; lsp-perl.el --- lsp-perl config  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode developers

;; Author: Hiroki Noda <kubo39@gmail.com>
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

;; lsp-perl client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-perl nil
  "LSP support for Perl"
  :group 'lsp-mode
  :link '(url-link "https://github.com/richterger/Perl-LanguageServer")
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-perl-language-server-path "perl"
  "Path to perl interpreter."
  :type 'string
  :group 'lsp-perl
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-perl-language-server-port 13603
  "Choose listen port."
  :type 'integer
  :group 'lsp-perl
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-perl-language-server-client-version "2.1.0"
  "Choose client version."
  :type 'string
  :group 'lsp-perl
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-perl-perl-cmd nil
  "Path to perl interpreter used in Perl Language Server.
Defaults to `perl' if nil."
  :type 'string
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.0.1"))
(defcustom lsp-perl-perl-inc nil
  "A vector of paths to add to perl library path."
  :type 'lsp-string-vector
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.0.1"))
(defcustom lsp-perl-file-filter nil
  "A vector of directories filtering perl file.
Defaults to `[\".pm\" \".pl\"]' if nil."
  :type 'lsp-string-vector
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.0.1"))
(defcustom lsp-perl-ignore-dirs nil
  "A vector of directories to ignore.
Defaults to `[\".vscode\" \".git\" \".svn\"]' if nil."
  :type 'lsp-string-vector
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-perl-show-local-vars nil
  "If true, show also local variables in symbol view.
Defaults to false if nil"
  :type 'boolean
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-perl-log-level nil
  "Log level 0-2.
Defaults to 0."
  :type 'integer
  :group 'lsp-perl
  :package-version '(lsp-mode . "7.1"))

(lsp-register-custom-settings
 '(("perl.perlCmd" lsp-perl-perl-cmd)
   ("perl.perlInc" lsp-perl-perl-inc)
   ("perl.fileFilter" lsp-perl-file-filter)
   ("perl.ignoreDirs" lsp-perl-ignore-dirs)
   ("perl.showLocalVars" lsp-perl-show-local-vars t)
   ("perl.logLevel" lsp-perl-log-level)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (list lsp-perl-language-server-path
                                           "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run" "--"
                                           (format "--port %d --version %s"
                                                   lsp-perl-language-server-port lsp-perl-language-server-client-version))))
                  :major-modes '(perl-mode cperl-mode)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "perl"))))
                  :priority -1
                  :server-id 'perl-language-server))

(lsp-consistency-check lsp-perl)

(provide 'lsp-perl)
;;; lsp-perl.el ends here
