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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (list lsp-perl-language-server-path
                                           (format "-MPerl::LanguageServer -e Perl::LanguageServer::run -- --port %o --version %s"
                                                   lsp-perl-language-server-port lsp-perl-language-server-client-version))))
                  :major-modes '(perl-mode)
                  :priority -1
                  :server-id 'perl-language-server))

(provide 'lsp-perl)
;;; lsp-perl.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
