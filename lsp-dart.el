;;; lsp-dart.el --- LSP mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Takeshi Tsukamoto

;; Author: Takeshi Tsukamoto <itometeam@gmail.com>
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

;; dart analysis server client

;;; Code:

(defgroup lsp-dart-analysis nil
  "LSP support for Dart, using dart analysis server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-analysis-sdk-dir "~/flutter/bin/cache/dart-sdk/"
  "Install directory for dart-sdk."
  :group 'lsp-dart-analysis
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-analysis-server-command nil
  "The analysis_server executable to use"
  :type '(repeat string)
  :group 'lsp-dart-analysis
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-analysis-only-analyze-projects-with-open-files t
  "When set to non-nil, analysis will only be performed for projects that have open files
rather than the root workspace folder. Defaults to t"
  :type 'boolean
  :group 'lsp-dart-analysis
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-dart-analysis-suggest-from-unimported-libraries t
  "When set to nil, completion will not include synbols that are not already
imported into the current file. Defaults to true"
  :type 'boolean
  :group 'lsp-dart-analysis
  :package-version '(lsp-mode . "6.2"))

(defun lsp-dart--analysis-server-command ()
  "Generate LSP startup command."
  (or
   lsp-dart-analysis-server-command
   `(,(expand-file-name (f-join lsp-dart-analysis-sdk-dir "bin/dart"))
     ,(expand-file-name (f-join lsp-dart-analysis-sdk-dir "bin/snapshots/analysis_server.dart.snapshot"))
     "--lsp")))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   'lsp-dart--analysis-server-command)
                  :major-modes '(dart-mode)
                  :priority -1
                  :initialization-options
                  `((onlyAnalyzeProjectsWithOpenFiles . ,lsp-dart-analysis-only-analyze-projects-with-open-files)
                    (suggestFromUnimportedLibraries . ,lsp-dart-analysis-suggest-from-unimported-libraries))
                  :server-id 'dart_analysis_server))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
