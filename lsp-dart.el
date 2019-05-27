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

(defgroup lsp-dart nil
  "LSP support for Dart, using dart_language_server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/natebosch/dart_language_server"))

(defcustom lsp-clients-dart-server-command
  (expand-file-name (if (equal system-type 'windows-nt)
                        "~/Pub/Cache/bin/dart_language_server"
                      "~/.pub-cache/bin/dart_language_server"))
  "The dart_language_server executable to use."
  :group 'lsp-dart
  :type 'file)

(defun lsp-dart--lsp-command ()
  "Generate LSP startup command."
  (let ((dls lsp-clients-dart-server-command)
        (pub (executable-find "pub")))
    (if pub
        (if (executable-find dls)
            dls
          (message "Installing dart_language_server...")
          (shell-command (concat pub " global activate dart_language_server"))
          (message "Installed dart_language_server")
          dls)
      (error "Please ensure /path/to/dart-sdk/bin is on system path"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-dart--lsp-command)
                  :major-modes '(dart-mode)
                  :priority -2
                  :server-id 'dart_language_server))

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
                  :server-id 'dart_analysis_server))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
