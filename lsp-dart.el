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

(require 'ht)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk/"
  "Install directory for dart-sdk."
  :group 'lsp-dart
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-server-command nil
  "The analysis_server executable to use"
  :type '(repeat string)
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-only-analyze-projects-with-open-files t
  "When set to non-nil, analysis will only be performed for projects that have open files
rather than the root workspace folder. Defaults to t"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-dart-suggest-from-unimported-libraries t
  "When set to nil, completion will not include synbols that are not already
imported into the current file. Defaults to true"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-dart-closing-labels t
  "When set to non-nil, dart/textDocument/publishClosingLabel notifications will
be sent with information to render editor closing labels. Defaults to nil"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-closing-labels-prefix " "
  "The prefix string to be concatened with the closing label.
Defaults to a single space"
  :type 'string
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-closing-labels-size 0.9
  "The font size factor to be multiplied by the closing labels font size.
Defaults to 0.9"
  :type 'float
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (or
   lsp-dart-server-command
   `(,(expand-file-name (f-join lsp-dart-sdk-dir "bin/dart"))
     ,(expand-file-name (f-join lsp-dart-sdk-dir "bin/snapshots/analysis_server.dart.snapshot"))
     "--lsp")))

(defun lsp-dart--handle-closing-labels (_workspace params)
  "Closing labels notification handling.
PARAMS closing labels notification data sent from WORKSPACE."
  (-let* (((&hash "uri" "labels") params)
          (buffer (get-file-buffer (string-remove-prefix "file://" uri))))
    (remove-overlays (point-min) (point-max) 'lsp-dart-closing-labels t)
    (seq-doseq (label-ht labels)
      (save-excursion
        (-let* ((label (gethash "label" label-ht))
                (range (gethash "range" label-ht))
                ((beg . end) (lsp--range-to-region range))
                (end-line (progn
                            (goto-char end)
                            (line-end-position)))
                (overlay (make-overlay beg end-line buffer)))
          (overlay-put overlay 'lsp-dart-closing-labels t)
          (overlay-put overlay 'after-string (propertize (concat lsp-dart-closing-labels-prefix " " label)
                                                         'display `((height ,lsp-dart-closing-labels-size))
                                                         'cursor t
                                                         'font-lock-face 'font-lock-comment-face)))))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   'lsp-dart--server-command)
                  :major-modes '(dart-mode)
                  :priority -1
                  :initialization-options
                  `((onlyAnalyzeProjectsWithOpenFiles . ,lsp-dart-only-analyze-projects-with-open-files)
                    (suggestFromUnimportedLibraries . ,lsp-dart-suggest-from-unimported-libraries)
                    (closingLabels . ,lsp-dart-closing-labels))
                  :notification-handlers (ht ("dart/textDocument/publishClosingLabels" 'lsp-dart--handle-closing-labels))
                  :server-id 'dart_analysis_server))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
