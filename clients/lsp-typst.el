;;; lsp-typst.el --- lsp-mode Typst integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, languages

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

;;  Client for the Tinymist language server for Typst.
;;  https://github.com/Myriad-Dreamin/tinymist

;;; Code:

(require 'lsp-mode)

(defgroup lsp-typst nil
  "LSP support for Typst, using Tinymist."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Myriad-Dreamin/tinymist")
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-typst-server-command '("tinymist")
  "Command to run the Tinymist language server."
  :type '(repeat string)
  :group 'lsp-typst
  :risky t
  :package-version '(lsp-mode . "9.0.1"))

;;; Formatter options

(lsp-defcustom lsp-typst-formatter-mode "typstyle"
  "Formatter to use for Typst documents."
  :type '(choice (const "disable")
                 (const "typstyle")
                 (const "typstfmt"))
  :group 'lsp-typst
  :lsp-path "tinymist.formatterMode"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-formatter-indent-size 2
  "Number of spaces for indentation."
  :type 'number
  :group 'lsp-typst
  :lsp-path "tinymist.formatterIndentSize"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-formatter-print-width 120
  "Soft limit on line width for the formatter."
  :type 'number
  :group 'lsp-typst
  :lsp-path "tinymist.formatterPrintWidth"
  :package-version '(lsp-mode . "9.0.1"))

;;; Export options

(lsp-defcustom lsp-typst-export-pdf "never"
  "When to export PDF.
Possible values: never, onSave, onType, onDocumentHasTitle."
  :type '(choice (const "never")
                 (const "onSave")
                 (const "onType")
                 (const "onDocumentHasTitle"))
  :group 'lsp-typst
  :lsp-path "tinymist.exportPdf"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-export-target "paged"
  "Export target format."
  :type '(choice (const "paged")
                 (const "html"))
  :group 'lsp-typst
  :lsp-path "tinymist.exportTarget"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-output-path ""
  "Path pattern for output files.
Supports $root, $dir, $name variable substitution."
  :type 'string
  :group 'lsp-typst
  :risky t
  :lsp-path "tinymist.outputPath"
  :package-version '(lsp-mode . "9.0.1"))

;;; Lint options

(lsp-defcustom lsp-typst-lint-enabled nil
  "Enable or disable lint checks."
  :type 'boolean
  :group 'lsp-typst
  :lsp-path "tinymist.lint.enabled"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-lint-when "onSave"
  "When to perform lint checks."
  :type '(choice (const "onSave")
                 (const "onType"))
  :group 'lsp-typst
  :lsp-path "tinymist.lint.when"
  :package-version '(lsp-mode . "9.0.1"))

;;; Font and path options

(lsp-defcustom lsp-typst-font-paths nil
  "List of file or directory paths to fonts."
  :type 'lsp-string-vector
  :group 'lsp-typst
  :risky t
  :lsp-path "tinymist.fontPaths"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-system-fonts t
  "Include system fonts in font search."
  :type 'boolean
  :group 'lsp-typst
  :lsp-path "tinymist.systemFonts"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-root-path nil
  "Root path for absolute paths in Typst.
Set to \"-\" to use parent directory of the file."
  :type '(choice (const nil)
                 string)
  :group 'lsp-typst
  :risky t
  :lsp-path "tinymist.rootPath"
  :package-version '(lsp-mode . "9.0.1"))

;;; Miscellaneous options

(lsp-defcustom lsp-typst-semantic-tokens t
  "Enable semantic tokens for syntax highlighting."
  :type 'boolean
  :group 'lsp-typst
  :lsp-path "tinymist.semanticTokens"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-project-resolution "singleFile"
  "How to resolve projects.
singleFile: Manage documents like Markdown.
lockDatabase: Manage documents like Rust with project tracking."
  :type '(choice (const "singleFile")
                 (const "lockDatabase"))
  :group 'lsp-typst
  :lsp-path "tinymist.projectResolution"
  :package-version '(lsp-mode . "9.0.1"))

(lsp-defcustom lsp-typst-extra-args ""
  "Extra arguments to pass to the Typst compiler."
  :type 'string
  :group 'lsp-typst
  :lsp-path "tinymist.typstExtraArgs"
  :package-version '(lsp-mode . "9.0.1"))

;;; Client registration

(lsp-dependency 'tinymist
                '(:system "tinymist"))

(lsp-register-client
 (make-lsp-client :server-id 'tinymist
                  :new-connection (lsp-stdio-connection (lambda () lsp-typst-server-command))
                  :activation-fn (lsp-activate-on "typst")
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "tinymist"))))
                  :synchronize-sections '("tinymist")))

(lsp-consistency-check lsp-typst)

(provide 'lsp-typst)
;;; lsp-typst.el ends here
