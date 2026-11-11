;;; lsp-zuban.el --- Zuban Python language server client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, python

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

;; LSP client for Zuban, a Mypy-compatible Python type checker and
;; language server (https://zubanls.com/).

;;; Code:

(require 'lsp-mode)

(defgroup lsp-zuban nil
  "LSP support for Python (Zuban)."
  :group 'lsp-mode
  :link '(url-link "https://zubanls.com/")
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-server-command '("zuban" "server")
  "Command to start the Zuban language server."
  :type '(repeat string)
  :risky t
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-type-checking-mode nil
  "Type checking mode used by Zuban.
When nil, the option is not sent and Zuban uses its default (`auto')."
  :type '(choice (const :tag "Server default" nil)
                 (const :tag "Auto" "auto")
                 (const :tag "Default" "default")
                 (const :tag "Mypy compatible" "mypy")
                 (const :tag "Off" "off"))
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-disable-language-services nil
  "When non-nil, disable all Zuban language services except type checking."
  :type 'boolean
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-diagnostic-mode nil
  "Which files Zuban reports diagnostics for.
When nil, the option is not sent and Zuban uses its default,
`open-files-only'."
  :type '(choice (const :tag "Server default" nil)
                 (const :tag "Open files only" "open-files-only")
                 (const :tag "Whole workspace" "workspace"))
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-python-executable nil
  "Path to the Python executable of the environment to analyze.
When nil, the option is not sent and Zuban detects the environment itself."
  :type '(choice (const :tag "Server default" nil)
                 (file :tag "Python executable"))
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defcustom lsp-zuban-inlay-hint-mode nil
  "Whether Zuban provides inlay hints.
When nil, the option is not sent and Zuban uses its default (`default')."
  :type '(choice (const :tag "Server default" nil)
                 (const :tag "Default" "default")
                 (const :tag "Off" "off"))
  :group 'lsp-zuban
  :package-version '(lsp-mode . "10.0.1"))

(defun lsp-zuban--initialization-options ()
  "Build the `initializationOptions' sent to Zuban.
Options left at their nil default are omitted so that Zuban's own
defaults apply."
  (append
   (when lsp-zuban-type-checking-mode
     (list :typeCheckingMode lsp-zuban-type-checking-mode))
   (when lsp-zuban-disable-language-services
     (list :disableLanguageServices t))
   (when lsp-zuban-diagnostic-mode
     (list :diagnosticMode lsp-zuban-diagnostic-mode))
   (when lsp-zuban-python-executable
     (list :pythonExecutable (expand-file-name lsp-zuban-python-executable)))
   (when lsp-zuban-inlay-hint-mode
     (list :inlayHintMode lsp-zuban-inlay-hint-mode))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-zuban-server-command))
  :activation-fn (lsp-activate-on "python")
  :initialization-options #'lsp-zuban--initialization-options
  :priority -1
  :server-id 'zuban))

(lsp-consistency-check lsp-zuban)

(provide 'lsp-zuban)
;;; lsp-zuban.el ends here
