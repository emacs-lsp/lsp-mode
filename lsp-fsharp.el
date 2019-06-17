;;; lsp-fsharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
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

;; lsp-fsharp client

;;; Code:

(require 'lsp-mode)
(require 'pcase)

(defgroup lsp-fsharp nil
  "LSP support for the F# Programming Language, using the FsharpAutoComplete server."
  :link '(url-link "https://github.com/fsharp/FsAutoComplete")
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-runtime 'net-core
  "The .NET runtime to use."
  :group 'lsp-fsharp
  :type '(choice (const :tag "Use .Net Core" 'net-core)
                 (const :tag "Use Mono" 'mono)
                 (const :tag "Use .Net Framework" 'net-framework))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-path nil
  "The path to fsautocomplete."
  :group 'lsp-fsharp
  :risky t
  :type 'file
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-args nil
  "Extra arguments for the F# language server."
  :type '(repeat string)
  :group 'lsp-fsharp
  :package-version '(lsp-mode . "6.1"))

(defun lsp-fsharp--fsac-runtime-cmd ()
  "Get the command required to run fsautocomplete based off of the current runtime."
  (pcase lsp-fsharp-server-runtime
    ('net-core '("dotnet"))
    ('mono '("mono"))
    ('net-framework nil)))

(defun lsp-fsharp--make-launch-cmd ()
  "Build the command required to launch fsautocomplete."
  (unless lsp-fsharp-server-path (lsp-warn "Cannot locate fsautocomplete"))
  (append (lsp-fsharp--fsac-runtime-cmd)
          (list lsp-fsharp-server-path "--mode" "lsp")
          lsp-fsharp-server-args))

(defun lsp-fsharp--make-init-options ()
  "Init options for F#."
  `(:automaticWorkspaceInit true))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-fsharp--make-launch-cmd)
                  :major-modes '(fsharp-mode)
                  :notification-handlers (ht ("fsharp/notifyCancel" #'ignore)
                                             ("fsharp/notifyWorkspace" #'ignore)
                                             ("fsharp/notifyWorkspacePeek" #'ignore))
                  :initialization-options 'lsp-fsharp--make-init-options
                  :server-id 'fsac))

(provide 'lsp-fsharp)
;;; lsp-fsharp.el ends here
