;;; lsp-meson.el --- lsp client for meson -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, meson

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
;;
;; LSP client for Meson language.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-meson nil
  "LSP support for Meson."
  :group 'lsp-mode
  :link '(url-link "https://github.com/JCWasmx86/mesonlsp"))

(defcustom lsp-meson-server-executable '("mesonlsp")
  "The meson language server executable to use."
  :group 'lsp-meson
  :risky t
  :type '(repeat string))

(defcustom lsp-meson-ignore-subproject-diagnostics nil
  "Ignore diagnostics from subprojects."
  :type '(choice
          (const :tag "Off" nil)
          (const :tag "All subprojects" t)
          (lsp-repeatable-vector :tag "Specific subprojects" string))
  :group 'lsp-meson)

(defcustom lsp-meson-no-auto-downloads nil
  "Never automatically download subprojects/wraps."
  :type '(boolean)
  :group 'lsp-meson)

(defcustom lsp-meson-disable-inlay-hints nil
  "Disable inlay hints."
  :type '(boolean)
  :group 'lsp-meson)

(defgroup lsp-meson-linting nil
  "Linting settings for mesonlsp."
  :group 'lsp-meson)

(defcustom lsp-meson-disable-name-linting nil
  "Disable checking whether variable names are snake_case."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-all-id-lints nil
  "Disable linting for unknown string literals relating to compiler/machine IDs."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-compiler-id-linting nil
  "Disable lints for unknown IDs compared against `compiler.get_id()'."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-compiler-argument-id-linting nil
  "Disable lints for unknown IDs compared against `compiler.get_argument_syntax()'."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-linker-id-linting nil
  "Disable lints for unknown IDs compared against `compiler.get_linker_id()'."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-cpu-family-linting nil
  "Disable lints for unknown IDs compared against `X_machine.cpu_family()'."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defcustom lsp-meson-disable-os-family-linting nil
  "Disable lints for unknown IDs compared against `X_machine.system()'."
  :type '(boolean)
  :group 'lsp-meson-linting)

(defun lsp-meson--make-init-options ()
  "Init options for mesonlsp."
  `(:others (:ignoreDiagnosticsFromSubprojects
             ,(if (vectorp lsp-meson-ignore-subproject-diagnostics)
                  lsp-meson-ignore-subproject-diagnostics
                (lsp-json-bool lsp-meson-ignore-subproject-diagnostics))
             :neverDownloadAutomatically ,(lsp-json-bool lsp-meson-no-auto-downloads)
             :disableInlayHints ,(lsp-json-bool lsp-meson-disable-inlay-hints))
    :linting (:disableNameLinting ,(lsp-json-bool lsp-meson-disable-name-linting)
              :disableAllIdLinting ,(lsp-json-bool lsp-meson-disable-all-id-lints)
              :disableCompilerIdLinting ,(lsp-json-bool lsp-meson-disable-compiler-id-linting)
              :disableCompilerArgumentIdLinting ,(lsp-json-bool lsp-meson-disable-compiler-argument-id-linting)
              :disableLinkerIdLinting ,(lsp-json-bool lsp-meson-disable-linker-id-linting)
              :disableCpuFamilyLinting ,(lsp-json-bool lsp-meson-disable-cpu-family-linting)
              :disableOsFamilyLinting ,(lsp-json-bool lsp-meson-disable-os-family-linting))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (append lsp-meson-server-executable '("--lsp"))))
  :activation-fn (lsp-activate-on "meson")
  :multi-root nil
  :priority -1
  :major-modes '(meson-mode)
  :initialization-options #'lsp-meson--make-init-options
  :server-id 'mesonlsp))

(lsp-consistency-check lsp-meson)

(provide 'lsp-meson)
;;; lsp-meson.el ends here
