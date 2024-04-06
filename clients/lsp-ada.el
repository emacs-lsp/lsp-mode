;;; lsp-ada.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, ada

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

;; LSP Clients for the Ada Programming Language

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)

(defgroup lsp-ada nil
  "Settings for Ada Language Server."
  :group 'tools
  :tag "Language Server"
  :package-version '(lsp-mode . "6.2"))

(lsp-defcustom lsp-ada-project-file "default.gpr"
  "Set the project file full path to configure the language server with.
  The ~ prefix (for the user home directory) is supported.
  See https://github.com/AdaCore/ada_language_server for a per-project
  configuration example."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2")
  :lsp-path "ada.projectFile")

(lsp-defcustom lsp-ada-option-charset "UTF-8"
  "The charset to use by the Ada Language server. Defaults to 'UTF-8'."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2")
  :lsp-path "ada.defaultCharset")

(lsp-defcustom lsp-ada-enable-diagnostics t
  "A boolean to disable diagnostics. Defaults to true."
  :type 'boolean
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2")
  :lsp-path "ada.enableDiagnostics")

(defcustom lsp-ada-als-executable "ada_language_server"
  "Command to start the Ada language server."
  :group 'lsp-ada
  :risky t
  :type 'file)

(defcustom lsp-ada-alire-executable "alr"
  "The alire executable to run when a project is detected."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode "9.0.0"))

(defcustom lsp-ada-semantic-token-face-overrides
  '(("namespace" . default)
    ("modifier"  . lsp-face-semhl-keyword))
  "Semantic token face overrides to be applied."
  :type '(alist :key-type string
                :value-type (choice (face  :tag "Face")
                                    (const :tag "No Face" nil)))
  :group 'lsp-ada
  :package-version '(lsp-mode "9.0.0"))

(defcustom lsp-ada-semantic-token-modifier-face-overrides
  '(("declaration")
    ("definition")
    ("implementation")
    ("static")
    ("modification")
    ("documentation")
    ("defaultLibrary"))
  "Semantic token modifier face overrides to be applied."
  :type '(alist :key-type string
                :value-type (choice (face  :tag "Face")
                                    (const :tag "No Face" nil)))
  :group 'lsp-ada
  :package-version '(lsp-mode "9.0.0"))

(defvar lsp-ada--als-download-url-cache nil)

(defvar lsp-ada--als-downloaded-executable
  (f-join lsp-server-install-dir
          "ada-ls"
          (symbol-name (lsp-resolve-value lsp--system-arch))
          (pcase system-type
            ('gnu/linux  "linux")
            ('darwin     "darwin")
            ('windows-nt "win32")
            (_           "linux"))
          (concat "ada_language_server"
                  (pcase system-type
                    ('windows-nt ".exe")
                    (_ "")))))

(defun lsp-ada--als-latest-release-url ()
  "URL for the latest release of the Ada Language Server."
  (setq lsp-ada--als-download-url-cache
        (lsp--find-latest-gh-release-url
         "https://api.github.com/repos/AdaCore/ada_language_server/releases/latest"
         (format "%s.zip"
                 (pcase (list system-type (lsp-resolve-value lsp--system-arch))
                   ('(gnu/linux  x64)   "Linux_amd64")
                   ('(gnu/linux  arm64) "Linux_aarch64")
                   ('(darwin     x64)   "macOS_amd64")
                   ('(darwin     arm64) "macOS_aarch64")
                   ('(windows-nt x64)   "Windows_amd64")
                   (`(,_         x64)   "Linux_amd64"))))))

(defun lsp-ada--als-store-path ()
  "Store Path for the downloaded Ada Language Server."
  (f-join lsp-server-install-dir
          "ada-ls"
          (file-name-base (or lsp-ada--als-download-url-cache
                              (lsp-ada--als-latest-release-url)
                              "ada-ls"))))

(defun lsp-ada--environment ()
  "Add environmental variables if needed."
  (let ((project-root (lsp-workspace-root)))
    ;; When there is an alire project, include its environment
    (when (file-exists-p
           (concat (file-name-as-directory project-root)
                   "alire.toml"))
      (let ((alr-executable (executable-find lsp-ada-alire-executable)))
        (if alr-executable
            ;; Transform output variables to environment
            (let ((env-output (shell-command-to-string (concat alr-executable " printenv --unix"))))
              (let ((var-strings (split-string env-output "\n")))
                (mapcar (lambda (string)
                          (if (string-match (rx "export" space (group (one-or-more ascii)) "=" "\"" (group (one-or-more ascii)) "\"") string)
                              (let ((var-name (match-string 1 string))
                                    (var-value (match-string 2 string)))
                                (cons var-name var-value))))
                        var-strings)))
          (lsp--error "Found alire.toml but the executable %s could not be found" alr-executable))))))

(lsp-dependency
 'ada-ls
 '(:download :url lsp-ada--als-latest-release-url
             :store-path lsp-ada--als-store-path
             :decompress :zip
             :binary-path lsp-ada--als-downloaded-executable
             :set-executable? t)
 '(:system lsp-ada-als-executable))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (lsp-package-path 'ada-ls)))
                  :major-modes '(ada-mode ada-ts-mode)
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "ada"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'ada-ls callback error-callback))
                  :semantic-tokens-faces-overrides `( :types ,lsp-ada-semantic-token-face-overrides
                                                      :modifiers ,lsp-ada-semantic-token-modifier-face-overrides)
                  :server-id 'ada-ls
                  :synchronize-sections '("ada")
                  :environment-fn 'lsp-ada--environment))

(lsp-consistency-check lsp-ada)

(provide 'lsp-ada)
;;; lsp-ada.el ends here
