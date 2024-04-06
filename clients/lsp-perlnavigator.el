;;; lsp-perlnavigator.el --- Integrates the Perl Navigator LSP Server with lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Matthew Feinberg

;; Author: Matthew Feinberg <matthew.feinberg@gmail.com>
;; Keywords: lsp, perl

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

;;; Code:

(require 'lsp-mode)

(defgroup lsp-perlnavigator nil
  "LSP support for Perl Navigator."
  :group 'lsp-mode
  :link '(url-link "https://github.com/bscan/PerlNavigator")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-perl-path "perl"
  "Full path to the perl executable (no aliases, .bat files or ~/)."
  :type 'string
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-enable-warnings t
  "Enable warnings using -Mwarnings command switch."
  :type 'boolean
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-perltidy-profile nil
  "Path to perl tidy profile (no aliases, .bat files or ~/)."
  :type 'string
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-perlcritic-profile nil
  "Path to perl critic profile. Otherwise perlcritic itself will
default to ~/.perlcriticrc. (no aliases, .bat files or ~/)."
  :type 'string
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-perlcritic-enabled t
  "Enable perl critic."
  :type 'boolean
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-severity5 "warning"
  "Editor Diagnostic severity level for Critic severity 5."
  :type '(choice (:tag "error" "warning" "info" "hint" "none"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-severity4 "info"
  "Editor Diagnostic severity level for Critic severity 4."
  :type '(choice (:tag "error" "warning" "info" "hint" "none"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-severity3 "hint"
  "Editor Diagnostic severity level for Critic severity 3."
  :type '(choice (:tag "error" "warning" "info" "hint" "none"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-severity2 "hint"
  "Editor Diagnostic severity level for Critic severity 2."
  :type '(choice (:tag "error" "warning" "info" "hint" "none"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-severity1 "hint"
  "Editor Diagnostic severity level for Critic severity 1."
  :type '(choice (:tag "error" "warning" "info" "hint" "none"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-include-paths nil
  "Array of paths added to @INC.  You can use $workspaceRoot as a placeholder."
  :type 'lsp-string-vector
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-logging t
  "Log to stdout from the navigator.  Viewable in the Perl Navigator LSP log."
  :type 'boolean
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-perlnavigator-trace-server "messages"
  "Traces the communication between VS Code and the language server."
  :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-perlnavigator
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-custom-settings
 '(("perlnavigator.trace.server" lsp-perlnavigator-trace-server)
   ("perlnavigator.logging" lsp-perlnavigator-logging t)
   ("perlnavigator.includePaths" lsp-perlnavigator-include-paths)
   ("perlnavigator.severity1" lsp-perlnavigator-severity1)
   ("perlnavigator.severity2" lsp-perlnavigator-severity2)
   ("perlnavigator.severity3" lsp-perlnavigator-severity3)
   ("perlnavigator.severity4" lsp-perlnavigator-severity4)
   ("perlnavigator.severity5" lsp-perlnavigator-severity5)
   ("perlnavigator.perlcriticEnabled" lsp-perlnavigator-perlcritic-enabled t)
   ("perlnavigator.perlcriticProfile" lsp-perlnavigator-perlcritic-profile)
   ("perlnavigator.perltidyProfile" lsp-perlnavigator-perltidy-profile)
   ("perlnavigator.enableWarnings" lsp-perlnavigator-enable-warnings t)
   ("perlnavigator.perlPath" lsp-perlnavigator-perl-path)))

(defcustom lsp-perlnavigator-executable "perlnavigator"
  "Location of the perlnavigator binary."
  :group 'lsp-perlnavigator
  :risky t
  :type 'file)

(defvar lsp-perlnavigator--os-suffix
  (let ((x86_64 (eq (string-match "^x86_64" system-configuration) 0)))
    (cond ((and x86_64 (eq system-type 'windows-nt))
           "-win-x86_64")

          ((and x86_64 (eq system-type 'darwin))
           "-macos-x86_64")

          ((and x86_64 (eq system-type 'gnu/linux))
           "-linux-x86_64")))

  "The suffix used to specify the download for this operating system.")

(defcustom lsp-perlnavigator-download-url
  (let ((base-url "https://github.com/bscan/PerlNavigator/releases/latest/download/"))
    (if lsp-perlnavigator--os-suffix
        (concat base-url "perlnavigator" lsp-perlnavigator--os-suffix ".zip")))

  "Automatic download url for PerlNavigator."
  :group 'lsp-perlnavigator
  :type 'string)

(defcustom lsp-perlnavigator-autoinstall-dir
  (f-join lsp-server-install-dir "perlnavigator")
  "Automatic installation directory for Perl Navigator."
  :group 'lsp-perlnavigator
  :type 'directory)

(defvar lsp-perlnavigator--autoinstall-store-path
  (f-join lsp-perlnavigator-autoinstall-dir "latest" (concat "perlnavigator" lsp-perlnavigator--os-suffix ".zip"))
  "The path where the downloaded PerlNavigator .zip archive will be stored.")


(defvar lsp-perlnavigator--autoinstall-binary-path
    (let ((exe-name (if (eq system-type 'windows-nt) "perlnavigator.exe" "perlnavigator")))
      (f-join lsp-perlnavigator-autoinstall-dir "latest" (concat "perlnavigator" lsp-perlnavigator--os-suffix) exe-name))
    "The path to the automatically installed language server executable.")

(lsp-dependency
 'perlnavigator
 '(:system lsp-perlnavigator-executable)
 `(:download
   :decompress
   :zip
   :binary-path lsp-perlnavigator--autoinstall-binary-path
   :url lsp-perlnavigator-download-url
   :store-path lsp-perlnavigator--autoinstall-store-path
   :set-executable? t))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (list
                                                           (or (lsp-package-path 'perlnavigator)
                                                               lsp-perlnavigator-executable)
                                                           "--stdio")))
                  :activation-fn (lsp-activate-on "perl")
                  :priority 0
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'perlnavigator callback error-callback))
                  :server-id 'perlnavigator))

(provide 'lsp-perlnavigator)
;;; lsp-perlnavigator.el ends here
