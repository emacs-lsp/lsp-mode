;;; lsp-crates.el --- lsp-mode Crates integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: languages, tools

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

;;  client for the Crates language server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-crates nil
  "LSP support for Crates."
  :group 'lsp-mode
  :link '(url-link "https://github.com/MathiasPius/crates-lsp")
  :package-version `(lsp-mode . "9.0.1"))

(defcustom lsp-crates-executable "crates-lsp"
  "The crates-lsp executable to use.

Leave as just the executable name to use the default behavior of finding the
executable with variable `exec-path'."
  :group 'lsp-crates
  :type 'string)

(defcustom lsp-crates-cache-directory ""
  "Directory in which to cache information about available crate versions, to
avoid constantly querying crates.io.

Uses the OS-specific cache directory."
  :group 'lsp-crates
  :type 'string)

(defcustom lsp-crates-files ["Cargo.toml"]
  "List of exact filenames for which crates-lsp should provide feedback.
Avoids `crates-lsp' throwing errors if you happen to open a toml file with a
[dependencies] section, which does not contain Rust package names."
  :group 'lsp-crates
  :type '(vector string))

(defcustom lsp-crates-use-api nil
  "If true, uses the Crates API instead of the Crate Index.
There are almost no reasons to ever enable this, and doing so puts a lot more
strain on the services provided by `crates.io'."
  :group 'lsp-crates
  :type 'boolean)

(defcustom lsp-crates-inlay-hints t
  "If false, disables inlay hints."
  :group 'lsp-crates
  :type 'boolean)

(defcustom lsp-crates-up-to-date-hint "✓"
  "Text of inlay hint to show, when package is up to date."
  :group 'lsp-crates
  :type 'string)

(defcustom lsp-crates-needs-update-hint "❌ {}"
  "Text of inlay hint to show next to packages which should be updated. Any
appearance of {} within the string, will be replaced by the newer version,
which the package should be updated to."
  :group 'lsp-crates
  :type 'string)

(defcustom lsp-crates-diagnostics t
  "If false, disables diagnostics."
  :group 'lsp-crates
  :type 'boolean)

(defcustom lsp-crates-unknown-dep-severity 2
  "Sets severity of diagnostics indicating that a package could not be looked up."
  :group 'lsp-crates
  :type 'integer)

(defcustom lsp-crates-needs-update-severity 3
  "Sets severity of diagnostics indicating that a package needs to be updated."
  :group 'lsp-crates
  :type 'integer)

(defcustom lsp-crates-up-to-date-severity 4
  "Sets severity of diagnostics indicating that a package is up to date."
  :group 'lsp-crates
  :type 'integer)

;;
;;; Installation

(defcustom lsp-crates-server-store-path
  (expand-file-name "crates/" lsp-server-install-dir)
  "The path to the file in which crates-lsp will be stored."
  :type 'file
  :group 'lsp-crates)

(defconst lsp-crates-download-url-format
  "https://github.com/MathiasPius/crates-lsp/releases/latest/download/crates-lsp-%s-%s.%s"
  "Format to the download url link.")

(defun lsp-crates--download-url ()
  "Return Url points to the crates-lsp' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-crates-download-url-format arch "pc-windows-msvc" "zip"))
      (darwin
       (format lsp-crates-download-url-format arch "apple-darwin" "tar.gz"))
      (gnu/linux
       (format lsp-crates-download-url-format arch "unknown-linux-gnu" "tar.gz")))))

(defun lsp-crates--stored-executable ()
  "Return the stored crates-lsp executable.

This is differ from the variable `lsp-crates-executable'; this is local storage
and not the global storage."
  (f-join lsp-crates-server-store-path
          (pcase system-type
            ('windows-nt "crates-lsp.exe")
            (_ "crates-lsp"))))

;;
;;; Core

(lsp-register-custom-settings
 '(("crates-lsp.cache_directory" lsp-crates-cache-directory t)
   ("crates-lsp.files" lsp-crates-files t)
   ("crates-lsp.use_api" lsp-crates-use-api)
   ("crates-lsp.inlay_hints" lsp-crates-inlay-hints)
   ("crates-lsp.up_to_date_hint" lsp-crates-up-to-date-hint t)
   ("crates-lsp.needs_update_hint" lsp-crates-needs-update-hint t)
   ("crates-lsp.diagnostics" lsp-crates-diagnostics)
   ("crates-lsp.unknown_dep_severity" lsp-crates-unknown-dep-severity t)
   ("crates-lsp.needs_update_severity" lsp-crates-needs-update-severity t)
   ("crates-lsp.up_to_date_severity" lsp-crates-up-to-date-severity t)))

(lsp-dependency
 'crates-lsp
 '(:system "crates-lsp")
 `(:download :url ,(lsp-crates--download-url)
             :decompress ,(pcase system-type ('windows-nt :zip) (_ :targz))
             :store-path ,(f-join lsp-crates-server-store-path "temp")
             :set-executable? t)
 `(:system ,(lsp-crates--stored-executable)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (or (executable-find lsp-crates-executable)
                                  (lsp-crates--stored-executable)))
                   (lambda ()
                     (or (executable-find lsp-crates-executable)
                         (file-executable-p (lsp-crates--stored-executable)))))
  :activation-fn (lsp-activate-on "toml")
  :priority -1
  :server-id 'crates-lsp
  :initialization-options
  (lambda () (lsp-configuration-section "crates-lsp"))
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'crates-lsp callback error-callback))))

(lsp-consistency-check lsp-crates)

(provide 'lsp-crates)
;;; lsp-crates.el ends here
