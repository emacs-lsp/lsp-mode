;;; lsp-metals.el --- Scala Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Ross A. Baker <ross@rossabaker.com>, Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Author: Ross A. Baker <ross@rossabaker.com>, Evgeny Kurnevsky <kurnevsky@gmail.com>
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

;; lsp-metals client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-metals nil
  "LSP support for Scala, using Metals."
  :group 'lsp-mode
  :link '(url-link "https://scalameta.org/metals")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-server-command "metals-emacs"
  "The command to launch the Scala language server."
  :group 'lsp-metals
  :type 'file
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-server-args '()
  "Extra arguments for the Scala language server."
  :group 'lsp-metals
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-java-home ""
  "The Java Home directory used for indexing JDK sources and locating
the `java` binary."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-scalafmt-config-path ""
  "Optional custom path to the .scalafmt.conf file. Should be relative
to the workspace root directory and use forward slashes / for file
separators (even on Windows)."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-sbt-script ""
  "Optional absolute path to an `sbt` executable to use for running
`sbt bloopInstall`. By default, Metals uses `java -jar sbt-launch.jar`
with an embedded launcher while respecting `.jvmopts` and `.sbtopts`.
Update this setting if your `sbt` script requires more customizations
like using environment variables."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-gradle-script ""
  "Optional absolute path to a `gradle` executable to use for running
`gradle bloopInstall`. By default, Metals uses gradlew with 5.3.1
gradle version. Update this setting if your `gradle` script requires
more customizations like using environment variables."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))


(lsp-register-custom-settings
 '(("metals.java-home" lsp-metals-java-home)
   ("metals.scalafmt-config-path" lsp-metals-scalafmt-config-path)
   ("metals.sbt-script" lsp-metals-sbt-script)
   ("metals.gradle-script" lsp-metals-gradle-script)))

(defun lsp-metals--server-command ()
  "Generate the Scala language server startup command."
  `(,lsp-metals-server-command ,@lsp-metals-server-args))

(defun lsp-metals-build-import ()
  "Unconditionally run `sbt bloopInstall` and re-connect to the build server."
  (interactive)
  (lsp-send-execute-command "build-import" ()))

(defun lsp-metals-build-connect ()
  "Unconditionally cancel existing build server connection and re-connect."
  (interactive)
  (lsp-send-execute-command "build-connect" ()))

(defun lsp-metals-doctor-run ()
  "Open the Metals doctor to troubleshoot potential build problems."
  (interactive)
  (lsp-send-execute-command "doctor-run" ()))

(defun lsp-metals-sources-scan ()
  "Walk all files in the workspace and index where symbols are defined."
  (interactive)
  (lsp-send-execute-command "source-scan" ()))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-metals--server-command)
		  :major-modes '(scala-mode)
      :priority -1
      :notification-handlers (ht ("metals/treeViewDidChange" #'ignore))
		  :server-id 'metals
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "metals"))))))

(provide 'lsp-metals)
;;; lsp-metals.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
