;;; lsp-rf.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, rf, robot

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

;; LSP Clients for the Robot Framework.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-rf nil
  "Settings for Robot Framework Language Server."
  :group 'lsp-mode
  :tag "Language Server"
  :link '(url-link "https://github.com/tomi/vscode-rf-language-server"))

(defcustom lsp-rf-language-server-start-command '("~/.nvm/versions/node/v9.11.2/bin/node" "~/.vscode/extensions/tomiturtiainen.rf-intellisense-2.8.0/server/server.js")
  "Path to the server.js file of the rf-intellisense server. Accepts a list of strings (path/to/interpreter path/to/server.js)"
  :type 'list
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-include-paths []
  "An array of files that should be included by the parser. Glob patterns as strings are accepted (eg. *.robot between double quotes)"
  :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-exclude-paths []
  "An array of files that should be ignored by the parser. Glob patterns as strings are accepted (eg. *bad.robot between double quotes)"
  :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-dir "~/.vscode/extensions/tomiturtiainen.rf-intellisense-2.8.0/server/library-docs/"
  "Libraries directory for libraries in `lsp-rf-language-server-libraries'"
  :type 'string
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-libraries ["BuiltIn-3.1.1" "Collections-3.0.4"]
  "Libraries whose keywords are suggested with `auto-complete'."
  :type '(repeat string)
  ;; :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-log-level "debug"
  "What language server log messages are printed."
  :type 'string
  ;; :type '(choice (:tag "off" "errors" "info" "debug"))
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-trace-server "verbose"
  "Traces the communication between VSCode and the rfLanguageServer service."
  :type 'string
  ;; :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-rf)

(defun parse-rf-language-server-library-dirs (dirs)
  (vconcat (mapcar
   (lambda (x)
     (concat
      (expand-file-name
       lsp-rf-language-server-dir)
      x
      ".json"))
   dirs)))

(defun expand-start-command ()
  (mapcar 'expand-file-name lsp-rf-language-server-start-command))

(defun parse-rf-language-server-globs-to-regex (vector)
  "Convert a VECTOR of globs to a regex."
  (--> (mapcan #'lsp-glob-to-regexps vector)
       (s-join "\\|" it)
       (concat "\\(?:" it "\\)")))

(defun parse-rf-language-server-include-path-regex (vector)
  "Creates regexp to select files from workspace directory."
  (let ((globs (if (eq vector [])
                        ["*.robot" "*.resource"]
                      vector)))
    (parse-rf-language-server-globs-to-regex globs)))

(defun parse-rf-language-server-exclude-paths (seq)
  "Creates regexp to select files from workspace directory."
  (if (eq lsp-rf-language-server-exclude-paths [])
      seq
  (cl-delete-if (lambda (x) (string-match-p
                             (parse-rf-language-server-globs-to-regex
                              lsp-rf-language-server-exclude-paths)
                             x))
                seq)))

(lsp-register-custom-settings
 '(
   ("rfLanguageServer.trace.server" lsp-rf-language-server-trace-server)
   ("rfLanguageServer.logLevel" lsp-rf-language-server-log-level)
   ("rfLanguageServer.libraries" lsp-rf-language-server-libraries)
   ("rfLanguageServer.excludePaths" lsp-rf-language-server-exclude-paths)
   ("rfLanguageServer.includePaths" lsp-rf-language-server-include-paths)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (expand-start-command))
                  :major-modes '(robot-mode)
                  :server-id 'rf-intellisense
                  ;; :library-folders-fn (lambda (_workspace)
                  ;;                        lsp-rf-language-server-libraries)
                  :library-folders-fn (lambda (_workspace)
                                         (parse-rf-language-server-library-dirs
                                         lsp-rf-language-server-libraries))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "rfLanguageServer"))
                                      (lsp-request "buildFromFiles"
                                                   (list :files
                                                         (vconcat
                                                          (parse-rf-language-server-exclude-paths
                                                           (directory-files-recursively
                                                            (lsp--workspace-root workspace)
                                                            (parse-rf-language-server-include-path-regex
                                                             lsp-rf-language-server-include-paths))))))))))



(provide 'lsp-rf)
;;; lsp-rf.el ends here
