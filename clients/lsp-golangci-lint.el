;;; lsp-golangci-lint.el --- golangci-lint-langserver Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jim Myhrberg

;; Author: Jim Myhrberg
;; Keywords: lsp, go, golang, golangci-lint

;; This file is not part of GNU Emacs

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; lsp-golangci-lint client

;;; Code:

(require 'lsp-mode)
(require 'lsp-go)
(require 'cl-lib)

(defgroup lsp-golangci-lint nil
  "Configuration options for lsp-golangci-lint."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nametake/golangci-lint-langserver")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-server-path "golangci-lint-langserver"
  "Command to run golangci-lint-langserver."
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-server-debug nil
  "Whether to run golangci-lint-langserver in debug mode or not."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-server-args nil
  "Arguments to pass to golangci-lint-langserver."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-path "golangci-lint"
  "Command to run golangci-lint."
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-allow-parallel-runners t
  "If not nil, pass --allow-parallel-runners flag to golangci-lint run."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-build-tags nil
  "If non-empty list, pass as --build-tags flag value to golangci-lint run."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-fast nil
  "If not nil, pass --fast flag to golangci-lint run."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-enable-all nil
  "If not nil, pass --enable-all flag to golangci-lint run."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-enable nil
  "If non-empty list, pass as --enable flag value to golangci-lint run."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-disable-all nil
  "If not nil, pass --disable-all to golangci-lint run."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-disable nil
  "If non-empty list, pass as --disable flag value to golangci-lint run."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-config nil
  "If set, pass value as --config flag to golangci-lint run."
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-no-config nil
  "If not nil, pass --no-config flag to golangci-lint run."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-golangci-lint-run-args nil
  "Arguments to pass to golangci-lint run command."
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-golangci-lint-server--stdio-command ()
  "Return the command and args to start golangci-lint-langserver."
  (let ((args (list lsp-golangci-lint-server-path)))
    (when (and (listp lsp-golangci-lint-server-args)
               (> (length lsp-golangci-lint-server-args) 0))
      (setq args (append args lsp-golangci-lint-server-args)))
    (when lsp-golangci-lint-server-debug
      (setq args (append args '("-debug"))))
    args))

(defun lsp-golangci-lint--run-args ()
  "Return the arguments to pass to golangci-lint run command."
  (let* ((tags (string-join lsp-golangci-lint-build-tags " "))
         (enable (string-join lsp-golangci-lint-enable ","))
         (disable (string-join lsp-golangci-lint-disable ","))
         (args (cl-loop for (condition flag value) in
                        `((,lsp-golangci-lint-fast "--fast" nil)
                          (,(not (string-empty-p tags)) "--build-tags" ,tags)
                          (,lsp-golangci-lint-enable-all "--enable-all" nil)
                          (,lsp-golangci-lint-disable-all "--disable-all" nil)
                          (,(not (string-empty-p enable)) "--enable" ,enable)
                          (,(not (string-empty-p disable)) "--disable" ,disable)
                          (,lsp-golangci-lint-allow-parallel-runners
                           "--allow-parallel-runners" nil)
                          (,(and (stringp lsp-golangci-lint-config)
                                 (not (string-empty-p lsp-golangci-lint-config)))
                           "--config" lsp-golangci-lint-config))
                        when condition
                        append (if value (list flag value) (list flag)))))
    (when (and (listp lsp-golangci-lint-run-args)
               (> (length lsp-golangci-lint-run-args) 0))
      (setq args (append args lsp-golangci-lint-run-args)))
    args))

(defun lsp-golangci-lint--get-initialization-options ()
  "Return initialization options for golangci-lint-langserver."
  (let ((opts (make-hash-table :test 'equal))
        (command (vconcat `(,lsp-golangci-lint-path)
                          ["run" "--out-format=json" "--issues-exit-code=1"]
                          (lsp-golangci-lint--run-args))))
    (puthash "command" command opts)
    opts))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-golangci-lint-server--stdio-command)
                  :activation-fn (lsp-activate-on "go")
                  :language-id "go"
                  :priority 0
                  :server-id 'golangci-lint
                  :add-on? t
                  :library-folders-fn #'lsp-go--library-default-directories
                  :initialization-options #'lsp-golangci-lint--get-initialization-options))

(lsp-consistency-check lsp-golangci-lint)

(provide 'lsp-golangci-lint)
;;; lsp-golangci-lint.el ends here
