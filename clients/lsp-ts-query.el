;;; lsp-ts-query.el --- LSP client for tree-sitter query  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  emacs-lsp maintainers

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP client for tree-sitter query.
;;

;;; Code:

(require 'lsp-mode)

(declare-function tree-sitter-langs--bin-dir "ext:tree-sitter-langs-build.el")

(defgroup lsp-ts-query nil
  "LSP client for tree-sitter query."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ribru17/ts_query_ls"))

(defcustom lsp-ts-query-executable "ts_query_ls"
  "The ts-query-ls executable to use.

Leave as just the executable name to use the default behavior of finding the
executable with variable `exec-path'."
  :group 'lsp-ts-query
  :type 'string)

(defcustom lsp-ts-query-parser-install-directories
  (cl-remove-if #'nil
                (vector (expand-file-name (locate-user-emacs-file "tree-sitter"))
                        (and (featurep 'tree-sitter-langs)
                             (tree-sitter-langs--bin-dir))))
  "Where to look for parsers, of the form <lang>.(so|dll|dylib) or
tree-sitter-<lang>.wasm."
  :group 'lsp-ts-query
  :type '(vector string))

(defcustom lsp-ts-query-language-retrieval-patterns nil
  "A list of patterns to aid the LSP in finding a language, given a file
path. Patterns must have one capture group which represents the language
name. Ordered from highest to lowest precedence."
  :group 'lsp-ts-query
  :type '(vector string))

;;
;;; Installation

(defcustom lsp-ts-query-server-store-path
  (expand-file-name "ts-query/" lsp-server-install-dir)
  "The path to the file in which ts-query will be stored."
  :type 'file
  :group 'lsp-ts-query)

(defconst lsp-ts-query--download-url-format
  "https://github.com/ribru17/ts_query_ls/releases/latest/download/ts_query_ls-%s-%s.%s"
  "Format to the download url link.")

(defun lsp-ts-query--url ()
  "Return Url points to the ts-query' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-ts-query--download-url-format arch "pc-windows-msvc" "zip"))
      (darwin
       (format lsp-ts-query--download-url-format arch "apple-darwin" "tar.gz"))
      (gnu/linux
       (format lsp-ts-query--download-url-format arch "unknown-linux-gnu" "tar.gz")))))

(defun lsp-ts-query--stored-executable ()
  "Return the stored ts-query executable.

This is differ from the variable `lsp-ts-query-executable'; this is local storage
and not the global storage."
  (f-join lsp-ts-query-server-store-path
          (pcase system-type ('windows-nt "ts_query_ls.exe") (_ "ts_query_ls"))))

(lsp-dependency
 'ts-query-ls
 '(:system "ts_query_ls")
 `(:download :url ,(lsp-ts-query--url)
             :decompress ,(pcase system-type ('windows-nt :zip) (_ :targz))
             :store-path ,(f-join lsp-ts-query-server-store-path "temp")
             :set-executable? t)
 `(:system ,(lsp-ts-query--stored-executable)))

(defun lsp-ts-query--check-enabled (file-name _mode)
  "Check if the the language server should be enabled in this buffer
or FILE-NAME."
  (let ((ext (file-name-extension file-name)))
    (when (and (string-match-p "/queries/" file-name)
               (equal "scm" ext))
      t)))

(defun lsp-ts-query--make-init-options ()
  "Init options for ts-query server."
  `( :parser_install_directories ,lsp-ts-query-parser-install-directories
     :language_retrieval_patterns ,lsp-ts-query-language-retrieval-patterns))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (or (executable-find lsp-ts-query-executable)
                                  (lsp-ts-query--stored-executable)))
                   (lambda ()
                     (or (executable-find lsp-ts-query-executable)
                         (file-executable-p (lsp-ts-query--stored-executable)))))
  :activation-fn #'lsp-ts-query--check-enabled
  :priority -1
  :add-on? t
  :initialization-options #'lsp-ts-query--make-init-options
  :server-id 'ts-query-ls
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'ts-query-ls callback error-callback))))

(lsp-consistency-check lsp-ts-query)

(provide 'lsp-ts-query)
;;; lsp-ts-query.el ends here
