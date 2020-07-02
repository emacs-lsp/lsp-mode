;;; lsp-serenata.el --- Serenata server configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

;; Author: Fermin Munoz <fmfs@posteo.net>
;; Keywords: php lsp

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

;; lsp-serenata client

;;; Code:

(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-serenata nil
  "LSP support for the PHP programming language, using serenata"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/Serenata/Serenata")
  :package-version '(lsp-mode . "7.0"))

(defcustom lsp-serenata-server-path
  "serenata.phar"
  "Path to the Serenata Language Server phar file.
It can be downloaded from https://gitlab.com/Serenata/Serenata/-/releases."
  :group 'lsp-serenata
  :type 'file)

(defcustom lsp-serenata-php-version
  7.3
  "Allows you to specify the PHP version your project is written in.
At the moment this directive is still ignored, but it will
influence functionality such as refactoring in the future, where
older PHP versions may not support scalar type hints, which may
then be omitted from places such as getters and setters."
  :group 'lsp-serenata
  :type 'number)

(defcustom lsp-serenata-file-extensions
  ["php"]
  "List of file extensions (without dot) to process.
Files that do not match this whitelist will be ignored during
indexing.  Usually you'll want to set this to at least include
php, as it is the most common PHP extension.  phpt is not
included by default as it is often used to contain test code that
is not directly part of the code.  Note that for existing
projects, removing extensions will not not automatically prune
files having them from the index if they are already present.
Adding new ones will cause the files having them to be picked up
on the next project initialization."
  :group 'lsp-serenata
  :type 'lsp-string-vector)

(defun lsp-serenata-server-start-fun (port)
  "Define serenata start function, it requires a PORT."
  `(,lsp-serenata-server-path
    "-u" ,(number-to-string port)))

(defun lsp-serenata-init-options ()
  "Init options for lsp-serenata."
  `(
    :phpVersion ,lsp-serenata-php-version
    :fileExtensions ,lsp-serenata-file-extensions))

(lsp-interface (serenata:didProgressIndexing (:sequenceOfIndexedItem :totalItemsToIndex :progressPercentage :folderUri :fileUri :info) nil ))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-serenata-server-start-fun)
  :major-modes '(php-mode)
  :priority -2
  :notification-handlers (ht ("serenata/didProgressIndexing"
			      (lambda (_server data)
				(lsp--info "%s" (lsp:serenata-did-progress-indexing-info data)) )))
  :initialization-options #'lsp-serenata-init-options
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "serenata"))))
  :server-id 'serenata))

(provide 'lsp-serenata)
;;; lsp-serenata.el ends here
