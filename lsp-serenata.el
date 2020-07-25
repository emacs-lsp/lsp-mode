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

(defcustom lsp-serenata-uris
  []
  "A list of folders to index for your project.
This does not have to include the root of the project itself, in
case you have need of an exotic configuration where the root of
the project is at some location but your actual PHP code is
somewhere else.  Note that if you are running Serenata in a
container, you will have to ensure that these URI's are mapped
inside it.  Avoid using file paths containing spaces. This is
currently broken due to apparent PHP quirks.  By default, the
value is taken from the lsp workspace location."
  :group 'lsp-serenata
  :type 'lsp-string-vector)

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

(defcustom lsp-serenata-index-database-uri (lsp--path-to-uri (f-join  user-emacs-directory "index.sqlite"))
  "The location to store the index database.
Note that, as the index database uses SQLite and WAL mode,
additional files (usually two) may be generated and used in the
same folder.  Note also that Serenata relies on the Doctrine DBAL
library as well as the SQLite backends in PHP, which may not
support non-file URI's, which may prevent you from using these."
  :group 'lsp-serenata
  :type 'file)

(defcustom lsp-serenata-exclude-path-expressions ["/.+Test.php$/"]
  "One or more expressions of paths to ignore.
This uses Symfony's Finder in the background, so this means you
can configure anything here that can also be passed to the name
function, which includes plain strings, globs, as well as regular
expressions.  Note that for existing projects, modifying these
will not not automatically prune them from the index if they are
already present."
  :group 'lsp-serenata
  :type 'lsp-string-vector)

(defun lsp-serenata-server-start-fun (port)
  "Define serenata start function, it requires a PORT."
  `(,lsp-serenata-server-path
    "-u" ,(number-to-string port)))

(defun lsp-serenata-init-options ()
  "Init options for lsp-serenata."
  `(:config (:uris ,lsp-serenata-uris
		   :indexDatabaseUri ,lsp-serenata-index-database-uri
		   :phpVersion ,lsp-serenata-php-version
		   :excludedPathExpressions ,lsp-serenata-exclude-path-expressions
		   :fileExtensions ,lsp-serenata-file-extensions)))


(lsp-interface (serenata:didProgressIndexing (:sequenceOfIndexedItem :totalItemsToIndex :progressPercentage :folderUri :fileUri :info) nil ))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-serenata-server-start-fun)
  :major-modes '(php-mode)
  :priority -2
  :notification-handlers (ht ("serenata/didProgressIndexing"
			      (lambda (_server data)
				(lsp--info "%s" (lsp:serenata-did-progress-indexing-info data)))))

  :initialization-options #'lsp-serenata-init-options
  :initialized-fn (lambda (workspace)
		    (when (equal (length lsp-serenata-uris) 0)
		      (let* ((lsp-root (lsp--path-to-uri (lsp-workspace-root))))
			(setq lsp-serenata-uris (vector lsp-root))))
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "serenata"))))
  :server-id 'serenata))

(provide 'lsp-serenata)
;;; lsp-serenata.el ends here
