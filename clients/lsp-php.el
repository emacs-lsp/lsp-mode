;;; lsp-php.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, php

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

;; LSP Clients for the PHP Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'lsp-protocol)

;; PHP Language Server
(defgroup lsp-php nil
  "LSP support for PHP, using php-language-server."
  :link '(url-link "https://github.com/felixfbecker/php-language-server")
  :group 'lsp-mode)

(defun lsp-php-get-composer-dir ()
  "Get composer home directory if possible."
  (if (executable-find "composer")
      (replace-regexp-in-string "\n$" "" (shell-command-to-string "composer config --global home"))
    "~/.composer"))

(defcustom lsp-php-composer-dir nil
  "Home directory of composer."
  :group 'lsp-php
  :type 'string)

(defcustom lsp-clients-php-server-command nil
  "Install directory for php-language-server."
  :group 'lsp-php
  :type '(repeat string))

(defun lsp-php--create-connection ()
  "Create lsp connection."
  (lsp-stdio-connection
   (lambda ()
     (unless lsp-php-composer-dir
       (setq lsp-php-composer-dir (lsp-php-get-composer-dir)))
     (unless lsp-clients-php-server-command
       (setq lsp-clients-php-server-command
             `("php",
               (expand-file-name
                (f-join lsp-php-composer-dir "vendor/felixfbecker/language-server/bin/php-language-server.php")))))
     lsp-clients-php-server-command)
   (lambda ()
     (if (and (cdr lsp-clients-php-server-command)
              (eq (string-match-p "php[0-9.]*\\'" (car lsp-clients-php-server-command)) 0))
         ;; Start with the php command and the list has more elems. Test the existence of the PHP script.
         (let ((php-file (nth 1 lsp-clients-php-server-command)))
           (or (file-exists-p php-file)
               (progn
                 (lsp-log "%s is not present." php-file)
                 nil)))
       t))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-php--create-connection)
                  :activation-fn (lsp-activate-on "php")
                  :priority -3
                  :server-id 'php-ls))

;;; Intelephense
(defgroup lsp-intelephense nil
  "LSP support for PHP, using Intelephense."
  :group 'lsp-mode
  :link '(url-link "https://github.com/bmewburn/vscode-intelephense")
  :package-version '(lsp-mode . "6.1"))

(lsp-defcustom lsp-intelephense-php-version "9.0.0"
  "Minimum version of PHP to refer to. Affects code actions, diagnostic &
completions."
  :type 'string
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.environment.phpVersion")

(lsp-defcustom lsp-intelephense-files-max-size 1000000
  "Maximum file size in bytes."
  :type 'number
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense-files.maxSize")

(lsp-defcustom lsp-intelephense-files-associations
  ["*.php" "*.phtml"]
  "Configure glob patterns to make files available for language
server features."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.files.associations")

(lsp-defcustom lsp-intelephense-files-exclude
  ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
   "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"]
  "Configure glob patterns to exclude certain files and folders
from all language server features."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.files.exclude")

(lsp-defcustom lsp-intelephense-paths-include
  []
  "Configure additional paths outside workspace."
  :type 'lsp-string-vector
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "8.1")
  :lsp-path "intelephense.environment.includePaths")

(lsp-defcustom lsp-intelephense-stubs
  ["apache" "bcmath" "bz2" "calendar"
   "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant"
   "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase"
   "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli"
   "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql"
   "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode"
   "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets"
   "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg"
   "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc"
   "xmlwriter" "Zend OPcache" "zip" "zlib"]
  "Configure stub files for built in symbols and common
extensions. The default setting includes PHP core and all
bundled extensions."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.stubs")

(lsp-defcustom lsp-intelephense-completion-insert-use-declaration t
  "Use declarations will be automatically inserted for namespaced
classes, traits, interfaces, functions, and constants."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.completion.insertUseDeclaration")

(lsp-defcustom lsp-intelephense-completion-fully-qualify-global-constants-and-functions nil
  "Global namespace constants and functions will be fully
qualified (prefixed with a backslash)."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.completion.fullyQualifyGlobalConstantsAndFunctions")

(lsp-defcustom lsp-intelephense-completion-trigger-parameter-hints t
  "Method and function completions will include parentheses and
trigger parameter hints."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2")
  :lsp-path "intelephense.completion.triggerParameterHints")

(lsp-defcustom lsp-intelephense-completion-max-items 100
  "The maximum number of completion items returned per request."
  :type 'number
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2")
  :lsp-path "intelephense.completion.maxItems")

(lsp-defcustom lsp-intelephense-format-enable t
  "Enables formatting."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.format.enable")

(lsp-defcustom lsp-intelephense-format-braces "psr12"
  "Formatting braces style. psr12, allman or k&r"
  :type 'string
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "8.1")
  :lsp-path "intelephense.format.braces")

(defcustom lsp-intelephense-licence-key nil
  "Enter your intelephense licence key here to access premium
features."
  :type 'string
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(lsp-defcustom lsp-intelephense-telemetry-enabled nil
  "Anonymous usage and crash data will be sent to Azure
Application Insights."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2")
  :lsp-path "intelephense.telemetry.enabled")

(lsp-defcustom lsp-intelephense-rename-exclude
  ["**/vendor/**"]
  "Glob patterns to exclude files and folders from having symbols
renamed. Rename operation will fail if references and/or
definitions are found in excluded files/folders."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2")
  :lsp-path "intelephense.rename.exclude")

(lsp-defcustom lsp-intelephense-trace-server "off"
  "Traces the communication between VSCode and the intelephense
language server."
  :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1")
  :lsp-path "intelephense.trace.server")

(defcustom lsp-intelephense-storage-path
  (expand-file-name (locate-user-emacs-file "lsp-cache"))
  "Optional absolute path to storage dir."
  :type 'directory
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-global-storage-path
  (expand-file-name (locate-user-emacs-file "intelephense"))
  "Optional absolute path to global storage dir."
  :type 'directory
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-intelephense-clear-cache nil
  "Optional flag to clear server state."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-multi-root t
  "Flag to control if the server supports multi-root projects."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.3"))

(define-obsolete-variable-alias
  'lsp-clients-php-iph-server-command
  'lsp-intelephense-server-command
  "lsp-mode 6.1")

(defcustom lsp-intelephense-server-command
  `("intelephense" "--stdio")
  "Command to start Intelephense."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(lsp-dependency 'intelephense
                '(:system "intelephense")
                '(:npm :package "intelephense"
                       :path "intelephense"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find
                                             (cl-first lsp-intelephense-server-command))
                                            (lsp-package-path 'intelephense))
                                       ,@(cl-rest lsp-intelephense-server-command))))
                  :activation-fn (lsp-activate-on "php")
                  :priority -1
                  :notification-handlers (ht ("indexingStarted" #'ignore)
                                             ("indexingEnded" #'ignore))
                  :initialization-options (lambda ()
                                            (list :storagePath lsp-intelephense-storage-path
                                                  :globalStoragePath lsp-intelephense-global-storage-path
                                                  :licenceKey lsp-intelephense-licence-key
                                                  :clearCache lsp-intelephense-clear-cache))
                  :multi-root lsp-intelephense-multi-root
                  :completion-in-comments? t
                  :server-id 'iph
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'intelephense
                                                            callback error-callback))
                  :synchronize-sections '("intelephense")))


;;; Serenata
(defgroup lsp-serenata nil
  "LSP support for the PHP programming language, using serenata."
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
  `( :configuration ( :uris ,lsp-serenata-uris
                      :indexDatabaseUri ,lsp-serenata-index-database-uri
                      :phpVersion ,lsp-serenata-php-version
                      :excludedPathExpressions ,lsp-serenata-exclude-path-expressions
                      :fileExtensions ,lsp-serenata-file-extensions)))


(lsp-interface (serenata:didProgressIndexing (:sequenceOfIndexedItem :totalItemsToIndex :progressPercentage :folderUri :fileUri :info) nil ))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-serenata-server-start-fun)
  :activation-fn (lsp-activate-on "php")
  :priority -2
  :notification-handlers (ht ("serenata/didProgressIndexing"
                              (lambda (_server data)
                                (lsp-log "%s" (lsp:serenata-did-progress-indexing-info data)))))

  :initialization-options #'lsp-serenata-init-options
  :initialized-fn (lambda (workspace)
                    (when (equal (length lsp-serenata-uris) 0)
                      (let* ((lsp-root (lsp--path-to-uri (lsp-workspace-root))))
                        (setq lsp-serenata-uris (vector lsp-root))))
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "serenata"))))
  :server-id 'serenata))

;;; phpactor

(defgroup lsp-phpactor nil
  "LSP support for Phpactor."
  :link '(url-link "https://github.com/phpactor/phpactor")
  :group 'lsp-mode)

(defcustom lsp-phpactor-path nil
  "Path to the `phpactor' command."
  :group 'lsp-phpactor
  :type 'string)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (unless lsp-php-composer-dir
                       (setq lsp-php-composer-dir (lsp-php-get-composer-dir)))
                     (unless lsp-phpactor-path
                       (setq lsp-phpactor-path (or (executable-find "phpactor")
                                                   (f-join lsp-php-composer-dir "vendor/phpactor/phpactor/bin/phpactor"))))
                     (list lsp-phpactor-path "language-server")))
  :activation-fn (lsp-activate-on "php")
  ;; `phpactor' is not really that feature-complete: it doesn't support
  ;; `textDocument/showOccurence' and sometimes errors (e.g. find references on
  ;; a global free-standing function).
  :priority -4
  ;; Even though `phpactor' itself supports no options, this needs to be
  ;; serialized as an empty object (otherwise the LS won't even start, due to a
  ;; type error).
  :initialization-options (ht)
  :server-id 'phpactor))

(defcustom lsp-phpactor-extension-alist '(("Phpstan" . "phpactor/language-server-phpstan-extension")
                                          ("Behat" . "phpactor/behat-extension")
                                          ("PHPUnit" . "phpactor/phpunit-extension"))
  "Alist mapping extension names to `composer' packages.
These extensions can be installed using
`lsp-phpactor-install-extension'."
  :type '(alist :key-type "string" :value-type "string")
  :group 'lsp-phpactor)

(defun lsp-phpactor-install-extension (extension)
  "Install a `phpactor' EXTENSION.
See `lsp-phpactor-extension-alist' and
https://phpactor.readthedocs.io/en/develop/extensions.html."
  (interactive (list (completing-read "Select extension: "
                                      lsp-phpactor-extension-alist)))
  (compilation-start
   (format "%s extension:install %s"
           (shell-quote-argument (expand-file-name lsp-phpactor-path))
           (shell-quote-argument
            (cdr (assoc extension lsp-phpactor-extension-alist))))
   nil
   (lambda (_mode)
     (format "*Phpactor install %s*" extension))))

(lsp-consistency-check lsp-php)

(provide 'lsp-php)
;;; lsp-php.el ends here
