;;; lsp-intelephense.el --- LSP intelephense server configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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

;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-intelephense nil
  "Settings for php-language-server."
  :group 'tools
  :tag "PHP Language Server")

(defcustom lsp-intelephense-files-max-size 1000000
  "Maximum file size in bytes."
  :type 'number)

(defcustom lsp-intelephense-files-associations
  ["*.php" "*.phtml"]
  "Configure glob patterns to make files available for language
  server features."
  :type '(repeat string))

(defcustom lsp-intelephense-files-exclude
  ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
   "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"]
  "Configure glob patterns to exclude certain files and folders from all language server features." :type
  '(repeat string))

(defcustom lsp-intelephense-stubs
  ["apache" "bcmath" "bz2" "calendar"
   "com_dotnet" "Core" "csprng" "ctype" "curl" "date" "dba" "dom" "enchant"
   "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase"
   "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "mssql" "mysql" "mysqli"
   "oci8" "odcb" "openssl" "password" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql"
   "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode"
   "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets"
   "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg"
   "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc"
   "xmlwriter" "Zend OPcache" "zip" "zlib"]
  "Configure stub files for built in symbols and common
  extensions. The default setting includes PHP core and all
  bundled extensions."
  :type '(repeat string))

(defcustom lsp-intelephense-completion-insert-use-declaration t
  "Use declarations will be automatically inserted for namespaced
  classes, traits, interfaces, functions, and constants."
  :type 'boolean)

(defcustom lsp-intelephense-completion-fully-qualify-global-constants-and-functions nil
  "Global namespace constants and functions will be fully
  qualified (prefixed with a backslash)."
  :type 'boolean)

(defcustom lsp-intelephense-format-enable t
  "Enables formatting"
  :type 'boolean)

(defcustom lsp-intelephense-trace-server "off"
  "Traces the communication between VSCode and the intelephense
  language server."
  :type '(choice (:tag "off" "messages" "verbose")))

(defcustom lsp-intelephense-storage-path (expand-file-name (locate-user-emacs-file "lsp-cache"))
  "Optional absolute path to storage dir."
  :group 'lsp-php-ip
  :type 'directory)

(lsp-register-custom-settings
 '(("intelephense.trace.server" lsp-intelephense-trace-server)
   ("intelephense.format.enable" lsp-intelephense-format-enable t)
   ("intelephense.completion.fullyQualifyGlobalConstantsAndFunctions" lsp-intelephense-completion-fully-qualify-global-constants-and-functions t)
   ("intelephense.completion.insertUseDeclaration" lsp-intelephense-completion-insert-use-declaration t)
   ("intelephense.stubs" lsp-intelephense-stubs)
   ("intelephense.files.exclude" lsp-intelephense-files-exclude)
   ("intelephense.files.associations" lsp-intelephense-files-associations)
   ("intelephense.files.maxSize" lsp-intelephense-files-max-size)))

(defcustom lsp-clients-php-iph-server-command
  `("intelephense" "--stdio")
  "Install directory for php-language-server."
  :group 'lsp-php-ip
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clients-php-iph-server-command))
                  :major-modes '(php-mode)
                  :priority -1
                  :notification-handlers (ht ("indexingStarted" #'ignore)
                                             ("indexingEnded" #'ignore))
                  :initialization-options (lambda ()
                                            (list :storagePath lsp-intelephense-storage-path))
                  :multi-root t
                  :server-id 'iph))

(provide 'lsp-intelephense)

;;; lsp-intelephense.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
