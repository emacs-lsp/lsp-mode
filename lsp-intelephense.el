;;; lsp-intelephense.el --- Intelephense server configuration  -*- lexical-binding: t; -*-

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
  "LSP support for PHP, using Intelephense."
  :group 'lsp-mode
  :link '(url-link "https://github.com/bmewburn/vscode-intelephense")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-files-max-size 1000000
  "Maximum file size in bytes."
  :type 'number
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-files-associations
  ["*.php" "*.phtml"]
  "Configure glob patterns to make files available for language
server features."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-files-exclude
  ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
   "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"]
  "Configure glob patterns to exclude certain files and folders
from all language server features."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-stubs
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
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-completion-insert-use-declaration t
  "Use declarations will be automatically inserted for namespaced
classes, traits, interfaces, functions, and constants."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-completion-fully-qualify-global-constants-and-functions nil
  "Global namespace constants and functions will be fully
qualified (prefixed with a backslash)."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-completion-trigger-parameter-hints t
  "Method and function completions will include parentheses and
trigger parameter hints."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-completion-max-items 100
  "The maximum number of completion items returned per request."
  :type 'number
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-format-enable t
  "Enables formatting."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-licence-key nil
  "Enter your intelephense licence key here to access premium
features."
  :type 'string
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-telemetry-enabled nil
  "Anonymous usage and crash data will be sent to Azure
Application Insights."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-rename-exclude
  ["**/vendor/**"]
  "Glob patterns to exclude files and folders from having symbols
renamed. Rename operation will fail if references and/or
definitions are found in excluded files/folders."
  :type '(repeat string)
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-intelephense-trace-server "off"
  "Traces the communication between VSCode and the intelephense
language server."
  :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-storage-path
  (expand-file-name (locate-user-emacs-file "lsp-cache"))
  "Optional absolute path to storage dir."
  :type 'directory
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-intelephense-clear-cache nil
  "Optional flag to clear server state."
  :type 'boolean
  :group 'lsp-intelephense
  :package-version '(lsp-mode . "6.2"))

(lsp-register-custom-settings
 '(("intelephense.trace.server" lsp-intelephense-trace-server)
   ("intelephense.rename.exclude" lsp-intelephense-rename-exclude)
   ("intelephense.telemetry.enabled" lsp-intelephense-telemetry-enabled t)
   ("intelephense.licenceKey" lsp-intelephense-licence-key)
   ("intelephense.format.enable" lsp-intelephense-format-enable t)
   ("intelephense.completion.maxItems" lsp-intelephense-completion-max-items)
   ("intelephense.completion.triggerParameterHints" lsp-intelephense-completion-trigger-parameter-hints t)
   ("intelephense.completion.fullyQualifyGlobalConstantsAndFunctions" lsp-intelephense-completion-fully-qualify-global-constants-and-functions t)
   ("intelephense.completion.insertUseDeclaration" lsp-intelephense-completion-insert-use-declaration t)
   ("intelephense.stubs" lsp-intelephense-stubs)
   ("intelephense.files.exclude" lsp-intelephense-files-exclude)
   ("intelephense.files.associations" lsp-intelephense-files-associations)
   ("intelephense.files.maxSize" lsp-intelephense-files-max-size)))

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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-intelephense-server-command))
                  :major-modes '(php-mode)
                  :priority -1
                  :notification-handlers (ht ("indexingStarted" #'ignore)
                                             ("indexingEnded" #'ignore))
                  :initialization-options (lambda ()
                                            (list :storagePath lsp-intelephense-storage-path
                                                  :clearCache lsp-intelephense-clear-cache))
                  :multi-root t
                  :server-id 'iph))

(provide 'lsp-intelephense)

;;; lsp-intelephense.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
