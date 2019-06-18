;;; lsp-xml.el --- LSP XML server integration        -*- lexical-binding: t; -*-

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

(defgroup lsp-xml nil
  "Settings for rls."
  :group 'tools
  :tag "Language Server"
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-trace-server "off"
  "Traces the communication between VS Code and the XML language server."
  :type '(choice
          (const "off")
          (const "messages")
          (const "verbose"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-catalogs nil
  "Array of XML Catalogs"
  :type '(repeat string)
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-logs-client t
  "Should the server log to client output"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-split-attributes nil
  "Split multiple attributes each onto a new line"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-join-cdata-lines nil
  "Join lines in a CDATA tag's content"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-join-comment-lines nil
  "Join comment content on format"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-space-before-empty-close-tag t
  "Insert space before end of self closing tag.
Example: <tag/> -> <tag />"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-join-content-lines nil
  "Normalize the whitespace of content inside an element.
Newlines and excess whitespace are removed."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-preserve-empty-content nil
  "Preserve empty content/whitespace in a tag."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-enabled t
  "Enable/disable ability to format document"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-format-quotations "doubleQuotes"
  "Which type of quotes to use for attribute values when
  formatting."
  :type '(choice
          (const "doubleQuotes")
          (const "singleQuotes"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-file-associations nil
  "Allows XML schemas to be associated to file name patterns.
  Example: [{ \"systemId\":\"path/to/file.xsd\",\"pattern\":
  \"file1.xml\" },{ \"systemId\":
  \"http://www.w3.org/2001/XMLSchema.xsd\",\"pattern\":
  \"**/*.xsd\" }]"
  :type '(repeat string)
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-completion-auto-close-tags t
  "Enable/disable autoclosing of XML tags. IMPORTANT: Turn off
  editor.autoClosingTags for this to work"
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-server-vmargs "-noverify -Xmx64M
  -XX:+UseG1GC -XX:+UseStringDeduplication" "Specifies extra VM
  arguments used to launch the XML Language Server. Eg. use
  `-noverify -Xmx1G -XX:+UseG1GC -XX:+UseStringDeduplication` to
  bypass class verification, increase the heap size to 1GB and
  enable String deduplication with the G1 Garbage collector"
  :type '(repeat string)
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-server-work-dir "~/.lsp4xml"
  "Set a custom folder path for cached XML Schemas. An absolute
  path is expected, although the ~ prefix (for the user home
  directory) is supported."
  :type 'string
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-no-grammar "hint"
  "The message severity when a document has no associated
  grammar."
  :type '(choice (:tag "ignore" "hint" "info" "warning" "error"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-enabled t
  "Enable/disable all validation."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-schema t
  "Enable/disable schema based validation. Ignored if
  \"xml.validation.enabled\": false."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings '
 (("xml.validation.schema" lsp-xml-validation-schema t)
  ("xml.validation.enabled" lsp-xml-validation-enabled t)
  ("xml.validation.noGrammar" lsp-xml-validation-no-grammar)
  ("xml.server.workDir" lsp-xml-server-work-dir)
  ("xml.server.vmargs" lsp-xml-server-vmargs)
  ("xml.completion.autoCloseTags" lsp-xml-completion-auto-close-tags t)
  ("xml.fileAssociations" lsp-xml-file-associations)
  ("xml.format.quotations" lsp-xml-format-quotations)
  ("xml.format.enabled" lsp-xml-format-enabled t)
  ("xml.format.preserveEmptyContent" lsp-xml-format-preserve-empty-content t)
  ("xml.format.joinContentLines" lsp-xml-format-join-content-lines t)
  ("xml.format.spaceBeforeEmptyCloseTag" lsp-xml-format-space-before-empty-close-tag t)
  ("xml.format.joinCommentLines" lsp-xml-format-join-comment-lines t)
  ("xml.format.joinCDATALines" lsp-xml-format-join-cdata-lines t)
  ("xml.format.splitAttributes" lsp-xml-format-split-attributes t)
  ("xml.logs.client" lsp-xml-logs-client t)
  ("xml.catalogs" lsp-xml-catalogs)
  ("xml.trace.server" lsp-xml-trace-server)))

(defcustom lsp-xml-jar-file (expand-file-name
                             (locate-user-emacs-file
                              "org.eclipse.lsp4xml-0.3.0-uber.jar"))
  "Xml server jar command."
  :type 'string
  :group 'lsp-xml
  :type 'file
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-server-command `("java" "-jar" ,lsp-xml-jar-file)
  "Xml server command."
  :type '(repeat string)
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defun lsp-xml--create-connection ()
  (plist-put
   (lsp-stdio-connection
    (lambda () lsp-xml-server-command))
   :test? (lambda ()
            (f-exists? lsp-xml-jar-file))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-xml--create-connection)
                  :activation-fn (lambda (file-name _mode)
                                   (string= (f-ext file-name) "xml"))
                  :priority 0
                  :server-id 'xmlls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "xml"))))))

(provide 'lsp-xml)
;;; lsp-xml.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
