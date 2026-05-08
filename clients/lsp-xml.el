;;; lsp-xml.el --- LSP XML server integration        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski
;; Copyright (C) 2019-2026 lsp-mode maintainers

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

(defcustom lsp-xml-preferences-quote-style "double"
  "The preferred quote style for attribute values."
  :type '(choice
          (const "double")
          (const "single"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-comments t
  "Enable/disable comment formatting."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-closing-bracket-new-line nil
  "Enable/disable moving the closing bracket.

This only affects tags with two or more (split) attributes."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-split-attributes-indent-size 2
  "The indentation used for split attributes."
  :type 'integer
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-preserve-attribute-line-breaks t
  "Enable/disable preserving line breaks in attributes."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-enforce-quote-style "ignore"
  "The way in which quote style should be enforced."
  :type '(choice
          (const "ignore")
          (const "preferred"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-preserved-newlines 2
  "The number of empty newlines to be preserved."
  :type 'integer
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-xsi-schema-location-split "onPair"
  "XSI schema location split settings."
  :type '(choice
          (const "onElement")
          (const "onPair")
          (const "none"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-experimental nil
  "Enable/disable experimental formatter."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-max-line-width 80
  "Max line width.

This only applies to experimental formatter."
  :type 'integer
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-preserve-space ["xsl:text"
                                          "xsl:comment"
                                          "xsl:processing-instruction"
                                          "literallayout"
                                          "programlisting"
                                          "screen"
                                          "synopsis"
                                          "pre"
                                          "xd:pre"]
  "List of elements which must preserve space.

This option only affects the experimental formatter."
  :type 'lsp-string-vector
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-xml-format-grammar-aware-formatting t
  "Enable/disable grammar aware formatting.

This only affects the experimental formatter."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "9.0.0"))

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

(defcustom lsp-xml-server-vmargs ["-noverify" "-Xmx64M" "-XX:+UseG1GC"
                                  "-XX:+UseStringDeduplication"]
  "Specifies extra VM arguments used to launch the XML Language
  Server. Eg. use `-noverify -Xmx1G -XX:+UseG1GC
  -XX:+UseStringDeduplication` to bypass class verification,
  increase the heap size to 1GB and enable String deduplication
  with the G1 Garbage collector"
  :type 'lsp-string-vector
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-server-work-dir (expand-file-name ".lsp4xml" "~")
  "Set a custom folder path for cached XML Schemas. An absolute
  path is expected, although the ~ prefix (for the user home
  directory) is supported."
  :type 'string
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-no-grammar "hint"
  "The message severity when a document has no associated
  grammar."
  :type '(choice  (const "ignore")
                  (const "hint")
                  (const "info")
                  (const "warning")
                  (const "error"))
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-enabled t
  "Enable/disable all validation."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-validation-resolve-external-entities nil
  "Enable/disable resolution (downloading) of external entities from the internet."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-xml-validation-schema '((enabled . "always"))
  "The XML schema settings.

The value for `enabled' can be always, never or onValidSchema."
  :type 'alist
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings '
 (("xml.validation.schema" lsp-xml-validation-schema)
  ("xml.validation.resolveExternalEntities" lsp-xml-validation-resolve-external-entities)
  ("xml.validation.enabled" lsp-xml-validation-enabled t)
  ("xml.validation.noGrammar" lsp-xml-validation-no-grammar)
  ("xml.server.workDir" lsp-xml-server-work-dir)
  ("xml.server.vmargs" lsp-xml-server-vmargs)
  ("xml.completion.autoCloseTags" lsp-xml-completion-auto-close-tags t)
  ("xml.fileAssociations" lsp-xml-file-associations)
  ("xml.preferences.quoteStyle" lsp-xml-preferences-quote-style)
  ("xml.format.enabled" lsp-xml-format-enabled t)
  ("xml.format.preserveEmptyContent" lsp-xml-format-preserve-empty-content t)
  ("xml.format.joinContentLines" lsp-xml-format-join-content-lines t)
  ("xml.format.spaceBeforeEmptyCloseTag" lsp-xml-format-space-before-empty-close-tag t)
  ("xml.format.joinCommentLines" lsp-xml-format-join-comment-lines t)
  ("xml.format.joinCDATALines" lsp-xml-format-join-cdata-lines t)
  ("xml.format.splitAttributes" lsp-xml-format-split-attributes t)
  ("xml.format.formatComments" lsp-xml-format-comments t)
  ("xml.format.closingBracketNewLine" lsp-xml-format-closing-bracket-new-line t)
  ("xml.format.splitAttributesIndentSize" lsp-xml-format-split-attributes-indent-size)
  ("xml.format.preserveAttributeLineBreaks" lsp-xml-format-preserve-attribute-line-breaks t)
  ("xml.format.enforceQuoteStyle" lsp-xml-format-enforce-quote-style)
  ("xml.format.preservedNewlines" lsp-xml-format-preserved-newlines)
  ("xml.format.xsiSchemaLocationSplit" lsp-xml-format-xsi-schema-location-split)
  ("xml.format.experimental" lsp-xml-format-experimental t)
  ("xml.format.maxLineWidth" lsp-xml-format-max-line-width)
  ("xml.format.preserveSpace" lsp-xml-format-preserve-space)
  ("xml.format.grammarAwareFormatting" lsp-xml-format-grammar-aware-formatting t)
  ("xml.logs.client" lsp-xml-logs-client t)
  ("xml.catalogs" lsp-xml-catalogs)
  ("xml.trace.server" lsp-xml-trace-server)))

(defcustom lsp-xml-prefer-jar t
  "Prefer using the jar file instead of the native binary."
  :type 'boolean
  :group 'lsp-xml
  :package-version '(lsp-mode . "8.0.2"))

(defconst lsp-xml-jar-version "0.27.0")

(defconst lsp-xml-jar-name "org.eclipse.lemminx-uber.jar")

(defcustom lsp-xml-jar-file (f-join lsp-server-install-dir "xmlls" lsp-xml-jar-name)
  "Xml server jar command."
  :group 'lsp-xml
  :type 'file
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-xml-jar-download-url
  (format
   "https://download.eclipse.org/lemminx/releases/%s/%s"
   lsp-xml-jar-version
   lsp-xml-jar-name)
  "Automatic download url for lsp-xml."
  :type 'string
  :group 'lsp-xml
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency
 'xmlls
 '(:system lsp-xml-jar-file)
 `(:download :url lsp-xml-jar-download-url
             :store-path lsp-xml-jar-file))

(defconst lsp-xml-bin-base-name
  (format "lemminx-%s" (let ((arch (if (string-prefix-p "x86_64" system-configuration) "x86_64" "aarch_64")))
                         (pcase system-type
                           ('darwin (format "osx-%s" arch))
                           ('gnu/linux "linux")
                           ('windows-nt "win32")))))

(defconst lsp-xml-bin-name (format "%s%s" lsp-xml-bin-base-name (if (eq system-type 'windows-nt) ".exe" "")))

(defcustom lsp-xml-bin-file (f-join lsp-server-install-dir "xmlls" lsp-xml-bin-name)
  "Xml server binary."
  :group 'lsp-xml
  :type 'file
  :package-version '(lsp-mode . "8.0.2"))

(defcustom lsp-xml-bin-download-url
  ;; This is the version with `latest` tag
  (format "https://github.com/redhat-developer/vscode-xml/releases/latest/download/%s.zip"
          lsp-xml-bin-base-name)
  "Automatic download url for lsp-xml's native binary."
  :type 'string
  :group 'lsp-xml
  :package-version '(lsp-mode . "8.0.2"))

(lsp-dependency
 'xmlls-bin
 '(:system ,(file-name-nondirectory lsp-xml-bin-file))
 `(:download :url lsp-xml-bin-download-url
             :decompress :zip
             :store-path lsp-xml-bin-file))

(defsubst lsp-xml-has-java? () (executable-find "java"))

(defcustom lsp-xml-server-command
  (lambda () (or (and (lsp-xml-has-java?) lsp-xml-prefer-jar `("java" "-jar" ,lsp-xml-jar-file))
                 `(,lsp-xml-bin-file)))
  "Xml server command."
  :type '(choice (repeat string) (function))
  :group 'lsp-xml
  :package-version '(lsp-mode . "6.1"))

(defun lsp-xml--create-connection ()
  "Create a connection for the XML language server."
  (lsp-stdio-connection
   (lambda () (lsp-resolve-value lsp-xml-server-command))
   (lambda () (or (and (lsp-xml-has-java?) lsp-xml-prefer-jar (f-exists? lsp-xml-jar-file))
                  (f-exists? lsp-xml-bin-file)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-xml--create-connection)
                  :activation-fn (lsp-activate-on "xml")
                  :priority 0
                  :server-id 'xmlls
                  :multi-root t
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "xml"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure (or (and (lsp-xml-has-java?) lsp-xml-prefer-jar 'xmlls)
                                                                'xmlls-bin)
                                                            callback error-callback))))

(lsp-consistency-check lsp-xml)

(provide 'lsp-xml)
;;; lsp-xml.el ends here
