;;; lsp-toml.el --- lsp-mode TOML integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: lsp, toml

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

;; Client for taplo-lsp.

;;; Code:

(require 'lsp-mode)
(require 'ht)
(require 'f)

(defgroup lsp-toml nil
  "LSP support for TOML, using taplo-lsp."
  :group 'lsp-mode
  :link '(url-link "https://taplo.tamasfe.dev/lsp/"))

(defcustom lsp-toml-command "taplo-lsp"
  "Path to taplo-lsp command."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-cache-path (expand-file-name
                                (locate-user-emacs-file (f-join ".cache" "lsp-toml")))
  "Path to cache."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))


(defcustom lsp-toml-taplo-config nil
  "An absolute, or workspace relative path to the Taplo configuration file."
  :type 'file
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-taplo-config-enabled t
  "Whether to enable the usage of a Taplo configuration file."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))


(defcustom lsp-toml-schema-enabled t
  "Enable completion and validation based on JSON schemas."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-schema-links nil
  "Enable editor links."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-schema-repository-enabled t
  "Whether to use schemas from the provided schema repository."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-schema-repository-url "https://taplo.tamasfe.dev/schema_index.json"
  "A HTTP(S) URL that points to a schema index."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-schema-associations
  `((,(make-symbol "^(.*(/|\\\\)\\.?taplo\\.toml|\\.?taplo\\.toml)$") . "taplo://taplo.toml"))
  "Document and schema associations."
  :type '(alist :key-type symbol :value-type string)
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))


(defcustom lsp-toml-formatter-align-entries nil
  "Align consecutive entries vertically."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-align-comments t
  "Align comments vertically after entries and array values."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-array-trailing-comma t
  "Append trailing commas for multi-line arrays."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-array-auto-expand t
  "Expand arrays to multiple lines that exceed the maximum column width."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-array-auto-collapse t
  "Collapse arrays that don't exceed the maximum column width and
don't contain comments."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-compact-arrays t
  "Omit white space padding from single-line arrays."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-compact-inline-tables nil
  "Omit white space padding from the start and end of inline tables."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-compact-entries nil
  "Omit white space padding around `=` for entries."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-column-width 80
  "Maximum column width in characters, affects array expansion
and collapse, this doesn't take whitespace into account."
  :type 'number
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-indent-tables nil
  "Indent based on tables and arrays of tables and their
subtables, subtables out of order are not indented."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-indent-entries nil
  "Indent entries under tables."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-indent-string nil
  "The substring that is used for indentation, should be tabs or
spaces, but technically can be anything.  Uses the IDE setting if
not set."
  :type '(repeat string)
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-reorder-keys nil
  "Alphabetically reorder keys that are not separated by empty lines."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-allowed-blank-lines 2
  "Maximum amount of allowed consecutive blank lines.
This does not affect the whitespace at the end of the document,
as it is always stripped."
  :type 'number
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-trailing-newline t
  "Add trailing newline at the end of the file if not present."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-toml-formatter-crlf nil
  "Use CRLF for line endings."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "8.0.1"))


(lsp-register-custom-settings
 '(
   ("evenBetterToml.taploConfig" lsp-toml-taplo-config)
   ("evenBetterToml.taploConfigEnabled" lsp-toml-taplo-config-enabled t)

   ("evenBetterToml.schema.enabled" lsp-toml-schema-enabled t)
   ("evenBetterToml.schema.links" lsp-toml-schema-links t)
   ("evenBetterToml.schema.repositoryEnabled" lsp-toml-schema-repository-enabled t)
   ("evenBetterToml.schema.repositoryUrl" lsp-toml-schema-repository-url)
   ("evenBetterToml.schema.associations" lsp-toml-schema-associations)

   ("evenBetterToml.formatter.alignEntries" lsp-toml-formatter-align-entries t)
   ("evenBetterToml.formatter.alignComments" lsp-toml-formatter-align-comments t)
   ("evenBetterToml.formatter.arrayTrailingComma" lsp-toml-formatter-array-trailing-comma t)
   ("evenBetterToml.formatter.arrayAutoExpand" lsp-toml-formatter-array-auto-expand t)
   ("evenBetterToml.formatter.arrayAutoCollapse" lsp-toml-formatter-array-auto-collapse t)
   ("evenBetterToml.formatter.compactArrays" lsp-toml-formatter-compact-arrays t)
   ("evenBetterToml.formatter.compactInlineTables" lsp-toml-formatter-compact-inline-tables t)
   ("evenBetterToml.formatter.compactEntries" lsp-toml-formatter-compact-entries t)
   ("evenBetterToml.formatter.columnWidth" lsp-toml-formatter-column-width)
   ("evenBetterToml.formatter.indentTables" lsp-toml-formatter-indent-tables t)
   ("evenBetterToml.formatter.indentEntries" lsp-toml-formatter-indent-entries t)
   ("evenBetterToml.formatter.indentString" lsp-toml-formatter-indent-string)
   ("evenBetterToml.formatter.reorderKeys" lsp-toml-formatter-reorder-keys t)
   ("evenBetterToml.formatter.allowedBlankLines" lsp-toml-formatter-allowed-blank-lines)
   ("evenBetterToml.formatter.trailingNewline" lsp-toml-formatter-trailing-newline t)
   ("evenBetterToml.formatter.crlf" lsp-toml-formatter-crlf t)))


(defun lsp-toml--initialized (workspace)
  (with-lsp-workspace workspace
    (lsp--set-configuration (lsp-configuration-section "evenBetterToml"))
    (lsp-notify "taplo/cachePath" `(:path ,lsp-toml-cache-path))))

(defun lsp-toml--initialization-options ()
  (list :configuration (ht-get (lsp-configuration-section "evenBetterToml") "evenBetterToml")))

(defun lsp-toml--message-with-output (_workspace params)
  (funcall (pcase (ht-get params "kind")
             ("error" 'lsp--error)
             ("warn" 'lsp--warn)
             ("info" 'lsp--info)
             (_ 'lsp--info))
           "lsp-toml: %s"
           (ht-get params "message")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (list lsp-toml-command "run")))
  :activation-fn (lsp-activate-on "toml")
  :initialized-fn #'lsp-toml--initialized
  :initialization-options #'lsp-toml--initialization-options
  :notification-handlers (ht ("taplo/messageWithOutput" #'lsp-toml--message-with-output))
  :server-id 'taplo-lsp
  :priority -1))

(lsp-consistency-check lsp-toml)

(provide 'lsp-toml)
;;; lsp-toml.el ends here
