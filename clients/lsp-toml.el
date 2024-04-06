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

;; Client for taplo.

;;; Code:

(require 'lsp-mode)
(require 'ht)
(require 'f)

(defgroup lsp-toml nil
  "LSP support for TOML, using Taplo."
  :group 'lsp-mode
  :link '(url-link "https://github.com/tamasfe/taplo"))

(defcustom lsp-toml-command "taplo"
  "Path to taplo command."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-toml-cache-path (expand-file-name
                                (locate-user-emacs-file (f-join ".cache" "lsp-toml")))
  "Path to cache."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-toml-taplo-config-file-path nil
  "An absolute, or workspace relative path to the Taplo configuration file."
  :type 'string
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.taplo.configFile.path")

(lsp-defcustom lsp-toml-taplo-config-file-enabled t
  "Whether to enable the usage of a Taplo configuration file."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.taplo.configFile.enabled")

(lsp-defcustom lsp-toml-semantic-tokens nil
  "Enable semantic tokens for inline table and array keys."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.semanticTokens")

(lsp-defcustom lsp-toml-schema-enabled t
  "Enable completion and validation based on JSON schemas."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.enabled")

(lsp-defcustom lsp-toml-schema-links nil
  "Whether to show clickable links for keys in the editor."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.links")

(lsp-defcustom lsp-toml-schema-catalogs
  ["https://www.schemastore.org/api/json/catalog.json"]
  "A list of URLs to schema catalogs where schemas and associations
can be fetched from"
  :type 'lsp-string-vector
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.catalogs")

(lsp-defcustom lsp-toml-schema-associations nil
  "Additional document and schema associations.

The key must be a regular expression, this pattern is used to
associate schemas with absolute document URIs.

The value must be an absolute URI to the JSON schema"
  :type '(alist :key-type symbol :value-type string)
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.associations")

(lsp-defcustom lsp-toml-schema-cache-memory-expiration 60
  "The amount of seconds after which schemas will be invalidated from memory."
  :type 'number
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.cache.memoryExpiration")

(lsp-defcustom lsp-toml-schema-cache-disk-expiration 600
  "The amount of seconds after which cached catalogs and schemas
expire and will be attempted to be fetched again."
  :type 'number
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.schema.cache.diskExpiration")

(lsp-defcustom lsp-toml-completion-max-keys 5
  "The maximum amount of keys in a dotted key to display during
completion, 0 effectively disables key completions."
  :type 'number
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.completion.maxKeys")

(lsp-defcustom lsp-toml-syntax-semantic-tokens t
  "Whether to enable semantic tokens for tables and arrays."
  :type 'boolean
  :group 'lsp-toml
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "evenBetterToml.syntax.semanticTokens")


(defun lsp-toml--initialization-options ()
  "Initialization options for taplo."
  (list :configurationSection "evenBetterToml"
        :cachePath lsp-toml-cache-path))

(defun lsp-toml--handle-message-with-output (_workspace params)
  "Handle taplo/messageWithOutput notification with PARAMS."
  (funcall (pcase (ht-get params "kind")
             ("error" 'lsp--error)
             ("warn" 'lsp--warn)
             ("info" 'lsp--info)
             (_ 'lsp--info))
           "lsp-toml: %s"
           (ht-get params "message")))

(defun lsp-toml--check-enabled (_file-name _mode)
  "Check if the taplo language server should be enabled in this buffer."
  (when (string= (lsp-buffer-language) "toml")
    (make-directory lsp-toml-cache-path t)
    t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (list lsp-toml-command "lsp" "stdio")))
  :activation-fn #'lsp-toml--check-enabled
  :initialization-options #'lsp-toml--initialization-options
  :notification-handlers (ht ("taplo/messageWithOutput" #'lsp-toml--handle-message-with-output)
                             ("taplo/didChangeSchemaAssociation" #'ignore))
  :multi-root t
  :server-id 'taplo
  :priority -1))

(lsp-consistency-check lsp-toml)

(provide 'lsp-toml)
;;; lsp-toml.el ends here
