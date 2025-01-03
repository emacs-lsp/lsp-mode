;;; lsp-kubernetes-helm.el --- LSP YAML server integration        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Aaron Gonzales

;; Author: Aaron Gonzales <aarongonzales1@gmail.com>
;; Keywords: lsp, kubernetes, helm, yaml

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
(require 'dash)

(defgroup lsp-kubernetes-helm nil
  "LSP support for YAML, using Helm Language Server (helm-ls)."
  :group 'lsp-mode
  :link '(url-link "https://github.com/mrjosh/helm-ls")
  :package-version '(lsp-mode . "9.0.0"))

(defconst lsp-kubernetes-helm--lsp-configuration-section-name "helm-ls"
  "Key used to grab the lsp configuration section for helm-ls.")

(defcustom lsp-kubernetes-helm-ls-server-path "helm_ls"
  "Path to the Helm Language Server binary."
  :group 'lsp-kubernetes-helm
  :risky t
  :type 'file)

(defcustom lsp-kubernetes-helm-ls-log-level "info"
  "Options for the log level of the Helm Language Server."
  :group 'lsp-kubernetes-helm
  :type '(choice
           (const "trace")
           (const "debug")
           (const "info")
           (const "warning")
           (const "error")
           (const "fatal")
           (const "panic"))
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-ls-main-values-file-path "values.yaml"
  "Path to main values file for Helm Chart."
  :group 'lsp-kubernetes-helm
  :type 'file
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-overlay-values-file-path "values.lint.yaml"
  "Path to values file that may be merged with main values files for Helm Chart."
  :group 'lsp-kubernetes-helm
  :type 'file
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-additional-values-files-pattern "values*.yaml"
  "Pattern for additional values files, which will be shown for completion and hover."
  :group 'lsp-kubernetes-helm
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-server-path "yaml-language-server"
  "Path to the Yaml Language Server binary that supports the Helm Language Server."
  :group 'lsp-kubernetes-helm
  :link '(url-link :tag "Yaml Language Server"
                   "https://github.com/redhat-developer/yaml-language-server")
  :risky t
  :type 'file)

(defcustom lsp-kubernetes-helm-yaml-ls-enable t
  "Enable/disable default YAML Language Server."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-enable-for-globs "*.{yaml,yml}"
  "Enable/disable default YAML Language Server."
  :group 'lsp-kubernetes-helm
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-diagnostics-limit 25
  "Limit the amount of yaml diagnostics to return.
Should typically be set to a low number when editing helm files."
  :group 'lsp-kubernetes-helm
  :type 'number
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-schemas '((https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.30.3-standalone-strict/all.json
                                                   . ["*.y*"])
                                                  (kubernetes . [""]))
  "List used by yaml language server to match schemas to globs.
This list is prioritized over the schema store schemas. Recommended to set
kubernetes to an empty string and at the end of the list to override the
default provided in yaml-language-server."
  :group 'lsp-kubernetes-helm
  :type '(alist :key-type (symbol :tag "schema") :value-type (lsp-repeatable-vector :tag "files (glob)"))
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-format-enable t
  "Enable/disable default YAML formatter."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-single-quote nil
  "Use single quote instead of double quotes."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-bracket-spacing t
  "Print spaces between brackets in objects."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-prose-wrap "preserve"
  "Options for prose-wrap.
Always: wrap prose if it exceeds the print width.
Never: never wrap the prose.
Preserve: wrap prose as-is."
  :group 'lsp-kubernetes-helm
  :type '(choice
          (const "always")
          (const "never")
          (const "preserve"))
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-print-width 80
  "Specify the line length that the printer will wrap on."
  :group 'lsp-kubernetes-helm
  :type 'number
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-validate t
  "Enable/disable validation feature."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-hover t
  "Enable/disable hover feature."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-completion t
  "Enable/disable completion feature."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-schema-store-extensions '(((name . "Kubernetes v1.30.3")
                                                                   (description . "Kubernetes v1.30.3 manifest schema definition")
                                                                   (url . "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.30.3-standalone-strict/all.json")))
  "Schemas defined by user schemas to files in a glob pattern.
Used by Yaml Language Server to determine which schema to use for which types of files."
  :group 'lsp-kubernetes-helm
  :type '(list (list (alist
                       :key-type (choice
                                   (const :tag "Name" name)
                                   (const :tag "Description" description)
                                   (const :tag "URL" url))
                       :value-type string)))
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-schema-store-enable nil
  "Enable/disable JSON Schema store.  When set to true, available YAML \
schemas will be automatically pulled from
`lsp-kubernetes-helm-yaml-ls-schema-store-uri'."
  :group 'lsp-kubernetes-helm
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-schema-store-uri "https://www.schemastore.org/api/json/catalog.json"
  "URL of schema store catalog to use."
  :group 'lsp-kubernetes-helm
  :type 'string
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-schema-store-local-db
  (expand-file-name
    (locate-user-emacs-file
      (f-join ".cache" "lsp" "lsp-kubernetes-helm-schemas.json")))
  "Cached database of schema store."
  :group 'lsp-kubernetes-helm
  :type 'file
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-custom-tags nil
  "Custom tags for the parser to use."
  :group 'lsp-kubernetes-helm
  :type '(lsp-repeatable-vector string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-yaml-ls-max-items-computed 5000
  "The maximum number of outline symbols and folding regions computed.
Limited for performance reasons."
  :group 'lsp-kubernetes-helm
  :type 'number
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-kubernetes-helm-server-arguments '("serve" "--stdio")
  "Command to start helm-ls.  Minimally needs serve otherwise the server wont start properly."
  :type '(repeat string)
  :group 'lsp-kubernetes-helm
  :package-version '(lsp-mode . "9.0.0"))

(defvar lsp-kubernetes-helm-yaml-ls--schema-store nil
  "A list of schemas provided by schema store uri.")

(defun lsp-kubernetes-helm-download-or-refresh-schema-store-db (&optional force-download)
  "Download remote schema store at `lsp-yaml-schema-store-uri' into local cache.
Set FORCE-DOWNLOAD to non-nil to force re-download the database.
FORCE-DOWNLOADING is set to t by default"
  (interactive "P")
  (let ((local-db-directory (file-name-directory lsp-kubernetes-helm-yaml-ls-schema-store-local-db))
         (force-download (or force-download t)))
    (when (not (file-exists-p lsp-kubernetes-helm-yaml-ls-schema-store-local-db))
      (unless (file-directory-p local-db-directory)
        (mkdir local-db-directory t))
      (url-copy-file lsp-kubernetes-helm-yaml-ls-schema-store-uri lsp-kubernetes-helm-yaml-ls-schema-store-local-db force-download))))

(defun lsp-kubernetes-helm--get-available-schemas ()
  "Get list of supported schemas."
  (when (and lsp-kubernetes-helm-yaml-ls-schema-store-enable
          (not lsp-kubernetes-helm-yaml-ls--schema-store))
    (lsp-kubernetes-helm-download-or-refresh-schema-store-db nil)
    (setq lsp-kubernetes-helm-yaml-ls--schema-store
          (alist-get 'schemas (json-read-file lsp-kubernetes-helm-yaml-ls-schema-store-local-db))))
  (seq-concatenate 'list lsp-kubernetes-helm-yaml-ls-schema-store-extensions lsp-kubernetes-helm-yaml-ls--schema-store))

(defun lsp-kubernetes-helm-set-buffer-schema (schema-uri-string)
  "Set yaml schema for the current buffer to SCHEMA-URI-STRING.
Remove buffer from all other schema associations."
  (interactive "MURI: ")
  (let* ((schema-uri (intern schema-uri-string))
          (buffer-file-path (file-relative-name
                            (lsp--uri-to-path (lsp--buffer-uri))
                            (lsp-workspace-root (lsp--buffer-uri))))
          ;; yaml language server can do partial path matching
          (glob (concat "/" buffer-file-path))
          (current-config (assoc schema-uri lsp-kubernetes-helm-yaml-ls-schemas))
          (current-patterns (and current-config (cdr current-config))))
    (if current-config
      (or (member glob (append current-patterns nil))
        (setq lsp-kubernetes-helm-yaml-ls-schemas
          (cl-acons schema-uri
            (vconcat (vector glob) current-patterns)
            (assq-delete-all schema-uri (mapcar (lambda (x) (lsp-kubernetes-helm--remove-glob-from-all-schemas x glob)) lsp-kubernetes-helm-yaml-ls-schemas)))))
      (setq lsp-kubernetes-helm-yaml-ls-schemas
        (cl-acons schema-uri (vector glob) (mapcar (lambda (x) (lsp-kubernetes-helm--remove-glob-from-all-schemas x glob)) lsp-kubernetes-helm-yaml-ls-schemas))))
    (lsp--set-configuration (lsp-configuration-section lsp-kubernetes-helm--lsp-configuration-section-name))))

(defun lsp-kubernetes-helm-select-buffer-schema ()
  "Select schema for the current buffer based on the list of supported schemas."
  (interactive)
  (let* ((schema (lsp--completing-read "Select buffer schema: "
                                       (lsp-kubernetes-helm--get-available-schemas)
                                       (lambda (schema)
                                         (format "%s: %s" (alist-get 'name schema)(alist-get 'description schema)))
                                       nil t))
         (uri (alist-get 'url schema)))
    (lsp-kubernetes-helm-set-buffer-schema uri)))

(defun lsp-kubernetes-helm--remove-glob-from-all-schemas (schemas glob)
  "Removes GLOB from all keys in SCHEMAS."
  (let ((patterns (cdr schemas)))
    (cons (car schemas)
      (vconcat (-filter (lambda (p)
                          (not (equal p glob)))
                 (append patterns nil)) nil))))

(lsp-register-custom-settings
  '(("helm-ls.logLevel" lsp-kubernetes-helm-ls-log-level)
     ("helm-ls.valuesFiles.mainValuesFile" lsp-kubernetes-helm-ls-main-values-file-path)
     ("helm-ls.valuesFiles.lintOverlayValuesFile" lsp-kubernetes-helm-overlay-values-file-path)
     ("helm-ls.valuesFiles.additionalValuesFilesGlobPattern" lsp-kubernetes-helm-additional-values-files-pattern)
     ("helm-ls.yamlls.enabled" lsp-kubernetes-helm-yaml-ls-enable t)
     ("helm-ls.yamlls.enabledForFilesGlob" lsp-kubernetes-helm-yaml-ls-enable-for-globs)
     ("helm-ls.yamlls.diagnosticsLimit" lsp-kubernetes-helm-yaml-ls-diagnostics-limit)
     ("helm-ls.yamlls.path" lsp-kubernetes-helm-yaml-ls-server-path)
     ("helm-ls.yamlls.config.format.enable" lsp-kubernetes-helm-yaml-ls-format-enable t)
     ("helm-ls.yamlls.config.format.singleQuote" lsp-kubernetes-helm-yaml-ls-single-quote t)
     ("helm-ls.yamlls.config.format.bracketSpacing" lsp-kubernetes-helm-yaml-ls-bracket-spacing)
     ("helm-ls.yamlls.config.format.proseWrap" lsp-kubernetes-helm-yaml-ls-prose-wrap)
     ("helm-ls.yamlls.config.format.printWidth" lsp-kubernetes-helm-yaml-ls-print-width)
     ("helm-ls.yamlls.config.validate" lsp-kubernetes-helm-yaml-ls-validate t)
     ("helm-ls.yamlls.config.hover" lsp-kubernetes-helm-yaml-ls-hover t)
     ("helm-ls.yamlls.config.completion" lsp-kubernetes-helm-yaml-ls-completion t)
     ("helm-ls.yamlls.config.schemas" lsp-kubernetes-helm-yaml-ls-schemas)
     ("helm-ls.yamlls.config.schemaStore.enable" lsp-kubernetes-helm-yaml-ls-schema-store-enable nil)
     ("helm-ls.yamlls.config.schemaStore.url" lsp-kubernetes-helm-yaml-ls-schema-store-uri)
     ("helm-ls.yamlls.config.customTags" lsp-kubernetes-helm-yaml-ls-custom-tags)
     ("helm-ls.yamlls.config.maxItemsComputed" lsp-kubernetes-helm-yaml-ls-max-items-computed)))

(lsp-dependency 'kubernetes-helm-language-server
  `(:system ,lsp-kubernetes-helm-ls-server-path)
  `(:system ,lsp-kubernetes-helm-yaml-ls-server-path)
  `(:npm :package "yaml-language-server"
     :path ,lsp-kubernetes-helm-yaml-ls-server-path))

(lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda ()
                                       `(,(or (executable-find lsp-kubernetes-helm-ls-server-path)
                                            (lsp-package-path 'kubernetes-helm-language-server))
                                          ,@lsp-kubernetes-helm-server-arguments)))
    :activation-fn (lsp-activate-on "helm-ls")
    :priority 0
    :server-id 'helm-ls
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                          (lsp-configuration-section lsp-kubernetes-helm--lsp-configuration-section-name))))))

(lsp-consistency-check lsp-kubernetes-helm)

(provide 'lsp-kubernetes-helm)
;;; lsp-kubernetes-helm.el ends here
