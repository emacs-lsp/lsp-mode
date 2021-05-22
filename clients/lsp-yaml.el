;;; lsp-yaml.el --- LSP YAML server integration        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
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
(require 'dash)

(defgroup lsp-yaml nil
  "LSP support for YAML, using yaml-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/redhat-developer/yaml-language-server")
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-format-enable t
  "Enable/disable default YAML formatter."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-single-quote nil
  "Use single quote instead of double quotes."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-bracket-spacing t
  "Print spaces between brackets in objects."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-prose-wrap "preserve"
  "Options for prose-wrap.
   Always: wrap prose if it exceeds the print width.
   Never: never wrap the prose.
   Preserve: wrap prose as-is."
  :type '(choice
          (const "always")
          (const "never")
          (const "preserve"))
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-print-width 80
  "Specify the line length that the printer will wrap on."
  :type 'number
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-validate t
  "Enable/disable validation feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-hover t
  "Enable/disable hover feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-completion t
  "Enable/disable completion feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-schemas '()
  "Associate schemas to YAML files in a glob pattern."
  :type '(alist :key-type (symbol :tag "schema") :value-type (lsp-string-vector :tag "files (glob)"))
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-schema-store-enable t
  "Enable/disable JSON Schema store. When set to true, available YAML
   schemas will be automatically pulled from the store."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-custom-tags nil
  "Custom tags for the parser to use."
  :type '(lsp-repeatable-vector string)
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-schema-store-uri "https://www.schemastore.org/api/json/catalog.json"
  "URI of schema store that would be fetched to get the list of schemas."
  :type 'string
  :group 'lsp-yaml)

(defcustom lsp-yaml-schema-store-local-db (expand-file-name
                                           (locate-user-emacs-file
                                            (f-join ".cache" "lsp" "lsp-yaml-schemas.json")))
  "Cached databse of schema store."
  :type 'file
  :group 'lsp-yaml)

(defvar lsp-yaml--schema-store-schemas-alist nil
  "A list of schemas fetched from schema stores.")

(lsp-register-custom-settings
 '(("yaml.format.enable" lsp-yaml-format-enable t)
   ("yaml.format.singleQuote" lsp-yaml-single-quote t)
   ("yaml.format.bracketSpacing" lsp-yaml-bracket-spacing)
   ("yaml.format.proseWrap" lsp-yaml-prose-wrap)
   ("yaml.format.printWidth" lsp-yaml-print-width)
   ("yaml.validate" lsp-yaml-validate t)
   ("yaml.hover" lsp-yaml-hover t)
   ("yaml.completion" lsp-yaml-completion t)
   ("yaml.schemas" lsp-yaml-schemas)
   ("yaml.schemaStore.enable" lsp-yaml-schema-store-enable t)
   ("yaml.customTags" lsp-yaml-custom-tags)))

(defcustom lsp-yaml-server-command '("yaml-language-server" "--stdio")
  "Command to start yaml-languageserver."
  :type '(repeat string)
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(lsp-dependency 'yaml-language-server
                '(:system "yaml-language-server")
                '(:npm :package "yaml-language-server"
                       :path "yaml-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find (cl-first lsp-yaml-server-command))
                                            (lsp-package-path 'yaml-language-server))
                                       ,@(cl-rest lsp-yaml-server-command))))
                  :major-modes '(yaml-mode)
                  :priority 0
                  :server-id 'yamlls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "yaml"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'yaml-language-server
                                                            callback error-callback))))

(defconst lsp-yaml--built-in-kubernetes-schema
  '((name . "Kubernetes")
    (description . "Built-in kubernetes manifest schema definition")
    (url . "kubernetes")
    (fileMatch . ["*-k8s.yaml" "*-k8s.yml"])))

(defun lsp-yaml-download-schema-store-db (&optional force-downloading)
  "Download the remote schema store at `lsp-yaml-schema-store-uri' into local cache.
Set FORCE-DOWNLOADING to non-nil to force re-download the database."
  (interactive "P")
  (when (or force-downloading (not (file-exists-p lsp-yaml-schema-store-local-db)))
    (unless (file-directory-p (file-name-directory lsp-yaml-schema-store-local-db))
      (mkdir (file-name-directory lsp-yaml-schema-store-local-db)))
    (url-copy-file lsp-yaml-schema-store-uri lsp-yaml-schema-store-local-db force-downloading)))

(defun lsp-yaml--get-supported-schemas ()
  "Get out the list of supported schemas."
  (when (and lsp-yaml-schema-store-enable
             (not lsp-yaml--schema-store-schemas-alist))
    (lsp-yaml-download-schema-store-db)
    (setq lsp-yaml--schema-store-schemas-alist
          (alist-get 'schemas (json-read-file lsp-yaml-schema-store-local-db))))
  (seq-concatenate 'list (list lsp-yaml--built-in-kubernetes-schema) lsp-yaml--schema-store-schemas-alist))

(defun lsp-yaml-set-buffer-schema (uri-string)
  "Set yaml schema for the current buffer to URI-STRING."
  (interactive "MURI: ")
  (let* ((uri (intern uri-string))
         (workspace-path (file-relative-name
                          (lsp--uri-to-path (lsp--buffer-uri))
                          (lsp-workspace-root (lsp--buffer-uri))))
         (glob (concat "/" workspace-path))
         (current-config (assoc uri lsp-yaml-schemas))
         (current-patterns (and current-config (cdr current-config))))
    (if current-config
        (or (member glob (append current-patterns nil))
            (setq lsp-yaml-schemas
                  (cl-acons uri
                            (vconcat (vector glob) current-patterns)
                            (assq-delete-all uri
                                             (mapcar (lambda (x) (lsp-yaml--remove-glob x glob))
                                                     lsp-yaml-schemas)))))
      (setq lsp-yaml-schemas
            (cl-acons uri (vector glob) (mapcar (lambda (x) (lsp-yaml--remove-glob x glob))
                                                lsp-yaml-schemas))))
    (lsp--set-configuration (lsp-configuration-section "yaml"))))

(defun lsp-yaml-select-buffer-schema ()
  "Select schema for the current buffer based on the list of supported schemas."
  (interactive)
  (let* ((schema (lsp--completing-read "Select buffer schema: "
                                       (lsp-yaml--get-supported-schemas)
                                       (lambda (schema)
                                         (alist-get 'description schema))
                                       nil t))
         (uri (alist-get 'url schema)))
    (lsp-yaml-set-buffer-schema uri)))

(defun lsp-yaml--remove-glob (mapping glob)
  (let ((patterns (cdr mapping)))
    (cons (car mapping)
          (vconcat (-filter (lambda (p) (not (equal p glob)))
                            (append patterns nil)) nil))))

(lsp-consistency-check lsp-yaml)

(provide 'lsp-yaml)
;;; lsp-yaml.el ends here
