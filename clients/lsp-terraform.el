;;; lsp-terraform.el --- Terraform Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ross Donaldson

;; Author: Ross Donaldson
;; Keywords: terraform lsp

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

;; LSP client for Terraform

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)
(require 'dash)


;; terraform-lsp

(defgroup lsp-terraform nil
  "LSP support for Terraform, using terraform-lsp"
  :group 'lsp-mode
  :link '(url-link "https://github.com/juliosueiras/terraform-lsp")
  :package-version `(lsp-mode . "6.2"))

(defcustom lsp-terraform-server "terraform-lsp"
  "Path to the `terraform-lsp' binary."
  :group 'lsp-terraform
  :risky t
  :type '(choice
          (file :tag "File")
          (repeat string))
  :package-version `(lsp-mode . "6.2"))

(defcustom lsp-terraform-enable-logging nil
  "If non-nil, enable `terraform-ls''s native logging."
  :group 'lsp-terraform
  :risky t
  :type 'boolean
  :package-version `(lsp-mode . "6.2"))


(defun lsp-terraform--make-launch-cmd ()
  (-let [base (if (stringp lsp-terraform-server)
                  `(,lsp-terraform-server)
                lsp-terraform-server)]
    (when lsp-terraform-enable-logging
      (push "-enable-log-file" base))
    base))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-terraform--make-launch-cmd)
                  :major-modes '(terraform-mode)
                  :priority -1
                  :server-id 'tfls))


;; terraform-ls

(defgroup lsp-terraform-ls nil
  "LSP support for Terraform, using terraform-ls from Hashicorp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/hashicorp/terraform-ls")
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-terraform-ls-server "terraform-ls"
  "Path to the `terraform-ls' binary."
  :group 'lsp-terraform-ls
  :risky t
  :type '(choice
          (file :tag "File")
          (repeat string))
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-terraform-ls-enable-show-reference nil
  "Enable reference counts.

Display reference counts above top level blocks and
attributes.  This is an experimental feature provided by the
language server."
  :group 'lsp-terraform-ls
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-terraform-ls-providers-position-params nil
  "The optional providers tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-terraform-ls
  :package-version '(lsp-mode . "8.0.1"))

(defun lsp-terraform-ls--make-launch-cmd ()
  `(,lsp-terraform-ls-server "serve"))

(lsp-defun lsp-terraform-ls--show-references ((&Command :arguments?))
  "Show references for command with ARGS."
  (lsp-show-xrefs
     (lsp--locations-to-xref-items
      (lsp-request "textDocument/references"
                   (lsp--make-reference-params
                    (lsp--text-document-position-params nil (elt arguments? 0)))))
     t
     t))

(defun lsp-terraform-ls--custom-capabilities ()
  "Construct custom capabilities for the language server."
  (when lsp-terraform-ls-enable-show-reference
    '((experimental . ((showReferencesCommandId . "client.showReferences"))))))

(defun lsp-terraform-ls--set-tokens ()
  (setq lsp-semantic-token-faces
      '(("namespace" . lsp-face-semhl-namespace)
        ("type" . lsp-face-semhl-type)
        ("class" . lsp-face-semhl-class)
        ("enum" . lsp-face-semhl-enum)
        ("interface" . lsp-face-semhl-interface)
        ("struct" . lsp-face-semhl-struct)
        ("typeParameter" . lsp-face-semhl-type-parameter)
        ("parameter" . lsp-face-semhl-parameter)
        ("variable" . lsp-face-semhl-variable)
        ("property" . lsp-face-semhl-property)
        ("enumMember" . lsp-face-semhl-constant)
        ("event" . lsp-face-semhl-event)
        ("function" . lsp-face-semhl-function)
        ("method" . lsp-face-semhl-method)
        ("macro" . lsp-face-semhl-macro)
        ("keyword" . lsp-face-semhl-keyword)
        ("modifier" . lsp-face-semhl-member)
        ("comment" . lsp-face-semhl-comment)
        ("string" . lsp-face-semhl-string)
        ("number" . lsp-face-semhl-number)
        ("regexp" . lsp-face-semhl-regexp)
        ("operator" . lsp-face-semhl-operator)
        ("hcl-attrName" . lsp-face-semhl-member)
        ("hcl-blockType" . lsp-face-semhl-struct)
        ("hcl-blockLabel" . lsp-face-semhl-member)
        ("hcl-bool" . lsp-face-semhl-constant)
        ("hcl-string" . lsp-face-semhl-string)
        ("hcl-number" . lsp-face-semhl-number)
        ("hcl-objectKey" . lsp-face-semhl-label)
        ("hcl-mapKey" . lsp-face-semhl-label)
        ("hcl-keyword" . lsp-face-semhl-keyword)
        ("hcl-traversalStep" . lsp-face-semhl-label)
        ("hcl-typeCapsule" . lsp-face-semhl-type)
        ("hcl-typePrimitive" . lsp-face-semhl-type)))
  (setq lsp-semantic-token-modifier-faces
      '(("declaration" . lsp-face-semhl-class)
        ("definition" . lsp-face-semhl-definition)
        ("readonly" . lsp-face-semhl-constant)
        ("static" . lsp-face-semhl-static)
        ("deprecated" . lsp-face-semhl-deprecated)
        ("abstract" . lsp-face-semhl-keyword)
        ("async" . lsp-face-semhl-macro)
        ("modification" . lsp-face-semhl-operator)
        ("documentation" . lsp-face-semhl-comment)
        ("defaultLibrary" . lsp-face-semhl-default-library)
        ("hcl-dependent" . lsp-face-semhl-constant)
        ("terraform-data" . lsp-face-semhl-constant)
        ("terraform-locals" . lsp-face-semhl-variable)
        ("terraform-module" . lsp-face-semhl-namespace)
        ("terraform-output" . lsp-face-semhl-constant)
        ("terraform-provider" . lsp-face-semhl-class)
        ("terraform-resource" . lsp-face-semhl-interface)
        ("terraform-provisioner" . lsp-face-semhl-default-library)
        ("terraform-connection" . lsp-face-semhl-constant)
        ("terraform-variable" . lsp-face-semhl-variable)
        ("terraform-terraform" . lsp-face-semhl-constant)
        ("terraform-backend" . lsp-face-semhl-definition)
        ("terraform-name" . lsp-face-semhl-label)
        ("terraform-type" . lsp-face-semhl-type)
        ("terraform-requiredProviders" . lsp-face-semhl-default-library))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-terraform-ls--make-launch-cmd)
                  :major-modes '(terraform-mode)
                  :priority 1
                  :server-id 'tfmls
                  :action-handlers (ht ("client.showReferences" #'lsp-terraform-ls--show-references))
                  :custom-capabilities (lsp-terraform-ls--custom-capabilities)))

(defun lsp-terraform-ls-validate ()
  "Execute terraform validate on project root."
  (interactive)
  (lsp-request
   "workspace/executeCommand"
   (list :command "terraform-ls.terraform.validate"
         :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))
         )
   :no-wait t
   :no-merge t))

(defun lsp-terraform-ls-init ()
  "Execute terraform init on project root.

This is a synchronous action."
  (interactive)
  (lsp-request
     "workspace/executeCommand"
     (list :command "terraform-ls.terraform.init"
           :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
     :no-wait nil
     :no-merge t))

(lsp-consistency-check lsp-terraform)

(defvar treemacs-position)
(defvar treemacs-width)
(declare-function lsp-treemacs-render "ext:lsp-treemacs" (tree title expand-depth &optional buffer-name))

(defvar-local lsp-terraform-ls--providers-tree-data nil)
(defconst lsp-terraform-ls--providers-buffer-name "*Terraform Providers*")

(cl-defstruct tf-package display-name doc-link installed-version version-constraint)

(defun construct-tf-package (provider installed-version)
  "Construct `TF-PACKAGE' using PROVIDER and INSTALLED-VERSION."
  (make-tf-package :display-name (gethash "display_name" provider)
                   :doc-link (gethash "docs_link" provider)
                   :installed-version installed-version
                   :version-constraint (gethash "version_constraint" provider)))

(defun lsp-terraform-ls--providers-to-tf-package (providers-tree-data)
  "Convert PROVIDERS-TREE-DATA to list of `tf-package'."
  (let* ((json-object-type 'hash-table)
         (json-key-type 'string)
         (installed-providers (gethash "installed_providers" providers-tree-data))
         (provider-requirements (gethash "provider_requirements" providers-tree-data))
         (provider-requirements-keys (hash-table-keys provider-requirements))
         (installed-versions (mapcar (lambda (x) (gethash x installed-providers))  provider-requirements-keys))
         (providers (mapcar (lambda (x) (gethash x provider-requirements)) provider-requirements-keys))
         (tf-packages (-zip-with (lambda (x y) (construct-tf-package x y)) providers installed-versions) )
         )
    tf-packages))

(defun lsp-terraform-ls--fetch-providers ()
  "Fetch providers data and set it in `lsp-terraform-ls--providers-tree-data'."
  (let* ((tree-data (lsp-request
                     "workspace/executeCommand"
                     (list :command "terraform-ls.module.providers"
                           :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
                     :no-wait nil
                     :no-merge nil))
         (tf-packages (lsp-terraform-ls--providers-to-tf-package tree-data)))
    (setq lsp-terraform-ls--providers-tree-data tf-packages)))

(defun lsp-terraform-ls--tf-packages-to-treemacs (tf-packages)
  "Convert list of `TF-PACKAGES' to treemacs compatible data."
  (mapcar (lambda (package) (list :label (format "%s %s" (tf-package-display-name package) (tf-package-installed-version package))
                                  :icon 'package
                                  :key (tf-package-display-name package)
                                  :children (list (list
                                                   :icon 'library
                                                   :label (tf-package-version-constraint package)))
                                  :ret-action (lambda (&rest _) (browse-url (tf-package-doc-link package))))) tf-packages))

(defun lsp-terraform-ls--show-providers (ignore-focus?)
  "Show terraform providers and focus on it if IGNORE-FOCUS? is nil."
  (unless lsp-terraform-ls--providers-tree-data
    (lsp-terraform-ls--fetch-providers))
  (let* ((lsp-terraform-treemacs
          (lsp-terraform-ls--tf-packages-to-treemacs lsp-terraform-ls--providers-tree-data))
         (buffer (lsp-treemacs-render lsp-terraform-treemacs
                                      lsp-terraform-ls--providers-buffer-name
                                      t
                                      "Terraform Providers"))
         (position-params (or lsp-terraform-ls-providers-position-params
                              `((side . ,treemacs-position)
                                (slot . 2)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-terraform-ls-providers (&optional ignore-focus?)
  "Show terraform providers with focus on it if IGNORE-FOCUS? is nil."
  (interactive)
  (if (require 'lsp-treemacs nil t)
      (lsp-terraform-ls--show-providers ignore-focus?)
    (error "The package lsp-treemacs is not installed")))

(with-eval-after-load 'terraform-mode
  (when lsp-semantic-tokens-enable
    (lsp-terraform-ls--set-tokens)
    (add-hook 'terraform-mode-hook #'lsp-terraform-ls--set-tokens)))

(provide 'lsp-terraform)
;;; lsp-terraform.el ends here
