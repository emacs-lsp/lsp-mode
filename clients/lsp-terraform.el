;;; lsp-terraform.el --- Terraform Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ross Donaldson, Sibi Prabakaran

;; Author: Ross Donaldson, Sibi Prabakaran
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
(require 'lsp-protocol)
(require 'dash)

;; terraform-lsp

(defgroup lsp-terraform nil
  "LSP support for Terraform, using terraform-lsp."
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
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-server "terraform-ls"
  "Path to the `terraform-ls' binary."
  :group 'lsp-terraform-ls
  :risky t
  :type '(choice
          (file :tag "File")
          (repeat string))
  :package-version `(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-enable-show-reference nil
  "Enable reference counts.

Display reference counts above top level blocks and
attributes.  This is an experimental feature provided by the
language server."
  :group 'lsp-terraform-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-validate-on-save nil
  "Enable validating the current open file on save.

This is an experimental feature provided by the language server."
  :group 'lsp-terraform-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-prefill-required-fields nil
  "Enable completion of required fields.

Enable autocompletion for required fields when completing
Terraform blocks.  This is an experimental feature provided by the
language server."
  :group 'lsp-terraform-ls
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-providers-position-params nil
  "The optional providers tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-terraform-ls
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-terraform-ls-module-calls-position-params nil
  "The optional module calls tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-terraform-ls
  :package-version '(lsp-mode . "9.0.0"))

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

(defun lsp-terraform-ls--init-options ()
  "Construct initialization options for the lanague server."
  `((experimentalFeatures . ((validateOnSave . ,(lsp-json-bool lsp-terraform-ls-validate-on-save))
                             (prefillRequiredFields . ,(lsp-json-bool lsp-terraform-ls-prefill-required-fields))))))

(defcustom lsp-terraform-semantic-token-faces
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
    ("hcl-objectKey" . lsp-face-semhl-member)
    ("hcl-mapKey" . lsp-face-semhl-member)
    ("hcl-keyword" . lsp-face-semhl-keyword)
    ("hcl-traversalStep" . lsp-face-semhl-member)
    ("hcl-typeCapsule" . lsp-face-semhl-type)
    ("hcl-typePrimitive" . lsp-face-semhl-type))
  "Mapping between terrafom-ls tokens and fonts to apply."
  :group 'lsp-terraform
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(defcustom lsp-terraform-semantic-token-modifier-faces
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
    ("terraform-name" . lsp-face-semhl-interface)
    ("terraform-type" . lsp-face-semhl-type)
    ("terraform-requiredProviders" . lsp-face-semhl-default-library))
  "Mapping between terraform-ls modifiers and fonts to apply."
  :group 'lsp-terraform
  :type '(alist :key-type string :value-type face)
  :package-version '(lsp-mode . "8.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-terraform-ls--make-launch-cmd)
                  :major-modes '(terraform-mode)
                  :priority 1
                  :server-id 'tfmls
                  :action-handlers (ht ("client.showReferences" #'lsp-terraform-ls--show-references))
                  :semantic-tokens-faces-overrides `(:discard-default-modifiers t
                                                     :discard-default-types t
                                                     :modifiers ,lsp-terraform-semantic-token-modifier-faces
                                                     :types ,lsp-terraform-semantic-token-faces)
                  :initialization-options (lsp-terraform-ls--init-options)
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

(defun lsp-terraform-ls-version ()
  "Get information about the terraform binary version for the current module."
  (interactive)
  (let ((terraform-data (lsp-request
                         "workspace/executeCommand"
                         (list :command "terraform-ls.module.terraform"
                               :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))))))
    (lsp--info "Required: %s, Current: %s"
               (lsp:terraform-ls-module-terraform-required-version terraform-data)
               (lsp:terraform-ls-module-terraform-discovered-version terraform-data))))

(lsp-consistency-check lsp-terraform)

(defvar treemacs-position)
(defvar treemacs-width)
(declare-function lsp-treemacs-render "ext:lsp-treemacs" (tree title expand-depth &optional buffer-name))

(defvar-local lsp-terraform-ls--providers-tree-data nil)
(defvar-local lsp-terraform-ls--modules-call-tree-data nil)
(defvar-local lsp-tf--modules-control-buffer nil)
(defconst lsp-terraform-ls--providers-buffer-name "*Terraform Providers*")
(defconst lsp-terraform-ls--modules-buffer-name "*Terraform Modules*")

(defvar lsp-terraform-modules-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") 'lsp-terraform-ls--modules-refresh)
    m)
  "Keymap for `lsp-terraform-modules-mode'.")

(define-minor-mode lsp-terraform-modules-mode "LSP Treemacs mode for terraform modules."
  :keymap lsp-terraform-modules-mode-map
  :group 'lsp-terraform-ls)

(cl-defstruct tf-package display-name doc-link installed-version version-constraint)

(cl-defstruct tf-module name doc-link version source-type dependent-modules)

(defun construct-tf-package (provider installed-version)
  "Construct `TF-PACKAGE' using PROVIDER and INSTALLED-VERSION."
  (make-tf-package :display-name (lsp-get provider :display_name)
                   :doc-link (lsp-get provider :docs_link)
                   :installed-version installed-version
                   :version-constraint (lsp-get provider :version_constraint)))

(lsp-defun construct-tf-module ((&terraform-ls:Module :name :docs-link :version :source-type :dependent-modules))
  "Construct `TF-MODULE' using MODULE."
  (make-tf-module :name name
                  :doc-link docs-link
                  :version version
                  :source-type source-type
                  :dependent-modules dependent-modules))

(lsp-defun lsp-terraform-ls--providers-to-tf-package ((&terraform-ls:Providers :provider-requirements :installed-providers))
  "Convert PROVIDERS-TREE-DATA to list of `tf-package'."
  (let* ((provider-requirements-keys (hash-table-keys provider-requirements))
         (installed-versions (mapcar (lambda (x) (lsp-get installed-providers (make-symbol (format ":%s" x)))) provider-requirements-keys))
         (providers (mapcar (lambda (x) (lsp-get provider-requirements (make-symbol (format ":%s" x)))) provider-requirements-keys))
         (tf-packages (-zip-with (lambda (x y) (construct-tf-package x y)) providers installed-versions)))
    tf-packages))

(lsp-defun lsp-terraform-ls--modules-to-tf-module ((&terraform-ls:ModuleCalls :module-calls))
  "Convert MODULES-TREE-DATA to list of `TF-MODULE'."
  (let* ((modules (-map (lambda (x) (construct-tf-module x)) module-calls)))
    modules))

(defun lsp-terraform-ls--fetch-modules-data (project-root)
  "Fetch modules data and set it in `lsp-terraform-ls--modules-call-tree-data'."
  (let* ((tree-data (lsp-request
                     "workspace/executeCommand"
                     (list :command "terraform-ls.module.calls"
                           :arguments (vector (format "uri=%s" (lsp--path-to-uri project-root))))
                     :no-wait nil
                     :no-merge nil))
         (modules (lsp-terraform-ls--modules-to-tf-module tree-data)))
    (setq-local lsp-terraform-ls--modules-call-tree-data modules)))

(defun lsp-terraform-ls--fetch-providers ()
  "Fetch modules call data and set it in `lsp-terraform-ls--providers-tree-data'."
  (let* ((tree-data (lsp-request
                     "workspace/executeCommand"
                     (list :command "terraform-ls.module.providers"
                           :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
                     :no-wait nil
                     :no-merge nil))
         (tf-packages (lsp-terraform-ls--providers-to-tf-package tree-data)))
    (setq-local lsp-terraform-ls--providers-tree-data tf-packages)))

(defun lsp-terraform-ls--tf-packages-to-treemacs (tf-packages)
  "Convert list of `TF-PACKAGES' to treemacs compatible data."
  (mapcar (lambda (package) (list :label (format "%s %s" (tf-package-display-name package) (tf-package-installed-version package))
                                  :icon 'package
                                  :key (tf-package-display-name package)
                                  :children (list (list
                                                   :icon 'library
                                                   :label (tf-package-version-constraint package)))
                                  :ret-action (lambda (&rest _) (browse-url (tf-package-doc-link package))))) tf-packages))

(defun lsp-terraform-ls--tf-modules-to-treemacs (tf-modules)
  "Convert list of `TF-MODULES' to treemacs compatible data."
  (mapcar (lambda (module) (list :label (format "%s %s" (tf-module-name module) (tf-module-version module))
                                 :icon 'package
                                 :key (tf-module-name module)
                                 :ret-action (lambda (&rest _) (browse-url (tf-module-doc-link module)))
                                 )) tf-modules))

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

(defun lsp-terraform-ls--show-module-calls (ignore-focus? project-root)
  "Show terraform modules and focus on it if IGNORE-FOCUS? is nil."
  (unless lsp-terraform-ls--modules-call-tree-data
    (lsp-terraform-ls--fetch-modules-data project-root))
  (unless lsp-terraform-ls--modules-call-tree-data
    (error "Modules data is empty"))
  (let* ((lsp-terraform-treemacs
          (lsp-terraform-ls--tf-modules-to-treemacs lsp-terraform-ls--modules-call-tree-data))
         (buffer (lsp-treemacs-render lsp-terraform-treemacs
                                      lsp-terraform-ls--modules-buffer-name
                                      t
                                      "Terraform Modules"))
         (modules-buffer (current-buffer))
         (position-params (or lsp-terraform-ls-module-calls-position-params
                              `((side . ,treemacs-position)
                                (slot . 1)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (select-window window)
    (setq-local lsp-tf--modules-control-buffer modules-buffer)
    (lsp-terraform-modules-mode t)
    (set-window-dedicated-p window t)
    (when ignore-focus?
      (select-window (previous-window)))))

(defun lsp-terraform-ls--refresh-module-calls ()
  "Refresh terraform modules."
  (lsp-terraform-ls--fetch-modules-data (lsp-workspace-root))
  (unless lsp-terraform-ls--modules-call-tree-data
    (error "Modules data is empty"))
  (let* ((lsp-terraform-treemacs
          (lsp-terraform-ls--tf-modules-to-treemacs lsp-terraform-ls--modules-call-tree-data))
         (buffer (lsp-treemacs-render lsp-terraform-treemacs
                                      lsp-terraform-ls--modules-buffer-name
                                      t
                                      "Terraform Modules"))
         (position-params (or lsp-terraform-ls-module-calls-position-params
                              `((side . ,treemacs-position)
                                (slot . 1)
                                (window-width . ,treemacs-width))))
         (window
          (display-buffer-in-side-window buffer position-params)))
    (select-window window)
    (lsp-terraform-modules-mode t)
    (set-window-dedicated-p window t)
    (lsp--info "Refresh completed")))

(defun lsp-terraform-ls-providers (&optional ignore-focus?)
  "Show terraform providers with focus on it if IGNORE-FOCUS? is nil."
  (interactive)
  (if (require 'lsp-treemacs nil t)
      (lsp-terraform-ls--show-providers ignore-focus?)
    (error "The package lsp-treemacs is not installed")))

(defun lsp-terraform-ls-module-calls (&optional ignore-focus?)
  "Show terraform modules with focus on it if IGNORE-FOCUS? is nil."
  (interactive)
  (if (require 'lsp-treemacs nil t)
      (lsp-terraform-ls--show-module-calls ignore-focus? (lsp-workspace-root))
    (error "The package lsp-treemacs is not installed")))

(defun lsp-terraform-ls--modules-refresh ()
  "Refresh terraform modules data."
  (interactive)
  (unless (buffer-live-p lsp-tf--modules-control-buffer)
    (error "Original buffer not present.  Do M-x lsp-terraform-ls-module-calls"))
  (with-current-buffer lsp-tf--modules-control-buffer
    (lsp-terraform-ls--refresh-module-calls)))

(provide 'lsp-terraform)
;;; lsp-terraform.el ends here
