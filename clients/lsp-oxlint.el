;;; lsp-oxlint.el --- LSP client for Oxlint -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pradyuman Vig
;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: Pradyuman Vig <me@pmn.co>
;; Keywords: languages, tools

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

;; LSP client for Oxlint, using the `oxlint --lsp' command.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-oxlint nil
  "LSP support for Oxlint."
  :group 'lsp-mode
  :link '(url-link "https://oxc.rs/docs/guide/usage/linter/editors"))

(lsp-defcustom lsp-oxlint-config-path nil
  "Path to an Oxlint configuration file.
When set, Oxlint disables automatic configuration discovery."
  :type '(choice (const :tag "Automatic" nil)
                 string)
  :group 'lsp-oxlint
  :lsp-path "oxc.configPath")

(lsp-defcustom lsp-oxlint-tsconfig-path nil
  "Path to the TypeScript configuration file used for import resolution."
  :type '(choice (const :tag "Automatic" nil)
                 string)
  :group 'lsp-oxlint
  :lsp-path "oxc.tsConfigPath")

(lsp-defcustom lsp-oxlint-unused-disable-directives nil
  "Severity for unused Oxlint disable directives.
When nil, use the language server default."
  :type '(choice (const :tag "Server default" nil)
                 (const "allow")
                 (const "warn")
                 (const "deny"))
  :group 'lsp-oxlint
  :lsp-path "oxc.unusedDisableDirectives")

(lsp-defcustom lsp-oxlint-type-aware nil
  "Whether to enable type-aware linting.
When nil, use the setting from the root Oxlint configuration."
  :type '(choice (const :tag "Use project configuration" nil)
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" :json-false))
  :group 'lsp-oxlint
  :lsp-path "oxc.typeAware")

(lsp-defcustom lsp-oxlint-disable-nested-config nil
  "Whether to disable nested Oxlint configuration discovery.
When nil, use the language server default."
  :type '(choice (const :tag "Server default" nil)
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" :json-false))
  :group 'lsp-oxlint
  :lsp-path "oxc.disableNestedConfig")

(lsp-defcustom lsp-oxlint-fix-kind nil
  "Maximum class of fixes offered by Oxlint.
When nil, use the language server default."
  :type '(choice (const :tag "Server default" nil)
                 (const "safe_fix")
                 (const "safe_fix_or_suggestion")
                 (const "dangerous_fix")
                 (const "dangerous_fix_or_suggestion")
                 (const "none")
                 (const "all"))
  :group 'lsp-oxlint
  :lsp-path "oxc.fixKind")

(lsp-defcustom lsp-oxlint-rules-customization nil
  "Per-rule diagnostic severity and automatic-fix overrides.
The value is an alist or hash table keyed by Oxlint rule name."
  :type '(choice (const :tag "None" nil)
                 sexp)
  :group 'lsp-oxlint
  :lsp-path "oxc.rulesCustomization")

(lsp-defcustom lsp-oxlint-run nil
  "When Oxlint publishes diagnostics.
When nil, use the language server default."
  :type '(choice (const :tag "Server default" nil)
                 (const "onType")
                 (const "onSave"))
  :group 'lsp-oxlint
  :lsp-path "oxc.run")

(lsp-dependency 'oxlint
                '(:system "oxlint")
                '(:npm :package "oxlint"
                       :path "oxlint"))

(defun lsp-oxlint--server-command ()
  "Return the command used to start the Oxlint language server."
  (let* ((local-path (concat "node_modules/.bin/oxlint"
                             (when (eq system-type 'windows-nt) ".cmd")))
         (root (locate-dominating-file default-directory local-path)))
    (list (if root
              (expand-file-name local-path root)
            (lsp-package-path 'oxlint))
          "--lsp")))

(defun lsp-oxlint-fix-all ()
  "Apply all safe Oxlint fixes to the current buffer."
  (interactive)
  (lsp-send-execute-command
   "oxc.fixAll"
   (vector (list :uri (lsp--buffer-uri)))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-oxlint--server-command)
  :activation-fn
  (lsp-activate-on
   "javascript"
   "javascriptreact"
   "typescript"
   "typescriptreact"
   "vue"
   "svelte"
   "astro")
  :server-id 'oxlint
  :priority -1
  :add-on? t
  :multi-root t
  :initialized-fn
  (lambda (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration
       (lsp-configuration-section "oxc"))))
  :synchronize-sections '("oxc")
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'oxlint callback error-callback))))

(lsp-consistency-check lsp-oxlint)

(provide 'lsp-oxlint)
;;; lsp-oxlint.el ends here
