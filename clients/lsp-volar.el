;;; lsp-volar.el --- A lsp-mode client for Vue3 -*- lexical-binding: t; -*-

;; Copyright (C) 2021 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>

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
;; provide the connection to lsp-mode and volar language server
;;

;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-volar nil
  "lsp support for vue3"
  :group 'lsp-mode
  :link '(url-link "https://github.com/johnsoncodehk/volar")
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-volar-server-command '("volar-server")
  "Command to start vetur."
  :type '(repeat string)
  :risky t
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-volar-take-over-mode t
  "Enable Take Over Mode."
  :type 'boolean
  :group 'lsp-volar)

(defun lsp-volar--get-typescript-server-path ()
  "Get tsserver.js file path."
  (if-let ((package-path (lsp-package-path 'typescript))
           (system-server-path (apply #'f-join (append (cl-subseq (f-split (file-truename package-path)) 0 -2) '("lib" "tsserver.js"))))
           (is-exist (f-file-p system-server-path)))
      system-server-path
    ""))

(defun lsp-volar--activate-p (filename &optional _)
  "Check if the volar-language-server should be enabled base on FILENAME."
  (if lsp-volar-take-over-mode
      (and (or (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vue.config.js")))
               (locate-dominating-file (buffer-file-name) "vue.config.js")
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vue.config.ts")))
               (locate-dominating-file (buffer-file-name) "vue.config.ts")
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vite.config.js")))
               (locate-dominating-file (buffer-file-name) "vite.cofnig.js")
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vite.config.ts")))
               (locate-dominating-file (buffer-file-name) "vite.config.ts")
               (f-file-p (f-join (lsp-workspace-root) ".volarrc")))
           (or (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
                   (and (derived-mode-p 'js-mode 'typescript-mode)
                        (not (derived-mode-p 'json-mode))))
               (string= (file-name-extension filename) "vue")))
    (string= (file-name-extension filename) "vue")))

(lsp-register-custom-settings
 '(("typescript.serverPath" (lambda () (if-let ((project-root (lsp-workspace-root))
                                                (server-path (concat project-root "node_modules/typescript/lib/tsserverlibrary.js"))
                                                (is-exist (file-exists-p server-path)))
                                           server-path
                                         (lsp-volar--get-typescript-server-path)))
    t)
   ("languageFeatures.references" t t)
   ("languageFeatures.definition" t t)
   ("languageFeatures.typeDefinition" t t)
   ("languageFeatures.callHierarchy" t t)
   ("languageFeatures.hover" t t)
   ("languageFeatures.rename" t t)
   ("languageFeatures.renameFileRefactoring" t t)
   ("languageFeatures.signatureHelp" t t)
   ("languageFeatures.codeAction" t t)
   ("languageFeatures.completion.defaultTagNameCase" "both" t)
   ("languageFeatures.completion.defaultAttrNameCase" "kebabCase" t t)
   ("languageFeatures.completion.getDocumentNameCasesRequest" nil t)
   ("languageFeatures.completion.getDocumentSelectionRequest" nil t)
   ("languageFeatures.schemaRequestService" t t)

   ("documentFeatures.documentColor" nil t)
   ("documentFeatures.selectionRange" t t)
   ("documentFeatures.foldingRange" t t)
   ("documentFeatures.linkedEditingRange" t t)
   ("documentFeatures.documentSymbol" t t)
   ("documentFeatures.documentFormatting" 100 t)
   ("html.hover" t t)))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(lsp-dependency 'volar-language-server
                '(:system "volar-server")
                '(:npm :package "@volar/server" :path "@volar/server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :server-id 'volar-api
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (lsp-configuration-section "html")
                                               (lsp-configuration-section "languageFeatures")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "html")
                                 (lsp-configuration-section "languageFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-server
                                            callback error-callback))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :add-on? t
  :server-id 'volar-doc
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (ht ("languageFeatures" (ht-merge (ht ("semanticTokens" nil))
                                                                                 (ht ("documentHighlight" t))
                                                                                 (ht ("documentLink" t))
                                                                                 (ht ("codeLens" t))
                                                                                 (ht ("diagnostics" t)))))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "languageFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-server
                                            callback error-callback))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :add-on? t
  :server-id 'volar-html
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (lsp-configuration-section "documentFeatures")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "documentFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-server
                                            callback error-callback))))

(provide 'lsp-volar)
;;; lsp-volar.el ends here
