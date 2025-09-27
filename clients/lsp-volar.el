;;; lsp-volar.el --- A lsp-mode client for Vue3 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 JadeStrong
;;
;; Author: JadeStrong <https://github.com/jadestrong>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: November 08, 2021
;; Modified: November 08, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-volar
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;
;;; Commentary:
;;
;; provide the connection to lsp-mode and volar language server
;;
;;; Code:
(require 'lsp-mode)
(require 'lsp-javascript)
(require 'dash)

(defgroup lsp-volar nil
  "Lsp support for vue3."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vuejs/language-tools")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-volar-typescript-server-id 'ts-ls
  "The server id of the typescript language server to use."
  :group 'lsp-volar
  :package-version '(lsp-mode . "9.0.1")
  :type 'symbol)

(defcustom lsp-volar-support-vue2 nil
  "Whether to ping Volar's version to ~3.0 to support vue2.

Volar is dropping Vue 2 and vue-class-component Support in v3.1.
Reference: https://github.com/vuejs/language-tools/discussions/5455"
  :group 'lsp-volar
  :package-version '(lsp-mode . "9.0.1")
  :type 'boolean)

(defun lsp-volar--activate-p (filename &optional _)
  "Check if the volar-language-server should be enabled base on FILENAME."
  (and filename (string-suffix-p ".vue" filename)))

(lsp-dependency 'volar-language-server
                '(:system "vue-language-server")
                '(:npm :package "@vue/language-server" :path "vue-language-server"
                       :version (lambda () (when lsp-volar-support-vue2 "~3.0"))))

;; Set lsp-clients-typescript-plugins
(condition-case nil
  (when-let* ((vue-language-server-path (lsp-package-path 'volar-language-server)))
    (let ((vue-plugin (list :name "@vue/typescript-plugin"
                        :location (f-join vue-language-server-path "../.." "lib/node_modules/@vue/language-server/")
                        :languages (vector "vue")
                        :configNamespace "typescript"
                        :enableForWorkspaceTypeScriptVersions t)))
      (setq lsp-clients-typescript-plugins
        (vconcat lsp-clients-typescript-plugins (vector vue-plugin)))))
  (error nil))

(defun lsp-volar--send-notify (workspace method params)
  "Send notification to WORKSPACE with METHOD PARAMS."
  (with-lsp-workspace workspace
    (let ((body (lsp--make-notification method params)))
      (lsp--send-no-wait body
        (lsp--workspace-proc lsp--cur-workspace)))))

(defun lsp-volar--tsserver-request-handler (volar-workspace params)
  "Handles `tsserver/request` notification from VOLAR-WORKSPACE.
And forwarding PARAMS to the typescript LSP server.

Reference:
- https://github.com/vuejs/language-tools/discussions/5456
- https://github.com/vuejs/language-tools/wiki/Neovim#configuration"
  (if-let* ((ts-ls-workspace (lsp-find-workspace lsp-volar-typescript-server-id nil)))
    (with-lsp-workspace ts-ls-workspace
      (-let [[[id command payload]] params]
        (lsp-request-async
          "workspace/executeCommand"
          (list :command "typescript.tsserverRequest"
            :arguments (vector command payload))
          ;; response callback
          (lambda (response)
            (let ((body (lsp-get response :body)))
              (lsp-volar--send-notify volar-workspace "tsserver/response" (vector (vector id body)))))
          ;; error callback
          :error-handler (lambda (error-response)
                           (lsp--warn "tsserver/request async error: %S" error-response)))))
    (lsp--error "[lsp-volar] Could not found `%s` lsp client, lsp-volar would not work without it" lsp-volar-typescript-server-id)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :add-on? t  ;; work with typescript server
  :server-id 'vue-semantic-server
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "vue")))
  :notification-handlers (ht ("tsserver/request" #'lsp-volar--tsserver-request-handler))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[
                                               ,(lsp-make-file-system-watcher :glob-pattern "**/*.vue")
                                              ])))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-language-server
                                            callback error-callback))))

(provide 'lsp-volar)
;;; lsp-volar.el ends here
