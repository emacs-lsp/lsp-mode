;;; lsp-haxe.el --- Haxe Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Yannik Böttcher
;; Copyright (C) 2019-2026 emacs-lsp maintainers

;; Author: Yannik Böttcher <yannikboettcher@outlook.de>
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

;; lsp-haxe client

;;; Code:


;; adapted from lsp-clangd configuration
(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-haxe nil
  "LSP support for Haxe using the language server provided by vshaxe"
  :group 'lsp-mode
  :link '(url-link "https://github.com/vshaxe/vshaxe"))


;; Haxe ls is executed via node
(defcustom lsp-clients--haxe-executable "node"
  "Haxe ls is executed via node."
  :group 'lsp-haxe
  :risky t
  :type 'file)

;; The server.js is being passed to node as an argument
(defcustom lsp-clients--haxe-server-path (expand-file-name "~/.haxe-language-server/bin/server.js")
  "The path to the server.js file."
  :group 'lsp-haxe
  :risky t
  :type 'file)

;; Build the actual Haxe ls command.
(defun lsp-clients--haxe-command ()
  "Haxe ls startup command."
  `(,lsp-clients--haxe-executable ,lsp-clients--haxe-server-path))

;; https://github.com/yyoncho/lsp-mode/commit/72186e1adc089d772c87ed8f287eb3333b66bfa7
;; This is to force the client to send a didChangeConfiguration Message. Without this, the server won't start, https://github.com/vshaxe/vshaxe/issues/328#issuecomment-471809093
(defcustom lsp-clients--haxe-settings (list :haxe.executable "haxe")
  "Lsp clients configuration settings."
  :group 'lsp-haxe
  :risky t
  :type '(repeat string))

;; The build spec for the project.
(defcustom lsp-haxe-hxml "build.hxml"
  "The compile file for the haxe project."
  :type 'file
  :group 'lsp-haxe
  :package-version '(lsp-mode . "7.0"))

;; https://github.com/emacs-lsp/lsp-mode/blob/150a933694349df960dc8fd7a15e04f5727e6433/lsp-rust.el#L251
(lsp-defun lsp-clients--haxe-processStart (_workspace (&haxe:ProcessStartNotification :title))
  "Handle processStart notification.  Just logs PARAMS."
  (lsp-log title))

(defcustom lsp-haxe-executable "haxe"
  nil
  :type 'file
  :group 'lsp-haxe)

(defcustom lsp-haxe-configurations nil
  nil
  :type '(repeat string)
  :group 'lsp-haxe)

(defcustom lsp-haxe-display-configurations nil
  nil
  :type '(repeat string)
  :group 'lsp-haxe)

(defcustom lsp-haxe-display-server nil
  nil
  :type 'string
  :group 'lsp-haxe)

(defcustom lsp-haxe-display-port "auto"
  nil
  :type 'number
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-compilation-server t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-task-presentation
  '((echo . t)
    (reveal . "always")
    (focus . :json-false)
    (panel . "shared")
    (showReuseMessage . t)
    (clear . :json-false))
  nil
  :type 'plist
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-code-lens t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-diagnostics t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-server-view nil
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-methods-view nil
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-signature-help-documentation t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-diagnostics-path-filter "${workspaceRoot}"
  nil
  :type 'string
  :group 'lsp-haxe)

(defcustom lsp-haxe-build-completion-cache t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-enable-completion-cache-warning t
  nil
  :type 'boolean
  :group 'lsp-haxe)

(defcustom lsp-haxe-code-generation nil
  nil
  :type 'string
  :group 'lsp-haxe)

(defcustom lsp-haxe-exclude ["zpp_nape"]
  nil
  :type '(repeat string)
  :group 'lsp-haxe)

(defcustom lsp-haxe-postfix-completion nil
  nil
  :type 'string
  :group 'lsp-haxe)

(lsp-register-custom-settings
 '(("haxe.hxml" lsp-haxe-hxml)
   ("haxe.postfixCompletion" lsp-haxe-postfix-completion)
   ("haxe.exclude" lsp-haxe-exclude)
   ("haxe.codeGeneration" lsp-haxe-code-generation)
   ("haxe.enableCompletionCacheWarning" lsp-haxe-enable-completion-cache-warning t)
   ("haxe.buildCompletionCache" lsp-haxe-build-completion-cache t)
   ("haxe.diagnosticsPathFilter" lsp-haxe-diagnostics-path-filter)
   ("haxe.enableSignatureHelpDocumentation" lsp-haxe-enable-signature-help-documentation t)
   ("haxe.enableMethodsView" lsp-haxe-enable-methods-view t)
   ("haxe.enableServerView" lsp-haxe-enable-server-view t)
   ("haxe.enableDiagnostics" lsp-haxe-enable-diagnostics t)
   ("haxe.enableCodeLens" lsp-haxe-enable-code-lens t)
   ("haxe.taskPresentation" lsp-haxe-task-presentation)
   ("haxe.enableCompilationServer" lsp-haxe-enable-compilation-server t)
   ("haxe.displayPort" lsp-haxe-display-port)
   ("haxe.displayServer" lsp-haxe-display-server)
   ("haxe.displayConfigurations" lsp-haxe-display-configurations)
   ("haxe.configurations" lsp-haxe-configurations)
   ("haxe.executable" lsp-haxe-executable)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-clients--haxe-command)
  :major-modes '(haxe-mode)  ; force didChangeConfiguration message
  :initialized-fn
  (lambda (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration (lsp-configuration-section "haxe"))))
  :priority -1
  :server-id 'haxe
  :initialization-options
  (lambda ()
    `(:sendMethodResults t
      :haxelibConfig (:executable "haxelib")
      :displayServerConfig
      ( :print (:reusing :json-false :completion :json-false)
               :arguments []
               :env nil
               :path "haxe")
      :displayArguments [,lsp-haxe-hxml]))
  :notification-handlers
  (lsp-ht ("haxe/progressStart" 'lsp-clients--haxe-processStart)
          ("haxe/progressStop" 'ignore)
          ("haxe/didDetectOldPreview" 'ignore)
          ("haxe/didChangeDisplayPort" 'ignore)
          ("haxe/didRunHaxeMethod" 'ignore)
          ("haxe/didChangeRequestQueue" 'ignore)
          ("haxe/cacheBuildFailed" 'ignore))))

(lsp-consistency-check lsp-haxe)

(provide 'lsp-haxe)
;;; lsp-haxe.el ends here
