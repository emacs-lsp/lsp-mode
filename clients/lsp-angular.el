;;; lsp-angular.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp,

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

;; LSP Clients for the Angular Web application framework.

;;; Code:

(require 'lsp-mode)
(require 'f)


;;; Angular
(defgroup lsp-angular nil
  "Angular LSP client, provided by the Angular Language Service Server."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/angular/vscode-ng-language-service"))

(defcustom lsp-clients-angular-language-server-command
  nil
  "The command that starts the angular language server."
  :group 'lsp-angular
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(defcustom lsp-clients-angular-node-get-prefix-command
  "npm config get --global prefix"
  "The shell command that returns the path of NodeJS's prefix.
Has no effects when `lsp-clients-angular-language-server-command' is set."
  :group 'lsp-angular
  :type 'string)

(defun lsp-client--angular-start-loading (_workspace params)
  (lsp--info "Started loading project %s" params))

(defun lsp-client--angular-finished-loading (_workspace params)
  (lsp--info "Finished loading project %s" params))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda ()
     (if lsp-clients-angular-language-server-command
         lsp-clients-angular-language-server-command
       (let ((node-modules-path
              (f-join
               (string-trim
                (shell-command-to-string lsp-clients-angular-node-get-prefix-command))
               "lib/node_modules")))
         ;; The shell command takes a significant time to run,
         ;; so we "cache" its results after running once
         (setq lsp-clients-angular-language-server-command
               (list
                "node"
                (f-join node-modules-path "@angular/language-server")
                "--ngProbeLocations"
                node-modules-path
                "--tsProbeLocations"
                node-modules-path
                "--stdio"))
         lsp-clients-angular-language-server-command))))
  :activation-fn
  (lambda (&rest _args)
    (and (string-match-p "\\(\\.html\\|\\.ts\\)\\'" (buffer-file-name))
         (lsp-workspace-root)
         (file-exists-p (f-join (lsp-workspace-root) "angular.json"))))
  :priority -1
  :notification-handlers
  (ht ("angular/projectLoadingStart" #'lsp-client--angular-start-loading)
      ("angular/projectLoadingFinish" #'lsp-client--angular-finished-loading)
      ("angular/projectLanguageService" #'ignore))
  :add-on? t
  :server-id 'angular-ls))


(lsp-consistency-check lsp-angular)

(provide 'lsp-angular)
;;; lsp-angular.el ends here
