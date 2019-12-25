;;; lsp-eslint.el --- lsp-mode eslint integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages

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

(defconst lsp-eslint-status-ok 1)
(defconst lsp-eslint-status-warn 2)
(defconst lsp-eslint-status-error 3)

(defgroup lsp-eslint nil
  "ESlint language server group."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Microsoft/vscode-eslint"))

(defcustom lsp-eslint-server-command `("node"
                                       "~/server/out/eslintServer.js"
                                       "--stdio")
  "Command to start eslint server."
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-enable t
  "Controls whether eslint is enabled for JavaScript files or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-package-manager "npm"
  "The package manager you use to install node modules."
  :type '(choice (:tag "npm" "yarn" "pnpm"))
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-format t
  "Whether to perform format."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-node-path nil
  "A path added to NODE_PATH when resolving the eslint module."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-options nil
  "The eslint options object to provide args normally passed to
  eslint when executed from a command line (see
  http://eslint.org/docs/developer-guide/nodejs-api#cliengine)."
  :type 'alist)

(defcustom lsp-eslint-trace-server "off"
  "Traces the communication between VSCode and the eslint linter service."
  :type 'string)

(defcustom lsp-eslint-run "onType"
  "Run the linter on save (onSave) or on type (onType)"
  :type '(choice (:tag "onSave" "onType"))
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-auto-fix-on-save nil
  "Turns auto fix on save on or off."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-quiet nil
  "Turns on quiet mode, which ignores warnings."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-working-directories []
  ""
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-validate ["javascript" "javascriptreact"]
  "An array of language ids which should be validated by ESLint"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-provide-lint-task nil
  "Controls whether a task for linting the whole workspace will be available."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-lint-task-enable nil
  "Controls whether a task for linting the whole workspace will be available."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-lint-task-options "."
  "Command line options applied when running the task for linting the whole workspace (see https://eslint.org/docs/user-guide/command-line-interface)."
  :type 'string
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-runtime nil
  "The location of the node binary to run ESLint under."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-code-action-disable-rule-comment '((enable . t) (location . "separateLine"))
  ""
  :type 'alist
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-code-action-show-documentation '((enable . t))
  ""
  :type 'alist)

(defcustom lsp-eslint-experimental-incremental-sync t
  "Controls whether the new incremental text document synchronization should be used."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defun lsp--find-eslint ()
  (or
   (when-let (workspace-folder (lsp-find-session-folder (lsp-session) default-directory))
     (let ((eslint-local-path (f-join workspace-folder "node_modules" ".bin"
                                      (if (eq system-type 'windows-nt) "eslint.cmd" "eslint"))))
       (when (f-exists? eslint-local-path)
         eslint-local-path)))
   "eslint"))

(defun lsp-eslint-create-default-configuration ()
  "Create default eslint configuration."
  (interactive)
  (unless (lsp-session-folders (lsp-session))
    (user-error "There are no workspace folders."))
  (pcase (->> (lsp-session)
              lsp-session-folders
              (-filter (lambda (dir)
                         (-none?
                          (lambda (file) (f-exists? (f-join dir file)))
                          '(".eslintrc.js" ".eslintrc.yaml" ".eslintrc.yml" ".eslintrc" ".eslintrc.json")))))
    (`nil (user-error "All workspace folders contain eslint configuration."))
    (folders (let ((default-directory (completing-read "Select project folder: " folders nil t)))
               (async-shell-command (format "%s --init" (lsp--find-eslint)))))))

(defun lsp-eslint-status-handler (workspace params)
  (setf (lsp--workspace-status-string workspace)
        (propertize "ESLint"
                    'face (cond
                           ((eq (gethash "state" params) lsp-eslint-status-error) 'error)
                           ((eq (gethash "state" params) lsp-eslint-status-warn) 'warn)
                           (t 'success)))))

(defun lsp-eslint--configuration (_workspace params)
  (->> params
       (gethash "items")
       (seq-map (-lambda ((&hash "scopeUri" uri))
                  (-when-let* ((file (lsp--uri-to-path uri))
                               (buffer (find-buffer-visiting file))
                               (workspace-folder (lsp-find-session-folder (lsp-session) file)))
                    (with-current-buffer buffer
                      (list :validate "probe"
                            :packageManager lsp-eslint-package-manager
                            :codeActionOnSave t
                            :format (lsp-json-bool lsp-eslint-format)
                            :options (or lsp-eslint-options (ht))
                            :run (or lsp-eslint-run "onType")
                            :nodePath lsp-eslint-node-path
                            :onIgnoredFiles "off"
                            :quiet (lsp-json-bool lsp-eslint-quiet)
                            :workspaceFolder (list :uri (lsp--path-to-uri workspace-folder)
                                                   :name (f-filename workspace-folder))
                            :codeAction (list
                                         :disableRuleComment (or lsp-eslint-code-action-disable-rule-comment
                                                                 (list :enable t
                                                                       :location "separateLine"))
                                         :showDocumentation (or lsp-eslint-code-action-show-documentation (list :enable t)) ))))))
       (apply #'vector)))

(defun lsp-eslint--open-doc (_workspace params)
  (browse-url (gethash "url" params)))

(defun lsp-eslint-apply-all-fixes ()
  "Apply all autofixes in the current buffer."
  (interactive)
  (lsp-send-execute-command "eslint.applyAllFixes" (vector (lsp--versioned-text-document-identifier))))



(lsp-register-client
 (make-lsp-client
  :new-connection
  (plist-put (lsp-stdio-connection
              (lambda () lsp-eslint-server-command))
             :test? (lambda ()
                      (and (cl-second lsp-eslint-server-command)
                           (file-exists-p (cl-second lsp-eslint-server-command)))))
  :activation-fn (lambda (filename &optional _)
                   (or (string-match-p (rx (one-or-more anything) "."
                                           (or "ts" "js" "jsx" "tsx" "html" "vue"))
                                       filename)
                       (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode)))
  :priority -1
  :completion-in-comments? t
  :add-on? t
  :multi-root t
  :notification-handlers (ht ("eslint/status" #'lsp-eslint-status-handler))
  :request-handlers (ht ("workspace/configuration" #'lsp-eslint--configuration)
                        ("eslint/openDoc" 'lsp-eslint--open-doc))
  :server-id 'eslint
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (ht ("id" "random-id")
                           ("method" "workspace/didChangeWatchedFiles")
                           ("registerOptions" (ht ("watchers"
                                                   (vector (ht ("globPattern" "**/.eslintr{c.js,c.yaml,c.yml,c,c.json}"))
                                                           (ht ("globPattern" "**/.eslintignore"))
                                                           (ht ("globPattern" "**/package.json"))))))))))))

(provide 'lsp-eslint)
;;; lsp-eslint.el ends here
