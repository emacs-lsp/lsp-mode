;;; lsp-copilot.el --- lsp-mode client for copilot       -*- lexical-binding: t -*-

;; Copyright (C) 2024 Rodrigo Virote Kassick

;; Author: Rodrigo Virote Kassick <kassick@gmail.com>
;; Keywords: lsp-mode, generative-ai, code-assistant

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Commentary:

;; LSP client for the copilot node server -- https://www.npmjs.com/package/copilot-node-server

;; Package-Requires: (lsp-mode secrets s compile dash cl-lib request company)

;; Code:

(require 'dash)
(require 'lsp-mode)
(require 's)

(defgroup lsp-copilot ()
  "Copilot LSP configuration"
  :group 'lsp-mode
  :tag "Copilot LSP"
  :link '(url-link "https://www.npmjs.com/package/copilot-node-server"))

(defcustom lsp-copilot-enabled t
  "Whether the server should be started to provide completions."
  :type 'boolean
  :group 'lsp-copilot)

(defcustom lsp-copilot-langserver-command-args '("--stdio")
  "Command to start copilot-langserver."
  :type '(repeat string)
  :group 'lsp-copilot)

(defcustom lsp-copilot-executable "copilot-lsp"
  "The system-wise executable of lsp-copilot.
When this executable is not found, you can stil use
lsp-install-server to fetch an emacs-local version of the LSP."
  :type 'string
  :group 'lsp-copilot)


(defcustom lsp-copilot-major-modes '(python-mode
                                     python-ts-mode
                                     go-mode
                                     go-ts-mode
                                     js-mode
                                     js-ts-mode
                                     java-mode
                                     java-ts-mode
                                     kotlin-mode
                                     kotlin-ts-mode
                                     ruby-mode
                                     ruby-ts-mode
                                     rust-mode
                                     rust-ts-mode
                                     tsx-ts-mode
                                     typescript-mode
                                     typescript-ts-mode
                                     vue-mode
                                     yaml-mode
                                     yaml-ts-mode)

  "The major modes for which lsp-copilot should be used"
  :type '(repeat symbol)
  :group 'lsp-copilot)

(defcustom lsp-copilot-server-disabled-languages nil
  "The lanuages for which the server must not be enabled (initialization setup for copilot)"
  :type '(repeat string)
  :group 'lsp-copilot)

(defcustom lsp-copilot-server-multi-root t
  "Whether the copilot server is started with multi-root"
  :type 'boolean
  :group 'lsp-copilot)

(lsp-dependency 'lsp-copilot
                `(:system ,lsp-copilot-executable)
                '(:npm :package "copilot-node-server"
                       :path "language-server.js"))


(defun lsp-copilot--client-active-for-mode-p (_ mode)
  (and lsp-copilot-enabled (member mode lsp-copilot-major-modes)))

(defun lsp-copilot--find-active-workspaces ()
  "Returns a list of lsp-copilot workspaces"
  (-some->> (lsp-session)
    (lsp--session-workspaces)
    (--filter (member (lsp--client-server-id (lsp--workspace-client it))
                      '(copilot-ls copilot-ls-remote)))))

(defun lsp-copilot--authenticated-as ()
  "Returns nil when not authorized; otherwise, the user name"
  (-if-let (workspace (--some (lsp-find-workspace it (buffer-file-name))
                              '(copilot-ls copilot-ls-remote)))
      (-if-let (checkStatusResponse (with-lsp-workspace workspace
                                      (lsp-request "checkStatus" '(:dummy "dummy"))))
          (-let* (((&copilot-ls:CheckStatusResponse? :status :user) checkStatusResponse))
            (unless (s-present-p status)
              (error "No status in response %S" checkStatusResponse))
            ;; Result:
            (when (s-equals-p status "OK")
              user))
        (error "No response from the LSP server"))
    (error "No lsp-copilot workspace found!")))

;;;###autoload
(defun lsp-copilot-check-status ()
  "Checks the status of the Copilot Server"
  (interactive)

  (condition-case err
      (progn
        (let ((user (lsp-copilot--authenticated-as)))
          (if user
              (message "Authenticated as %s" user)
            (user-error "Not Authenticated"))))
    (t (user-error "Error checking status: %s" err))))


;;;###autoload
(defun lsp-copilot-login ()
  "Log in with copilot.

This function is automatically called during the client initialization if needed"
  (interactive)

  (-when-let (workspace (--some (lsp-find-workspace it) '(lsp-copilot lsp-copilot-remote)))
    (with-lsp-workspace workspace
      (-when-let* ((response (lsp-request "signInInitiate" '(:dummy "dummy"))))
        (-let (((&copilot-ls:SignInInitiateResponse? :status :user-code :verification-uri :expires-in :interval :user) response))

          ;; Bail if already signed in
          (when (s-equals-p status "AlreadySignedIn")
            (lsp-message "Copilot :: Already signed in as %s" user))

          (if (display-graphic-p)
              (progn
                (gui-set-selection 'CLIPBOARD user-code)
                (read-from-minibuffer (format "Your one-time code %s is copied. Press \
ENTER to open GitHub in your browser. If your browser does not open \
automatically, browse to %s." user-code verification-uri))
                (browse-url verification-uri)
                (read-from-minibuffer "Press ENTER if you finish authorizing."))
            ;; Console:
            (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
            (read-from-minibuffer (format "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri)))

          (lsp-message "Verifying...")
          (-let* ((confirmResponse (lsp-request "signInConfirm" (list :userCode user-code)))
                  ((&copilot-ls:SignInConfirmResponse? :status :user) confirmResponse))
            (when (s-equals-p status "NotAuthorized")
              (user-error "User %s is not authorized" user))
            (lsp-message "User %s is authorized: %s" user status))

          ;; Do we need to confirm?
          (-let* ((checkStatusResponse (lsp-request "checkStatus" '(:dummy "dummy")))
                  ((&copilot-ls:CheckStatusResponse? :status :user) checkStatusResponse))
            (when (s-equals-p status "NotAuthorized")
              (user-error "User %s is not authorized" user))

            (lsp-message "Authenticated as %s" user)))))))


(defun lsp-copilot--server-initialization-options ()
  ;; Trying to replicate Copilot.vim initialization here ...
  (list :editorInfo (list :name "emacs" :version (symbol-value 'emacs-version))
        :editorPluginInfo (list :name "lsp-copilot" :version "1.38.0")
        :editorConfig (list :enableAutoCompletions lsp-copilot-enabled
                            :disabledLanguages lsp-copilot-server-disabled-languages)
        :name "emacs"
        :version "0.1.0"))

(defun lsp-copilot--server-initialized-fn (workspace)
  (unless (lsp-copilot--authenticated-as)
    (lsp-copilot-login)))

(defun lsp-copilot--cmdline ()
  (-if-let (candidates (directory-files-recursively
                        (f-join lsp-server-install-dir "npm" "copilot-node-server")
                        "^language-server.js$"))
      `("node" ,(car candidates) ,@lsp-copilot-langserver-command-args)
    (error "language-server.js not found")))

;; Server installed by emacs
(lsp-register-client
 (make-lsp-client
  :server-id 'lsp-copilot
  :new-connection (lsp-stdio-connection #'lsp-copilot--cmdline)
  :activation-fn #'lsp-copilot--client-active-for-mode-p
  :multi-root lsp-copilot-server-multi-root
  :priority -2
  :add-on? t
  :completion-in-comments? t
  :initialization-options #'lsp-copilot--server-initialization-options
  :initialized-fn #'lsp-copilot--server-initialized-fn
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'lsp-copilot callback error-callback))
  :notification-handlers (lsp-ht
                          ("$/progress" (lambda (&rest args) (lsp-message "$/progress with %S" args)))
                          ("featureFlagsNotification" #'ignore)
                          ("statusNotification" #'ignore)
                          ("window/logMessage" #'lsp--window-log-message)
                          ("conversation/preconditionsNotification" #'ignore))))

(lsp-register-client
 (make-lsp-client
  :server-id 'lsp-copilot-remote
  :new-connection (lsp-stdio-connection (lambda ()
                                          `(,lsp-copilot-executable ,@lsp-copilot-langserver-command-args)))
  :activation-fn #'lsp-copilot--client-active-for-mode-p
  :multi-root lsp-copilot-server-multi-root
  :priority -2
  :add-on? t
  :completion-in-comments? t
  :initialization-options #'lsp-copilot--server-initialization-options
  :initialized-fn #'lsp-copilot--server-initialized-fn
  :notification-handlers (lsp-ht
                          ("$/progress" (lambda (&rest args) (lsp-message "$/progress with %S" args)))
                          ("featureFlagsNotification" #'ignore)
                          ("statusNotification" #'ignore)
                          ("window/logMessage" #'lsp--window-log-message)
                          ("conversation/preconditionsNotification" #'ignore))))

(lsp-consistency-check lsp-copilot)

(provide 'lsp-copilot)
