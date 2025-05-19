;;; lsp-copilot.el --- lsp-mode client for Copilot       -*- lexical-binding: t -*-

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

;; LSP client for the Copilot Language Server:
;; https://www.npmjs.com/package/@github/copilot-language-server

;; Package-Requires: (lsp-mode secrets s compile dash cl-lib request company)

;; Code:

(require 'dash)
(require 'lsp-mode)
(require 's)

(defgroup lsp-copilot ()
  "Copilot LSP configuration"
  :group 'lsp-mode
  :tag "Copilot LSP"
  :link '(url-link "https://www.npmjs.com/package/@github/copilot-language-server"))

(defcustom lsp-copilot-enabled nil
  "Whether the server should be started to provide completions."
  :type 'boolean
  :group 'lsp-copilot)

(defcustom lsp-copilot-auth-check-delay 5
  "How much time to wait before checking if the server is properly authenticated.

Set this value to nil if you do not want for the check to be made."
  :type '(choice (const :tag "Do not check" nil)
                 (integer :tag "Seconds" 5)))

(defcustom lsp-copilot-langserver-command-args '("--stdio")
  "Command to start copilot-langserver."
  :type '(repeat string)
  :group 'lsp-copilot)

(defcustom lsp-copilot-executable "copilot-language-server"
  "The system-wise executable of lsp-copilot.
When this executable is not found, you can stil use
lsp-install-server to fetch an emacs-local version of the LSP."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-applicable-fn (lambda (&rest _) lsp-copilot-enabled)
  "A function which returns whether the Copilot is applicable for the buffer.
The input are the file name and the major mode of the buffer."
  :type 'function
  :group 'lsp-copilot)

(defcustom lsp-copilot-server-disabled-languages nil
  "The languages for which the server must not be enabled (initialization setup for Copilot)"
  :type '(repeat string)
  :group 'lsp-copilot)

(defcustom lsp-copilot-server-multi-root t
  "Whether the Copilot server is started with multi-root."
  :type 'boolean
  :group 'lsp-copilot)

(defcustom lsp-copilot-version "1.270.0"
  "Copilot version."
  :type '(choice (const :tag "Latest" nil)
                 (string :tag "Specific Version"))
  :group 'lsp-copilot)

(lsp-dependency 'copilot-ls
                `(:system ,lsp-copilot-executable)
                '(:npm :package "@github/copilot-language-server"
                       :path "copilot-language-server"
                       :version lsp-copilot-version))

(defun lsp-copilot--find-active-workspaces ()
  "Returns a list of lsp-copilot workspaces"
  (-some->> (lsp-session)
    (lsp--session-workspaces)
    (--filter (member (lsp--client-server-id (lsp--workspace-client it))
                      '(copilot-ls copilot-ls-tramp)))))

(defun lsp-copilot--authenticated-as ()
  "Returns nil when not authorized; otherwise, the user name"
  (-if-let (workspace (--some (lsp-find-workspace it (buffer-file-name))
                              '(copilot-ls copilot-ls-tramp)))
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
  "Log in with Copilot.

This function is automatically called during the client initialization if needed"
  (interactive)

  (-when-let (workspace (--some (lsp-find-workspace it) '(copilot-ls copilot-ls-tramp)))
    (with-lsp-workspace workspace
      (-when-let* ((response (lsp-request "signInInitiate" '(:dummy "dummy"))))
        (-let (((&copilot-ls:SignInInitiateResponse? :status :user-code :verification-uri :user) response))

          ;; Bail if already signed in
          (cond
           ((s-equals-p status "AlreadySignedIn")
            (lsp--info "Copilot :: Already signed in as %s" user))
           ((yes-or-no-p "Copilot requires you to log into your GitHub account. Proceed now?")
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

            (lsp--info "Verifying...")
            (-let* ((confirmResponse (lsp-request "signInConfirm" (list :userCode user-code)))
                    ((&copilot-ls:SignInConfirmResponse? :status :user) confirmResponse))
              (when (s-equals-p status "NotAuthorized")
                (user-error "User %s is not authorized" user))
              (lsp--info "User %s is authorized: %s" user status))

            ;; Do we need to confirm?
            (-let* ((checkStatusResponse (lsp-request "checkStatus" '(:dummy "dummy")))
                    ((&copilot-ls:CheckStatusResponse? :status :user) checkStatusResponse))
              (when (s-equals-p status "NotAuthorized")
                (user-error "User %s is not authorized" user))

              (lsp--info "Authenticated as %s" user)))
           (t
            (message "Aborting Copilot login. To avoid being asked again, customize `lsp-copilot-enabled'"))))))))

(defun lsp-copilot-logout ()
  "Logout from Copilot."
  (interactive)
  (-when-let (workspace (--some (lsp-find-workspace it) '(copilot-ls copilot-ls-tramp)))
    (with-lsp-workspace workspace
      (lsp-request "signOut" '(:dummy "dummy"))
      (lsp--info "Logged out."))))

(defun lsp-copilot--server-initialization-options ()
  ;; Trying to replicate Copilot.vim initialization here ...
  (list :editorInfo (list :name "emacs" :version emacs-version)
        :editorPluginInfo (list :name "lsp-copilot" :version (lsp-package-version))
        :editorConfig (list :enableAutoCompletions lsp-copilot-enabled
                            :disabledLanguages lsp-copilot-server-disabled-languages)
        :name "emacs"
        :version "0.1.0"))

(defun lsp-copilot--server-initialized-fn (workspace)
  ;; Patch capabilities -- server may respond with an empty dict. In plist,
  ;; this would become nil
  (let ((caps (lsp--workspace-server-capabilities workspace)))
    (lsp:set-server-capabilities-inline-completion-provider? caps t))


  (when lsp-copilot-auth-check-delay
    (run-at-time lsp-copilot-auth-check-delay
                 nil
                 (lambda ()
                   (condition-case err
                       (unless (lsp-copilot--authenticated-as)
                         (lsp-copilot-login))
                     (t (lsp--error "Could not authenticate with Copilot: %s" (error-message-string err)))))))
  t)

;; Server installed by emacs
(lsp-register-client
 (make-lsp-client
  :server-id 'copilot-ls
  :new-connection (lsp-stdio-connection
                   (lambda () `(,(lsp-package-path 'copilot-ls) ,@lsp-copilot-langserver-command-args))
                   )
  :activation-fn lsp-copilot-applicable-fn
  :multi-root lsp-copilot-server-multi-root
  :priority -2
  :add-on? t
  :completion-in-comments? t
  :initialization-options #'lsp-copilot--server-initialization-options
  :initialized-fn #'lsp-copilot--server-initialized-fn
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'copilot-ls callback error-callback))
  :notification-handlers (lsp-ht
                          ("$/progress" (lambda (&rest args) (lsp-message "$/progress with %S" args)))
                          ("featureFlagsNotification" #'ignore)
                          ("statusNotification" #'ignore)
                          ("didChangeStatus" #'ignore)
                          ("window/logMessage" #'lsp--window-log-message)
                          ("conversation/preconditionsNotification" #'ignore))))

(lsp-consistency-check lsp-copilot)

(provide 'lsp-copilot)
