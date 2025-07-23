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

;; Package-Requires: (lsp-mode secrets s compile dash cl-lib request uuidgen)

;; Code:

(require 'dash)
(require 'lsp-mode)
(require 's)
(require 'uuidgen)
(require 'lsp-inline-completion)

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


;;; Panel Completion

(declare-function company--active-p "ext:company")
(declare-function company-cancel "ext:company" (&optional result))
(declare-function org-forward-heading-same-level "ext:org" (arg &optional invisible-ok))
(declare-function org-backward-heading-same-level "ext:org" (arg &optional invisible-ok))
(declare-function org-down-element "ext:org")
(defvar-local lsp-copilot-panel-completion-items nil
  "A list of completion items returned by the Panel Completion call")

(defvar-local lsp-copilot-panel-completion-token nil
  "The per-request token")

(defvar-local lsp-copilot-panel-modification-tick nil
  "Modification tick when the panel was called")

(defun lsp-copilot--panel-accept-item (completing-buffer-name original-tick item)
  (when (buffer-live-p (get-buffer completing-buffer-name))
    (with-current-buffer completing-buffer-name
      (unless (= original-tick (buffer-chars-modified-tick))
        (user-error "Can not accept the suggestion on a modified buffer. Try copying it"))

      (-let* (((&copilot-ls:PanelCompletionItem? :insert-text :range? :command?) item)
              ((start . end) (when range?
                               (-let (((&RangeToPoint :start :end) range?)) (cons start end)))))

        (unless (and start end)
          (error "Server did not provide a range -- Can not insert"))

        (lsp-with-undo-amalgamate
          (delete-region start end)
          (goto-char start)
          (insert insert-text))

        ;; Post command
        (when command?
          (lsp--execute-command command?))))))

(defun lsp-copilot--panel-accept-suggestion-at-point ()
  (interactive)
  (let ((completing-buffer-name (get-text-property (point) 'lsp-panel-completing-buffer-name))
        (original-tick (get-text-property (point) 'lsp-original-tick))
        (item (get-text-property (point) 'lsp-panel-item)))
    (lsp-copilot--panel-accept-item completing-buffer-name original-tick item)
    (quit-window)))

(defun lsp-copilot--panel-accept-button-click (b)
  (when-let* ((item (overlay-get b 'lsp-panel-item))
              (completing-buffer-name (overlay-get b 'lsp-panel-completing-buffer-name))
              (original-tick (overlay-get b 'lsp-original-tick)))
    (lsp-copilot--panel-accept-item completing-buffer-name original-tick item)

    (quit-window)))

(defun lsp-copilot--panel-copy-button-click (b)

  (kill-new (lsp:copilot-ls-panel-completion-item-insert-text (overlay-get b 'lsp-panel-item)))
  (quit-window))

(defun lsp-copilot--panel-forward-suggestion (arg)
  (interactive "p")
  (org-forward-heading-same-level arg)
  (recenter-top-bottom 0)
  (org-down-element))

(defun lsp-copilot--panel-backward-suggestion (arg)
  (interactive "p")
  (org-backward-heading-same-level arg)
  (recenter-top-bottom 0)
  (org-down-element))

(define-derived-mode lsp-copilot-panel-buffer-mode org-mode "CopilotPanel"
  "A major mode for completions "
  :group 'lsp-copilot)


(let ((key-bindings '(("C-n" . lsp-copilot--panel-forward-suggestion)
                      ("C-p" . lsp-copilot--panel-backward-suggestion)
                      ("C-<return>" . lsp-copilot--panel-accept-suggestion-at-point)
                      ("q" . quit-window))))
  (dolist (binding key-bindings)
    (define-key lsp-copilot-panel-buffer-mode-map (kbd (car binding)) (cdr binding))))


(defcustom lsp-copilot-panel-display-fn #'pop-to-buffer
  "Function used to display the panel completions buffer"
  :type 'function
  :group 'lsp-copilot)


(defun lsp-copilot--panel-display-buffer (completing-buffer-name items original-tick)
  "Builds and display the panel buffer"
  (if (lsp-inline-completion--active-p)
    (progn
      (lsp-inline-completion-cancel)))

  (when (and (bound-and-true-p company-mode)
             (company--active-p))
    (company-cancel))

  (let ((buf (get-buffer-create (format "*lsp-copilot-panel-results-%s*" completing-buffer-name) )))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (fundamental-mode)

      ;; (insert "#+STARTUP: noindent\n\n")
      (setq-local org-startup-indented nil)
      (cl-loop for item in items
               for i from 1
               do
               (-let* ((start-point (point))
                       ((&copilot-ls:PanelCompletionItem? :insert-text) item)
                       (heading (format "* Solution %d" i))
                       (src (format "#+begin_src %s\n%s\n#+end_src\n" "python" insert-text)))

                 (insert heading)
                 (insert ?\n ?\n)
                 (insert-button "Accept"
                                'action #'lsp-copilot--panel-accept-button-click
                                'lsp-original-tick original-tick
                                'lsp-panel-item item
                                'lsp-panel-completing-buffer-name completing-buffer-name)

                 (insert ?\s)

                 (insert-button "Copy"
                                'action #'lsp-copilot--panel-copy-button-click
                                'lsp-original-tick original-tick
                                'lsp-panel-item item
                                'lsp-panel-completing-buffer-name completing-buffer-name)

                 (insert ?\n)
                 (insert src)
                 (insert ?\n ?\n)

                 (put-text-property start-point (point) 'lsp-original-tick original-tick)
                 (put-text-property start-point (point) 'lsp-panel-item item)
                 (put-text-property start-point (point) 'lsp-panel-completing-buffer-name completing-buffer-name)))

      (lsp-delete-all-space t)
      (lsp-copilot-panel-buffer-mode)
      (read-only-mode +1)

      (goto-char (point-min))
      (org-down-element))

    (if (get-buffer-window completing-buffer-name 'visible)
        (progn
          (select-window (get-buffer-window completing-buffer-name 'visible))
          (funcall lsp-copilot-panel-display-fn buf))
      (user-error "The original buffer for completions was not active; not showing panel"))))


(defun lsp-copilot-panel-display-buffer ()
  "Displays a completion panel with the items collected by the last call of lsp-copilot-panel-completion"
  (interactive)

  (if lsp-copilot-panel-completion-items
      (lsp-copilot--panel-display-buffer (buffer-name) lsp-copilot-panel-completion-items lsp-copilot-panel-modification-tick)
    (lsp--error "No completions to display")))

(defun lsp-copilot--panel-completions-progress-handler (_ params)
  (-let* (((&ProgressParams :token :value) params)
          ((action completing-buffer-name panel-completion-token) (s-split " /// " token)))
    (pcase action
      ;; copilot sends results in the report
      ("PANEL-PARTIAL-RESULT"
       (when (and (lsp-copilot-ls-panel-completion-items? value)
                  (buffer-live-p (get-buffer completing-buffer-name)))
         (with-current-buffer completing-buffer-name
           (when (string-equal panel-completion-token lsp-copilot-panel-completion-token)
             (setq-local lsp-copilot-panel-completion-items
                         (nconc lsp-copilot-panel-completion-items
                                (seq-into (lsp:copilot-ls-panel-completion-items-items value) 'list)))))))
      ("PANEL-WORK-DONE"
       (when (and (lsp-work-done-progress-end? value)
                  (buffer-live-p (get-buffer completing-buffer-name))
                  (string-equal (lsp:work-done-progress-end-kind value) "end"))
         (with-current-buffer completing-buffer-name
           (when (string-equal panel-completion-token lsp-copilot-panel-completion-token)
             (lsp-copilot-panel-display-buffer))))))))

(defun lsp-copilot-panel-completion ()
  "Use a Completion Panel to provide suggestions at point"
  (interactive)

  (lsp-inline-completion-inhibit-idle-completions)
  (lsp-inline-completion-cancel-timer)

  (let* ((token (uuidgen-1))
         (partial-token (format "PANEL-PARTIAL-RESULT /// %s /// %s" (buffer-name) token))
         (done-token (format "PANEL-WORK-DONE /// %s /// %s"  (buffer-name) token))
         (params (lsp-make-copilot-ls-panel-completion-params :text-document (lsp--text-document-identifier)
                                                              :position (lsp--cur-position)
                                                              :partial-result-token partial-token
                                                              :work-done-token done-token)))
    (setq-local lsp-copilot-panel-modification-tick (buffer-chars-modified-tick))
    (setq-local lsp-copilot-panel-completion-token token)
    (setq-local lsp-copilot-panel-completion-items nil)

    ;; call the complation and do not wait for any result -- the completions
    ;; will be provided via $/progress notifications
    (lsp-request-async "textDocument/copilotPanelCompletion" params #'ignore)))


;;; LSP Client

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

(defun lsp-copilot--progress-callback (workspace params)
  (when lsp-progress-function
    (funcall lsp-progress-function workspace params))

  (lsp-copilot--panel-completions-progress-handler workspace params))

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
                          ("$/progress" #'lsp-copilot--progress-callback)
                          ("featureFlagsNotification" #'ignore)
                          ("statusNotification" #'ignore)
                          ("didChangeStatus" #'ignore)
                          ("window/logMessage" #'lsp--window-log-message)
                          ("conversation/preconditionsNotification" #'ignore))))

(lsp-consistency-check lsp-copilot)

(provide 'lsp-copilot)
