;;; lsp-metals.el --- Scala Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Ross A. Baker <ross@rossabaker.com>, Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Author: Ross A. Baker <ross@rossabaker.com>, Evgeny Kurnevsky <kurnevsky@gmail.com>
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

;; lsp-metals client

;;; Code:

(require 'lsp-mode)
(require 'view)

(defgroup lsp-metals nil
  "LSP support for Scala, using Metals."
  :group 'lsp-mode
  :link '(url-link "https://scalameta.org/metals")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-server-command "metals-emacs"
  "The command to launch the Scala language server."
  :group 'lsp-metals
  :type 'file
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-server-args '()
  "Extra arguments for the Scala language server."
  :group 'lsp-metals
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-java-home ""
  "The Java Home directory used for indexing JDK sources and locating
the `java` binary."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-scalafmt-config-path ""
  "Optional custom path to the .scalafmt.conf file. Should be relative
to the workspace root directory and use forward slashes / for file
separators (even on Windows)."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-sbt-script ""
  "Optional absolute path to an `sbt` executable to use for running
`sbt bloopInstall`. By default, Metals uses `java -jar sbt-launch.jar`
with an embedded launcher while respecting `.jvmopts` and `.sbtopts`.
Update this setting if your `sbt` script requires more customizations
like using environment variables."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-metals-gradle-script ""
  "Optional absolute path to a `gradle` executable to use for running
`gradle bloopInstall`. By default, Metals uses gradlew with 5.3.1
gradle version. Update this setting if your `gradle` script requires
more customizations like using environment variables."
  :type '(string)
  :group 'lsp-metals
  :package-version '(lsp-mode . "6.1"))


(lsp-register-custom-settings
 '(("metals.java-home" lsp-metals-java-home)
   ("metals.scalafmt-config-path" lsp-metals-scalafmt-config-path)
   ("metals.sbt-script" lsp-metals-sbt-script)
   ("metals.gradle-script" lsp-metals-gradle-script)))

(defun lsp-metals--server-command ()
  "Generate the Scala language server startup command."
  `(,lsp-metals-server-command ,@lsp-metals-server-args))

(defun lsp-metals-build-import ()
  "Unconditionally run `sbt bloopInstall` and re-connect to the build server."
  (interactive)
  (lsp-send-execute-command "build-import" ()))

(defun lsp-metals-build-connect ()
  "Unconditionally cancel existing build server connection and re-connect."
  (interactive)
  (lsp-send-execute-command "build-connect" ()))

(defun lsp-metals-doctor-run ()
  "Open the Metals doctor to troubleshoot potential build problems."
  (interactive)
  (lsp-send-execute-command "doctor-run" ()))

(defun lsp-metals-sources-scan ()
  "Walk all files in the workspace and index where symbols are defined."
  (interactive)
  (lsp-send-execute-command "sources-scan" ()))

(defun lsp-metals--doctor-render (html)
  "Render the Metals doctor html in the current buffer."
  (require 'shr)
  (setq-local show-trailing-whitespace nil)
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (insert html)
  (shr-render-region (point-min) (point-max))
  (goto-char (point-min))
  (view-mode 1)
  (setq view-exit-action 'kill-buffer))

(defun lsp-metals--generate-doctor-buffer-name (workspace)
  (format "*Metals Doctor: %s*" (process-id (lsp--workspace-cmd-proc workspace))))

(defun lsp-metals--doctor-run (workspace html)
  "Focus on a window displaying troubleshooting help from the Metals doctor."
  (pop-to-buffer (lsp-metals--generate-doctor-buffer-name workspace))
  (lsp-metals--doctor-render html))

(defun lsp-metals--doctor-reload (workspace html)
  "Reload the HTML contents of an open Doctor window, if any.
Should be ignored if there is no open doctor window."
  (when-let ((buffer (get-buffer (lsp-metals--generate-doctor-buffer-name workspace))))
    (with-current-buffer buffer
      (lsp-metals--doctor-render html))))

(defun lsp-metals--goto-location (_workspace location)
  "Move the cursor focus to the provided location."
  (let ((xrefs (lsp--locations-to-xref-items (list location))))
    (if (boundp 'xref-show-definitions-function)
      (with-no-warnings
        (funcall xref-show-definitions-function
          (-const xrefs)
          `((window . ,(selected-window)))))
      (xref--show-xrefs xrefs nil))))

(defun lsp-metals--echo-command (workspace command)
  "A client command that should be forwarded back to the Metals server."
  (with-lsp-workspace workspace
    (lsp-send-execute-command command)))

(defun lsp-metals--publish-decorations (workspace params)
  "Handle the metals/publishDecorations extension notification."
  (with-lsp-workspace workspace
    (let* ((file (lsp--uri-to-path (ht-get params "uri")))
            (buffer (lsp--buffer-for-file file)))
      (when buffer
        (with-current-buffer buffer
          (lsp--remove-overlays 'metals-decoration)
          (mapc #'lsp-metals--make-overlay (ht-get params "options")))))))

(defun lsp-metals--make-overlay (decoration)
  "Create overlay from metals decoration."
  (let* ((region (lsp--range-to-region (ht-get decoration "range")))
          (content (ht-get (ht-get (ht-get decoration "renderOptions") "after") "contentText"))
          (hover (ht-get (ht-get decoration "hoverMessage") "value"))
          (ov (make-overlay (car region) (cdr region) nil t t)))
    (overlay-put ov 'after-string (propertize content 'cursor t 'font-lock-face 'font-lock-comment-face))
    (overlay-put ov 'help-echo hover)
    (overlay-put ov 'metals-decoration t)))

(defun lsp-metals--logs-toggle (_workspace)
  "Focus or remove focus on the output logs reported by the
server via `window/logMessage'."
  (switch-to-buffer (get-buffer-create "*lsp-log*")))

(defun lsp-metals--diagnostics-focus (_workspace)
  "Focus on the window that lists all published diagnostics."
  (defvar flymake-mode)
  (defvar flycheck-mode)
  (cond ((fboundp 'flymake-show-diagnostics-buffer) (flymake-show-diagnostics-buffer))
        ((and flymake-mode (fboundp 'flymake-show-diagnostics-buffer)) (flymake-show-diagnostics-buffer))
        ((and flycheck-mode (fboundp 'flycheck-list-errors)) (flycheck-list-errors))))

(defun lsp-metals--execute-client-command (workspace params)
  "Handle the metals/executeClientCommand extension notification."
  (when-let ((command (pcase (ht-get params "command")
                        (`"metals-doctor-run" #'lsp-metals--doctor-run)
                        (`"metals-doctor-reload" #'lsp-metals--doctor-reload)
                        (`"metals-logs-toggle" #'lsp-metals--logs-toggle)
                        (`"metals-diagnostics-focus" #'lsp-metals--diagnostics-focus)
                        (`"metals-goto-location" #'lsp-metals--goto-location)
                        (`"metals-echo-command" #'lsp-metals--echo-command)
                        (`"metals-model-refresh" #'lsp-metals--model-refresh)
                        (c (ignore (lsp-warn "Unknown metals client command: %s" c))))))
    (apply command (append (list workspace) (ht-get params "arguments") nil))))

(defvar lsp-metals--current-buffer nil
  "Current buffer used to send `metals/didFocusTextDocument' notification.")

(defun lsp-metals--did-focus ()
  "Send `metals/didFocusTextDocument' when current buffer changes."
  (unless (eq lsp-metals--current-buffer (current-buffer))
    (setq lsp-metals--current-buffer (current-buffer))
    (lsp-notify "metals/didFocusTextDocument" (lsp--buffer-uri))))

(declare-function dap-debug "ext:dap-mode" (conf))
(declare-function dap-register-debug-provider "ext:dap-mode" (name conf))

(defun lsp-metals--debug-start (no-debug params)
  (dap-register-debug-provider "scala" #'identity)
  (-let [session-params (lsp-send-execute-command
                         "debug-adapter-start"
                         (gethash "arguments" params))]
    (dap-debug
     (list :debugServer (-> (gethash "uri" session-params)
                            (split-string ":")
                            cl-third
                            string-to-number)
           :type "scala"
           :name (gethash "name" session-params)
           :host "localhost"
           :request "launch"
           :noDebug no-debug))))

(defun lsp-metals--model-refresh (workspace)
  (->> workspace
       (lsp--workspace-buffers)
       (mapc (lambda (buffer)
               (with-current-buffer buffer
                 (when (bound-and-true-p lsp-lens-mode)
                   (lsp--lens-schedule-refresh t)))))))

(defun lsp-metals--status-string-keymap (workspace command)
  "Keymap for `metals/status' notification."
  (when command
    (-doto (make-sparse-keymap)
      (define-key [mode-line mouse-1]
        (lambda ()
          (interactive)
          (lsp-metals--execute-client-command workspace (ht ("command" command))))))))

(defun lsp-metals--status-string (workspace params)
  "Handle `metals/status' notification."
  (-let (((&hash "text" "hide" "tooltip" "command") params))
    (if (or hide (s-blank-str? text))
        (lsp-workspace-status nil workspace)
      (lsp-workspace-status (propertize text
                              'help-echo tooltip
                              'local-map (lsp-metals--status-string-keymap workspace command))
        workspace))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-metals--server-command)
                  :major-modes '(scala-mode)
                  :priority -1
                  :custom-capabilities `((experimental (decorationProvider . t)
                                                       (debuggingProvider . ,(fboundp 'dap-mode))
                                                       (didFocusProvider . t)
                                                       (executeClientCommandProvider . t)
                                                       (doctorProvider . "html")
                                                       (statusBarProvider . "on")))
                  :notification-handlers (ht ("metals/executeClientCommand" #'lsp-metals--execute-client-command)
                                             ("metals/publishDecorations" #'lsp-metals--publish-decorations)
                                             ("metals/treeViewDidChange" #'ignore)
                                             ("metals-model-refresh" #'lsp-metals--model-refresh)
                                             ("metals/status" #'lsp-metals--status-string))
                  :action-handlers (ht ("metals-debug-session-start" (-partial #'lsp-metals--debug-start :json-false))
                                       ("metals-run-session-start" (-partial #'lsp-metals--debug-start t)))
                  :server-id 'metals
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "metals"))))
                  :after-open-fn (lambda ()
                                   (add-hook 'lsp-on-idle-hook #'lsp-metals--did-focus nil t))
                  :completion-in-comments? t))

(provide 'lsp-metals)
;;; lsp-metals.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
