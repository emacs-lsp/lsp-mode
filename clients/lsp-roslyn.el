;;; lsp-roslyn.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ruin0x11

;; Author: Ruin0x11 <ipickering2@gmail.com>
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

;; C# client using the Roslyn language server

;;; Code:

(require 'lsp-mode)
(require 'cl)

(defgroup lsp-roslyn nil
  "LSP support for the C# programming language, using the Roslyn language server."
  :link '(url-link "https://github.com/dotnet/roslyn/tree/main/src/Features/LanguageServer")
  :group 'lsp-mode
  :package-version '(lsp-mode . "8.0.1"))

(defvar lsp-roslyn--stdpipe-path (expand-file-name
                                  "lsp-roslyn-stdpipe.ps1"
                                  (file-name-directory (file-truename load-file-name)))
  "Path to the 'stdpipe' script.
On Windows, this script is used as a proxy for the language server's named pipe.
Unused on other platforms.")

(defcustom lsp-roslyn-server-log-level "Information"
  "Log level for the Roslyn language server."
  :type 'string
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-log-directory (concat (temporary-file-directory) (file-name-as-directory "lsp-roslyn"))
  "Log directory for the Roslyn language server."
  :type 'string
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-extra-args '()
  "Extra arguments for the Roslyn language server."
  :type '(repeat string)
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-dotnet-executable "dotnet"
  "Dotnet executable to use with the Roslyn language server."
  :type 'string
  :group 'lsp-roslyn)

;; TODO replace with server install
(defvar lsp-roslyn--TEMP-server-path "G:\\build\\roslyn\\artifacts\\bin\\Microsoft.CodeAnalysis.LanguageServer\\Debug\\net7.0\\Microsoft.CodeAnalysis.LanguageServer.dll")

(defvar lsp-roslyn--pipe-name nil)

(defun lsp-roslyn--parent-process-filter (process output)
  "Parses the named pipe's name that the Roslyn server process prints on stdout."
  (let* ((data (json-parse-string output :object-type 'plist))
         (pipe (plist-get data :pipeName)))
    (when pipe
      (string-match "\\([a-z0-9]+\\)$" pipe)
      (let ((pipe-name (match-string 1 pipe)))
        (message "%s %s" pipe pipe-name)
        (setq lsp-roslyn--pipe-name pipe-name)))))

(defun lsp-roslyn--connect (filter sentinel name environment-fn workspace)
  "Creates a connection to the Roslyn language server's named pipe.

First creates an instance of the language server process, then
creates another process connecting to the named pipe it specifies."
  (setq lsp-roslyn--pipe-name nil)
  (let* ((parent-process-name (format "%s-process" name))
         (parent-stderr-buf (format "*%s::stderr*" parent-process-name))
         (command-process (make-process
                          :name parent-process-name
                          :buffer (generate-new-buffer-name parent-process-name)
                          :coding 'no-conversion
                          :filter 'lsp-roslyn--parent-process-filter
                          :sentinel sentinel
                          :stderr parent-stderr-buf
                          :command (append
                                    (list lsp-roslyn-server-dotnet-executable
                                          lsp-roslyn--TEMP-server-path
                                          (format "--logLevel=%s" lsp-roslyn-server-log-level)
                                          (format "--extensionLogDirectory=%s" lsp-roslyn-server-log-directory))
                                    lsp-roslyn-server-extra-args)
                          :noquery t)))
    (accept-process-output command-process 5) ; wait for JSON with pipe name to print on stdout, like {"pipeName":"\\\\.\\pipe\\d1b72351"}
    (when (not lsp-roslyn--pipe-name)
      (error "Failed to receieve pipe name from Roslyn server process"))
    (let* ((process-environment
            (lsp--compute-process-environment environment-fn))
           (process-name (generate-new-buffer-name name))
           (stderr-buf (format "*%s::stderr*" process-name))
           (default-directory (lsp--default-directory-for-connection))
           (communication-process (make-process
                                   :name process-name
                                   :connection-type 'pipe
                                   :buffer (format "*%s*" process-name)
                                   :coding 'no-conversion
                                   :filter filter
                                   :sentinel sentinel
                                   :stderr stderr-buf
                                   :noquery t
                                   :command (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" lsp-roslyn--stdpipe-path "." lsp-roslyn--pipe-name))))
      (message "go %s" lsp-roslyn--pipe-name)
      (with-current-buffer (get-buffer stderr-buf)
        ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
        (special-mode))
      (with-current-buffer (get-buffer parent-stderr-buf)
        (special-mode))
      (set-process-query-on-exit-flag command-process nil)
      (set-process-query-on-exit-flag communication-process nil)
      (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)
      (cons communication-process communication-process))))

(defun lsp-roslyn--make-connection ()
  (list :connect (lambda (f s n e w) (lsp-roslyn--connect f s n e w))
        :test? (lambda () t)))

(defun lsp-roslyn--uri-to-path (uri)
  "Convert a URI to a file path, without unhexifying."
  (let* ((url (url-generic-parse-url uri))
         (type (url-type url))
         (target (url-target url))
         (file
          (concat (decode-coding-string (url-filename url)
                                        (or locale-coding-system 'utf-8))
                  (when (and target
                             (not (s-match
                                   (rx "#" (group (1+ num)) (or "," "#")
                                       (group (1+ num))
                                       string-end)
                                   uri)))
                    (concat "#" target))))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          uri)
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))
    (->> file-name
         (concat (-some #'lsp--workspace-host-root (lsp-workspaces)))
         (lsp-remap-path-if-needed))))

(defun lsp-roslyn--path-to-uri (path)
  "Convert PATH to a URI, without hexifying."
  (url-unhex-string (lsp--path-to-uri-1 path)))

(lsp-defun lsp-roslyn--log-message (workspace params)
  (let ((type (gethash "type" params))
        (mes (gethash "message" params)))
    (case type
      (1 (lsp--error "%s" mes))   ; Error
      (2 (lsp--warn "%s" mes))    ; Warning
      (3 (lsp--info "%s" mes))    ; Info
      (t (lsp--info "%s" mes))))) ; Log

(lsp-defun lsp-roslyn--on-project-initialization-complete (workspace params)
  (lsp--info "%s: Project initialized successfully."
             (lsp--workspace-print workspace)))

(defun lsp-roslyn--find-files-in-parent-directories (directory regex &optional result)
  "Search DIRECTORY for files matching REGEX and return their full paths if found."
  (let* ((parent-dir (file-truename (concat (file-name-directory directory) "../")))
        (found (directory-files directory 't regex))
        (result (append (or result '()) found)))
    (if (and (not (string= (file-truename directory) parent-dir))
             (< (length parent-dir) (length (file-truename directory))))
        (lsp-roslyn--find-files-in-parent-directories parent-dir regex result)
      result)))

(defun lsp-roslyn--pick-solution-file-interactively (solution-files)
  (completing-read "Solution file for this workspace: " solution-files nil t))

(defun lsp-roslyn--find-solution-file ()
  (let ((solutions (lsp-roslyn--find-files-in-parent-directories
                    (file-name-directory (buffer-file-name))
                    (rx (* any) ".sln" eos))))
    (cond
     ((not solutions) nil)
     ((eq (length solutions) 1) (cl-first solutions))
     (t (lsp-roslyn--pick-solution-file-interactively solutions)))))

(defun lsp-roslyn-open-solution-file ()
  "Chooses the solution file to associate with the Roslyn language server."
  (interactive)
  (let ((solution-file (lsp-roslyn--find-solution-file)))
    ;; (progn (message "No .sln files were found for this workspace.") nil)
    (if solution-file
      (lsp-notify "solution/open" (list :solution (lsp--path-to-uri solution-file)))
      (message "No solution file selected for this workspace."))))

(defun lsp-roslyn--on-initialized (workspace)
  "Handler for Roslyn server initialization."
  (lsp-roslyn-open-solution-file))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-roslyn--make-connection)
                  :priority 0
                  :server-id 'csharp-roslyn
                  :activation-fn (lsp-activate-on "csharp")
                  :notification-handlers (ht ("window/logMessage" 'lsp-roslyn--log-message)
                                             ("workspace/projectInitializationComplete" 'lsp-roslyn--on-project-initialization-complete))

                  ;; Parts of the Roslyn server do not strictly follow the LSP spec.
                  ;; These two functions are the same as lsp-mode's except they do not
                  ;; (un)hexify URIs.
                  :path->uri-fn 'lsp-roslyn--path-to-uri
                  :uri->path-fn 'lsp-roslyn--uri-to-path

                  ;; :before-file-open-fn #'lsp-roslyn--before-file-open
                  :initialized-fn #'lsp-roslyn--on-initialized
                  ;; :uri-handlers (ht ("csharp" #'lsp-csharp--cls-metadata-uri-handler))
                  ;; :download-server-fn #'lsp-csharp--cls-download-server)
                  ))

(provide 'lsp-roslyn)
;;; lsp-roslyn.el ends here
