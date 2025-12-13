;;; lsp-csharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jostein Kjønigsen, Saulius Menkevicius

;; Author: Saulius Menkevicius <saulius.menkevicius@fastmail.com>
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

;; lsp-csharp client

;;; Code:

(require 'lsp-mode)
(require 'gnutls)
(require 'f)

(defgroup lsp-csharp nil
  "LSP support for C#, using the Omnisharp Language Server.
Version 1.34.3 minimum is required."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OmniSharp/omnisharp-roslyn"))

(defgroup lsp-csharp-omnisharp nil
  "LSP support for C#, using the Omnisharp Language Server.
Version 1.34.3 minimum is required."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OmniSharp/omnisharp-roslyn")
  :package-version '(lsp-mode . "9.0.0"))

(defconst lsp-csharp--omnisharp-metadata-uri-re
  "^file:///%24metadata%24/Project/\\(.+\\)/Assembly/\\(.+\\)/Symbol/\\(.+\\)\.cs$"
  "Regular expression matching omnisharp's metadata uri.
Group 1 contains the Project name
Group 2 contains the Assembly name
Group 3 contains the Type name")

(defcustom lsp-csharp-server-install-dir
  (f-join lsp-server-install-dir "omnisharp-roslyn/")
  "Installation directory for OmniSharp Roslyn server."
  :group 'lsp-csharp-omnisharp
  :type 'directory)

(defcustom lsp-csharp-server-path
  nil
  "The path to the OmniSharp Roslyn language-server binary.
Set this if you have the binary installed or have it built yourself."
  :group 'lsp-csharp-omnisharp
  :type '(string :tag "Single string value or nil"))

(defcustom lsp-csharp-test-run-buffer-name
  "*lsp-csharp test run*"
  "The name of buffer used for outputting lsp-csharp test run results."
  :group 'lsp-csharp-omnisharp
  :type 'string)

(defcustom lsp-csharp-solution-file
  nil
  "Solution to load when starting the server.
Usually this is to be set in your .dir-locals.el on the project root directory."
  :group 'lsp-csharp-omnisharp
  :type 'string)

(defcustom lsp-csharp-omnisharp-roslyn-download-url
  (concat "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/"
          (cond ((eq system-type 'windows-nt)
                 ; On Windows we're trying to avoid a crash starting 64bit .NET PE binaries in
                 ; Emacs by using x86 version of omnisharp-roslyn on older (<= 26.4) versions
                 ; of Emacs. See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2017-06/msg00893.html"
                 (if (and (string-match "^x86_64-.*" system-configuration)
                          (version<= "26.4" emacs-version))
                     "omnisharp-win-x64.zip"
                   "omnisharp-win-x86.zip"))

                ((eq system-type 'darwin)
                 (if (string-match "aarch64-.*" system-configuration)
                     "omnisharp-osx-arm64-net6.0.zip"
                   "omnisharp-osx-x64-net6.0.zip"))

                ((and (eq system-type 'gnu/linux)
                      (or (eq (string-match "^x86_64" system-configuration) 0)
                          (eq (string-match "^i[3-6]86" system-configuration) 0)))
                 "omnisharp-linux-x64-net6.0.zip")

                (t "omnisharp-mono.zip")))
  "Automatic download url for omnisharp-roslyn."
  :group 'lsp-csharp-omnisharp
  :type 'string)

(defcustom lsp-csharp-omnisharp-roslyn-store-path
  (f-join lsp-csharp-server-install-dir "latest" "omnisharp-roslyn.zip")
  "The path where omnisharp-roslyn .zip archive will be stored."
  :group 'lsp-csharp-omnisharp
  :type 'file)

(defcustom lsp-csharp-omnisharp-roslyn-binary-path
  (f-join lsp-csharp-server-install-dir "latest" (if (eq system-type 'windows-nt)
                                                     "OmniSharp.exe"
                                                   "OmniSharp"))
  "The path where omnisharp-roslyn binary after will be stored."
  :group 'lsp-csharp-omnisharp
  :type 'file)

(defcustom lsp-csharp-omnisharp-roslyn-server-dir
  (f-join lsp-csharp-server-install-dir "latest" "omnisharp-roslyn")
  "The path where omnisharp-roslyn .zip archive will be extracted."
  :group 'lsp-csharp-omnisharp
  :type 'file)


(defcustom lsp-csharp-omnisharp-enable-decompilation-support
  nil
  "Decompile bytecode when browsing method metadata for types in assemblies.
Otherwise only declarations for the methods are visible (the default)."
  :group 'lsp-csharp
  :type 'boolean)

(defcustom lsp-csharp-csharpls-use-dotnet-tool t
  "Whether to use a dotnet tool version of the expected C#
 language server; only available for csharp-ls"
  :group 'lsp-csharp
  :type 'boolean
  :risky t)

(defcustom lsp-csharp-csharpls-use-local-tool nil
  "Whether to use csharp-ls as a global or local dotnet tool.

Note: this variable has no effect if
lsp-csharp-csharpls-use-dotnet-tool is nil."
  :group 'lsp-csharp
  :type 'boolean
  :risky t)

(lsp-dependency
 'omnisharp-roslyn
 `(:download :url lsp-csharp-omnisharp-roslyn-download-url
             :decompress :zip
             :store-path lsp-csharp-omnisharp-roslyn-store-path
             :binary-path lsp-csharp-omnisharp-roslyn-binary-path
             :set-executable? t)
 '(:system "OmniSharp"))

(defun lsp-csharp--omnisharp-download-server (_client callback error-callback _update?)
  "Download zip package for omnisharp-roslyn and install it.
Will invoke CALLBACK on success, ERROR-CALLBACK on error."
  (lsp-package-ensure 'omnisharp-roslyn callback error-callback))

(defun lsp-csharp--language-server-path ()
  "Resolve path to use to start the server."
  (let ((executable-name (if (eq system-type 'windows-nt)
                             "OmniSharp.exe"
                           "OmniSharp")))
    (or (and lsp-csharp-server-path
             (executable-find lsp-csharp-server-path))
        (executable-find executable-name)
        (lsp-package-path 'omnisharp-roslyn))))

(defun lsp-csharp-open-project-file ()
  "Open corresponding project file  (.csproj) for the current file."
  (interactive)
  (-let* ((project-info-req (lsp-make-omnisharp-project-information-request :file-name (buffer-file-name)))
          (project-info (lsp-request "o#/project" project-info-req))
          ((&omnisharp:ProjectInformation :ms-build-project) project-info)
          ((&omnisharp:MsBuildProject :path) ms-build-project))
    (find-file path)))

(defun lsp-csharp--get-buffer-code-elements ()
  "Retrieve code structure by calling into the /v2/codestructure endpoint.
Returns :elements from omnisharp:CodeStructureResponse."
  (-let* ((code-structure (lsp-request "o#/v2/codestructure"
                                       (lsp-make-omnisharp-code-structure-request :file-name (buffer-file-name))))
          ((&omnisharp:CodeStructureResponse :elements) code-structure))
    elements))

(defun lsp-csharp--inspect-code-elements-recursively (fn elements)
  "Invoke FN for every omnisharp:CodeElement found recursively in ELEMENTS."
  (seq-each
   (lambda (el)
     (funcall fn el)
     (-let (((&omnisharp:CodeElement :children) el))
       (lsp-csharp--inspect-code-elements-recursively fn children)))
   elements))

(defun lsp-csharp--collect-code-elements-recursively (predicate elements)
  "Flatten the omnisharp:CodeElement tree in ELEMENTS matching PREDICATE."
  (let ((results nil))
    (lsp-csharp--inspect-code-elements-recursively (lambda (el)
                                                     (when (funcall predicate el)
                                                       (setq results (cons el results))))
                                                   elements)
    results))

(lsp-defun lsp-csharp--l-c-within-range (l c (&omnisharp:Range :start :end))
  "Determine if L (line) and C (column) are within RANGE."
  (-let* (((&omnisharp:Point :line start-l :column start-c) start)
          ((&omnisharp:Point :line end-l :column end-c) end))
    (or (and (= l start-l) (>= c start-c) (or (> end-l start-l) (<= c end-c)))
        (and (> l start-l) (< l end-l))
        (and (= l end-l) (<= c end-c)))))

(defun lsp-csharp--code-element-stack-on-l-c (l c elements)
  "Return omnisharp:CodeElement stack at L (line) and C (column) in ELEMENTS tree."
  (when-let* ((matching-element (seq-find (lambda (el)
                                           (-when-let* (((&omnisharp:CodeElement :ranges) el)
                                                        ((&omnisharp:RangeList :full?) ranges))
                                             (lsp-csharp--l-c-within-range l c full?)))
                                         elements)))
    (-let (((&omnisharp:CodeElement :children) matching-element))
      (cons matching-element (lsp-csharp--code-element-stack-on-l-c l c children)))))

(defun lsp-csharp--code-element-stack-at-point ()
  "Return omnisharp:CodeElement stack at point as a list."
  (let ((pos-line (plist-get (lsp--cur-position) :line))
        (pos-col (plist-get (lsp--cur-position) :character)))
    (lsp-csharp--code-element-stack-on-l-c pos-line
                                           pos-col
                                           (lsp-csharp--get-buffer-code-elements))))

(lsp-defun lsp-csharp--code-element-test-method-p (element)
  "Return test method name and test framework for a given ELEMENT."
  (when element
    (-when-let* (((&omnisharp:CodeElement :properties) element)
                 ((&omnisharp:CodeElementProperties :test-method-name? :test-framework?) properties))
      (list test-method-name? test-framework?))))

(defun lsp-csharp--reset-test-buffer (present-buffer)
  "Create new or reuse an existing test result output buffer.
PRESENT-BUFFER will make the buffer be presented to the user."
  (with-current-buffer (get-buffer-create lsp-csharp-test-run-buffer-name)
    (compilation-mode)
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (when present-buffer
    (display-buffer lsp-csharp-test-run-buffer-name)))

(defun lsp-csharp--start-tests (test-method-framework test-method-names)
  "Run test(s) identified by TEST-METHOD-NAMES using TEST-METHOD-FRAMEWORK."
  (if (and test-method-framework test-method-names)
      (let ((request-message (lsp-make-omnisharp-run-tests-in-class-request
                              :file-name (buffer-file-name)
                              :test-frameworkname test-method-framework
                              :method-names (vconcat test-method-names))))
        (lsp-csharp--reset-test-buffer t)
        (lsp-session-set-metadata "last-test-method-framework" test-method-framework)
        (lsp-session-set-metadata "last-test-method-names" test-method-names)
        (lsp-request-async "o#/v2/runtestsinclass"
                           request-message
                           (-lambda ((&omnisharp:RunTestResponse))
                             (message "lsp-csharp: Test run has started"))))
    (message "lsp-csharp: No test methods to run")))

(defun lsp-csharp--test-message (message)
  "Emit a MESSAGE to lsp-csharp test run buffer."
  (when-let* ((existing-buffer (get-buffer lsp-csharp-test-run-buffer-name))
             (inhibit-read-only t))
    (with-current-buffer existing-buffer
      (save-excursion
        (goto-char (point-max))
        (insert message "\n")))))

(defun lsp-csharp-run-test-at-point ()
  "Start test run at current point (if any)."
  (interactive)
  (let* ((stack (lsp-csharp--code-element-stack-at-point))
         (element-on-point (car (last stack)))
         (test-method (lsp-csharp--code-element-test-method-p element-on-point))
         (test-method-name (car test-method))
         (test-method-framework (car (cdr test-method))))
    (lsp-csharp--start-tests test-method-framework (list test-method-name))))

(defun lsp-csharp-run-all-tests-in-buffer ()
  "Run all test methods in the current buffer."
  (interactive)
  (let* ((elements (lsp-csharp--get-buffer-code-elements))
         (test-methods (lsp-csharp--collect-code-elements-recursively 'lsp-csharp--code-element-test-method-p elements))
         (test-method-framework (car (cdr (lsp-csharp--code-element-test-method-p (car test-methods)))))
         (test-method-names (mapcar (lambda (method)
                                      (car (lsp-csharp--code-element-test-method-p method)))
                                    test-methods)))
    (lsp-csharp--start-tests test-method-framework test-method-names)))

(defun lsp-csharp-run-test-in-buffer ()
  "Run selected test in current buffer."
  (interactive)
  (when-let* ((elements (lsp-csharp--get-buffer-code-elements))
              (test-methods (lsp-csharp--collect-code-elements-recursively 'lsp-csharp--code-element-test-method-p elements))
              (test-method-framework (car (cdr (lsp-csharp--code-element-test-method-p (car test-methods)))))
              (test-method-names (mapcar (lambda (method)
                                           (car (lsp-csharp--code-element-test-method-p method)))
                                         test-methods))
              (selected-test-method-name (lsp--completing-read "Select test:" test-method-names 'identity)))
    (lsp-csharp--start-tests test-method-framework (list selected-test-method-name))))

(defun lsp-csharp-run-last-tests ()
  "Re-run test(s) that were run last time."
  (interactive)
  (if-let* ((last-test-method-framework (lsp-session-get-metadata "last-test-method-framework"))
           (last-test-method-names (lsp-session-get-metadata "last-test-method-names")))
      (lsp-csharp--start-tests last-test-method-framework last-test-method-names)
    (message "lsp-csharp: No test method(s) found to be ran previously on this workspace")))

(lsp-defun lsp-csharp--handle-os-error (_workspace (&omnisharp:ErrorMessage :file-name :text))
  "Handle the `o#/error' (interop) notification displaying a message."
  (lsp-warn "%s: %s" file-name text))

(lsp-defun lsp-csharp--handle-os-testmessage (_workspace (&omnisharp:TestMessageEvent :message))
  "Handle the `o#/testmessage and display test message on test output buffer."
  (lsp-csharp--test-message message))

(lsp-defun lsp-csharp--handle-os-testcompleted (_workspace (&omnisharp:DotNetTestResult
                                                            :method-name
                                                            :outcome
                                                            :error-message
                                                            :error-stack-trace
                                                            :standard-output
                                                            :standard-error))
  "Handle the `o#/testcompleted' message from the server.

Will display the results of the test on the lsp-csharp test output buffer."
  (let ((passed (string-equal "passed" outcome)))
    (lsp-csharp--test-message
     (format "[%s] %s "
             (propertize (upcase outcome) 'font-lock-face (if passed 'success 'error))
             method-name))

    (unless passed
      (lsp-csharp--test-message error-message)

      (when error-stack-trace
        (lsp-csharp--test-message error-stack-trace))

      (unless (seq-empty-p standard-output)
        (lsp-csharp--test-message "STANDARD OUTPUT:")
        (seq-doseq (stdout-line standard-output)
          (lsp-csharp--test-message stdout-line)))

      (unless (seq-empty-p standard-error)
        (lsp-csharp--test-message "STANDARD ERROR:")
        (seq-doseq (stderr-line standard-error)
          (lsp-csharp--test-message stderr-line))))))

(lsp-defun lsp-csharp--action-client-find-references ((&Command :arguments?))
  "Read first argument from ACTION as Location and display xrefs for that location
using the `textDocument/references' request."
  (-if-let* (((&Location :uri :range) (lsp-seq-first arguments?))
             ((&Range :start range-start) range)
             (find-refs-params (append (lsp--text-document-position-params (list :uri uri) range-start)
                                       (list :context (list :includeDeclaration json-false))))
             (locations-found (lsp-request "textDocument/references" find-refs-params)))
      (lsp-show-xrefs (lsp--locations-to-xref-items locations-found) nil t)
    (message "No references found")))

(defun lsp-csharp--omnisharp-path->qualified-name (path)
  "Convert PATH to qualified-namespace-like name."
  (replace-regexp-in-string
   (regexp-quote "/")
   "."
   path))

(defun lsp-csharp--omnisharp-metadata-uri-handler (uri)
  "Handle `file:/(metadata)' URI from omnisharp-roslyn server.

The URI is parsed and then `o#/metadata' request is issued to retrieve
metadata from the server. A cache file is created on project root dir that
stores this metadata and filename is returned so lsp-mode can display this file."
  (string-match lsp-csharp--omnisharp-metadata-uri-re uri)
  (-when-let* ((project-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 1 uri))))
               (assembly-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 2 uri))))
               (type-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 3 uri))))
               (metadata-req (lsp-make-omnisharp-metadata-request :project-name project-name
                                                                  :assembly-name assembly-name
                                                                  :type-name type-name))
               (metadata (lsp-request "o#/metadata" metadata-req))
               ((&omnisharp:MetadataResponse :source-name :source) metadata)
               (filename (f-join ".cache"
                                 "lsp-csharp"
                                 "metadata"
                                 "Project" project-name
                                 "Assembly" assembly-name
                                 "Symbol" (concat type-name ".cs")))
               (file-location (expand-file-name filename (lsp--suggest-project-root)))
               (metadata-file-location (concat file-location ".metadata-uri"))
               (path (f-dirname file-location)))

    (unless (find-buffer-visiting file-location)
      (unless (file-directory-p path)
        (make-directory path t))

      (with-temp-file metadata-file-location
        (insert uri))

      (with-temp-file file-location
        (insert source)))

    file-location))

(defun lsp-csharp--omnisharp-uri->path-fn (uri)
  "Custom implementation of lsp--uri-to-path function to glue omnisharp's
metadata uri."
  (if (string-match-p lsp-csharp--omnisharp-metadata-uri-re uri)
      (lsp-csharp--omnisharp-metadata-uri-handler uri)
    (lsp--uri-to-path-1 uri)))

(defun lsp-csharp--omnisharp-environment-fn ()
  "Build environment structure for current values of lsp-csharp customizables.
See https://github.com/OmniSharp/omnisharp-roslyn/wiki/Configuration-Options"
  `(("OMNISHARP_RoslynExtensionsOptions:enableDecompilationSupport" . ,(if lsp-csharp-omnisharp-enable-decompilation-support "true" "false"))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   #'(lambda ()
                       (append
                        (list (lsp-csharp--language-server-path) "-lsp")
                        (when lsp-csharp-solution-file
                          (list "-s" (expand-file-name lsp-csharp-solution-file)))))
                   #'(lambda ()
                       (when-let* ((binary (lsp-csharp--language-server-path)))
                         (f-exists? binary))))
                  :activation-fn (lsp-activate-on "csharp")
                  :server-id 'omnisharp
                  :priority -1
                  :uri->path-fn #'lsp-csharp--omnisharp-uri->path-fn
                  :environment-fn #'lsp-csharp--omnisharp-environment-fn
                  :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                  :notification-handlers (ht ("o#/projectadded" 'ignore)
                                             ("o#/projectchanged" 'ignore)
                                             ("o#/projectremoved" 'ignore)
                                             ("o#/packagerestorestarted" 'ignore)
                                             ("o#/msbuildprojectdiagnostics" 'ignore)
                                             ("o#/packagerestorefinished" 'ignore)
                                             ("o#/unresolveddependencies" 'ignore)
                                             ("o#/error" 'lsp-csharp--handle-os-error)
                                             ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                             ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                             ("o#/projectconfiguration" 'ignore)
                                             ("o#/projectdiagnosticstatus" 'ignore)
                                             ("o#/backgrounddiagnosticstatus" 'ignore))
                  :download-server-fn #'lsp-csharp--omnisharp-download-server))

;;
;; Alternative "csharp-ls" language server support
;; see https://github.com/razzmatazz/csharp-language-server
;;
(lsp-defun lsp-csharp--cls-metadata-uri-handler (uri)
  "Handle `csharp:/(metadata)' uri from csharp-ls server.

`csharp/metadata' request is issued to retrieve metadata from the server.
A cache file is created on project root dir that stores this metadata and
filename is returned so lsp-mode can display this file."

  (-when-let* ((metadata-req (lsp-make-csharp-ls-c-sharp-metadata
                              :text-document (lsp-make-text-document-identifier :uri uri)))
               (metadata (lsp-request "csharp/metadata" metadata-req))
               ((&csharp-ls:CSharpMetadataResponse :project-name
                                                   :assembly-name
                                                   :symbol-name
                                                   :source) metadata)
               (filename (f-join ".cache"
                                 "lsp-csharp"
                                 "metadata"
                                 "projects" project-name
                                 "assemblies" assembly-name
                                 (concat symbol-name ".cs")))
               (file-location (expand-file-name filename (lsp-workspace-root)))
               (metadata-file-location (concat file-location ".metadata-uri"))
               (path (f-dirname file-location))
               (coding-system-for-write 'utf-8-unix))

    (unless (file-exists-p file-location)
      (unless (file-directory-p path)
        (make-directory path t))

      (with-temp-file metadata-file-location
        (insert uri))

      (with-temp-file file-location
        (insert source)))

    file-location))

(defun lsp-csharp--cls-before-file-open (_workspace)
  "Set `lsp-buffer-uri' variable after C# file is open from *.metadata-uri file."

  (let ((metadata-file-name (concat buffer-file-name ".metadata-uri")))
    (setq-local lsp-buffer-uri
                (when (file-exists-p metadata-file-name)
                  (with-temp-buffer (insert-file-contents metadata-file-name)
                                    (buffer-string))))))

(defun lsp-csharp--cls-find-executable ()
  (or (when lsp-csharp-csharpls-use-dotnet-tool
        (if lsp-csharp-csharpls-use-local-tool
            (list "dotnet" "tool" "run" "csharp-ls")
          (list "csharp-ls")))
      (executable-find "csharp-ls")      
      (f-join (or (getenv "USERPROFILE") (getenv "HOME"))
              ".dotnet" "tools" "csharp-ls")))

(defun lsp-csharp--cls-make-launch-cmd ()
  "Return command line to invoke csharp-ls."

  ;; emacs-28.1 on macOS has an issue
  ;; that it launches processes using posix_spawn but does not reset sigmask properly
  ;; thus causing dotnet runtime to lockup awaiting a SIGCHLD signal that never comes
  ;; from subprocesses that quit
  ;;
  ;; as a workaround we will wrap csharp-ls invocation in "/bin/ksh -c" on macos
  ;; so it launches with proper sigmask
  ;;
  ;; see https://lists.gnu.org/archive/html/emacs-devel/2022-02/msg00461.html

  (let ((startup-wrapper (cond ((and (eq 'darwin system-type)
                                     (version= "28.1" emacs-version))
                                (list "/bin/ksh" "-c"))

                               (t nil)))

        (csharp-ls-exec (lsp-csharp--cls-find-executable))

        (solution-file-params (when lsp-csharp-solution-file
                                (list "-s" lsp-csharp-solution-file))))
    (append startup-wrapper
            (if (listp csharp-ls-exec)
                csharp-ls-exec
              (list csharp-ls-exec))
            solution-file-params)))

(defun lsp-csharp--cls-test-csharp-ls-present ()
  "Return non-nil if dotnet tool csharp-ls is installed as a dotnet tool."
  (string-match-p "csharp-ls"
                  (shell-command-to-string
                   (if lsp-csharp-csharpls-use-local-tool
                       "dotnet tool list"
                     "dotnet tool list -g"))))

(defun lsp-csharp--cls-download-server (_client callback error-callback update?)
  "Install/update csharp-ls language server using `dotnet tool'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t"
  (lsp-async-start-process
   callback
   error-callback
   "dotnet" "tool" (if update? "update" "install") (if lsp-csharp-csharpls-use-local-tool "" "-g") "csharp-ls"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-csharp--cls-make-launch-cmd)
                  :priority -2
                  :server-id 'csharp-ls
                  :activation-fn (lsp-activate-on "csharp")
                  :before-file-open-fn #'lsp-csharp--cls-before-file-open
                  :uri-handlers (ht ("csharp" #'lsp-csharp--cls-metadata-uri-handler))
                  :download-server-fn #'lsp-csharp--cls-download-server))

(lsp-consistency-check lsp-csharp)

(provide 'lsp-csharp)
;;; lsp-csharp.el ends here
