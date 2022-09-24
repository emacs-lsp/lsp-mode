;;; lsp-fsharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
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

;; lsp-fsharp client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-fsharp nil
  "LSP support for the F# Programming Language, using the FsharpAutoComplete server."
  :link '(url-link "https://github.com/fsharp/FsAutoComplete")
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-install-dir (f-join lsp-server-install-dir "fsautocomplete/")
  "Install directory for fsautocomplete server.
The slash is expected at the end."
  :group 'lsp-fsharp
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-args nil
  "Extra arguments for the F# language server."
  :type '(repeat string)
  :group 'lsp-fsharp
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-keywords-autocomplete t
  "Provides keywords in autocomplete list."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-external-autocomplete nil
  "Provides autocompletion for symbols from not opened namespaces/modules;
inserts open on accept."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-linter t
  "Enables FSharpLint integration, provides additional warnings and code
action fixes."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-union-case-stub-generation t
  "Enables a code action to generate pattern matching cases."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-union-case-stub-generation-body "failwith \"Not Implemented\""
  "Defines dummy body used by pattern matching generator."
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-record-stub-generation t
  "Enables code action to generate record stub."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-record-stub-generation-body "failwith \"Not Implemented\""
  "Defines dummy body used by record stub generator."
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation t
  "Enables code action to generate an interface stub."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation-object-identifier "this"
  "Defines object identifier used by interface stub generator,
e.g. `this' or `self'."
  :group 'lsp-fsharp
  :type 'string
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation-method-body "failwith \"Not Implemented\""
  "Defines dummy body used by interface stub generator."
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-unused-opens-analyzer t
  "Enables unused open detection."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-unused-declarations-analyzer t
  "Enables unused symbol detection."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-simplify-name-analyzer nil
  "Enables simplify name analyzer and remove redundant qualifier quick fix."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-resolve-namespaces t
  "Enables resolve namespace quick fix; adds `open' if symbol is from not yet
opened module/namespace."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-enable-reference-code-lens t
  "Enables reference count code lenses.
It is recommended to disable if `--background-service-enabled' is not used."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-auto-workspace-init nil
  "Enable automatic workspace initialization.
Do note that this can cause unexpected or challenging behaviors, as solutions
with test projects are not autoloaded by FSharpAutoComplete."
  :group 'lsp-fsharp
  :type 'boolean
  :risky t)

(defcustom lsp-fsharp-generate-binlog nil
  "Generate a binlog for debugging project cracking."
  :group 'lsp-fsharp
  :type 'boolean
  :package-version '(lsp-mode . "8.0.1"))

(defun lsp-fsharp--fsac-install (_client callback error-callback update?)
  "Install/update fsautocomplete language server using `dotnet tool'.
Will invoke CALLBACK or ERROR-CALLBACK based on result. Will update if
UPDATE? is t."
  (lsp-async-start-process
   callback
   error-callback
   "dotnet" "tool" (if update? "update" "install") "-g" "fsautocomplete"))

(defcustom lsp-fsharp-use-dotnet-tool-for-fsac t
  "Run FsAutoComplete as a dotnet tool.

The binary will be invoked via \"dotnet fsautocomplete\" in the
project's root directory, which will run a project-local tool if
available, else the globally installed tool."
  :group 'lsp-fsharp
  :type 'boolean
  :risky t)

(defun lsp-fsharp--fsac-cmd ()
  "The location of fsautocomplete executable."
  (or (expand-file-name "fsautocomplete" lsp-fsharp-server-install-dir)
      (executable-find "fsautocomplete")
      (f-join (or (getenv "USERPROFILE") (getenv "HOME"))
              ".dotnet" "tools" "fsautocomplete")))

(defun lsp-fsharp--make-launch-cmd ()
  "Build the command required to launch fsautocomplete."

  ;; emacs-28.1 on macOS has an issue
  ;; that it launches processes using posix_spawn but does not reset sigmask properly
  ;; thus causing dotnet runtime to lockup awaiting a SIGCHLD signal that never comes
  ;; from subprocesses that quit
  ;;
  ;; as a workaround we will wrap fsautocomplete invocation in "/bin/ksh -c" (on macos)
  ;; so it launches with proper sigmask
  ;;
  ;; see https://lists.gnu.org/archive/html/emacs-devel/2022-02/msg00461.html
  ;; --
  ;; we also try to resolve full path to fsautocomplete using `executable-find' as
  ;; our `startup-wrapper' may use $PATH to interpret the location of fsautocomplete
  ;; and we want to actually use `exec-path' here

  (let ((startup-wrapper (cond ((and (eq 'darwin system-type)
                                     (version= "28.1" emacs-version))
                                (list "/bin/ksh" "-c"))

                               (t nil)))
        (fsautocomplete-exec (lsp-fsharp--fsac-cmd)))
    (append startup-wrapper
            (list fsautocomplete-exec "--background-service-enabled")
            lsp-fsharp-server-args)))

(defun lsp-fsharp--test-fsautocomplete-present ()
  "Return non-nil if dotnet tool fsautocomplete is installed globally."
  (if lsp-fsharp-use-dotnet-tool-for-fsac
      (string-match-p "fsautocomplete"
                      (shell-command-to-string "dotnet tool list -g"))
    (f-exists? (lsp-fsharp--fsac-cmd))))

(defun lsp-fsharp--project-list ()
  "Get the list of files we need to send to fsharp/workspaceLoad."
  (let* ((response (lsp-request "fsharp/workspacePeek"
                                `(:directory ,(lsp-workspace-root)
                                             :deep 10
                                             :excludedDirs ["paket-files" ".git" "packages" "node_modules"])))
         (data (lsp--read-json (lsp-get response :content)))
         (found (-> data (lsp-get :Data) (lsp-get :Found)))
         (directory (seq-find (lambda (d) (equal "directory" (lsp-get d :Type))) found)))
    (-> directory (lsp-get :Data) (lsp-get :Fsprojs))))

;;;###autoload
(defun lsp-fsharp--workspace-load (projects)
  "Load all of the provided PROJECTS."
  (lsp-request-async "fsharp/workspaceLoad"
                     `(:textDocuments ,(vconcat [] (mapcar (lambda (p) `(:uri ,p)) projects)))
                     (lambda (_)
                       (lsp--info "Workspace Loaded!"))))

(defvar lsp-fsharp--default-init-options  (list)
  "Default init options to be passed to FSharpAutoComplete,
  updated conditionally by `lsp-fsharp--make-init-options'.")

(defun lsp-fsharp--make-init-options ()
  "Init options for F#."
  (-let [opts lsp-fsharp--default-init-options]
    (if lsp-fsharp-auto-workspace-init
        (push '(:AutomaticWorkspaceInit . t) opts)
      opts)))

(lsp-register-custom-settings
 `(("FSharp.KeywordsAutocomplete" lsp-fsharp-keywords-autocomplete t)
   ("FSharp.ExternalAutocomplete" lsp-fsharp-external-autocomplete t)
   ("FSharp.Linter" lsp-fsharp-linter t)
   ("FSharp.UnionCaseStubGeneration" lsp-fsharp-union-case-stub-generation t)
   ("FSharp.UnionCaseStubGenerationBody" lsp-fsharp-union-case-stub-generation-body)
   ("FSharp.RecordStubGeneration" lsp-fsharp-record-stub-generation t)
   ("FSharp.RecordStubGenerationBody" lsp-fsharp-record-stub-generation-body)
   ("FSharp.InterfaceStubGeneration" lsp-fsharp-interface-stub-generation t)
   ("FSharp.InterfaceStubGenerationObjectIdentifier" lsp-fsharp-interface-stub-generation-object-identifier)
   ("FSharp.InterfaceStubGenerationMethodBody" lsp-fsharp-interface-stub-generation-method-body)
   ("FSharp.UnusedOpensAnalyzer" lsp-fsharp-unused-opens-analyzer t)
   ("FSharp.UnusedDeclarationsAnalyzer" lsp-fsharp-unused-declarations-analyzer t)
   ("FSharp.SimplifyNameAnalyzer" lsp-fsharp-simplify-name-analyzer t)
   ("FSharp.ResolveNamespaces" lsp-fsharp-resolve-namespaces t)
   ("FSharp.EnableReferenceCodeLens" lsp-fsharp-enable-reference-code-lens t)
   ("FSharp.GenerateBinlog" lsp-fsharp-generate-binlog t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-fsharp--make-launch-cmd
                                   #'lsp-fsharp--test-fsautocomplete-present)
                  :major-modes '(fsharp-mode)
                  :notification-handlers (ht ("fsharp/notifyCancel" #'ignore)
                                             ("fsharp/notifyWorkspace" #'ignore)
                                             ("fsharp/fileParsed" #'ignore)
                                             ("fsharp/notifyWorkspacePeek" #'ignore)
                                             ("fsharp/documentAnalyzed" #'ignore)
                                             ("fsharp/testDetected" #'ignore))
                  :initialization-options 'lsp-fsharp--make-init-options
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      ;; Something needs to be calling lsp--set-configuration
                                      (progn
                                        (lsp--set-configuration
                                         (lsp-configuration-section "fsharp"))
                                        (lsp-fsharp--workspace-load
                                         (lsp-fsharp--project-list)))))
                  :after-open-fn ;; workaround https://github.com/fsharp/FsAutoComplete/issues/833
                  (lambda ()
                    (setq-local lsp-default-create-error-handler-fn
                                (lambda (method)
                                  (lambda (error)
                                    (when
                                        (not
                                         (seq-find (lambda (s)
                                                     (string= s (lsp-get error :message)))
                                                   '("Index was outside the bounds of the array."
                                                     "No symbol information found"
                                                     "No ident at this location")))
                                      (lsp--warn
                                       "%s"
                                       (or (lsp--error-string error)
                                           (format "%s Request has failed" method))))))))
                  :server-id 'fsac
                  :download-server-fn #'lsp-fsharp--fsac-install))

(lsp-consistency-check lsp-fsharp)

(provide 'lsp-fsharp)
;;; lsp-fsharp.el ends here
