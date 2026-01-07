;;; lsp-roslyn.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ruin0x11
;; Copyright (C) 2023-2026 lsp-mode maintainers

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

(defgroup lsp-roslyn nil
  "LSP support for the C# programming language, using the Roslyn language server."
  :link '(url-link "https://github.com/dotnet/roslyn/tree/main/src/LanguageServer")
  :group 'lsp-mode
  :package-version '(lsp-mode . "8.0.0"))

(defconst lsp-roslyn--stdpipe-path (expand-file-name
                                    "lsp-roslyn-stdpipe.ps1"
                                    (file-name-directory (locate-library "lsp-roslyn")))
  "Path to the `stdpipe' script.
On Windows, this script is used as a proxy for the language server's named pipe.
Unused on other platforms.")

(defcustom lsp-roslyn-install-path (expand-file-name "roslyn" lsp-server-install-dir)
  "The path to install the Roslyn server to."
  :type 'string
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-dll-override-path nil
  "Custom path to Microsoft.CodeAnalysis.LanguageServer.dll."
  :type '(choice (const nil) string)
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-timeout-seconds 60
  "Amount of time to wait for Roslyn server startup, in seconds."
  :type 'integer
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-log-level "Information"
  "Log level for the Roslyn language server."
  :type '(choice (const "None")
                 (const "Trace")
                 (const "Debug")
                 (const "Information")
                 (const "Warning")
                 (const "Error")
                 (const "Critical"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-log-directory (concat (temporary-file-directory) (file-name-as-directory "lsp-roslyn"))
  "Log directory for the Roslyn language server."
  :type 'string
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-server-extra-args '()
  "Extra arguments for the Roslyn language server."
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-dotnet-executable "dotnet"
  "Dotnet executable to use with the Roslyn language server."
  :type 'string
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defcustom lsp-roslyn-package-version "4.13.0-2.24564.12"
  "Version of the Roslyn package to install.
Gotten from https://dev.azure.com/azure-public/vside/_artifacts/feed/vs-impl/NuGet/Microsoft.CodeAnalysis.LanguageServer.win-x64"
  :type 'string
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-roslyn)

(defvar lsp-roslyn--pipe-name nil)

(defun lsp-roslyn--parse-pipe-name (pipe)
  (if (eq system-type 'windows-nt)
      (progn
        (string-match "\\([a-z0-9]+\\)$" pipe)
        (match-string 1 pipe))
    pipe))

(defun lsp-roslyn--parent-process-filter (_process output)
  "Parses the named pipe's name that the Roslyn server process prints on stdout."
  (let* ((data (json-parse-string output :object-type 'plist))
         (pipe (plist-get data :pipeName)))
    (when pipe
      (setq lsp-roslyn--pipe-name (lsp-roslyn--parse-pipe-name pipe)))))

(defun lsp-roslyn--make-named-pipe-process (filter sentinel environment-fn process-name stderr-buf)
  "Creates the process that will handle the JSON-RPC communication."
  (let* ((process-environment
          (lsp--compute-process-environment environment-fn))
         (default-directory (lsp--default-directory-for-connection)))
    (cond
     ((eq system-type 'windows-nt)
      (make-process
       :name process-name
       :connection-type 'pipe
       :buffer (format "*%s*" process-name)
       :coding 'no-conversion
       :filter filter
       :sentinel sentinel
       :stderr stderr-buf
       :noquery t
       :command (lsp-resolve-final-command
                 `("PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command"
                   ,lsp-roslyn--stdpipe-path "."
                   ,lsp-roslyn--pipe-name))))
     (t (make-network-process
         :service "roslyn"
         :name process-name
         :remote lsp-roslyn--pipe-name
         :sentinel sentinel
         :filter filter
         :noquery t)))))

(defun lsp-roslyn--connect (filter sentinel name environment-fn _workspace)
  "Creates a connection to the Roslyn language server's named pipe.

First creates an instance of the language server process, then
creates another process connecting to the named pipe it specifies."
  (setq lsp-roslyn--pipe-name nil)
  (let* ((parent-process-name name)
         (parent-stderr-buf (format "*%s::stderr*" parent-process-name))
         (command-process (make-process
                           :name parent-process-name
                           :buffer (generate-new-buffer-name parent-process-name)
                           :coding 'no-conversion
                           :filter 'lsp-roslyn--parent-process-filter
                           :sentinel sentinel
                           :stderr parent-stderr-buf
                           :command `(,lsp-roslyn-dotnet-executable
                                      ,(lsp-roslyn--get-server-dll-path)
                                      ,(format "--logLevel=%s" lsp-roslyn-server-log-level)
                                      ,(format "--extensionLogDirectory=%s" lsp-roslyn-server-log-directory)
                                      ,@lsp-roslyn-server-extra-args)
                           :noquery t)))
    (accept-process-output command-process lsp-roslyn-server-timeout-seconds) ; wait for JSON with pipe name to print on stdout, like {"pipeName":"\\\\.\\pipe\\d1b72351"}
    (when (not lsp-roslyn--pipe-name)
      (error "Failed to receive pipe name from Roslyn server process"))
    (let* ((process-name (generate-new-buffer-name (format "%s-pipe" name)))
           (stderr-buf (format "*%s::stderr*" process-name))
           (communication-process
            (lsp-roslyn--make-named-pipe-process filter sentinel environment-fn process-name stderr-buf)))
      (with-current-buffer (get-buffer parent-stderr-buf)
        (special-mode))
      (when-let* ((stderr-buffer (get-buffer stderr-buf)))
        (with-current-buffer stderr-buffer
          ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
          (special-mode))
        (set-process-query-on-exit-flag (get-buffer-process stderr-buffer) nil))
      (set-process-query-on-exit-flag command-process nil)
      (set-process-query-on-exit-flag communication-process nil)
      (cons communication-process communication-process))))

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
                        (if-let* ((handler (lsp--get-uri-handler type)))
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

(lsp-defun lsp-roslyn--on-project-initialization-complete (workspace _params)
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
                    (rx (* anychar) ".sln" eos))))
    (cond
     ((not solutions) nil)
     ((eq (length solutions) 1) (cl-first solutions))
     (t (lsp-roslyn--pick-solution-file-interactively solutions)))))

(defun lsp-roslyn-open-solution-file ()
  "Chooses the solution file to associate with the Roslyn language server."
  (interactive)
  (let ((solution-file (lsp-roslyn--find-solution-file)))
    (if solution-file
        (lsp-notify "solution/open" (list :solution (lsp--path-to-uri solution-file)))
      (lsp--error "No solution file was found for this workspace."))))

(defun lsp-roslyn--on-initialized (_workspace)
  "Handler for Roslyn server initialization."
  (lsp-roslyn-open-solution-file))

(defun lsp-roslyn--get-package-name ()
  "Gets the package name of the Roslyn language server."
  (format "microsoft.codeanalysis.languageserver.%s" (lsp-roslyn--get-rid)))

(defun lsp-roslyn--get-server-dll-path ()
  "Gets the path to the language server DLL.
Assumes it was installed with the server install function."
  (if lsp-roslyn-server-dll-override-path
      lsp-roslyn-server-dll-override-path
    (f-join lsp-roslyn-install-path "out"
            (lsp-roslyn--get-package-name)
            lsp-roslyn-package-version
            "content" "LanguageServer"
            (lsp-roslyn--get-rid)
            "Microsoft.CodeAnalysis.LanguageServer.dll")))

(defun lsp-roslyn--get-rid ()
  "Retrieves the .NET Runtime Identifier (RID) for the current system."
  (let* ((is-x64 (string-match-p "x86_64" system-configuration))
         (is-arm64 (string-match-p "aarch64" system-configuration))
         (is-x86 (and (string-match-p "x86" system-configuration) (not is-x64))))
    (if-let* ((platform-name (cond
                              ((eq system-type 'gnu/linux) "linux")
                              ((eq system-type 'darwin) "osx")
                              ((eq system-type 'windows-nt) "win")))
              (arch-name (cond
                          (is-x64 "x64")
                          (is-arm64 "arm64")
                          (is-x86 "x86"))))
        (format "%s-%s" platform-name arch-name)
      (error "Unsupported platform: %s (%s)" system-type system-configuration))))

;; Adapted from roslyn.nvim's version
(defconst lsp-roslyn--temp-project-nuget-config
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<configuration>
  <packageSources>
    <add key=\"vs-impl\" value=\"https://pkgs.dev.azure.com/azure-public/vside/_packaging/vs-impl/nuget/v3/index.json\" />
  </packageSources>
</configuration>"
  "The nuget.config to use when downloading Roslyn.")

;; Adapted from roslyn.nvim's version
(defun lsp-roslyn--temp-project-csproj (pkg-name pkg-version)
  "Generates a temporary .csproj to use for downloading the language server."
  (format
   "<Project Sdk=\"Microsoft.Build.NoTargets/1.0.80\">
    <PropertyGroup>
        <!-- Changes the global packages folder -->
        <RestorePackagesPath>out</RestorePackagesPath>
        <!-- This is not super relevant, as long as your SDK version supports it. -->
        <TargetFramework>net7.0</TargetFramework>
        <!-- If a package is resolved to a fallback folder, it may not be downloaded -->
        <DisableImplicitNuGetFallbackFolder>true</DisableImplicitNuGetFallbackFolder>
        <!-- We don't want to build this project, so we do not need the reference assemblies for the framework we chose -->
        <AutomaticallyUseReferenceAssemblyPackages>false</AutomaticallyUseReferenceAssemblyPackages>
    </PropertyGroup>
    <ItemGroup>
        <PackageDownload Include=\"%s\" version=\"[%s]\" />
    </ItemGroup>
</Project>"
   pkg-name pkg-version))

(defun lsp-roslyn--download-server (_client callback error-callback update?)
  "Downloads the Roslyn language server to `lsp-roslyn-install-path'.
CALLBACK is called when the download finish successfully otherwise
ERROR-CALLBACK is called.
UPDATE is non-nil if it is already downloaded.
FORCED if specified with prefix argument."

  (let ((pkg-name (lsp-roslyn--get-package-name)))
    (when update?
      (ignore-errors (delete-directory lsp-roslyn-install-path t)))
    (unless (f-exists? lsp-roslyn-install-path)
      (mkdir lsp-roslyn-install-path 'create-parent))
    (f-write-text lsp-roslyn--temp-project-nuget-config
                  'utf-8 (expand-file-name "nuget.config" lsp-roslyn-install-path))
    (f-write-text (lsp-roslyn--temp-project-csproj pkg-name lsp-roslyn-package-version)
                  'utf-8 (expand-file-name "DownloadRoslyn.csproj" lsp-roslyn-install-path))
    (lsp-async-start-process
     callback
     error-callback
     lsp-roslyn-dotnet-executable "restore" "--interactive" lsp-roslyn-install-path
     (format "/p:PackageName=%s" pkg-name)
     (format "/p:PackageVersion=%s" lsp-roslyn-package-version))))

(defun lsp-roslyn--make-connection ()
  (list :connect (lambda (f s n e w) (lsp-roslyn--connect f s n e w))
        :test? (lambda () (f-exists? (lsp-roslyn--get-server-dll-path)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-roslyn--make-connection)
                  :priority 0
                  :server-id 'csharp-roslyn
                  :activation-fn (lsp-activate-on "csharp")
                  :notification-handlers (ht ("workspace/projectInitializationComplete" 'lsp-roslyn--on-project-initialization-complete))

                  ;; These two functions are the same as lsp-mode's except they do not
                  ;; (un)hexify URIs.
                  :path->uri-fn 'lsp-roslyn--path-to-uri
                  :uri->path-fn 'lsp-roslyn--uri-to-path

                  :initialized-fn #'lsp-roslyn--on-initialized
                  :download-server-fn #'lsp-roslyn--download-server))

(provide 'lsp-roslyn)
;;; lsp-roslyn.el ends here
