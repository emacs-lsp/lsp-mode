;;; lsp-lua.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 E. Alexander Barbosa
;; Copyright (C) 2020-2026 emacs-lsp maintainers

;; Author: E. Alexander Barbosa <elxbarbosa@outlook.com>
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

;; LSP Clients for the Lua Programming Language

;;; Code:

(require 'lsp-mode)
(require 'f)
(require 'files)

(defgroup lsp-emmy-lua nil
  "Lua LSP client, provided by the EmmyLua Language Server."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/EmmyLua/EmmyLua-LanguageServer"))

(defcustom lsp-clients-emmy-lua-java-path "java"
  "Java Runtime binary location."
  :group 'lsp-emmy-lua
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-jar-path
  (f-join lsp-server-install-dir "EmmyLua-LS-all.jar")
  "Emmy Lua language server jar file."
  :group 'lsp-emmy-lua
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-args '("-jar")
  "Arguments to the Lua Language server."
  :group 'lsp-emmy-lua
  :version "8.0.0"
  :risky t
  :type  '(repeat string))

(defcustom lsp-clients-emmy-lua-command nil
  "Final command to call the Lua Language server."
  :group 'lsp-emmy-lua
  :version "8.0.0"
  :risky t
  :type '(repeat string))

(defun lsp-clients-emmy-lua-test ()
  "Test the Emmy Lua binaries and files."
  (and (executable-find lsp-clients-emmy-lua-java-path)
       (f-exists? lsp-clients-emmy-lua-jar-path)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (or lsp-clients-emmy-lua-command
                                                       `(,lsp-clients-emmy-lua-java-path
                                                         ,@lsp-clients-emmy-lua-args
                                                         ,lsp-clients-emmy-lua-jar-path)))
                                        #'lsp-clients-emmy-lua-test)
  :activation-fn (lsp-activate-on "lua")
  :server-id 'emmy-lua
  :priority -1
  :notification-handlers (lsp-ht ("emmy/progressReport" #'ignore))))


;;; lua-language-server
(defgroup lsp-lua-language-server nil
  "Lua LSP client, provided by the Lua Language Server."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/LuaLS/lua-language-server"))

(defcustom lsp-clients-lua-language-server-install-dir (f-join lsp-server-install-dir "lua-language-server/")
  "Installation directory for Lua Language Server."
  :group 'lsp-lua-language-server
  :version "8.0.0"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-language-server-bin
  (f-join lsp-clients-lua-language-server-install-dir
          "bin/"
          (pcase system-type
            ('gnu/linux "lua-language-server")
            ('darwin "lua-language-server")
            ('windows-nt "lua-language-server.exe")
            (_ "lua-language-server")))
  "Location of Lua Language Server."
  :group 'lsp-lua-language-server
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-main-location
  (f-join lsp-clients-lua-language-server-install-dir
          "main.lua")
  "Location of Lua Language Server main.lua."
  :group 'lsp-lua-language-server
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-args '("-E")
  "Arguments to run the Lua Language server."
  :group 'lsp-lua-language-server
  :version "8.0.0"
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-lua-language-server-command nil
  "Command to start Lua Language server."
  :group 'lsp-lua-language-server
  :type '(repeat string))


(defun lsp-clients-lua-language-server-test ()
  "Test Lua language server binaries and files."
  (and (f-exists? lsp-clients-lua-language-server-main-location)
       (f-exists? lsp-clients-lua-language-server-bin)))

(defcustom lsp-lua-color-mode "Semantic"
  "Color mode."
  :type '(choice (const "Grammar")
                 (const "Semantic"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-call-snippet "Disable"
  "Shows function call snippets."
  :type '(choice
          (const "Disable")
          (const "Both")
          (const "Replace"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-display-context 6
  "Previewing the relevant code snippet of the suggestion may help you
understand the usage of the suggestion.

The number set indicates the number of intercepted lines in the code
fragment.  If it is set to `0`, this feature can be disabled."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-enable t
  "Enable completion."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-keyword-snippet "Replace"
  "Shows keyword syntax snippets."
  :type '(choice (const "Disable")
                 (const "Both")
                 (const "Replace"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-workspace-word t
  "Show words within the workspace."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-debugger-port 11412
  "Listen port of debugger."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-debugger-wait nil
  "Suspend before debugger connects."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-enable nil
  "Developer mode.  Do not enable, performance will be affected."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-disable nil
  "Disabled diagnostic (Use code in hover brackets).
```json
\"Lua.diagnostics.disable\" : [
\"unused-local\",
\"lowercase-global\"
]
```"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-enable t
  "Enable diagnostics."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-globals nil
  "Defined global variables.
```json
\"Lua.diagnostics.globals\" : [
\"GLOBAL1\",
\"GLOBAL2\"
]
```"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-needed-file-status nil
  "If you want to check only opened files, choice Opened; else choice Any.
```json
\"Lua.diagnostics.neededFileStatus\" : {
\"ambiguity-1\" : \"Any\",
\"circle-doc-class\" : \"Opened\"
}
```"
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-severity nil
  "Modified diagnostic severity.
```json
\"Lua.diagnostics.severity\" : {
\"redefined-local\" : \"Warning\",
\"emmy-lua\" : \"Hint\"
}
```"
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-workspace-delay 0
  "Latency (milliseconds) for workspace diagnostics. When you start the
workspace, or edit any file, the entire workspace will be re-diagnosed in the
background. Set to negative to disable workspace diagnostics."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-workspace-rate 100
  "Workspace diagnostics run rate (%). Decreasing this value reduces CPU usage,
but also reduces the speed of workspace diagnostics. The diagnosis of the file
you are currently editing is always done at full speed and is not affected by
this setting."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hint-enable nil
  "Enable hint."
  :type 'boolean
  :package-version '(lsp-mmode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hint-param-name t
  "Hint parameter name when the parameter called is literal."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hint-param-type t
  "Show type hints at the parameter of the function."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hint-set-type nil
  "Hint type at assignment operation."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-enable t
  "Enable hover."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-field-infer 3000
  "When hovering to view a table, type infer will be performed for each field.
When the accumulated time of type infer reaches the set value (MS), the type
infer of subsequent fields will be skipped."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-preview-fields 100
  "When hovering to view a table, limits the maximum number of previews for
fields."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-number t
  "Hover to view numeric content (only if literal is not decimal)."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-string t
  "Hover to view the contents of a string (only if the literal contains an
escape character)."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-string-max 1000
  "The maximum length of a hover to view the contents of a string."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-intelli-sense-search-depth 0
  "Set the search depth for IntelliSense. Increasing this value increases
accuracy, but decreases performance. Different workspace have different
tolerance for this setting. Please adjust it to the appropriate value."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-file-encoding "utf8"
  "File encoding.  The `ansi' option is only available under the `Windows'
platform."
  :type '(choice (const "utf8")
                 (const "ansi"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-nonstandard-symbol nil
  "Supports non-standard symbols. Make sure that your runtime environment
supports these symbols."
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-path
  ["?.lua" "?/init.lua" "?/?.lua"]
  "`package.path`."
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-plugin nil
  "(Proposed) Plugin path. Default is `.vscode/lua/plugin.lua`"
  :type 'file
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-special nil
  "The custom global variables are regarded as some special built-in variables,
and the language server will provide special support.
```json
\"Lua.runtime.special\" : {
\"include\" : \"require\"
}
```"
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-unicode-name nil
  "Allows Unicode characters in name."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-version "Lua 5.4"
  "Lua runtime version."
  :type '(choice (const "Lua 5.1")
                 (const "Lua 5.2")
                 (const "Lua 5.3")
                 (const "Lua 5.4")
                 (const "LuaJIT"))
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-signature-help-enable t
  "Enable signature help."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-telemetry-enable nil
  "Enable telemetry to send your editor information and error logs over the
network."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-window-progress-bar t
  "Show progress bar in status bar."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-window-status-bar t
  "Show extension status in status bar."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-ignore-dir
  [".vscode"]
  "Ignored directories (Use `.gitignore` grammar).
```json
\"Lua.workspace.ignoreDir\" : [
\"temp/*.*\",
\"!temp/*.lua\"
]
```"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-ignore-submodules t
  "Ignore submodules."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-library nil
  "Load external library.

This feature can load external Lua files, which can be used for definition,
automatic completion and other functions.  Note that the language server does
not monitor changes in external files and needs to restart if the external
files are modified.  The following example shows loaded files in `C:/lua`
and `../lib` ,exclude `../lib/temp`.

```json
\"Lua.workspace.library\": {
\"C:/lua\": true,
\"../lib\": [
\"temp/*\"
]
}
```"
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-max-preload 1000
  "Max preloaded files."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-preload-file-size 100
  "Skip files larger than this value (KB) when preloading."
  :type 'number
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-use-git-ignore t
  "Ignore files list in `.gitignore` ."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-files-associations nil
  "Files.associations."
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-files-exclude nil
  "Files.exclude."
  :type 'alist
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-prefer-musl nil
  "Whether or not to download the musl-compiled server."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-lua-language-server)

(lsp-register-custom-settings
 '(("files.associations" lsp-lua-files-associations t)
   ("files.exclude" lsp-lua-files-exclude t)
   ("Lua.workspace.useGitIgnore" lsp-lua-workspace-use-git-ignore t)
   ("Lua.workspace.preloadFileSize" lsp-lua-workspace-preload-file-size)
   ("Lua.workspace.maxPreload" lsp-lua-workspace-max-preload)
   ("Lua.workspace.library" lsp-lua-workspace-library)
   ("Lua.workspace.ignoreSubmodules" lsp-lua-workspace-ignore-submodules t)
   ("Lua.workspace.ignoreDir" lsp-lua-workspace-ignore-dir)
   ("Lua.window.statusBar" lsp-lua-window-status-bar t)
   ("Lua.window.progressBar" lsp-lua-window-progress-bar t)
   ("Lua.telemetry.enable" lsp-lua-telemetry-enable t)
   ("Lua.signatureHelp.enable" lsp-lua-signature-help-enable t)
   ("Lua.runtime.version" lsp-lua-runtime-version)
   ("Lua.runtime.unicodeName" lsp-lua-runtime-unicode-name nil)
   ("Lua.runtime.special" lsp-lua-runtime-special)
   ("Lua.runtime.plugin" lsp-lua-runtime-plugin)
   ("Lua.runtime.path" lsp-lua-runtime-path)
   ("Lua.runtime.nonstandardSymbol" lsp-lua-runtime-nonstandard-symbol)
   ("Lua.runtime.fileEncoding" lsp-lua-runtime-file-encoding)
   ("Lua.intelliSense.searchDepth" lsp-lua-intelli-sense-search-depth)
   ("Lua.hover.viewStringMax" lsp-lua-hover-view-string-max)
   ("Lua.hover.viewString" lsp-lua-hover-view-string t)
   ("Lua.hover.viewNumber" lsp-lua-hover-view-number t)
   ("Lua.hover.previewFields" lsp-lua-hover-preview-fields)
   ("Lua.hover.fieldInfer" lsp-lua-hover-field-infer)
   ("Lua.hover.enable" lsp-lua-hover-enable t)
   ("Lua.hint.setType" lsp-lua-hint-set-type nil)
   ("Lua.hint.paramType" lsp-lua-hint-param-type t)
   ("Lua.hint.paramName" lsp-lua-hint-param-name t)
   ("Lua.hint.enable" lsp-lua-hint-enable t)
   ("Lua.diagnostics.workspaceRate" lsp-lua-diagnostics-workspace-rate)
   ("Lua.diagnostics.workspaceDelay" lsp-lua-diagnostics-workspace-delay)
   ("Lua.diagnostics.severity" lsp-lua-diagnostics-severity)
   ("Lua.diagnostics.neededFileStatus" lsp-lua-diagnostics-needed-file-status)
   ("Lua.diagnostics.globals" lsp-lua-diagnostics-globals)
   ("Lua.diagnostics.enable" lsp-lua-diagnostics-enable t)
   ("Lua.diagnostics.disable" lsp-lua-diagnostics-disable)
   ("Lua.develop.enable" lsp-lua-develop-enable t)
   ("Lua.develop.debuggerWait" lsp-lua-develop-debugger-wait t)
   ("Lua.develop.debuggerPort" lsp-lua-develop-debugger-port)
   ("Lua.completion.workspaceWord" lsp-lua-completion-workspace-word t)
   ("Lua.completion.keywordSnippet" lsp-lua-completion-keyword-snippet)
   ("Lua.completion.enable" lsp-lua-completion-enable t)
   ("Lua.completion.displayContext" lsp-lua-completion-display-context)
   ("Lua.completion.callSnippet" lsp-lua-completion-call-snippet)
   ("Lua.color.mode" lsp-lua-color-mode)))

(defun lsp-lua-language-server-install-latest (client callback error-callback update?)
  "Download the latest version of lua-language-server and extract it to
`lsp-lua-language-server-install-dir'."
  (ignore client update?)
  (let ((store-path (expand-file-name "lua-language-server-github" lsp-clients-lua-language-server-install-dir)))
    (lsp-download-install
     (lambda (&rest _)
       (set-file-modes lsp-clients-lua-language-server-bin #o0700)
       (funcall callback))
     error-callback
     :url (lsp--find-latest-gh-release-url
           "https://api.github.com/repos/LuaLS/lua-language-server/releases/latest"
           (format "%s%s.tar.gz"
                   (pcase system-type
                     ('gnu/linux
                      (pcase (lsp-resolve-value lsp--system-arch)
                        ('x64     "linux-x64")
                        ('arm64   "linux-arm64")))
                     ('darwin
                      (pcase (lsp-resolve-value lsp--system-arch)
                        ('x64     "darwin-x64")
                        ('arm64   "darwin-arm64")))
                     ('windows-nt
                      (pcase (lsp-resolve-value lsp--system-arch)
                        ('x64     "win32-x64")
                        ('arm64   "win32-ia32")))
                     (_
                      (pcase (lsp-resolve-value lsp--system-arch)
                        ('x64     "linux-x64"))))
                   (if lsp-lua-prefer-musl "-musl" "")))
     :store-path store-path
     :decompress (pcase system-type ('windows-nt :zip) (_ :targz)))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (or lsp-clients-lua-language-server-command
                                                       `(,lsp-clients-lua-language-server-bin
                                                         ,@lsp-clients-lua-language-server-args
                                                         ,lsp-clients-lua-language-server-main-location)))
                                        #'lsp-clients-lua-language-server-test)
  :activation-fn (lsp-activate-on "lua")
  :priority -2
  :server-id 'lua-language-server
  :download-server-fn #'lsp-lua-language-server-install-latest))

;;; lua-lsp
(defgroup lsp-lua-lsp nil
  "Lua LSP client, provided by the Lua-Lsp."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/Alloyed/lua-lsp"))

(defcustom lsp-clients-luarocks-bin-dir (f-join (getenv "HOME") ".luarocks/bin/")
  "LuaRocks bin directory."
  :group 'lsp-lua-lsp
  :version "8.0.0"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-lsp-server-install-dir nil
  "Installation directory for Lua-Lsp Language Server."
  :group 'lsp-lua-lsp
  :version "8.0.0"
  :risky t
  :type 'file)

(defun lsp-clients-lua-lsp-test ()
  "Test Lua-lsp language server files."
  (and (f-exists? lsp-clients-lua-lsp-server-install-dir)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda ()
                                          (or lsp-clients-lua-lsp-server-install-dir
                                              (f-join lsp-clients-luarocks-bin-dir "lua-lsp")))
                                        #'lsp-clients-lua-lsp-test)
  :activation-fn (lsp-activate-on "lua")
  :priority -3
  :server-id 'lsp-lua-lsp))

;;; lua-roblox-language-server
(defgroup lsp-lua-roblox-language-server nil
  "Roblox Lua LSP client, provided by the Roblox Lua Language Server."
  :group 'lsp-mode
  :version "8.0.0"
  :link '(url-link "https://github.com/NightrainsRbx/RobloxLsp"))

(defcustom lsp-lua-roblox-language-server-install-dir (f-join lsp-server-install-dir "lua-roblox-language-server/")
  "Installation directory for Lua Language Server."
  :group 'lsp-lua-roblox-language-server
  :version "8.0.0"
  :risky t
  :type 'directory)

(defcustom lsp-lua-roblox-language-server-bin
  (f-join lsp-lua-roblox-language-server-install-dir
          "extension/server/bin/"
          (pcase system-type
            ('gnu/linux "Linux/lua-language-server")
            ('darwin "macOS/lua-language-server")
            ('windows-nt "Windows/lua-language-server.exe")
            (_ "Linux/lua-language-server")))
  "Location of Roblox Lua Language Server."
  :group 'lsp-lua-roblox-language-server
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-lua-roblox-language-server-main-location
  (f-join lsp-lua-roblox-language-server-install-dir
          "extension/server/main.lua")
  "Location of Roblox Lua Language Server main.lua."
  :group 'lsp-lua-roblox-language-server
  :version "8.0.0"
  :risky t
  :type 'file)

(defcustom lsp-lua-roblox-server-download-url
  (lsp-vscode-extension-url "Nightrains" "robloxlsp" "1.5.11")
  "Download url for Roblox Lua vscode extension."
  :group 'lsp-lua-roblox-language-server
  :version "8.0.0"
  :type 'string)

(defcustom lsp-lua-roblox-server-store-path
  (expand-file-name "vs-lua-roblox" lsp-lua-roblox-language-server-install-dir)
  "Server file name for the vscode extension."
  :group 'lsp-lua-roblox-language-server
  :version "8.0.0"
  :type 'string)

(defun lsp-lua-roblox-language-server-test ()
  "Test Lua language server binaries and files."
  (and (f-exists? lsp-lua-roblox-language-server-main-location)
       (f-exists? lsp-lua-roblox-language-server-bin)))

(defun lsp-lua-roblox-language-server-install (_client callback error-callback _update?)
  "Download the latest version of lua-language-server and extract it to
`lsp-lua-roblox-language-server-download-url'."
  (lsp-download-install
   (lambda (&rest _)
     (set-file-modes lsp-lua-roblox-language-server-bin #o0700)
     (funcall callback))
   error-callback
   :url lsp-lua-roblox-server-download-url
   :store-path lsp-lua-roblox-server-store-path
   :decompress :zip))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (or lsp-clients-lua-language-server-command
                                                       `(,lsp-lua-roblox-language-server-bin
                                                         ,@lsp-clients-lua-language-server-args
                                                         ,lsp-lua-roblox-language-server-main-location)))
                                        #'lsp-lua-roblox-language-server-test)
  :activation-fn (lsp-activate-on "lua")
  :priority -4
  :server-id 'lua-roblox-language-server
  :download-server-fn #'lsp-lua-roblox-language-server-install))

(lsp-consistency-check lsp-lua)

(provide 'lsp-lua)
;;; lsp-lua.el ends here
