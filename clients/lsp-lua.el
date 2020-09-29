;;; lsp-lua.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 E. Alexander Barbosa

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
  :version "7.1"
  :link '(url-link "https://github.com/EmmyLua/EmmyLua-LanguageServer"))

(defcustom lsp-clients-emmy-lua-java-path "java"
  "Java Runtime binary location."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-jar-path (f-join lsp-server-install-dir "EmmyLua-LS-all.jar")
  "Emmy Lua language server jar file."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-args '("-jar")
  "Arguments to the Lua Language server."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type  '(repeat string))

(defcustom lsp-clients-emmy-lua-command `(,lsp-clients-emmy-lua-java-path
                                          ,@lsp-clients-emmy-lua-args
                                          ,lsp-clients-emmy-lua-jar-path)
  "Final command to call the Lua Language server."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type '(repeat string))

(defun lsp-clients-emmy-lua-test ()
  "Test the Emmy Lua binaries and files."
  (and (executable-find lsp-clients-emmy-lua-java-path)
       (f-exists? lsp-clients-emmy-lua-jar-path)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-emmy-lua-command
                                        #'lsp-clients-emmy-lua-test)
  :major-modes '(lua-mode)
  :server-id 'emmy-lua
  :priority -1
  :notification-handlers (lsp-ht ("emmy/progressReport" #'ignore))))


;;; lua-language-server
(defgroup lsp-lua-language-server nil
  "Lua LSP client, provided by the Lua Language Server."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/sumneko/lua-language-server"))

(defcustom lsp-clients-lua-language-server-install-dir (f-join lsp-server-install-dir "lua-language-server/")
  "Installation directory for Lua Language Server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/Linux/lua-language-server")
  "Location of Lua Language Server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
  "Location of Lua Language Server main.lua."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-args '("-E")
  "Arguments to run the Lua Language server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-lua-language-server-command `(,lsp-clients-lua-language-server-bin
                                                     ,@lsp-clients-lua-language-server-args
                                                     ,lsp-clients-lua-language-server-main-location)
  "Command to start Lua Language server."
  :group 'lsp-lua-language-server
  :type '(repeat string))


(defun lsp-clients-lua-language-server-test ()
  "Test Lua language server binaries and files."
  (and (f-exists? lsp-clients-lua-language-server-main-location)
       (f-exists? lsp-clients-lua-language-server-bin)))

(defcustom lsp-lua-color-mode "Semantic"
  "Color mode."
  :type '(choice (:tag "Grammar" "Semantic"))
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-call-snippet "Disable"
  "Shows function call snippets."
  :type '(choice (:tag "Disable" "Both" "Replace"))
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-display-context 6
  "Previewing the relevant code snippet of the suggestion may help you understand the usage of the suggestion. The number set indicates the number of intercepted lines in the code fragment. If it is set to `0`, this feature can be disabled."
  :type 'number
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-enable t
  "Enable completion."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-completion-keyword-snippet "Replace"
  "Shows keyword syntax snippets."
  :type '(choice (:tag "Disable" "Both" "Replace"))
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-debugger-port 11412
  "Listen port of debugger."
  :type 'number
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-debugger-wait nil
  "Suspend before debugger connects."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-develop-enable nil
  "Developer mode. Do not enable, performance will be affected."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
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
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-diagnostics-enable t
  "Enable diagnostics."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
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
  :package-version '(lsp-mode . "7.1")
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
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-enable t
  "Enable hover."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-number t
  "Hover to view numeric content (only if literal is not decimal)."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-string t
  "Hover to view the contents of a string (only if the literal contains an escape character)."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-hover-view-string-max 1000
  "The maximum length of a hover to view the contents of a string."
  :type 'number
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-path
  ["?.lua" "?/init.lua" "?/?.lua"]
  "`package.path`"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-runtime-version "Lua 5.3"
  "Lua runtime version."
  :type '(choice (:tag "Lua 5.1" "Lua 5.2" "Lua 5.3" "Lua 5.4" "LuaJIT"))
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-signature-help-enable t
  "Enable signature help."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
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
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-ignore-submodules t
  "Ignore submodules."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-library nil
  "Load external library.
This feature can load external Lua files, which can be used for definition, automatic completion and other functions. Note that the language server does not monitor changes in external files and needs to restart if the external files are modified.
The following example shows loaded files in `C:/lua` and `../lib` ,exclude `../lib/temp`.
```json
\"Lua.workspace.library\": {
\"C:/lua\": true,
\"../lib\": [
\"temp/*\"
]
}
```"
  :type 'alist
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-max-preload 300
  "Max preloaded files."
  :type 'number
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-preload-file-size 100
  "Skip files larger than this value (KB) when preloading."
  :type 'number
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-workspace-use-git-ignore t
  "Ignore files list in `.gitignore` ."
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-zzzzzz-cat nil
  "DONT TOUCH ME, LET ME SLEEP >_<"
  :type 'boolean
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-files-associations nil
  "files.associations"
  :type 'alist
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(defcustom lsp-lua-files-exclude nil
  "files.exclude"
  :type 'alist
  :package-version '(lsp-mode . "7.1")
  :group 'lsp-lua-language-server)

(lsp-register-custom-settings
 '(("Lua.zzzzzz.cat" lsp-lua-zzzzzz-cat t)
   ("files.associations" lsp-lua-files-associations t)
   ("files.exclude" lsp-lua-files-exclude t)
   ("Lua.workspace.useGitIgnore" lsp-lua-workspace-use-git-ignore t)
   ("Lua.workspace.preloadFileSize" lsp-lua-workspace-preload-file-size)
   ("Lua.workspace.maxPreload" lsp-lua-workspace-max-preload)
   ("Lua.workspace.library" lsp-lua-workspace-library)
   ("Lua.workspace.ignoreSubmodules" lsp-lua-workspace-ignore-submodules t)
   ("Lua.workspace.ignoreDir" lsp-lua-workspace-ignore-dir)
   ("Lua.signatureHelp.enable" lsp-lua-signature-help-enable t)
   ("Lua.runtime.version" lsp-lua-runtime-version)
   ("Lua.runtime.path" lsp-lua-runtime-path)
   ("Lua.hover.viewStringMax" lsp-lua-hover-view-string-max)
   ("Lua.hover.viewString" lsp-lua-hover-view-string t)
   ("Lua.hover.viewNumber" lsp-lua-hover-view-number t)
   ("Lua.hover.enable" lsp-lua-hover-enable t)
   ("Lua.diagnostics.severity" lsp-lua-diagnostics-severity)
   ("Lua.diagnostics.globals" lsp-lua-diagnostics-globals)
   ("Lua.diagnostics.enable" lsp-lua-diagnostics-enable t)
   ("Lua.diagnostics.disable" lsp-lua-diagnostics-disable)
   ("Lua.develop.enable" lsp-lua-develop-enable t)
   ("Lua.develop.debuggerWait" lsp-lua-develop-debugger-wait t)
   ("Lua.develop.debuggerPort" lsp-lua-develop-debugger-port)
   ("Lua.completion.keywordSnippet" lsp-lua-completion-keyword-snippet)
   ("Lua.completion.enable" lsp-lua-completion-enable t)
   ("Lua.completion.displayContext" lsp-lua-completion-display-context)
   ("Lua.completion.callSnippet" lsp-lua-completion-call-snippet)
   ("Lua.color.mode" lsp-lua-color-mode)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-lua-language-server-command
                                        #'lsp-clients-lua-language-server-test)
  :major-modes '(lua-mode)
  :priority -2
  :server-id 'lua-language-server))


;;; lua-lsp
(defgroup lsp-lua-lsp nil
  "Lua LSP client, provided by the Lua-Lsp."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/Alloyed/lua-lsp"))

(defcustom lsp-clients-luarocks-bin-dir (f-join (getenv "HOME") ".luarocks/bin/")
  "LuaRocks bin directory."
  :group 'lsp-lua-lsp
  :version "7.1"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-lsp-server-install-dir (f-join lsp-clients-luarocks-bin-dir "lua-lsp")
  "Installation directory for Lua-Lsp Language Server."
  :group 'lsp-lua-lsp
  :version "7.1"
  :risky t
  :type 'file)

(defun lsp-clients-lua-lsp-test ()
  "Test Lua-lsp language server files."
  (and (f-exists? lsp-clients-lua-lsp-server-install-dir)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-lua-lsp-server-install-dir
                                        #'lsp-clients-lua-lsp-test)
  :major-modes '(lua-mode)
  :priority -3
  :server-id 'lsp-lua-lsp))


(provide 'lsp-lua)
;;; lsp-lua.el ends here
