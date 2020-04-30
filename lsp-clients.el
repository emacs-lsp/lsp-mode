;;; lsp-clients.el --- lightweight clients                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>
;; Keywords: languages

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

;; Contains definitions for the simple clients.

;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'dash-functional)
(require 'rx)
(require 'cl-lib)
(require 'em-glob)

;;; Ada
(defgroup lsp-ada nil
  "Settings for Ada Language Server."
  :group 'tools
  :tag "Language Server"
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-project-file "default.gpr"
  "Set the project file full path to configure the language server with.
  The ~ prefix (for the user home directory) is supported.
  See https://github.com/AdaCore/ada_language_server for a per-project
  configuration example."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-option-charset "UTF-8"
  "The charset to use by the Ada Language server. Defaults to 'UTF-8'."
  :type 'string
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-ada-enable-diagnostics t
  "A boolean to disable diagnostics. Defaults to true."
  :type 'boolean
  :group 'lsp-ada
  :package-version '(lsp-mode . "6.2"))

(lsp-register-custom-settings
 '(("ada.projectFile" lsp-ada-project-file)
   ("ada.enableDiagnostics" lsp-ada-enable-diagnostics)
   ("ada.defaultCharset" lsp-ada-option-charset)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("ada_language_server"))
                  :major-modes '(ada-mode)
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "ada"))))
                  :server-id 'ada-ls))

;;; Bash
(defgroup lsp-bash nil
  "Settings for the Bash Language Server."
  :group 'tools
  :tag "Language Server"
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-explainshell-endpoint nil
  "The endpoint to use explainshell.com to answer 'onHover' queries.
See instructions at https://marketplace.visualstudio.com/items?itemName=mads-hartmann.bash-ide-vscode"
  :type 'string
  :risky t
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-highlight-parsing-errors nil
  "Consider parsing errors in scripts as 'problems'."
  :type 'boolean
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-bash-glob-pattern nil
  "Glob pattern used to find shell script files to parse."
  :type 'string
  :group 'lsp-bash
  :package-version '(lsp-mode . "6.3"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                  :major-modes '(sh-mode)
                  :priority -1
                  :environment-fn (lambda ()
                                    '(("EXPLAINSHELL_ENDPOINT" . lsp-bash-explainshell-endpoint)
                                      ("HIGHLIGHT_PARSING_ERRORS" . lsp-bash-highlight-parsing-errors)
                                      ("GLOB_PATTERN" . lsp-bash-glob-pattern)))
                  :server-id 'bash-ls))

;;; Groovy
(defgroup lsp-groovy nil
  "LSP support for Groovy, using groovy-language-server"
  :group 'lsp-mode
  :link '(url-link "https://github.com/prominic/groovy-language-server"))

(defcustom lsp-groovy-server-file
  (locate-user-emacs-file "groovy-language-server/groovy-language-server-all.jar")
  "JAR file path for groovy-language-server-all.jar."
  :group 'lsp-groovy
  :risky t
  :type 'file)

(defun lsp-groovy--lsp-command ()
  "Generate LSP startup command."
  `("java" "-jar" ,(expand-file-name lsp-groovy-server-file)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-groovy--lsp-command)
                  :major-modes '(groovy-mode)
                  :priority -1
                  :server-id 'groovy-ls))

;;; TypeScript/JavaScript

(lsp-dependency 'javascript-typescript-langserver
                '(:system "javascript-typescript-stdio")
                '(:npm :package "javascript-typescript-langserver"
                       :path "javascript-typescript-stdio"))

(defgroup lsp-typescript-javascript nil
  "Support for TypeScript/JavaScript, using Sourcegraph's JavaScript/TypeScript language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sourcegraph/javascript-typescript-langserver"))

(defcustom lsp-clients-typescript-javascript-server-args '()
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript-javascript
  :risky t
  :type '(repeat string))

(defun lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
  "Check if the javascript-typescript language server should be enabled based on FILENAME."
  (or (string-match-p (rx (one-or-more anything) "." (or "ts" "js") (opt "x") string-end) filename)
      (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode)
           (not (derived-mode-p 'json-mode)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons (lsp-package-path 'javascript-typescript-langserver)
                                                                lsp-clients-typescript-javascript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -3
                  :completion-in-comments? t
                  :server-id 'jsts-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'javascript-typescript-langserver
                                         callback
                                         error-callback))))


;;; TypeScript
(defgroup lsp-typescript nil
  "LSP support for TypeScript, using Theia/Typefox's TypeScript Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/theia-ide/typescript-language-server"))

(defcustom lsp-clients-typescript-server-args '("--stdio")
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-typescript-log-verbosity "info"
  "The server log verbosity."
  :group 'lsp-typescript
  :type 'string)

(defcustom lsp-clients-typescript-plugins (vector)
  "The list of plugins to load.
It should be a vector of plist with keys `:location' and `:name'
where `:name' is the name of the package and `:location' is the
directory containing the package. Example:
\(vector
   \(list :name \"@vsintellicode/typescript-intellicode-plugin\"
         :location \"<path>.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.1.9/\"))"
  :group 'lsp-typescript
  :type  '(restricted-sexp :tag "Vector"
                           :match-alternatives
                           (lambda (xs)
                             (and (vectorp xs) (seq-every-p
                                                (-lambda ((&plist :name :location))
                                                  (and name location))
                                                xs)))))

(lsp-dependency 'typescript-language-server
                '(:system "typescript-language-server")
                '(:npm :package "typescript-language-server"
                       :path "typescript-language-server"))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '  (:npm :package "typescript"
                         :path "tsserver"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          `(,(lsp-package-path 'typescript-language-server)
                                                            "--tsserver-path"
                                                            ,(lsp-package-path 'typescript)
                                                            ,@lsp-clients-typescript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -2
                  :completion-in-comments? t
                  :initialization-options (lambda ()
                                            (list :plugins lsp-clients-typescript-plugins
                                                  :logVerbosity lsp-clients-typescript-log-verbosity
                                                  :tsServerPath (lsp-package-path 'typescript)))
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'typescript
                                         (-partial #'lsp-package-ensure
                                                   'typescript-language-server
                                                   callback
                                                   error-callback)
                                         error-callback))))



;;; JavaScript Flow
(defgroup lsp-flow nil
  "LSP support for the Flow Javascript type checker."
  :group 'lsp-mode
  :link '(url-link "https://flow.org/"))

(defcustom lsp-clients-flow-server "flow"
  "The Flow executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-flow
  :risky t
  :type 'file)

(defcustom lsp-clients-flow-server-args '("lsp")
  "Extra arguments for starting the Flow language server."
  :group 'lsp-flow
  :risky t
  :type '(repeat string))

(defun lsp-clients-flow-tag-file-present-p (file-name)
  "Check if the '// @flow' or `/* @flow */' tag is present in
the contents of FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (lsp-clients-flow-tag-string-present-p (buffer-string))))

(defun lsp-clients-flow-tag-string-present-p (file-contents)
  "Helper for `lsp-clients-flow-tag-file-present-p' that works
with the file contents."
  (with-temp-buffer
    (insert file-contents)
    (save-excursion
      (goto-char (point-min))
      (let (stop found)
        (while (not stop)
          (when (not (re-search-forward "[^\n[:space:]]" nil t))
            (setq stop t))
          (if (equal (point) (point-min))
              (setq stop t)
            (backward-char))
          (cond ((or (looking-at "//+[ ]*@flow")
                     (looking-at "/\\**[ ]*@flow")
                     (looking-at "[ ]*\\*[ ]*@flow"))
                 (setq found t)
                 (setq stop t))
                ((looking-at "//")
                 (forward-line))
                ((looking-at "*")
                 (forward-line))
                ((looking-at "/\\*")
                 (save-excursion
                   (when (not (re-search-forward "*/" nil t))
                     (setq stop t)))
                 (forward-line))
                (t (setq stop t))))
        found))))

(defun lsp-clients-flow-project-p (file-name)
  "Check if FILE-NAME is part of a Flow project, that is, if
there is a .flowconfig file in the folder hierarchy."
  (locate-dominating-file file-name ".flowconfig"))

(defun lsp-clients-flow-activate-p (file-name _mode)
  "Check if the Flow language server should be enabled for a
particular FILE-NAME and MODE."
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (or (lsp-clients-flow-project-p file-name)
	   (lsp-clients-flow-tag-file-present-p file-name))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons lsp-clients-flow-server
                                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))


;; PHP
(defgroup lsp-php nil
  "LSP support for PHP, using php-language-server."
  :link '(url-link "https://github.com/felixfbecker/php-language-server")
  :group 'lsp-mode)

(defcustom lsp-clients-php-server-command
  `("php" ,(expand-file-name "~/.composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))
  "Install directory for php-language-server."
  :group 'lsp-php
  :type '(repeat string))

(defun lsp-php--create-connection ()
  "Create lsp connection."
  (lsp-stdio-connection
   (lambda () lsp-clients-php-server-command)
   (lambda ()
     (if (and (cdr lsp-clients-php-server-command)
              (eq (string-match-p "php[0-9.]*\\'" (car lsp-clients-php-server-command)) 0))
         ;; Start with the php command and the list has more elems. Test the existence of the PHP script.
         (let ((php-file (nth 1 lsp-clients-php-server-command)))
           (or (file-exists-p php-file)
               (progn
                 (lsp-log "%s is not present." php-file)
                 nil)))
       t))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-php--create-connection)
                  :major-modes '(php-mode)
                  :priority -2
                  :server-id 'php-ls))



(defgroup lsp-ocaml nil
  "LSP support for OCaml, using ocaml-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/freebroccolo/ocaml-language-server"))

(define-obsolete-variable-alias
  'lsp-ocaml-ocaml-lang-server-command
  'lsp-ocaml-lang-server-command
  "lsp-mode 6.1")

(defcustom lsp-ocaml-lang-server-command
  '("ocaml-language-server" "--stdio")
  "Command to start ocaml-language-server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-ocaml-lang-server-command))
                  :major-modes '(reason-mode caml-mode tuareg-mode)
                  :priority -1
                  :server-id 'ocaml-ls))

(defgroup lsp-ocaml-lsp-server nil
  "LSP support for OCaml, using ocaml-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ocaml/ocaml-lsp"))

(define-obsolete-variable-alias 'lsp-merlin 'lsp-ocaml-lsp-server)
(define-obsolete-variable-alias 'lsp-merlin-command 'lsp-ocaml-lsp-server-command)

(defcustom lsp-ocaml-lsp-server-command
  '("ocamllsp")
  "Command to start ocaml-language-server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (lambda () lsp-ocaml-lsp-server-command))
  :major-modes '(caml-mode tuareg-mode)
  :priority 0
  :server-id 'ocaml-lsp-server))


;; C-family (C, C++, Objective-C, Objective-C++)

(defgroup lsp-clangd nil
  "LSP support for C-family languages (C, C++, Objective-C, Objective-C++), using clangd."
  :group 'lsp-mode
  :link '(url-link "https://clang.llvm.org/extra/clangd/"))

(defcustom lsp-clients-clangd-executable "clangd"
  "The clangd executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-clangd
  :risky t
  :type 'file)

(defcustom lsp-clients-clangd-args '()
  "Extra arguments for the clangd executable."
  :group 'lsp-clangd
  :risky t
  :type '(repeat string))

(defun lsp-clients--clangd-command ()
  "Generate the language server startup command."
  `(,lsp-clients-clangd-executable ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--clangd-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd))

;; Elixir
(defgroup lsp-elixir nil
  "LSP support for Elixir, using elixir-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/JakeBecker/elixir-ls"))

(defcustom lsp-clients-elixir-server-executable
  (if (equal system-type 'windows-nt)
      "language_server.bat"
    "language_server.sh")
  "The elixir-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-elixir
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          `(,lsp-clients-elixir-server-executable)))
                  :major-modes '(elixir-mode)
                  :priority -1
                  :server-id 'elixir-ls
                  :initialized-fn (lambda (workspace)
                                    (puthash
                                     "textDocumentSync"
                                     (ht ("save" t)
                                         ("change" 2))
                                     (lsp--workspace-server-capabilities workspace)))))

;; Fortran
(defgroup lsp-fortran nil
  "LSP support for Fortran, using the Fortran Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/hansec/fortran-language-server"))

(defcustom lsp-clients-fortls-executable "fortls"
  "The fortls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-fortran
  :risky t
  :type 'file)

(defcustom lsp-clients-fortls-args '()
  "Extra arguments for the fortls executable"
  :group 'lsp-fortran
  :risky t
  :type '(repeat string))

(defun lsp-clients--fortls-command ()
  "Generate the language server startup command."
  `(,lsp-clients-fortls-executable,@lsp-clients-fortls-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-clients--fortls-command)
                  :major-modes '(f90-mode fortran-mode)
                  :priority -1
                  :server-id 'fortls))



;; Kotlin
(defgroup lsp-kotlin nil
  "LSP support for Kotlin, using KotlinLanguageServer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/fwcd/KotlinLanguageServer"))

(define-obsolete-variable-alias
  'lsp-kotlin-language-server-path
  'lsp-clients-kotlin-server-executable
  "lsp-mode 6.4")

(defcustom lsp-clients-kotlin-server-executable "kotlin-language-server"
  "The kotlin-language-server executable to use.
Leave as just the executable name to use the default behavior of finding the
executable with `exec-path'."
  :type 'string
  :group 'lsp-kotlin)

(defcustom lsp-kotlin-trace-server "off"
  "Traces the communication between VSCode and the Kotlin language server."
  :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-compiler-jvm-target "1.8"
  "Specifies the JVM target, e.g. \"1.6\" or \"1.8\""
  :type 'string
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-linting-debounce-time 250
  "[DEBUG] Specifies the debounce time limit. Lower to increase
responsiveness at the cost of possible stability issues."
  :type 'number
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-completion-snippets-enabled t
  "Specifies whether code completion should provide snippets (true) or plain-text items (false)."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-debug-adapter-enabled t
  "[Recommended] Specifies whether the debug adapter should be used. When enabled a debugger for Kotlin will be available."
  :type 'boolean)

(defcustom lsp-kotlin-debug-adapter-path ""
  "Optionally a custom path to the debug adapter executable."
  :type 'string
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-external-sources-use-kls-scheme t
  "[Recommended] Specifies whether URIs inside JARs should be represented using the 'kls'-scheme."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-kotlin-external-sources-auto-convert-to-kotlin t
  "Specifies whether decompiled/external classes should be auto-converted to Kotlin."
  :type 'boolean
  :group 'lsp-kotlin
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings
 '(("kotlin.externalSources.autoConvertToKotlin" lsp-kotlin-external-sources-auto-convert-to-kotlin t)
   ("kotlin.externalSources.useKlsScheme" lsp-kotlin-external-sources-use-kls-scheme t)
   ("kotlin.debugAdapter.path" lsp-kotlin-debug-adapter-path)
   ("kotlin.debugAdapter.enabled" lsp-kotlin-debug-adapter-enabled t)
   ("kotlin.completion.snippets.enabled" lsp-kotlin-completion-snippets-enabled t)
   ("kotlin.linting.debounceTime" lsp-kotlin-linting-debounce-time)
   ("kotlin.compiler.jvm.target" lsp-kotlin-compiler-jvm-target)
   ("kotlin.trace.server" lsp-kotlin-trace-server)
   ("kotlin.languageServer.path" lsp-clients-kotlin-server-executable)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-kotlin-server-executable)
  :major-modes '(kotlin-mode)
  :priority -1
  :server-id 'kotlin-ls
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "kotlin"))))))


;; Hack
(defgroup lsp-hack nil
  "LSP support for Hack, using HHVM."
  :group 'lsp-mode
  :link '(url-link "https://docs.hhvm.com/hhvm/"))

(defcustom lsp-clients-hack-command '("hh_client" "lsp" "--from" "emacs")
  "Command to start hh_client."
  :group 'lsp-hack
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-hack-command))
                  :major-modes '(hack-mode)
                  :priority -1
                  :server-id 'hack
                  ;; ignore some unsupported messages from Nuclide
                  :notification-handlers (lsp-ht ("telemetry/event" 'ignore)
                                                 ("$/cancelRequest" 'ignore))
                  :request-handlers (lsp-ht ("window/showStatus" 'ignore))))


;;; Dockerfile
(defcustom lsp-dockerfile-language-server-command
  '("docker-langserver" "--stdio")
  "The command that starts the docker language server."
  :group 'lsp-dockerfile
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const lsp-dockerfile-language-server-command))
                  :major-modes '(dockerfile-mode)
                  :priority -1
                  :server-id 'dockerfile-ls))


;;; Angular
(defcustom lsp-clients-angular-language-server-command
  '("node"
    "/usr/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/lib/node_modules"
    "--tsProbeLocations"
    "/usr/lib/node_modules"
    "--stdio")
  "The command that starts the angular language server."
  :group 'lsp-clients-angular
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(defun lsp-client--angular-start-loading (_workspace params)
  (lsp--info "Started loading project %s" params))

(defun lsp-client--angular-finished-loading (_workspace params)
  (lsp--info "Finished loading project %s" params))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clients-angular-language-server-command))
                  :activation-fn (lambda (&rest _args)
                                   (and (string-match-p ".*\.html$" (buffer-file-name))
                                        (lsp-workspace-root)
                                        (file-exists-p (f-join (lsp-workspace-root) "angular.json"))))
                  :priority -1
                  :notification-handlers (ht ("angular-language-service/projectLoadingStart" #'lsp-client--angular-start-loading)
                                             ("angular-language-service/projectLoadingFinish" #'lsp-client--angular-finished-loading))
                  :add-on? t
                  :server-id 'angular-ls))


;; TeX
(defgroup lsp-tex nil
  "LSP support for TeX and friends, using Digestif and texlab."
  :group 'lsp-mode
  :link '(url-link "https://github.com/astoff/digestif/")
  :link '(url-link "https://github.com/latex-lsp/texlab"))

(defcustom lsp-tex-server 'texlab
  "Choose LSP tex server."
  :type '(choice (const :tag "texlab" texlab)
                 (const :tag "digestif" digestif))
  :group 'lsp-tex)

(defcustom lsp-clients-digestif-executable "digestif"
  "Command to start the Digestif language server."
  :group 'lsp-tex
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-digestif-executable)
                  :major-modes '(plain-tex-mode latex-mode)
                  :priority (if (eq lsp-tex-server 'digestif) 1 -1)
                  :server-id 'digestif))

(defcustom lsp-clients-texlab-executable "texlab"
  "Command to start the texlab language server."
  :group 'lsp-tex
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-texlab-executable)
                  :major-modes '(plain-tex-mode latex-mode)
                  :priority (if (eq lsp-tex-server 'texlab) 1 -1)
                  :server-id 'texlab))


;; Vim script
(defgroup lsp-vim nil
  "LSP support for viml using vim-language-server"
  :group 'lsp-mode)

(defcustom lsp-clients-vim-executable '("vim-language-server" "--stdio")
  "Command to start the vim language server."
  :group 'lsp-vim
  :risky t
  :type 'file)

(defcustom lsp-clients-vim-initialization-options '((iskeyword . "vim iskeyword option")
                                                    (vimruntime . "/usr/bin/vim")
                                                    (runtimepath . "/usr/bin/vim")
                                                    (diagnostic . ((enable . t)))
                                                    (indexes . ((runtimepath . t)
                                                                (gap . 100)
                                                                (count . 3)))
                                                    (suggest . ((fromVimruntime . t)
                                                                (fromRuntimepath . :json-false))))
  "Initialization options for vim language server."
  :group 'lsp-vim
  :type 'alist)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-vim-executable)
                  :major-modes '(vimrc-mode)
                  :priority -1
                  :server-id 'vimls
                  :initialization-options (lambda ()
                                            lsp-clients-vim-initialization-options)))


;; LUA
(defgroup lsp-emmy-lua nil
  "LSP support for emmy-lua."
  :group 'lsp-mode
  :link '(url-link "https://github.com/EmmyLua/EmmyLua-LanguageServer"))

(defcustom lsp-clients-emmy-lua-java-path "java"
  "Path to java which will be used for running emmy-lua language server."
  :group 'lsp-emmy-lua
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-jar-path (f-expand "~/.emacs.d/EmmyLua-LS-all.jar")
  "Path to jar which will be used for running EmmyLua language server."
  :group 'lsp-emmy-lua
  :risky t
  :type 'file)

(defun lsp-clients-emmy-lua--create-connection ()
  "Create connection to emmy lua language server."
  (lsp-stdio-connection
   (lambda ()
     (list lsp-clients-emmy-lua-java-path "-jar" lsp-clients-emmy-lua-jar-path))
   (lambda ()
     (f-exists? lsp-clients-emmy-lua-jar-path))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-clients-emmy-lua--create-connection)
                  :major-modes '(lua-mode)
                  :priority -1
                  :server-id 'emmy-lua
                  :notification-handlers (lsp-ht ("emmy/progressReport" #'ignore))))


;; R
(defgroup lsp-r nil
  "LSP support for R."
  :group 'lsp-mode
  :link '(url-link "https://github.com/REditorSupport/languageserver"))

(defcustom lsp-clients-r-server-command '("R" "--slave" "-e" "languageserver::run()")
  "Command to start the R language server."
  :group 'lsp-r
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-r-server-command)
                  :major-modes '(ess-r-mode)
                  :server-id 'lsp-r))


;; Crystal
(defgroup lsp-crystal nil
  "LSP support for Crystal via scry."
  :group 'lsp-mode
  :link '(url-link "https://github.com/crystal-lang-tools/scry"))

(defcustom lsp-clients-crystal-executable '("scry" "--stdio")
  "Command to start the scry language server."
  :group 'lsp-crystal
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-crystal-executable)
                  :major-modes '(crystal-mode)
                  :server-id 'scry))


;; Nim
(defgroup lsp-nim nil
  "LSP support for Nim, using nimlsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/PMunch/nimlsp"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
                  :major-modes '(nim-mode)
                  :priority -1
                  :server-id 'nimls))

;; Dhall
(defgroup lsp-dhall nil
  "LSP support for Dhall, using dhall-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/dhall-lang/dhall-haskell"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "dhall-lsp-server")
                  :major-modes '(dhall-mode)
                  :priority -1
                  :server-id 'dhallls))

;; CMake
(defgroup lsp-cmake nil
  "LSP support for CMake, using cmake-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/regen100/cmake-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                  :major-modes '(cmake-mode)
                  :priority -1
                  :server-id 'cmakels))

;; PureScript
(defgroup lsp-purescript nil
  "LSP support for PureScript, using purescript-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nwolverson/purescript-language-server"))

(defcustom lsp-purescript-server-executable
  "purescript-language-server"
  "Arguments to pass to the server."
  :type 'string
  :risky t
  :group 'lsp-purescript)

(defcustom lsp-purescript-server-args
  '("--stdio")
  "Arguments to pass to the server."
  :type '(repeat string)
  :risky t
  :group 'lsp-purescript)

(defun lsp-purescript--server-command ()
  "Generate LSP startup command for purescript-language-server."
  (cons lsp-purescript-server-executable
        lsp-purescript-server-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-purescript--server-command)
                  :major-modes '(purescript-mode)
                  :priority -1
                  :server-id 'pursls))

;;; Rf
(defgroup lsp-rf nil
  "Settings for Robot Framework Language Server."
  :group 'lsp-mode
  :tag "Language Server"
  :link '(url-link "https://github.com/tomi/vscode-rf-language-server.git"))

(defcustom lsp-rf-language-server-start-command '("~/.nvm/versions/node/v9.11.2/bin/node" "~/.vscode/extensions/tomiturtiainen.rf-intellisense-2.8.0/server/server.js")
  "Path to the server.js file of the rf-intellisense server. Accepts a list of strings (path/to/interpreter path/to/server.js)"
  :type 'list
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-include-paths []
  "An array of files that should be included by the parser. Glob patterns as strings are accepted (eg. *.robot between double quotes)"
  :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-exclude-paths []
  "An array of files that should be ignored by the parser. Glob patterns as strings are accepted (eg. *bad.robot between double quotes)"
  :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-dir "~/.vscode/extensions/tomiturtiainen.rf-intellisense-2.8.0/server/library-docs/"
  "Libraries directory for libraries in lsp-rf-language-server-libraries"
  :type 'string
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-libraries ["BuiltIn-3.1.1" "Collections-3.0.4"]
  "Libraries whose keywords are suggested with auto-complete"
  :type '(repeat string)
  ;; :type 'lsp-string-vector
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-log-level "debug"
  "What language server log messages are printed"
  :type 'string
  ;; :type '(choice (:tag "off" "errors" "info" "debug"))
  :group 'lsp-rf)

(defcustom lsp-rf-language-server-trace-server "verbose"
  "Traces the communication between VSCode and the rfLanguageServer service."
  :type 'string
  ;; :type '(choice (:tag "off" "messages" "verbose"))
  :group 'lsp-rf)

(defun parse-rf-language-server-library-dirs (dirs)
  (vconcat (mapcar
   (lambda (x)
     (concat
      (expand-file-name
       lsp-rf-language-server-dir)
      x
      ".json"))
   dirs)))

(defun expand-start-command ()
  (mapcar 'expand-file-name lsp-rf-language-server-start-command))

(defun parse-rf-language-server-globs-to-regex (vector)
  "Converts vector with globs to regex"
    (concat "\\("(mapconcat 'eshell-glob-regexp vector "\\|") "\\)"))

(defun parse-rf-language-server-include-path-regex (vector)
  "Creates regexp to select files from workspace directory"
  (let ((globs (if (eq vector [])
                        ["*.robot" "*.resource"]
                      vector)))
    (parse-rf-language-server-globs-to-regex globs)))

(defun parse-rf-language-server-exclude-paths (seq)
  "Creates regexp to select files from workspace directory"
  (if (eq lsp-rf-language-server-exclude-paths [])
      seq
  (cl-delete-if (lambda (x) (string-match-p
                             (parse-rf-language-server-globs-to-regex
                              lsp-rf-language-server-exclude-paths)
                             x))
                seq)))

(lsp-register-custom-settings
 '(
   ("rfLanguageServer.trace.server" lsp-rf-language-server-trace-server)
   ("rfLanguageServer.logLevel" lsp-rf-language-server-log-level)
   ("rfLanguageServer.libraries" lsp-rf-language-server-libraries)
   ("rfLanguageServer.excludePaths" lsp-rf-language-server-exclude-paths)
   ("rfLanguageServer.includePaths" lsp-rf-language-server-include-paths)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (expand-start-command))
                  :major-modes '(robot-mode)
                  :server-id 'rf-intellisense
                  ;; :library-folders-fn (lambda (_workspace)
                  ;;                        lsp-rf-language-server-libraries)
                  :library-folders-fn (lambda (_workspace)
                                         (parse-rf-language-server-library-dirs
                                         lsp-rf-language-server-libraries))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "rfLanguageServer"))
                                      (lsp-request "buildFromFiles"
                                                   (list :files
                                                         (vconcat
                                                          (parse-rf-language-server-exclude-paths
                                                           (directory-files-recursively
                                                            (lsp--workspace-root workspace)
                                                            (parse-rf-language-server-include-path-regex
                                                             lsp-rf-language-server-include-paths))))))))))

(provide 'lsp-clients)
;;; lsp-clients.el ends here
