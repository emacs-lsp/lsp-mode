;;; lsp.el --- LSP mode                              -*- lexical-binding: t; -*-

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

(require 'lsp)
(require 'dash)
(require 'dash-functional)


;; Python

(defgroup lsp-python nil
  "Python."
  :group 'lsp-mode
  :tag "Python")

(defcustom lsp-clients-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :group 'lsp-python
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-python-command '("pyls")
  "PYLS command."
  :group 'lsp-python
  :risky t
  :type 'list)

(defcustom lsp-clients-python-settings
  '(
    :plugins.jedi_completion.enabled t
    :plugins.jedi_definition.follow_imports t
    ;; List of configuration sources to use. (enum: ["pycodestyle" "pyflakes"])
    :configurationSources ["pycodestyle"]
    ;; Enable or disable the plugin.
    :plugins.jedi_completion.enabled t
    ;; Enable or disable the plugin.
    :plugins.jedi_definition.enabled t
    ;; The goto call will follow imports.
    :plugins.jedi_definition.follow_imports nil
    ;; If follow_imports is True will decide if it follow builtin imports.
    :plugins.jedi_definition.follow_builtin_imports nil
    ;; Enable or disable the plugin.
    :plugins.jedi_hover.enabled t
    ;; Enable or disable the plugin.
    :plugins.jedi_references.enabled t
    ;; Enable or disable the plugin.
    :plugins.jedi_signature_help.enabled nil
    ;; Enable or disable the plugin.
    :plugins.jedi_symbols.enabled nil
    ;; If True lists the names of all scopes instead of only the module namespace.
    :plugins.jedi_symbols.all_scopes t
    ;; Enable or disable the plugin.
    :plugins.mccabe.enabled nil
    ;; The minimum threshold that triggers warnings about cyclomatic complexity.
    :plugins.mccabe.threshold 15
    ;; Enable or disable the plugin.
    :plugins.preload.enabled t
    ;; List of modules to import on startup
    :plugins.preload.modules nil
    ;; Enable or disable the plugin.
    :plugins.pycodestyle.enabled t
    ;; Exclude files or directories which match these patterns(list of strings).
    :plugins.pycodestyle.exclude nil
    ;;  When parsing directories, only check filenames matching these patterns.
    :plugins.pycodestyle.filename nil
    ;; Select errors and warnings (list of string)
    :plugins.pycodestyle.select nil
    ;; Ignore errors and warnings (list of string)
    :plugins.pycodestyle.ignore nil
    ;; Hang closing bracket instead of matching indentation of opening bracket's line.
    :plugins.pycodestyle.hangClosing nil
    ;; Set maximum allowed line length.
    :plugins.pycodestyle.maxLineLength nil
    ;; Enable or disable the plugin.
    :plugins.pydocstyle.enabled nil
    ;; Choose the basic list of checked errors by specifying an existing
    ;; convention. One of ("pep257","numpy")
    :plugins.pydocstyle.convention nil
    ;; Ignore errors and warnings in addition to the specified convention.
    :plugins.pydocstyle.addIgnore nil
    ;; Select errors and warnings in addition to the specified convention.
    :plugins.pydocstyle.addSelect nil
    ;; Ignore errors and warnings
    :plugins.pydocstyle.ignore nil
    ;; Select errors and warnings
    :plugins.pydocstyle.select nil
    ;; Check only files that exactly match the given regular expression; default
    ;; is to match files that don't start with 'test_' but end with '.py'.
    :plugins.pydocstyle.match "(?!test_).*\\.py"
    ;; Enable or disable the plugin.
    :plugins.pydocstyle.matchDir nil
    ;; Enable or disable the plugin.
    :plugins.pyflakes.enabled t
    ;; Enable or disable the plugin.
    :plugins.rope_completion.enabled t
    ;; Enable or disable the plugin.
    :plugins.yapf.enabled t
    ;; Builtin and c-extension modules that are allowed to be imported and inspected by rope.
    :rope.extensionModules nil
    ;; The name of the folder in which rope stores project configurations and
    ;; data. Pass `nil' for not using such a folder at all.
    :rope.ropeFolder nil)
  "Lsp clients configuration settings."
  :group 'lsp-python
  :risky t
  :type 'plist)

;;; pyls
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-python-command))
                  :major-modes '(python-mode)
                  :priority -1
                  :server-id 'pyls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration `(:pyls ,lsp-clients-python-settings))))
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-python-library-directories)))

;;; CSS
(defun lsp-clients-css--apply-code-action (action)
  "Apply ACTION as workspace edit command."
  (lsp--apply-text-edits (cl-caddr (gethash "arguments" action))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("css-languageserver" "--stdio"))
                  :major-modes '(css-mode less-mode less-css-mode sass-mode scss-mode)
                  :priority -1
                  :action-handlers (lsp-ht ("_css.applyCodeAction" 'lsp-clients-css--apply-code-action))
                  :server-id 'css-ls))

;;; Bash
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                  :major-modes '(sh-mode)
                  :priority -1
                  :server-id 'bash-ls))

;;; Groovy
(defgroup lsp-groovy nil
  "Groovy."
  :group 'lsp-mode
  :tag "Groovy")

(defcustom lsp-groovy-server-install-dir
  (locate-user-emacs-file "groovy-language-server/")
  "Install directory for groovy-language-server.
A slash is expected at the end.
This directory shoud contain a file matching groovy-language-server-*.jar"
  :group 'lsp-groovy
  :risky t
  :type 'directory)

(defun lsp-groovy--lsp-command ()
  "Generate LSP startup command."
  `("java"
    "-cp" ,(concat (file-truename lsp-groovy-server-install-dir) "*")
    "com.palantir.ls.groovy.GroovyLanguageServer"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-groovy--lsp-command)
                  :major-modes '(groovy-mode)
                  :priority -1
                  :server-id 'groovy-ls))

;;; HTML
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("html-languageserver" "--stdio"))
                  :major-modes '(html-mode sgml-mode mhtml-mode web-mode)
                  :priority -1
                  :server-id 'html-ls))

;;; TypeScript/JavaScript
(defgroup lsp-typescript-javascript nil
  "TypeScript/JavaScript."
  :group 'lsp-mode
  :tag "TypeScript/JavaScript")

(defcustom lsp-clients-javascript-typescript-server "javascript-typescript-stdio"
  "The javascript-typescript-stdio executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-typescript-javascript
  :risky t
  :type 'file)

(defcustom lsp-clients-typescript-javascript-server-args '()
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript-javascript
  :risky t
  :type '(repeat string))

(defun lsp-typescript-javascript-tsx-jsx-activate-p (filename mode)
  "Checks if the javascript-typescript language server should be enabled
based on FILE-NAME and MAJOR-MODE"
  (or (member mode '(typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode))
      (and (eq major-mode 'web-mode)
           (or (string-suffix-p ".tsx" filename t)
               (string-suffix-p ".jsx" filename t)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons lsp-clients-javascript-typescript-server
                                                                lsp-clients-typescript-javascript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -3
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'jsts-ls))


;;; TypeScript
(defgroup lsp-typescript nil
  "TypeScript."
  :group 'lsp-mode
  :tag "TypeScript")

(defcustom lsp-clients-typescript-server "typescript-language-server"
  "The typescript-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-typescript
  :risky t
  :type 'file)

(defcustom lsp-clients-typescript-server-args '("--stdio")
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons lsp-clients-typescript-server
                                                                lsp-clients-typescript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -2
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls))



;;; JavaScript Flow
(defgroup lsp-flow nil
  "JavaScript Flow."
  :group 'lsp-mode
  :tag "Flow")

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
  "Checks if the '// @flow' or `/* @flow */' tag is present in
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
  "Checks if FILE-NAME is part of a Flow project, that is, if
there is a .flowconfig file in the folder hierarchy."
  (locate-dominating-file file-name ".flowconfig"))

(defun lsp-clients-flow-activate-p (file-name _mode)
  "Checks if the Flow language server should be enabled for a
particular FILE-NAME and MODE."
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (lsp-clients-flow-project-p file-name)
       (lsp-clients-flow-tag-file-present-p file-name)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons lsp-clients-flow-server
                                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))

;;; Vue
(defgroup lsp-vue nil
  "Vue."
  :group 'lsp-mode
  :tag "Vue")

(defcustom lsp-clients-vue-server "vls"
  "The vue-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-vue
  :risky t
  :type 'file)

(defcustom lsp-clients-vue-server-args '()
  "Extra arguments for the vue-language-server language server."
  :group 'lsp-vue
  :risky t
  :type '(repeat string))

(defun lsp-vue--ls-command ()
  "Generate the language server startup command."
  `(,lsp-clients-vue-server
    ,@lsp-clients-vue-server-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-vue--ls-command)
                  :major-modes '(vue-mode)
                  :priority -1
                  :ignore-messages '("readFile .*? requested by Vue but content not available")
                  :server-id 'vls))


;;; GO language

(defgroup lsp-clients-go nil
  "Go language."
  :group 'lsp-mode
  :tag "Go language")

(defcustom lsp-clients-go-server "bingo"
  "The go language server executable to use."
  :group 'lsp-clients-go
  :risky t
  :type 'file)

(defcustom lsp-clients-go-server-args nil
  "Extra arguments for the go language server."
  :type '(repeat string)
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-func-snippet-enabled t
  "Enable the returning of argument snippets on `func' completions, eg.
`func(foo string, arg2 bar)'.  Requires code completion to be enabled."
  :type 'boolean
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-gocode-completion-enabled t
  "Enable code completion feature (using gocode)."
  :type 'boolean
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-format-tool "goimports"
  "The tool to be used for formatting documents.  Defaults to `goimports' if nil."
  :type '(choice (const :tag "goimports" "goimports")
                 (const :tag "gofmt" "gofmt"))
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-imports-local-prefix ""
  "The local prefix (comma-separated string) that goimports will use."
  :type 'string
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-max-parallelism nil
  "The maximum number of goroutines that should be used to fulfill requests.
This is useful in editor environments where users do not want results ASAP,
but rather just semi quickly without eating all of their CPU.  When nil,
defaults to half of your CPU cores."
  :type '(choice integer (const nil "Half of CPU cores."))
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-use-binary-pkg-cache t
  "Whether or not $GOPATH/pkg binary .a files should be used."
  :type 'boolean
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-diagnostics-enabled t
  "Whether diagnostics are enabled."
  :type 'boolean
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-library-directories '("/usr")
  "List of directories which will be considered to be libraries."
  :group 'lsp-clients-go
  :risky t
  :type '(repeat string))

(define-inline lsp-clients-go--bool-to-json (val)
  (inline-quote (if ,val t :json-false)))

(defun lsp-clients-go--make-init-options ()
  "Init options for golang."
  `(:funcSnippetEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-func-snippet-enabled)
    :disableFuncSnippet ,(lsp-clients-go--bool-to-json (not lsp-clients-go-func-snippet-enabled))
    :gocodeCompletionEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-gocode-completion-enabled)
    :formatTool ,lsp-clients-go-format-tool
    :goimportsLocalPrefix ,lsp-clients-go-imports-local-prefix
    :maxParallelism ,lsp-clients-go-max-parallelism
    :useBinaryPkgCache ,lsp-clients-go-use-binary-pkg-cache
    :diagnosticsEnabled ,lsp-clients-go-diagnostics-enabled))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (cons lsp-clients-go-server
                                                    lsp-clients-go-server-args)))
                  :major-modes '(go-mode)
                  :priority -1
                  :initialization-options 'lsp-clients-go--make-init-options
                  :server-id 'go-bingo
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-go-library-directories)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "go-langserver")
                  :major-modes '(go-mode)
                  :priority -2
                  :initialization-options 'lsp-clients-go--make-init-options
                  :server-id 'go-ls
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-go-library-directories)))


;; RUST
(defun lsp-clients--rust-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rls"))
                  :major-modes '(rust-mode rustic-mode)
                  :priority -1
                  :server-id 'rls
                  :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress))))

;; Ruby
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                  :major-modes '(ruby-mode enh-ruby-mode)
                  :priority -1
                  :multi-root t
                  :server-id 'ruby-ls))

;; PHP
(defgroup lsp-php nil
  "PHP."
  :group 'lsp-mode
  :tag "PHP")

(defcustom lsp-clients-php-server-command
  `("php" ,(expand-file-name "~/.composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))
  "Install directory for php-language-server."
  :group 'lsp-php
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-php-server-command))
                  :major-modes '(php-mode)
                  :priority -2
                  :server-id 'php-ls))



(defgroup lsp-ocaml nil
  "OCaml."
  :group 'lsp-mode
  :tag "OCaml")

(defcustom lsp-ocaml-ocaml-lang-server-command
  '("ocaml-language-server" "--stdio")
  "The command that starts the language server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-ocaml-ocaml-lang-server-command))
                  :major-modes '(reason-mode caml-mode tuareg-mode)
                  :priority -1
                  :server-id 'ocaml-ls))


;; C-family (C, C++, Objective-C, Objective-C++)

(defgroup lsp-clangd nil
  "C-family (C, C++, Objective-C, Objective-C++)"
  :group 'lsp-mode
  :tag "C-family")

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

(defun lsp-clients-register-clangd ()
  (warn "This call is no longer needed. clangd is now automatically registered. Delete lsp-clients-register-clangd call from your config."))


;; Dart
(defgroup lsp-dart nil
  "Dart."
  :group 'lsp-mode
  :tag "Dart")

(defcustom lsp-clients-dart-server-command
  (expand-file-name (if (equal system-type 'windows-nt)
                        "~/Pub/Cache/bin/dart_language_server"
                      "~/.pub-cache/bin/dart_language_server"))
  "The dart_language_server executable to use."
  :group 'lsp-dart
  :type 'file)

(defun lsp-dart--lsp-command ()
  "Generate LSP startup command."
  (let ((dls lsp-clients-dart-server-command)
        (pub (executable-find "pub")))
    (if pub
        (if (executable-find dls)
            dls
          (message "Installing dart_language_server...")
          (shell-command (concat pub " global activate dart_language_server"))
          (message "Installed dart_language_server")
          dls)
      (error "Please ensure /path/to/dart-sdk/bin is on system path"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-dart--lsp-command)
                  :major-modes '(dart-mode)
                  :priority -1
                  :server-id 'dart_language_server))


;; Elixir
(defgroup lsp-elixir nil
  "Elixir."
  :group 'lsp-mode
  :tag "Elixir")

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
                  :server-id 'elixir-ls))

;; Fortran
(defgroup lsp-fortran nil
  "Fortran."
  :group 'lsp-mode
  :tag "Fortran")

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
  "Kotlin."
  :group 'lsp-mode
  :tag "Kotlin")

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("kotlin-language-server"))
		              :major-modes '(kotlin-mode)
		              :priority -1
		              :server-id 'kotlin-ls))


;; PHP intelephense
(defgroup lsp-php-iph nil
  "PHP."
  :group 'lsp-mode
  :tag "PHP")

(defcustom lsp-clients-php-iph-server-command
  `("intelephense" "--stdio")
  "Install directory for php-language-server."
  :group 'lsp-php-ip
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-php-server-command))
                  :major-modes '(php-mode)
                  :priority -1
                  :server-id 'iph))


(provide 'lsp-clients)
;;; lsp-clients.el ends here
