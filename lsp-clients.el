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
(defcustom lsp-clients-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :group 'lsp-python
  :risky t
  :type '(repeat string))

;;; pyls
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                  :major-modes '(python-mode)
                  :server-id 'pyls
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-python-library-directories)))

;;; CSS
(defun lsp-clients-css--apply-code-action (action)
  "Apply ACTION as workspace edit command."
  (lsp--apply-text-edits (caddr (gethash "arguments" action))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("css-languageserver" "--stdio"))
                  :major-modes '(css-mode)
                  :action-handlers (lsp-ht ("_css.applyCodeAction" 'lsp-clients-css--apply-code-action))
                  :server-id 'css-ls))

;;; Bash
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                  :major-modes '(sh-mode)
                  :server-id 'bash-ls))

;;; Groovy
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
                  :server-id 'groovy-ls))

;;; HTML
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("html-languageserver" "--stdio"))
                  :major-modes '(html-mode sgml-mode mhtml-mode web-mode)
                  :server-id 'html-ls))

;;; Typescript
(defcustom lsp-clients-typescript-server "javascript-typescript-stdio"
  "The typescript-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-typescript
  :risky t
  :type 'file)

(defcustom lsp-clients-typescript-server-args '()
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript
  :risky t
  :type '(repeat string))

(defun lsp-typescript--ls-command ()
  "Generate the language server startup command."
  `(,lsp-clients-typescript-server
    ,@lsp-clients-typescript-server-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-typescript--ls-command)
                  :major-modes '(typescript-mode js-mode js2-mode rjsx-mode)
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls))


;;; GO language

(defcustom lsp-clients-go-server "go-langserver"
  "The go-langageserver executable to use."
  :group 'lsp-go
  :risky t
  :type 'file)

(defcustom lsp-clients-go-executable-path (executable-find "go-langserver")
  "Path to the go-langserver executable."
  :type 'string
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-language-server-flags '("-gocodecompletion")
  "Extra arguments for the go-langserver."
  :type '(repeat string)
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-func-snippet-enabled t
  "Enable the returning of argument snippets on `func' completions, eg.
`func(foo string, arg2 bar)'.  Requires code completion to be enabled."
  :type 'bool
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-gocode-completion-enabled t
  "Enable code completion feature (using gocode)."
  :type 'bool
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
  :type 'bool
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-diagnostics-enabled t
  "Whether diagnostics are enabled."
  :type 'bool
  :group 'lsp-clients-go)

(defcustom lsp-clients-go-library-directories '("/usr")
  "List of directories which will be considered to be libraries."
  :group 'lsp-go
  :risky t
  :type '(repeat string))

(define-inline lsp-clients-go--bool-to-json (val)
  (inline-quote (if ,val t :json-false)))

(defun lsp-clients-go--make-init-options ()
  "Init options for golang."
  `(:funcSnippetEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-func-snippet-enabled)
                        :gocodeCompletionEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-gocode-completion-enabled)
                        :formatTool ,lsp-clients-go-format-tool
                        :goimportsLocalPrefix ,lsp-clients-go-imports-local-prefix
                        :maxParallelism ,lsp-clients-go-max-parallelism
                        :useBinaryPkgCache ,lsp-clients-go-use-binary-pkg-cache
                        :diagnosticsEnabled ,lsp-clients-go-diagnostics-enabled))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-go-server))
                  :major-modes '(go-mode)
                  :initialization-options 'lsp-clients-go--make-init-options
                  :server-id 'go-ls
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-go-library-directories)))


;; RUST
(defun lsp-clients--rust-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (message (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rls"))
                  :major-modes '(rust-mode)
                  :server-id 'rls
                  :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress))))

;; Ruby
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                  :major-modes '(ruby-mode)
                  :multi-root t
                  :server-id 'ruby-ls))

;; PHP
(defcustom lsp-clients-php-server-command
  `("php" ,(expand-file-name "~/.composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))
  "Install directory for php-language-server."
  :group 'lsp-php
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const lsp-clients-php-server-command))
                  :major-modes '(php-mode)
                  :server-id 'php-ls))



(defcustom lsp-ocaml-ocaml-lang-server-command
  '("ocaml-language-server" "--stdio")
  "The command that starts the language server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(defcustom lsp-ocaml-reason-lang-server-command
  '("ocaml-language-server" "--stdio")
  "The command that starts the language server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const lsp-ocaml-ocaml-lang-server-command))
                  :major-modes '(reason-mode caml-mode tuareg-mode)
                  :server-id 'ocaml-ls))


;; C-family (C, C++, Objective-C, Objective-C++)

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

(defun lsp-clients-register-clangd ()
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     'lsp-clients--clangd-command)
                    :major-modes '(c-mode c++-mode objc-mode)
                    :server-id 'clangd)))


;; Dart
(defcustom lsp-clients-dart-server-command
  (expand-file-name (if (equal system-type 'windows-nt)
                        "~/Pub/Cache/bin/dart_language_server"
                      "~/.pub-cache/bin/dart_language_server"))
  "The dart_language_server executable to use."
  :group 'lsp-dart
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   lsp-clients-dart-server-command)
                  :major-modes '(dart-mode)
                  :server-id 'dart_language_server))


(provide 'lsp-clients)
;;; lsp-clients.el ends here
