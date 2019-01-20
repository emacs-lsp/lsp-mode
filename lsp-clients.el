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
                  :priority -1
                  :server-id 'pyls
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-python-library-directories)))

;;; CSS
(defun lsp-clients-css--apply-code-action (action)
  "Apply ACTION as workspace edit command."
  (lsp--apply-text-edits (caddr (gethash "arguments" action))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("css-languageserver" "--stdio"))
                  :major-modes '(css-mode less-mode sass-mode scss-mode)
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

;;; Typescript/Javascript
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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const `(,lsp-clients-javascript-typescript-server
                                             ,@lsp-clients-typescript-javascript-server-args)))
                  :major-modes '(typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode)
                  :priority -2
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'jsts-ls))


;;; Typescript/Javascript
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
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const `(,lsp-clients-typescript-server
                                             ,@lsp-clients-typescript-server-args)))
                  :major-modes '(typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode)
                  :priority -1
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls))



;;; JavaScript Flow
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

(defun lsp-clients-flow-tag-present-p (file-name)
  "Checks if the '// @flow' or `/* @flow */' tag is present in
the contents of FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
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
                     (looking-at "/\\**[ ]*@flow"))
                 (setq found t)
                 (setq stop t))
                ((looking-at "//")
                 (forward-line))
                ((looking-at "/\\*")
                 (when (not (re-search-forward "*/" nil t))
                   (setq stop t)))
                (t (setq stop t))))
        found))))

(defun lsp-clients-flow-project-p (file-name)
  "Checks if FILE-NAME is part of a Flow project, that is, if
there is a .flowconfig file in the folder hierarchy."
  (locate-dominating-file file-name ".flowconfig"))

(defun lsp-clients-flow-activate-p (file-name major-mode)
  "Checks if the Flow language server should be enabled for a
particular FILE-NAME and MAJOR-MODE."
  (and (member major-mode '(js-mode js2-mode flow-js2-mode))
       (lsp-clients-flow-project-p file-name)
       (lsp-clients-flow-tag-present-p file-name)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const `(,lsp-clients-flow-server
                                             ,@lsp-clients-flow-server-args)))
                  :activation-fn 'lsp-clients-flow-activate-p
                  :add-on? t
                  :server-id 'flow-ls))

;;; Vue
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

(defcustom lsp-clients-go-server "bingo"
  "The go-langageserver executable to use."
  :group 'lsp-go
  :risky t
  :type 'file)

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
  (message (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rls"))
                  :major-modes '(rust-mode rustic-mode)
                  :priority -1
                  :server-id 'rls
                  :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress))))

;; Ruby
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                  :major-modes '(ruby-mode)
                  :priority -1
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
                  :priority -1
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
                  :priority -1
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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--clangd-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd))

(defun lsp-clients-register-clangd ()
  (warn "This call is no longer needed. clangd is now automatically registered. Delete lsp-clients-register-clangd call from your config."))


;; Dart
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
(defcustom lsp-clients-elixir-server-executable "language_server.sh"
  "The elixir-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-elixir
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (-const lsp-clients-elixir-server-executable))
                  :major-modes '(elixir-mode)
                  :priority -1
                  :server-id 'elixir-ls))

;; Fortran
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

(defun fortls--suggest-project-root ()
  (and (memq major-mode '(f90-mode))
       (when-let (dir (locate-dominating-file default-directory ".fortls"))
         (expand-file-name dir))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--fortls-command)
                  :major-modes '(f90-mode)
                  :priority -1
                  :server-id 'fortls))


(provide 'lsp-clients)
;;; lsp-clients.el ends here
