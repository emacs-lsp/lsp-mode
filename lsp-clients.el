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
(require 'lsp-pyls)
(require 'lsp-rust)
(require 'lsp-solargraph)
(require 'lsp-vetur)
(require 'lsp-intelephense)
(require 'lsp-css)
(require 'lsp-xml)
(require 'lsp-go)
(require 'lsp-clojure)

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

(defun lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
  "Check if the javascript-typescript language server should be enabled based on FILENAME."
  (string-match-p (rx (one-or-more char) "." (or "ts" "js") (opt "x") string-end) filename))

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
       (lsp-clients-flow-project-p file-name)
       (lsp-clients-flow-tag-file-present-p file-name)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons lsp-clients-flow-server
                                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))


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


;; Hack
(defgroup lsp-hack nil
  "Hack."
  :group 'lsp-mode
  :tag "Hack")

(defcustom lsp-clients-hack-command '("hh_client" "lsp" "--from" "emacs")
  "hh_client command."
  :group 'lsp-hack
  :risky t
  :type 'list)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-hack-command))
                  :major-modes '(hack-mode)
                  :priority -1
                  :server-id 'hack
                  ;; ignore some unsupported messages from Nuclide
                  :notification-handlers (lsp-ht ("telemetry/event" 'ignore)
                                                 ("$/cancelRequest" 'ignore))
                  :request-handlers (lsp-ht ("window/showStatus" 'ignore))))


(provide 'lsp-clients)
;;; lsp-clients.el ends here
