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
                  :priority -3
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

(defcustom lsp-clients-clangd-executable nil
  "The clangd executable to use.
When `'non-nil' use the name of the clangd executable file
available in your path to use. Otherwise the system will try to
find a suitable one. Set this variable before loading lsp."
  :group 'lsp-clangd
  :risky t
  :type 'file)

(defvar lsp-clients--clangd-default-executable nil
  "Clang default executable full path when found.
This must be set only once after loading the clang client.")

(defcustom lsp-clients-clangd-args '()
  "Extra arguments for the clangd executable."
  :group 'lsp-clangd
  :risky t
  :type '(repeat string))

(defun lsp-clients--clangd-command ()
  "Generate the language server startup command."
  (unless lsp-clients--clangd-default-executable
    (setq lsp-clients--clangd-default-executable
          (catch 'path
            (mapc (lambda (suffix)
                    (let ((path (executable-find (concat "clangd" suffix))))
                      (when path (throw 'path path))))
                  '("" "-10" "-9" "-8" "-7" "-6")))))

  `(,(or lsp-clients-clangd-executable lsp-clients--clangd-default-executable)
    ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--clangd-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd))

(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql clangd)))
  "Extract a representative line from clangd's CONTENTS, to show in the echo area.
This function tries to extract the type signature from CONTENTS,
or the first line if it cannot do so. A single line is always
returned to avoid that the echo area grows uncomfortably."
  (with-temp-buffer
    (-let [value (lsp:markup-content-value contents)]
      (insert value)
      (goto-char (point-min))
      (if (re-search-forward (rx (seq "```cpp\n"
                                      (opt (group "//"
                                                  (zero-or-more nonl)
                                                  "\n"))
                                      (group
                                       (one-or-more
                                        (not (any "`")))
                                       "\n")
                                      "```")) nil t nil)
          (progn (narrow-to-region (match-beginning 2) (match-end 2))
                 (lsp--render-element (lsp-join-region (point-min) (point-max))))
        (car (s-lines (lsp--render-element contents)))))))

;; Elixir
(defgroup lsp-elixir nil
  "LSP support for Elixir, using elixir-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/elixir-lsp/elixir-ls"))

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
                                   (and (string-match-p "\\.html\\'" (buffer-file-name))
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

;; PureScript
(defgroup lsp-purescript nil
  "LSP support for PureScript, using purescript-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nwolverson/purescript-language-server"))

(defcustom lsp-purescript-server-executable nil
  "Path to server executable."
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
  (cons (or lsp-purescript-server-executable
            (lsp-package-path 'purescript-language-server))
        lsp-purescript-server-args))

(lsp-dependency 'purescript-language-server
                '(:system "purescript-language-server")
                '(:npm :package "purescript-language-server"
                       :path "purescript-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-purescript--server-command)
  :major-modes '(purescript-mode)
  :priority -1
  :server-id 'pursls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'purescript-language-server callback error-callback))))


(provide 'lsp-clients)
;;; lsp-clients.el ends here
