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
  (if-let ((buffer (find-buffer-visiting file-name)))
      (with-current-buffer buffer
        (lsp-clients-flow-tag-string-present-p))
    (with-temp-buffer
      (insert-file-contents file-name)
      (lsp-clients-flow-tag-string-present-p))))

(defun lsp-clients-flow-tag-string-present-p ()
  "Helper for `lsp-clients-flow-tag-file-present-p' that works
with the file contents."
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
      found)))

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
