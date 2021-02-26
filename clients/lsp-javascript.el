;;; lsp-javascript.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp,

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

;; LSP Clients for the JavaScript and TypeScript Programming Languages.

;;; Code:

(require 'lsp-mode)

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
  (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
      (and (derived-mode-p 'js-mode 'typescript-mode)
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


(defgroup lsp-typescript nil
  "LSP support for TypeScript, using Theia/Typefox's TypeScript Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/theia-ide/typescript-language-server"))

(defcustom lsp-clients-typescript-tls-path "typescript-language-server"
  "Path to the typescript-language-server binary."
  :group 'lsp-typescript
  :risky t
  :type 'string)

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
         :location \"<path>.vscode/extensions/visualstudioexptteam.
                            vscodeintellicode-1.1.9/\"))"
  :group 'lsp-typescript
  :type  '(restricted-sexp :tag "Vector"
                           :match-alternatives
                           (lambda (xs)
                             (and (vectorp xs) (seq-every-p
                                                (-lambda ((&plist :name :location))
                                                  (and name location))
                                                xs)))))

(lsp-dependency 'typescript-language-server
                '(:system lsp-clients-typescript-tls-path)
                '(:npm :package "typescript-language-server"
                       :path "typescript-language-server"))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(defun lsp-javascript--rename (_workspace args)
  (let ((path (lsp--uri-to-path (lsp-get (lsp-get args :textDocument) :uri))))
    (if (f-exists? path)
        (with-current-buffer (find-file path)
          (goto-char (lsp--position-to-point
                      (lsp-get args :position))))
      (error "There is no file %s" path)))
  (call-interactively #'lsp-rename)
  nil)

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
                  :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'typescript
                                         (-partial #'lsp-package-ensure
                                                   'typescript-language-server
                                                   callback
                                                   error-callback)
                                         error-callback))))


(defgroup lsp-flow nil
  "LSP support for the Flow Javascript type checker."
  :group 'lsp-mode
  :link '(url-link "https://flow.org"))

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
        (unless (re-search-forward "[^\n[:space:]]" nil t)
          (setq stop t))
        (if (= (point) (point-min)) (setq stop t) (backward-char))
        (cond ((or (looking-at "//+[ ]*@flow")
                   (looking-at "/\\**[ ]*@flow")
                   (looking-at "[ ]*\\*[ ]*@flow"))
               (setq found t) (setq stop t))
              ((or (looking-at "//") (looking-at "*"))
               (forward-line))
              ((looking-at "/\\*")
               (save-excursion
                 (unless (re-search-forward "*/" nil t) (setq stop t)))
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
       (not (derived-mode-p 'json-mode))
       (or (lsp-clients-flow-project-p file-name)
           (lsp-clients-flow-tag-file-present-p file-name))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection (lambda ()
                                          (cons lsp-clients-flow-server
                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))

(defgroup lsp-deno nil
  "LSP support for the Deno language server."
  :group 'lsp-mode
  :link '(url-link "https://deno.land/"))

(defcustom lsp-clients-deno-server "deno"
  "The Deno executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-server-args '("lsp")
  "Extra arguments for starting the Deno language server."
  :group 'lsp-deno
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-enable-lint t
  "Controls if linting information will be provided by the Deno Language Server."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-enable-code-lens-references t
  "Enables or disables the display of code lens information."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-enable-code-lens-references-all-functions t
  "Enables or disables the display of code lens information for all functions.
Setting this variable to `non-nil' implicitly enables
`lsp-clients-deno-enable-code-lens-references'."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-enable-code-lens-implementations t
  "Enables or disables the display of code lens information for implementations."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-config nil
  "The file path to a tsconfig.json file.
The path can be either be relative to the workspace, or an
absolute path.

Examples: `./tsconfig.json',
`/path/to/tsconfig.json', `C:\\path\\to\\tsconfig.json'"
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-import-map nil
  "The file path to an import map.
Import maps provide a way to relocate modules based on their
specifiers.  The path can either be relative to the workspace, or
an absolute path.

Examples: `./import-map.json',
`/path/to/import-map.json', `C:\\path\\to\\import-map.json'."
  :group 'lsp-deno
  :risky t
  :type 'file
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-clients-deno-enable-unstable nil
  "Controls if code will be type checked with Deno's unstable APIs."
  :group 'lsp-deno
  :risky t
  :type 'boolean
  :package-version '(lsp-mode . "7.1.0"))

(defun lsp-clients-deno--make-init-options ()
  "Initialization options for the Deno language server."
  `(:enable t
    :config ,lsp-clients-deno-config
    :importMap ,lsp-clients-deno-import-map
    :lint ,(lsp-json-bool lsp-clients-deno-enable-lint)
    :unstable ,(lsp-json-bool lsp-clients-deno-enable-unstable)
    :codeLens (:implementations ,(lsp-json-bool lsp-clients-deno-enable-code-lens-implementations)
               :references ,(lsp-json-bool (or lsp-clients-deno-enable-code-lens-references
                                               lsp-clients-deno-enable-code-lens-references-all-functions))
               :referencesAllFunctions ,(lsp-json-bool lsp-clients-deno-enable-code-lens-references-all-functions))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection (lambda ()
                                          (cons lsp-clients-deno-server
                                                lsp-clients-deno-server-args)))
                  :initialization-options #'lsp-clients-deno--make-init-options
                  :priority -5
                  :activation-fn #'lsp-typescript-javascript-tsx-jsx-activate-p
                  :server-id 'deno-ls))

(provide 'lsp-javascript)
;;; lsp-javascript.el ends here
