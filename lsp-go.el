;;; lsp-go.el --- Go Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Muir Manders

;; Author: Muir Manders <muir@mnd.rs>
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

;; lsp-go client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-gopls nil
  "LSP support for the Go Programming Language, using the gopls language server."
  :link '(url-link "https://github.com/golang/go/wiki/gopls")
  :group 'lsp-mode)

(defcustom lsp-gopls-use-placeholders t
  "Cause gopls to provide placeholder parameter snippets when
completing function calls."
  :type 'boolean
  :group 'lsp-gopls)

(defcustom lsp-gopls-server-path "gopls"
  "Path to gopls server binary."
  :type 'string
  :group 'lsp-gopls)

(defcustom lsp-gopls-server-args nil
  "Extra CLI arguments for gopls."
  :type '(repeat string)
  :group 'lsp-gopls)

(defcustom lsp-gopls-build-flags []
  "A vector of flags passed on to the build system when invoked,
  applied to queries like `go list'."
  :type 'lsp-string-vector
  :group 'lsp-gopls
  :risky t
  :package-version '(lsp-mode "6.2"))

(defcustom lsp-gopls-env (make-hash-table)
  "`gopls' has the unusual ability to set environment variables,
  intended to affect the behavior of commands invoked by `gopls'
  on the user's behalf. This variable takes a hash table of env
  var names to desired values."
  :type '(restricted-sexp :match-alternatives (hash-table-p))
  :group 'lsp-gopls
  :risky t
  :package-version '(lsp-mode "6.2"))

(defcustom lsp-gopls-hover-kind "SynopsisDocumentation"
  "`gopls' allows the end user to select the desired amount of
  documentation returned during e.g. hover and thing-at-point
  operations."
  :type '(choice (const "SynopsisDocumentation")
                 (const "NoDocumentation")
                 (const "FullDocumentation")
                 (const "SingleLine")
                 (const "Structured"))
  :group 'lsp-gopls
  :risky t
  :package-version '(lsp-mode "6.2"))

(lsp-register-custom-settings
 '(("gopls.usePlaceholders" lsp-gopls-use-placeholders t)
   ("gopls.hoverKind" lsp-gopls-hover-kind)
   ("gopls.buildFlags" lsp-gopls-build-flags)
   ("gopls.env" lsp-gopls-env)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (cons lsp-gopls-server-path lsp-gopls-server-args)))
                  :major-modes '(go-mode)
                  :priority 0
                  :server-id 'gopls
                  :library-folders-fn 'lsp-clients-go--library-default-directories))

(defgroup lsp-clients-go nil
  "LSP support for the Go Programming Language."
  :group 'lsp-mode)

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

(defcustom lsp-clients-go-library-directories-include-go-modules t
  "Whether or not $GOPATH/pkg/mod should be included as a library directory."
  :type 'boolean
  :group 'lsp-clients-go)

(defun lsp-clients-go--library-default-directories (_workspace)
  "Calculate go library directories.

If `lsp-clients-go-library-directories-include-go-modules' is non-nil
and the environment variable GOPATH is set this function will return
$GOPATH/pkg/mod along with the value of
`lsp-clients-go-library-directories'."
  (let ((library-dirs lsp-clients-go-library-directories))
    (when (and lsp-clients-go-library-directories-include-go-modules
               (or (and (not (file-remote-p default-directory)) (executable-find "go"))
                   (and (version<= "27.0" emacs-version) (with-no-warnings (executable-find "go" (file-remote-p default-directory))))))
      (with-temp-buffer
        (when (zerop (process-file "go" nil t nil "env" "GOPATH"))
          (setq library-dirs
                (append
                 library-dirs
                 (list
                  (concat
                   (string-trim-right (buffer-substring (point-min) (point-max)))
                   "/pkg/mod")))))))
    (if (file-remote-p default-directory)
        (mapcar (lambda (path) (concat (file-remote-p default-directory) path)) library-dirs)
      library-dirs)))

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
                  :library-folders-fn 'lsp-clients-go--library-default-directories))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "go-langserver")
                  :major-modes '(go-mode)
                  :priority -2
                  :initialization-options 'lsp-clients-go--make-init-options
                  :server-id 'go-ls
                  :library-folders-fn 'lsp-clients-go--library-default-directories))

(provide 'lsp-go)
;;; lsp-go.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
