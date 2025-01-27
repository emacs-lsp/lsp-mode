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
(require 'lsp-completion)

(defgroup lsp-go nil
  "LSP support for the Go Programming Language, using the gopls language server."
  :link '(url-link "https://github.com/golang/tools/blob/master/gopls/README.md")
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3.2"))

(define-obsolete-variable-alias
  'lsp-gopls-server-path
  'lsp-go-gopls-server-path
  "lsp-mode 7.0.1")

(defcustom lsp-go-gopls-server-path "gopls"
  "Path to gopls server binary."
  :type 'string
  :group 'lsp-go)

(define-obsolete-variable-alias
  'lsp-gopls-server-args
  'lsp-go-gopls-server-args
  "lsp-mode 7.0.1")

(defcustom lsp-go-gopls-server-args '("-remote=auto")
  "Extra CLI arguments for gopls."
  :type '(repeat string)
  :group 'lsp-go)

(define-obsolete-variable-alias
  'lsp-gopls-use-placeholders
  'lsp-go-use-placeholders
  "lsp-mode 7.0.1")

(defcustom lsp-go-use-placeholders t
  "Cause gopls to provide placeholder parameter snippets when
completing function calls."
  :type 'boolean
  :group 'lsp-go)

(define-obsolete-variable-alias
  'lsp-gopls-build-flags
  'lsp-go-build-flags
  "lsp-mode 7.0.1")

(defcustom lsp-go-build-flags []
  "A vector of flags passed on to the build system when invoked,
  applied to queries like `go list'."
  :type '(lsp-repeatable-vector string)
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "6.2"))

(define-obsolete-variable-alias
  'lsp-gopls-env
  'lsp-go-env
  "lsp-mode 7.0.1")

(defcustom lsp-go-env nil
  "`gopls' has the unusual ability to set environment variables,
  intended to affect the behavior of commands invoked by `gopls'
  on the user's behalf. This variable takes a hash table of env
  var names to desired values."
  :type '(alist :key-type (symbol :tag "env var name") :value-type (string :tag "value"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "6.2"))

(defcustom lsp-go-directory-filters []
  "A vector of directory filters."
  :link '(url-link "https://github.com/golang/tools/blob/67e49ef2d0f326051e22a4a55bdf9344ae1a8ed8/gopls/doc/settings.md#directoryfilters-string")
  :group 'lsp-go
  :type 'lsp-string-vector
  :package-version '(lsp-mode "8.0.0"))

(define-obsolete-variable-alias
  'lsp-gopls-hover-kind
  'lsp-go-hover-kind
  "lsp-mode 7.0.1")

(defcustom lsp-go-hover-kind "SynopsisDocumentation"
  "`gopls' allows the end user to select the desired amount of
  documentation returned during e.g. hover and thing-at-point
  operations."
  :type '(choice (const "SynopsisDocumentation")
                 (const "NoDocumentation")
                 (const "FullDocumentation")
                 (const "SingleLine")
                 (const "Structured"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "6.2"))

(define-obsolete-variable-alias
  'lsp-gopls-available-codelens
  'lsp-go-available-codelenses
  "lsp-mode 7.0.1")

(define-obsolete-variable-alias
  'lsp-go-available-codelens
  'lsp-go-available-codelenses
  "lsp-mode 7.0.1")

(defvar lsp-go-available-codelenses
  '(
    (gc_details . "Toggle the calculation of gc annotations")
    (generate . "Run `go generate` for a directory")
    (regenerate_cgo . "Regenerate cgo definitions")
    (test . "Run `go test` for a specific set of test or benchmark functions (legacy)")
    (tidy . "Run `go mod tidy` for a module")
    (upgrade_dependency . "Upgrade a dependency")
    (vendor . "Runs `go mod vendor' for a module"))
  "Available codelenses that can be further enabled or disabled
  through `lsp-go-codelenses'.")

(defun lsp-go--defcustom-available-as-alist-type (alist)
  "Return a list for the `:type' field in `defcustom' used to populate an alist.

The input ALIST has the form `((\"name\" . \"documentation sentence\") [...])'

The returned type provides a tri-state that either:
  - does not include the element in the alist
  - sets element to false (actually, :json-false)
  - sets element to true \(actually, t)"
  (let ((list '()))
    (dolist (v alist)
      (push `(cons
              :tag ,(cdr v)
              (const :format "" ,(car v))
              (choice (const :tag "Enable" t) (const :tag "Disable" :json-false)))
            list))
    (push 'set list)
    list))

(define-obsolete-variable-alias
  'lsp-gopls-codelens
  'lsp-go-codelenses
  "lsp-mode 7.0.1")

(define-obsolete-variable-alias
  'lsp-go-codelens
  'lsp-go-codelenses
  "lsp-mode 7.0.1")

(defcustom lsp-go-codelenses '((gc_details . :json-false)
                               (generate . t)
                               (regenerate_cgo . t)
                               (tidy . t)
                               (upgrade_dependency . t)
                               (test . t)
                               (vendor . t))
  "Select what codelenses should be enabled or not.

The codelenses can be found at https://github.com/golang/tools/blob/3fa0e8f87c1aae0a9adc2a63af1a1945d16d9359/internal/lsp/source/options.go#L106-L112."
  :type (lsp-go--defcustom-available-as-alist-type lsp-go-available-codelenses)
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "7.0"))

(define-obsolete-variable-alias
  'lsp-clients-go-library-directories
  'lsp-go-library-directories
  "lsp-mode 7.0.1")

(defcustom lsp-go-library-directories ["/usr"]
  "List of directories which will be considered to be libraries."
  :group 'lsp-go
  :risky t
  :type '(lsp-repeatable-vector string))

(define-obsolete-variable-alias
  'lsp-clients-go-library-directories-include-go-modules
  'lsp-go-library-directories-include-go-modules
  "lsp-mode 7.0.1")

(defcustom lsp-go-library-directories-include-go-modules t
  "Whether or not $GOPATH/pkg/mod should be included as a library directory."
  :type 'boolean
  :group 'lsp-go)

(defun lsp-go--library-default-directories (_workspace)
  "Calculate go library directories.

If `lsp-go-library-directories-include-go-modules' is non-nil
and the environment variable GOPATH is set this function will return
$GOPATH/pkg/mod along with the value of
`lsp-go-library-directories'."
  (let ((library-dirs lsp-go-library-directories))
    (when (and lsp-go-library-directories-include-go-modules
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

(defcustom lsp-go-link-target "pkg.go.dev"
  "Which website to use for displaying Go documentation."
  :type '(choice (const "pkg.go.dev")
                 (const "godoc.org")
                 (string :tag "A custom website"))
  :group 'lsp-go
  :package-version '(lsp-mode "7.0.1"))

(defcustom lsp-go-links-in-hover t
  "If non-nil, hover documentation includes links."
  :type 'boolean
  :group 'lsp-go
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-use-gofumpt nil
  "If non-nil, use gofumpt formatting."
  :type 'boolean
  :group 'lsp-go
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-goimports-local ""
  "Equivalent of the goimports -local flag, which puts imports beginning with
 this string after third-party packages.  It should be the prefix of the import
 path whose imports should be grouped separately."
  :type 'string
  :group 'lsp-go
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-analyses nil
  "Specify analyses that the user would like to enable or disable. A map of the
  names of analysis passes that should be enabled/disabled. A full list of
  analyzers that gopls uses can be found at
  https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md"
  :type '(alist :key-type (string :tag "analyzer name") :value-type (boolean :tag "value"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-import-shortcut "Both"
  "Specifies whether import statements should link to documentation or go to
  definitions."
  :type '(choice (const "Both")
                 (const "Link")
                 (const "Definition"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-symbol-matcher "FastFuzzy"
  "Sets the algorithm that is used when finding workspace symbols."
  :type '(choice (const "Fuzzy")
                 (const "FastFuzzy")
                 (const "CaseInsensitive")
                 (const "CaseSensitive"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-symbol-style "Dynamic"
  "Controls how symbols are qualified in symbol responses.

  `Dynamic' uses whichever qualifier results in the highest scoring match for
  the given symbol query. Here a `qualifier' is any `/' or '.' delimited suffix
  of the fully qualified symbol. i.e. `to/pkg.Foo.Field' or just `Foo.Field'.

  `Full' is fully qualified symbols, i.e. `path/to/pkg.Foo.Field'.

  `Package' is package qualified symbols i.e. `pkg.Foo.Field'."
  :type '(choice (const "Dynamic")
                 (const "Full")
                 (const "Package"))
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "8.0.0"))

(defcustom lsp-go-template-extensions []
  "The extensions of file names that are treated as template files.

The extension is the part of the file name after the final dot."
  :type '(lsp-repeatable-vector string)
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-standalone-tags ["ignore"]
  "Specifies a set of build constraints that identify individual Go
source files that make up the entire main package of an
executable."
  :type '(lsp-repeatable-vector string)
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-completion-budget "100ms"
  "Soft latency goal for completion requests"
  :type 'string
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-matcher "Fuzzy"
  "Sets the algorithm that is used when calculating completion candidates."
  :type '(choice (const "CaseInsensitive")
                 (const "CaseSensitive")
                 (const "Fuzzy"))
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-complete-function-calls t
  "Enables function call completion.

When completing a statement, or when a function return type
matches the expected of the expression being completed,
completion may suggest call expressions."
  :type 'boolean
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-diagnostics-delay "1s"
  "Controls the amount of time that gopls waits after the most
recent file modification before computing deep diagnostics."
  :type 'string
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-analysis-progress-reporting t
  "Controls whether gopls sends progress notifications when
construction of its index of analysis facts is taking a long
time."
  :type 'boolean
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-symbol-scope "all"
  "Controls which packages are searched for workspace/symbol
requests.

When the scope is \"workspace\", gopls searches only workspace
packages.

When the scope is \"all\", gopls searches all loaded packages,
including dependencies and the standard library."
  :type '(choice (const "all")
                 (const "workspace"))
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(defcustom lsp-go-verbose-output t
  "Enables additional debug logging."
  :type 'boolean
  :group 'lsp-go
  :package-version '(lsp-mode "9.1"))

(lsp-register-custom-settings
 '(("gopls.analyses" lsp-go-analyses)
   ("gopls.analysisProgressReporting" lsp-go-analysis-progress-reporting t)
   ("gopls.buildFlags" lsp-go-build-flags)
   ("gopls.codelenses" lsp-go-codelenses)
   ("gopls.completeFunctionCalls" lsp-go-complete-function-calls t)
   ("gopls.completionBudget" lsp-go-completion-budget)
   ("gopls.diagnosticsDelay" lsp-go-diagnostics-delay)
   ("gopls.directoryFilters" lsp-go-directory-filters)
   ("gopls.env" lsp-go-env)
   ("gopls.gofumpt" lsp-go-use-gofumpt t)
   ("gopls.hoverKind" lsp-go-hover-kind)
   ("gopls.importShortcut" lsp-go-import-shortcut)
   ("gopls.linkTarget" lsp-go-link-target)
   ("gopls.linksInHover" lsp-go-links-in-hover t)
   ("gopls.local" lsp-go-goimports-local)
   ("gopls.matcher" lsp-go-matcher)
   ("gopls.standaloneTags" lsp-go-standalone-tags)
   ("gopls.symbolMatcher" lsp-go-symbol-matcher)
   ("gopls.symbolScope" lsp-go-symbol-scope)
   ("gopls.symbolStyle" lsp-go-symbol-style)
   ("gopls.templateExtensions" lsp-go-template-extensions)
   ("gopls.usePlaceholders" lsp-go-use-placeholders t)
   ("gopls.verboseOutput" lsp-go-verbose-output t)))

(defcustom lsp-go-server-wrapper-function
  #'identity
  "Function to wrap the language server process started by lsp-go.

For example, you can pick a go binary provided by a repository's
flake.nix file with:

  (use-package nix-sandbox)
  (defun my/nix--lsp-go-wrapper (args)
    (if-let* ((sandbox (nix-current-sandbox)))
        (apply `nix-shell-command sandbox args)
      args))
  (setq lsp-go-server-path \"gopls\"
        lsp-go-server-wrapper-function `my/nix--lsp-go-wrapper)"
  :group 'lsp-go
  :type '(choice
          (function-item :tag "None" :value identity)
          (function :tag "Custom function")))

(defun lsp-go--server-command ()
  "Command and arguments for launching the inferior language server process.
These are assembled from the customizable variables `lsp-go-server-path'
and `lsp-go-server-wrapper-function'."
  (funcall lsp-go-server-wrapper-function (append (list lsp-go-gopls-server-path) lsp-go-gopls-server-args)))

(defun lsp-go--cls-download-server (_client callback error-callback _update?)
  "Install/update shader-ls language server using `go install'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t"
  (lsp-async-start-process
   callback
   error-callback
   "go" "install" "golang.org/x/tools/gopls@latest"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-go--server-command)
                  :activation-fn (lsp-activate-on "go" "go.mod")
                  :language-id "go"
                  :priority 0
                  :server-id 'gopls
                  :completion-in-comments? t
                  :library-folders-fn #'lsp-go--library-default-directories
                  :after-open-fn (lambda ()
                                   ;; https://github.com/golang/tools/commit/b2d8b0336
                                   (setq-local lsp-completion-filter-on-incomplete nil))
                  :download-server-fn #'lsp-go--cls-download-server))

(lsp-consistency-check lsp-go)

(provide 'lsp-go)
;;; lsp-go.el ends here
