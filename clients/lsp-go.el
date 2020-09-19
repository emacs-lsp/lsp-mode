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

(defcustom lsp-go-gopls-server-args nil
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
  :type 'lsp-string-vector
  :group 'lsp-go
  :risky t
  :package-version '(lsp-mode "6.2"))

(define-obsolete-variable-alias
  'lsp-gopls-env
  'lsp-go-env
  "lsp-mode 7.0.1")

(defcustom lsp-go-env (make-hash-table)
  "`gopls' has the unusual ability to set environment variables,
  intended to affect the behavior of commands invoked by `gopls'
  on the user's behalf. This variable takes a hash table of env
  var names to desired values."
  :type '(alist :key-type (string :tag "env var name") :value-type (string :tag "value"))
  :group 'lsp-gopls
  :risky t
  :package-version '(lsp-mode "6.2"))

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
  'lsp-go-available-codelens
  "lsp-mode 7.0.1")

(defvar lsp-go-available-codelens
  '((generate . "Run `go generate` for a directory")
	  (test . "Run `go test` for a specific test function")
	  (tidy . "Run `go mod tidy` for a module")
	  (upgrade_dependency . "Upgrade a dependency")
	  (regenerate_cgo . "Regenerate cgo definitions"))
  "Available codelens that can be further enabled or disabled
  through `lsp-gopls-codelens'.")


(defun lsp-go--defcustom-available-as-alist-type (alist)
  "Returns a list suitable for the `:type' field in a `defcustom' used to populate an alist.

The input ALIST has the form `((\"name\" . \"documentation sentence\") [...])'

The returned type provides a tri-state that either:
  - does not include the element in the alist
  - sets element to false (actually, :json-false)
  - sets element to true (actually, t)
"
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
  'lsp-go-codelens
  "lsp-mode 7.0.1")

(defcustom lsp-go-codelens '((generate . t) (test . t))
  "Select what codelens should be enabled or not.

The codelens can be found at https://github.com/golang/tools/blob/4d5ea46c79fe3bbb57dd00de9c167e93d94f4710/internal/lsp/source/options.go#L102-L108."
  :type (lsp-go--defcustom-available-as-alist-type lsp-go-available-codelens)
  :group 'lsp-gopls
  :risky t
  :package-version '(lsp-mode "7.0"))

(define-obsolete-variable-alias
  'lsp-clients-go-library-directories
  'lsp-go-library-directories
  "lsp-mode 7.0.1")

(defcustom lsp-go-library-directories '("/usr")
  "List of directories which will be considered to be libraries."
  :group 'lsp-go
  :risky t
  :type '(repeat string))

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

(defcustom lsp-go-link-target "godoc.org"
  "Which website to use for displaying Go documentation."
  :type '(choice (const "godoc.org")
		 (const "pkg.go.dev")
		 (string :tag "A custom website"))
  :group 'lsp-go
  :package-version '(lsp-mode "7.0.1"))

(lsp-register-custom-settings
 '(("gopls.usePlaceholders" lsp-go-use-placeholders t)
   ("gopls.hoverKind" lsp-go-hover-kind)
   ("gopls.buildFlags" lsp-go-build-flags)
   ("gopls.env" lsp-go-env)
   ("gopls.linkTarget" lsp-go-link-target)
   ("gopls.codelens" lsp-go-codelens)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
                  :major-modes '(go-mode)
                  :language-id "go"
                  :priority 0
                  :server-id 'gopls
                  :completion-in-comments? t
                  :library-folders-fn #'lsp-go--library-default-directories))

(provide 'lsp-go)
;;; lsp-go.el ends here
