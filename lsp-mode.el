;;; lsp-mode.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 emacs-lsp maintainers

;; Author: Vibhav Pant, Fangrui Song, Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (f "0.20.0") (ht "2.3") (spinner "1.7.3") (markdown-mode "2.3") (lv "0") (eldoc "1.11"))
;; Version: 9.0.0

;; URL: https://github.com/emacs-lsp/lsp-mode
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

;; Emacs client/library for the Language Server Protocol

;;; Code:

(require 'cl-generic)
(require 'cl-lib)
(require 'compile)
(require 'dash)
(require 'epg)
(require 'ewoc)
(require 'f)
(require 'filenotify)
(require 'files)
(require 'ht)
(require 'imenu)
(require 'inline)
(require 'json)
(require 'lv)
(require 'markdown-mode)
(require 'network-stream)
(require 'pcase)
(require 'rx)
(require 's)
(require 'seq)
(require 'spinner)
(require 'subr-x)
(require 'tree-widget)
(require 'url-parse)
(require 'url-util)
(require 'widget)
(require 'xref)
(require 'minibuffer)
(require 'help-mode)
(require 'lsp-protocol)

(defgroup lsp-mode nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server (lsp-mode)")

(declare-function evil-set-command-property "ext:evil-common")
(declare-function projectile-project-root "ext:projectile")
(declare-function yas-expand-snippet "ext:yasnippet")
(declare-function dap-mode "ext:dap-mode")
(declare-function dap-auto-configure-mode "ext:dap-mode")

(defvar yas-inhibit-overlay-modification-protection)
(defvar yas-indent-line)
(defvar yas-wrap-around-region)
(defvar yas-also-auto-indent-first-line)
(defvar dap-auto-configure-mode)
(defvar dap-ui-menu-items)
(defvar company-minimum-prefix-length)

(defconst lsp--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

(defconst lsp--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")
    (-32002 "Server Not Initialized")
    (-32001 "Unknown Error Code")
    (-32800 "Request Cancelled"))
  "Alist of error codes to user friendly strings.")

(defconst lsp--empty-ht (make-hash-table))

(eval-and-compile
  (defun dash-expand:&lsp-wks (key source)
    `(,(intern-soft (format "lsp--workspace-%s" (eval key))) ,source))

  (defun dash-expand:&lsp-cln (key source)
    `(,(intern-soft (format "lsp--client-%s" (eval key))) ,source)))

(define-obsolete-variable-alias 'lsp-print-io 'lsp-log-io "lsp-mode 6.1")

(defcustom lsp-log-io nil
  "If non-nil, log all messages from the language server to a *lsp-log* buffer."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-log-io-allowlist-methods '()
  "The methods to filter before print to lsp-log-io."
  :group 'lsp-mode
  :type '(repeat string)
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-log-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'lsp-mode
  :type '(choice (const :tag "Disable" nil)
                 (integer :tag "lines")
                 (const :tag "Unlimited" t))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-io-messages-max t
  "Maximum number of messages that can be locked in a `lsp-io' buffer."
  :group 'lsp-mode
  :type '(choice (const :tag "Unlimited" t)
                 (integer :tag "Messages"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-keep-workspace-alive t
  "If non nil keep workspace alive when the last workspace buffer is closed."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-snippet t
  "Enable/disable snippet completion support."
  :group 'lsp-completion
  :type 'boolean)

(defcustom lsp-enable-folding t
  "Enable/disable code folding support."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(define-obsolete-variable-alias 'lsp-enable-semantic-highlighting 'lsp-semantic-tokens-enable "lsp-mode 8.0.0")

(defcustom lsp-semantic-tokens-enable nil
  "Enable/disable support for semantic tokens.
As defined by the Language Server Protocol 3.16."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defcustom lsp-folding-range-limit nil
  "The maximum number of folding ranges to receive from the language server."
  :group 'lsp-mode
  :type '(choice (const :tag "No limit." nil)
                 (integer :tag "Number of lines."))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-folding-line-folding-only nil
  "If non-nil, only fold complete lines."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-client-packages
  '( ccls lsp-actionscript lsp-ada lsp-angular lsp-ansible lsp-asm lsp-astro
     lsp-autotools lsp-awk lsp-bash lsp-beancount lsp-bufls lsp-clangd
     lsp-clojure lsp-cmake lsp-cobol lsp-credo lsp-crystal lsp-csharp lsp-css
     lsp-cucumber lsp-cypher lsp-d lsp-dart lsp-dhall lsp-docker lsp-dockerfile
     lsp-elixir lsp-elm lsp-emmet lsp-erlang lsp-eslint lsp-fortran lsp-fsharp
     lsp-gdscript lsp-gleam lsp-glsl lsp-go lsp-golangci-lint lsp-grammarly
     lsp-graphql lsp-groovy lsp-hack lsp-haskell lsp-haxe lsp-idris lsp-java
     lsp-javascript lsp-jq lsp-json lsp-kotlin lsp-latex lsp-lisp lsp-ltex
     lsp-lua lsp-magik lsp-markdown lsp-marksman lsp-mdx lsp-metals lsp-mint
     lsp-mojo lsp-move lsp-mssql lsp-nginx lsp-nim lsp-nix lsp-nushell lsp-ocaml
     lsp-openscad lsp-pascal lsp-perl lsp-perlnavigator lsp-php lsp-pls
     lsp-purescript lsp-pwsh lsp-pyls lsp-pylsp lsp-pyright lsp-python-ms
     lsp-qml lsp-r lsp-racket lsp-remark lsp-rf lsp-rubocop lsp-ruby-lsp
     lsp-ruby-syntax-tree lsp-ruff-lsp lsp-rust lsp-semgrep lsp-shader
     lsp-solargraph lsp-solidity lsp-sonarlint lsp-sorbet lsp-sourcekit lsp-sqls
     lsp-steep lsp-svelte lsp-tailwindcss lsp-terraform lsp-tex lsp-tilt
     lsp-toml lsp-trunk lsp-ttcn3 lsp-typeprof lsp-v lsp-vala lsp-verilog
     lsp-vetur lsp-vhdl lsp-vimscript lsp-volar lsp-wgsl lsp-xml lsp-yaml
     lsp-yang lsp-zig)
  "List of the clients to be automatically required."
  :group 'lsp-mode
  :type '(repeat symbol))

(defcustom lsp-progress-via-spinner t
  "If non-nil, display LSP $/progress reports via a spinner in the modeline."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-progress-spinner-type 'progress-bar
  "Holds the type of spinner to be used in the mode-line.
Takes a value accepted by `spinner-start'."
  :group 'lsp-mode
  :type `(choice :tag "Choose a spinner by name"
                 ,@(mapcar (lambda (c) (list 'const (car c)))
                           spinner-types)))

(defvar-local lsp-use-workspace-root-for-server-default-directory nil
  "Use `lsp-workspace-root' for `default-directory' when starting LSP process.")

(defvar-local lsp--cur-workspace nil)

(defvar-local lsp--cur-version 0)
(defvar-local lsp--virtual-buffer-connections nil)
(defvar-local lsp--virtual-buffer nil)
(defvar lsp--virtual-buffer-mappings (ht))

(defvar lsp--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defvar-local lsp-buffer-uri nil
  "If set, return it instead of calculating it using `buffer-file-name'.")

(define-error 'lsp-error "Unknown lsp-mode error")
(define-error 'lsp-empty-response-error
  "Empty response from the language server" 'lsp-error)
(define-error 'lsp-timed-out-error
  "Timed out while waiting for a response from the language server" 'lsp-error)
(define-error 'lsp-capability-not-supported
  "Capability not supported by the language server" 'lsp-error)
(define-error 'lsp-file-scheme-not-supported
  "Unsupported file scheme" 'lsp-error)
(define-error 'lsp-client-already-exists-error
  "A client with this server-id already exists" 'lsp-error)
(define-error 'lsp-no-code-actions
  "No code actions" 'lsp-error)

(defcustom lsp-auto-guess-root nil
  "Automatically guess the project root using projectile/project.
Do *not* use this setting unless you are familiar with `lsp-mode'
internals and you are sure that all of your projects are
following `projectile'/`project.el' conventions."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-guess-root-without-session nil
  "Ignore the session file when calculating the project root.
You almost always want to set lsp-auto-guess-root too.
Do *not* use this setting unless you are familiar with `lsp-mode'
internals and you are sure that all of your projects are
following `projectile'/`project.el' conventions."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-restart 'interactive
  "Defines how server-exited events must be handled."
  :group 'lsp-mode
  :type '(choice (const interactive)
                 (const auto-restart)
                 (const ignore)))

(defcustom lsp-session-file (expand-file-name (locate-user-emacs-file ".lsp-session-v1"))
  "File where session information is stored."
  :group 'lsp-mode
  :type 'file)

(defcustom lsp-auto-configure t
  "Auto configure `lsp-mode' main features.
When set to t `lsp-mode' will auto-configure completion,
code-actions, breadcrumb, `flycheck', `flymake', `imenu', symbol highlighting,
lenses, links, and so on.

For finer granularity you may use `lsp-enable-*' properties."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-disabled-clients nil
  "A list of disabled/blocklisted clients.
Each entry in the list can be either:
a symbol, the server-id for the LSP client, or
a cons pair (MAJOR-MODE . CLIENTS), where MAJOR-MODE is the major-mode,
and CLIENTS is either a client or a list of clients.

This option can also be used as a file- or directory-local variable to
disable a language server for individual files or directories/projects
respectively."
  :group 'lsp-mode
  :type '(repeat (symbol))
  :safe 'listp
  :package-version '(lsp-mode . "6.1"))

(defvar lsp-clients (make-hash-table :test 'eql)
  "Hash table server-id -> client.
It contains all of the clients that are currently registered.")

(defvar lsp-enabled-clients nil
  "List of clients allowed to be used for projects.
When nil, all registered clients are considered candidates.")

(defvar lsp-last-id 0
  "Last request id.")

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-open-hook nil
  "List of functions to be called before a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-open-hook nil
  "List of functions to be called after a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-enable-file-watchers t
  "If non-nil lsp-mode will watch the files in the workspace if
the server has requested that."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))
;;;###autoload(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)

(define-obsolete-variable-alias 'lsp-file-watch-ignored 'lsp-file-watch-ignored-directories "8.0.0")

(defcustom lsp-file-watch-ignored-directories
  '(; SCM tools
    "[/\\\\]\\.git\\'"
    "[/\\\\]\\.github\\'"
    "[/\\\\]\\.gitlab\\'"
    "[/\\\\]\\.circleci\\'"
    "[/\\\\]\\.hg\\'"
    "[/\\\\]\\.bzr\\'"
    "[/\\\\]_darcs\\'"
    "[/\\\\]\\.svn\\'"
    "[/\\\\]_FOSSIL_\\'"
    ;; IDE or build tools
    "[/\\\\]\\.idea\\'"
    "[/\\\\]\\.ensime_cache\\'"
    "[/\\\\]\\.eunit\\'"
    "[/\\\\]node_modules"
    "[/\\\\]\\.yarn\\'"
    "[/\\\\]\\.fslckout\\'"
    "[/\\\\]\\.tox\\'"
    "[/\\\\]\\.nox\\'"
    "[/\\\\]dist\\'"
    "[/\\\\]dist-newstyle\\'"
    "[/\\\\]\\.stack-work\\'"
    "[/\\\\]\\.bloop\\'"
    "[/\\\\]\\.metals\\'"
    "[/\\\\]target\\'"
    "[/\\\\]\\.ccls-cache\\'"
    "[/\\\\]\\.vscode\\'"
    "[/\\\\]\\.venv\\'"
    "[/\\\\]\\.mypy_cache\\'"
    "[/\\\\]\\.pytest_cache\\'"
    ;; Swift Package Manager
    "[/\\\\]\\.build\\'"
    ;; Python
    "[/\\\\]__pycache__\\'"
    ;; Autotools output
    "[/\\\\]\\.deps\\'"
    "[/\\\\]build-aux\\'"
    "[/\\\\]autom4te.cache\\'"
    "[/\\\\]\\.reference\\'"
    ;; Bazel
    "[/\\\\]bazel-[^/\\\\]+\\'"
    ;; CSharp
    "[/\\\\]\\.meta\\'"
    ;; Unity
    "[/\\\\]Library\\'"
    ;; Clojure
    "[/\\\\]\\.lsp\\'"
    "[/\\\\]\\.clj-kondo\\'"
    "[/\\\\]\\.shadow-cljs\\'"
    "[/\\\\]\\.babel_cache\\'"
    "[/\\\\]\\.cpcache\\'"
    "[/\\\\]\\checkouts\\'"
    ;; Gradle
    "[/\\\\]\\.gradle\\'"
    ;; Maven
    "[/\\\\]\\.m2\\'"
    ;; .Net Core build-output
    "[/\\\\]bin/Debug\\'"
    "[/\\\\]obj\\'"
    ;; OCaml and Dune
    "[/\\\\]_opam\\'"
    "[/\\\\]_build\\'"
    ;; Elixir
    "[/\\\\]\\.elixir_ls\\'"
    ;; Elixir Credo
    "[/\\\\]\\.elixir-tools\\'"
    ;; terraform and terragrunt
    "[/\\\\]\\.terraform\\'"
    "[/\\\\]\\.terragrunt-cache\\'"
    ;; nix-direnv
    "[/\\\\]\\result"
    "[/\\\\]\\result-bin"
    "[/\\\\]\\.direnv\\'")
  "List of regexps matching directory paths which won't be monitored when
creating file watches. Customization of this variable is only honored at
the global level or at a root of an lsp workspace."
  :group 'lsp-mode
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0"))

(define-obsolete-function-alias 'lsp-file-watch-ignored 'lsp-file-watch-ignored-directories "7.0.1")

(defun lsp-file-watch-ignored-directories ()
  lsp-file-watch-ignored-directories)

;; Allow lsp-file-watch-ignored-directories as a file or directory-local variable
;;;###autoload(put 'lsp-file-watch-ignored-directories 'safe-local-variable 'lsp--string-listp)

(defcustom lsp-file-watch-ignored-files
  '(
    ;; Flycheck tempfiles
    "[/\\\\]flycheck_[^/\\\\]+\\'"
    ;; lockfiles
    "[/\\\\]\\.#[^/\\\\]+\\'"
    ;; backup files
    "[/\\\\][^/\\\\]+~\\'" )
  "List of regexps matching files for which change events will
not be sent to the server.

This setting has no impact on whether a file-watch is created for
a directory; it merely prevents notifications pertaining to
matched files from being sent to the server.  To prevent a
file-watch from being created for a directory, customize
`lsp-file-watch-ignored-directories'

Customization of this variable is only honored at the global
level or at a root of an lsp workspace."
  :group 'lsp-mode
  :type '(repeat string)
  :package-version '(lsp-mode . "8.0.0"))

;; Allow lsp-file-watch-ignored-files as a file or directory-local variable
;;;###autoload(put 'lsp-file-watch-ignored-files 'safe-local-variable 'lsp--string-listp)

(defcustom lsp-after-uninitialized-functions nil
  "List of functions to be called after a Language Server has been uninitialized."
  :type 'hook
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defconst lsp--sync-full 1)
(defconst lsp--sync-incremental 2)

(defcustom lsp-debounce-full-sync-notifications t
  "If non-nil debounce full sync events.
This flag affects only servers which do not support incremental updates."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-debounce-full-sync-notifications-interval 1.0
  "Time to wait before sending full sync synchronization after buffer modification."
  :type 'float
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defvar lsp--stderr-index 0)

(defvar lsp--delayed-requests nil)
(defvar lsp--delay-timer nil)

(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents are synced by always sending the full content of the document." lsp--sync-full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." lsp--sync-incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

(defcustom lsp-auto-execute-action t
  "Auto-execute single action."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-links t
  "If non-nil, all references to links in a file will be made clickable, if
supported by the language server."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-enable-imenu t
  "If non-nil, automatically enable `imenu' integration when server provides
`textDocument/documentSymbol'."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-enable-dap-auto-configure t
  "If non-nil, enable `dap-auto-configure-mode`."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "7.0"))

(defcustom lsp-eldoc-enable-hover t
  "If non-nil, `eldoc' will display hover info when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-render-all nil
  "Display all of the info returned by document/onHover.
If this is set to nil, `eldoc' will show only the symbol information."
  :type 'boolean
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-enable-completion-at-point
  'lsp-completion-enable "lsp-mode 7.0.1")

(defcustom lsp-completion-enable t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-completion)

(defcustom lsp-enable-symbol-highlighting t
  "Highlight references of the symbol at point."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-references-exclude-definition nil
  "If non-nil, exclude declarations when finding references."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the
language server."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-on-type-formatting t
  "Enable `textDocument/onTypeFormatting' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-text-document-color t
  "Enable `textDocument/documentColor' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-before-save-edits t
  "If non-nil, `lsp-mode' will apply edits suggested by the language server
before saving a document."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-after-apply-edits-hook nil
  "Hooks to run when text edit is applied.
It contains the operation source."
  :type 'hook
  :group 'lsp-mode
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-apply-edits-after-file-operations t
  "Whether to apply edits returned by server after file operations if any.
Applicable only if server supports workspace.fileOperations for operations:
`workspace/willRenameFiles', `workspace/willCreateFiles' and
`workspace/willDeleteFiles'."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-modeline-code-actions-enable t
  "Whether to show code actions on modeline."
  :type 'boolean
  :group 'lsp-modeline)

(defcustom lsp-modeline-diagnostics-enable t
  "Whether to show diagnostics on modeline."
  :type 'boolean
  :group 'lsp-modeline)

(defcustom lsp-modeline-workspace-status-enable t
  "Whether to show workspace status on modeline."
  :type 'boolean
  :group 'lsp-modeline
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-headerline-breadcrumb-enable t
  "Whether to enable breadcrumb on headerline."
  :type 'boolean
  :group 'lsp-headerline)

(defcustom lsp-configure-hook nil
  "Hooks to run when `lsp-configure-buffer' is called."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-unconfigure-hook nil
  "Hooks to run when `lsp-unconfig-buffer' is called."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received.
Note: it runs only if the receiving buffer is open. Use
`lsp-diagnostics-updated-hook'if you want to be notified when
diagnostics have changed."
  :type 'hook
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-after-diagnostics-hook
  'lsp-diagnostics-updated-hook "lsp-mode 6.4")

(defcustom lsp-diagnostics-updated-hook nil
  "Hooks to run after diagnostics are received."
  :type 'hook
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-workspace-folders-changed-hook
  'lsp-workspace-folders-changed-functions "lsp-mode 6.3")

(defcustom lsp-workspace-folders-changed-functions nil
  "Hooks to run after the folders has changed.
The hook will receive two parameters list of added and removed folders."
  :type 'hook
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-eldoc-hook 'eldoc-documentation-functions "lsp-mode 9.0.0")

(defcustom lsp-before-apply-edits-hook nil
  "Hooks to run before applying edits."
  :type 'hook
  :group 'lsp-mode)

(defgroup lsp-imenu nil
  "LSP Imenu."
  :group 'lsp-mode
  :tag "LSP Imenu")

(defcustom lsp-imenu-show-container-name t
  "Display the symbol's container name in an imenu entry."
  :type 'boolean
  :group 'lsp-imenu)

(defcustom lsp-imenu-container-name-separator "/"
  "Separator string to use to separate the container name from the symbol while
displaying imenu entries."
  :type 'string
  :group 'lsp-imenu)

(defcustom lsp-imenu-sort-methods '(kind name)
  "How to sort the imenu items.

The value is a list of `kind' `name' or `position'.  Priorities
are determined by the index of the element."
  :type '(repeat (choice (const name)
                         (const position)
                         (const kind)))
  :group 'lsp-imenu)

(defcustom lsp-imenu-index-symbol-kinds nil
  "Which symbol kinds to show in imenu."
  :type '(repeat (choice (const :tag "Miscellaneous" nil)
                         (const :tag "File" File)
                         (const :tag "Module" Module)
                         (const :tag "Namespace" Namespace)
                         (const :tag "Package" Package)
                         (const :tag "Class" Class)
                         (const :tag "Method" Method)
                         (const :tag "Property" Property)
                         (const :tag "Field" Field)
                         (const :tag "Constructor" Constructor)
                         (const :tag "Enum" Enum)
                         (const :tag "Interface" Interface)
                         (const :tag "Function" Function)
                         (const :tag "Variable" Variable)
                         (const :tag "Constant" Constant)
                         (const :tag "String" String)
                         (const :tag "Number" Number)
                         (const :tag "Boolean" Boolean)
                         (const :tag "Array" Array)
                         (const :tag "Object" Object)
                         (const :tag "Key" Key)
                         (const :tag "Null" Null)
                         (const :tag "Enum Member" EnumMember)
                         (const :tag "Struct" Struct)
                         (const :tag "Event" Event)
                         (const :tag "Operator" Operator)
                         (const :tag "Type Parameter" TypeParameter)))
  :group 'lsp-imenu)

;; vibhavp: Should we use a lower value (5)?
(defcustom lsp-response-timeout 10
  "Number of seconds to wait for a response from the language server before
timing out. Nil if no timeout."
  :type '(choice
          (number :tag "Seconds")
          (const :tag "No timeout" nil))
  :group 'lsp-mode)

(defcustom lsp-tcp-connection-timeout 2
  "The timeout for tcp connection in seconds."
  :type 'number
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defconst lsp--imenu-compare-function-alist
  (list (cons 'name #'lsp--imenu-compare-name)
        (cons 'kind #'lsp--imenu-compare-kind)
        (cons 'position #'lsp--imenu-compare-line-col))
  "An alist of (METHOD . FUNCTION).
METHOD is one of the symbols accepted by
`lsp-imenu-sort-methods'.

FUNCTION takes two hash tables representing DocumentSymbol.  It
returns a negative number, 0, or a positive number indicating
whether the first parameter is less than, equal to, or greater
than the second parameter.")

(defcustom lsp-diagnostic-clean-after-change nil
  "When non-nil, clean the diagnostics on change.

Note that when that setting is nil, `lsp-mode' will show stale
diagnostics until server publishes the new set of diagnostics"
  :type 'boolean
  :group 'lsp-diagnostics
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-server-trace nil
  "Request tracing on the server side.
The actual trace output at each level depends on the language server in use.
Changes take effect only when a new session is started."
  :type '(choice (const :tag "Disabled" "off")
                 (const :tag "Messages only" "messages")
                 (const :tag "Verbose" "verbose")
                 (const :tag "Default (disabled)" nil))
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-auto-touch-files t
  "If non-nil ensure the files exist before sending
`textDocument/didOpen' notification."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defvar lsp-language-id-configuration
  '(("\\(^CMakeLists\\.txt\\|\\.cmake\\)\\'" . "cmake")
    ("\\(^Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . "dockerfile")
    ("\\.astro$" . "astro")
    ("\\.cs\\'" . "csharp")
    ("\\.css$" . "css")
    ("\\.cypher$" . "cypher")
    ("\\.ebuild$" . "shellscript")
    ("\\.go\\'" . "go")
    ("\\.html$" . "html")
    ("\\.hx$" . "haxe")
    ("\\.hy$" . "hy")
    ("\\.java\\'" . "java")
    ("\\.jq$"  . "jq")
    ("\\.js$" . "javascript")
    ("\\.json$" . "json")
    ("\\.jsonc$" . "jsonc")
    ("\\.jsonnet$" . "jsonnet")
    ("\\.jsx$" . "javascriptreact")
    ("\\.lua$" . "lua")
    ("\\.mdx\\'" . "mdx")
    ("\\.nu$" . "nushell")
    ("\\.php$" . "php")
    ("\\.ps[dm]?1\\'" . "powershell")
    ("\\.rs\\'" . "rust")
    ("\\.spec\\'" . "rpm-spec")
    ("\\.sql$" . "sql")
    ("\\.svelte$" . "svelte")
    ("\\.toml\\'" . "toml")
    ("\\.ts$" . "typescript")
    ("\\.tsx$" . "typescriptreact")
    ("\\.ttcn3$" . "ttcn3")
    ("\\.vue$" . "vue")
    ("\\.xml$" . "xml")
    ("\\ya?ml$" . "yaml")
    ("^PKGBUILD$" . "shellscript")
    ("^go\\.mod\\'" . "go.mod")
    ("^settings\\.json$" . "jsonc")
    ("^yang\\.settings$" . "jsonc")
    (ada-mode . "ada")
    (ada-ts-mode . "ada")
    (awk-mode . "awk")
    (awk-ts-mode . "awk")
    (nxml-mode . "xml")
    (sql-mode . "sql")
    (vimrc-mode . "vim")
    (vimscript-ts-mode . "vim")
    (sh-mode . "shellscript")
    (bash-ts-mode . "shellscript")
    (ebuild-mode . "shellscript")
    (pkgbuild-mode . "shellscript")
    (envrc-file-mode . "shellscript")
    (scala-mode . "scala")
    (scala-ts-mode . "scala")
    (julia-mode . "julia")
    (julia-ts-mode . "julia")
    (clojure-mode . "clojure")
    (clojurec-mode . "clojure")
    (clojurescript-mode . "clojurescript")
    (clojure-ts-mode . "clojure")
    (clojure-ts-clojurec-mode . "clojure")
    (clojure-ts-clojurescript-mode . "clojurescript")
    (java-mode . "java")
    (java-ts-mode . "java")
    (jdee-mode . "java")
    (groovy-mode . "groovy")
    (python-mode . "python")
    (python-ts-mode . "python")
    (cython-mode . "python")
    ("\\(\\.mojo\\|\\.🔥\\)\\'" . "mojo")
    (lsp--render-markdown . "markdown")
    (move-mode . "move")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (rustic-mode . "rust")
    (kotlin-mode . "kotlin")
    (kotlin-ts-mode . "kotlin")
    (css-mode . "css")
    (css-ts-mode . "css")
    (less-mode . "less")
    (less-css-mode . "less")
    (lua-mode . "lua")
    (lua-ts-mode . "lua")
    (sass-mode . "sass")
    (ssass-mode . "sass")
    (scss-mode . "scss")
    (scad-mode . "openscad")
    (xml-mode . "xml")
    (c-mode . "c")
    (c-ts-mode . "c")
    (c++-mode . "cpp")
    (c++-ts-mode . "cpp")
    (cuda-mode . "cuda")
    (objc-mode . "objective-c")
    (html-mode . "html")
    (html-ts-mode . "html")
    (sgml-mode . "html")
    (mhtml-mode . "html")
    (mint-mode . "mint")
    (go-dot-mod-mode . "go.mod")
    (go-mod-ts-mode . "go.mod")
    (go-mode . "go")
    (go-ts-mode . "go")
    (graphql-mode . "graphql")
    (haskell-mode . "haskell")
    (hack-mode . "hack")
    (php-mode . "php")
    (php-ts-mode . "php")
    (powershell-mode . "powershell")
    (powershell-mode . "PowerShell")
    (powershell-ts-mode . "powershell")
    (json-mode . "json")
    (json-ts-mode . "json")
    (jsonc-mode . "jsonc")
    (rjsx-mode . "javascript")
    (js2-mode . "javascript")
    (js-mode . "javascript")
    (js-ts-mode . "javascript")
    (typescript-mode . "typescript")
    (typescript-ts-mode . "typescript")
    (tsx-ts-mode . "typescriptreact")
    (fsharp-mode . "fsharp")
    (reason-mode . "reason")
    (caml-mode . "ocaml")
    (tuareg-mode . "ocaml")
    (swift-mode . "swift")
    (elixir-mode . "elixir")
    (elixir-ts-mode . "elixir")
    (heex-ts-mode . "elixir")
    (conf-javaprop-mode . "spring-boot-properties")
    (yaml-mode . "yaml")
    (yaml-ts-mode . "yaml")
    (ruby-mode . "ruby")
    (enh-ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (fortran-mode . "fortran")
    (f90-mode . "fortran")
    (elm-mode . "elm")
    (dart-mode . "dart")
    (erlang-mode . "erlang")
    (dockerfile-mode . "dockerfile")
    (dockerfile-ts-mode . "dockerfile")
    (csharp-mode . "csharp")
    (csharp-tree-sitter-mode . "csharp")
    (csharp-ts-mode . "csharp")
    (plain-tex-mode . "plaintex")
    (context-mode . "context")
    (cypher-mode . "cypher")
    (latex-mode . "latex")
    (v-mode . "v")
    (vhdl-mode . "vhdl")
    (vhdl-ts-mode . "vhdl")
    (verilog-mode . "verilog")
    (terraform-mode . "terraform")
    (ess-julia-mode . "julia")
    (ess-r-mode . "r")
    (crystal-mode . "crystal")
    (nim-mode . "nim")
    (dhall-mode . "dhall")
    (cmake-mode . "cmake")
    (cmake-ts-mode . "cmake")
    (purescript-mode . "purescript")
    (gdscript-mode . "gdscript")
    (gdscript-ts-mode . "gdscript")
    (perl-mode . "perl")
    (cperl-mode . "perl")
    (robot-mode . "robot")
    (racket-mode . "racket")
    (nix-mode . "nix")
    (nix-ts-mode . "Nix")
    (prolog-mode . "prolog")
    (vala-mode . "vala")
    (actionscript-mode . "actionscript")
    (d-mode . "d")
    (zig-mode . "zig")
    (text-mode . "plaintext")
    (markdown-mode . "markdown")
    (gfm-mode . "markdown")
    (beancount-mode . "beancount")
    (conf-toml-mode . "toml")
    (toml-ts-mode . "toml")
    (org-mode . "org")
    (org-journal-mode . "org")
    (nginx-mode . "nginx")
    (magik-mode . "magik")
    (magik-ts-mode . "magik")
    (idris-mode . "idris")
    (idris2-mode . "idris2")
    (gleam-mode . "gleam")
    (graphviz-dot-mode . "dot")
    (tiltfile-mode . "tiltfile")
    (solidity-mode . "solidity")
    (bibtex-mode . "bibtex")
    (rst-mode . "restructuredtext")
    (glsl-mode . "glsl")
    (shader-mode . "shaderlab")
    (wgsl-mode . "wgsl")
    (jq-mode . "jq")
    (jq-ts-mode . "jq")
    (protobuf-mode . "protobuf")
    (nushell-mode . "nushell")
    (nushell-ts-mode . "nushell")
    (yang-mode . "yang"))
  "Language id configuration.")

(defvar lsp--last-active-workspaces nil
  "Keep track of last active workspace.
We want to try the last workspace first when jumping into a library
directory")

(defvar lsp-method-requirements
  '(("textDocument/callHierarchy" :capability :callHierarchyProvider)
    ("textDocument/codeAction" :capability :codeActionProvider)
    ("codeAction/resolve"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp:code-action-options-resolve-provider?
                         (lsp--capability-for-method "textDocument/codeAction")))))
    ("textDocument/codeLens" :capability :codeLensProvider)
    ("textDocument/completion" :capability :completionProvider)
    ("completionItem/resolve"
     :check-command (lambda (wk)
                      (with-lsp-workspace wk
                        (lsp:completion-options-resolve-provider?
                         (lsp--capability-for-method "textDocument/completion")))))
    ("textDocument/declaration" :capability :declarationProvider)
    ("textDocument/definition" :capability :definitionProvider)
    ("textDocument/documentColor" :capability :colorProvider)
    ("textDocument/documentLink" :capability :documentLinkProvider)
    ("textDocument/inlayHint" :capability :inlayHintProvider)
    ("textDocument/documentHighlight" :capability :documentHighlightProvider)
    ("textDocument/documentSymbol" :capability :documentSymbolProvider)
    ("textDocument/foldingRange" :capability :foldingRangeProvider)
    ("textDocument/formatting" :capability :documentFormattingProvider)
    ("textDocument/hover" :capability :hoverProvider)
    ("textDocument/implementation" :capability :implementationProvider)
    ("textDocument/linkedEditingRange" :capability :linkedEditingRangeProvider)
    ("textDocument/onTypeFormatting" :capability :documentOnTypeFormattingProvider)
    ("textDocument/prepareRename"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp:rename-options-prepare-provider?
                         (lsp--capability-for-method "textDocument/rename")))))
    ("textDocument/rangeFormatting" :capability :documentRangeFormattingProvider)
    ("textDocument/references" :capability :referencesProvider)
    ("textDocument/rename" :capability :renameProvider)
    ("textDocument/selectionRange" :capability :selectionRangeProvider)
    ("textDocument/semanticTokens" :capability :semanticTokensProvider)
    ("textDocument/semanticTokensFull"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp-get (lsp--capability :semanticTokensProvider) :full))))
    ("textDocument/semanticTokensFull/Delta"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (let ((capFull (lsp-get (lsp--capability :semanticTokensProvider) :full)))
                          (and (not (booleanp capFull)) (lsp-get capFull :delta))))))
    ("textDocument/semanticTokensRangeProvider"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp-get (lsp--capability :semanticTokensProvider) :range))))
    ("textDocument/signatureHelp" :capability :signatureHelpProvider)
    ("textDocument/typeDefinition" :capability :typeDefinitionProvider)
    ("textDocument/typeHierarchy" :capability :typeHierarchyProvider)
    ("workspace/executeCommand" :capability :executeCommandProvider)
    ("workspace/symbol" :capability :workspaceSymbolProvider))

  "Map methods to requirements.
It is used by request-sending functions to determine which server
must be used for handling a particular message.")

(defconst lsp--file-change-type
  `((created . 1)
    (changed . 2)
    (deleted . 3)))

(defconst lsp--watch-kind
  `((create . 1)
    (change . 2)
    (delete . 4)))

(defvar lsp-window-body-width 40
  "Window body width when rendering doc.")

(defface lsp-face-highlight-textual
  '((t :inherit highlight))
  "Face used for textual occurrences of symbols."
  :group 'lsp-mode)

(defface lsp-face-highlight-read
  '((t :inherit highlight :underline t))
  "Face used for highlighting symbols being read."
  :group 'lsp-mode)

(defface lsp-face-highlight-write
  '((t :inherit highlight :weight bold))
  "Face used for highlighting symbols being written to."
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-lens-auto-enable
  'lsp-lens-enable "lsp-mode 7.0.1")

(defcustom lsp-lens-enable t
  "Auto enable lenses if server supports."
  :group 'lsp-lens
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-symbol-highlighting-skip-current nil
  "If non-nil skip current symbol when setting symbol highlights."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-file-watch-threshold 1000
  "Show warning if the files to watch are more than.
Set to nil to disable the warning."
  :type 'number
  :group 'lsp-mode)
;;;###autoload(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(defvar lsp-custom-markup-modes
  '((rust-mode "no_run" "rust,no_run" "rust,ignore" "rust,should_panic"))
  "Mode to uses with markdown code blocks.
They are added to `markdown-code-lang-modes'")

(defcustom lsp-signature-render-documentation t
  "Display signature documentation in `eldoc'."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-signature-auto-activate '(:on-trigger-char :on-server-request)
  "Auto activate signature conditions."
  :type '(repeat (choice (const :tag "On trigger chars pressed." :on-trigger-char)
                         (const :tag "After selected completion." :after-completion)
                         (const :tag "When the server has sent show signature help." :on-server-request)))
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-signature-doc-lines 20
  "If number, limit the number of lines to show in the docs."
  :type 'number
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-signature-function 'lsp-lv-message
  "The function used for displaying signature info.
It will be called with one param - the signature info. When
called with nil the signature info must be cleared."
  :type 'function
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-keymap-prefix "s-l"
  "LSP-mode keymap prefix."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "6.3"))

(defvar-local lsp--buffer-workspaces ()
  "List of the buffer workspaces.")

(defvar lsp--session nil
  "Contain the `lsp-session' for the current Emacs instance.")

(defvar lsp--tcp-port 10000)

(defvar lsp--client-packages-required nil
  "If nil, `lsp-client-packages' are yet to be required.")

(defvar lsp--tcp-server-port 0
  "The server socket which is opened when using `lsp-tcp-server' (a server
socket is opened in Emacs and the language server connects to it).  The
default value of 0 ensures that a random high port is used. Set it to a positive
integer to use a specific port.")

(defvar lsp--tcp-server-wait-seconds 10
  "Wait this amount of time for the client to connect to our server socket
when using `lsp-tcp-server'.")

(defvar-local lsp--document-symbols nil
  "The latest document symbols.")

(defvar-local lsp--document-selection-range-cache nil
  "The document selection cache.")

(defvar-local lsp--document-symbols-request-async nil
  "If non-nil, request document symbols asynchronously.")

(defvar-local lsp--document-symbols-tick -1
  "The value of `buffer-chars-modified-tick' when document
  symbols were last retrieved.")

(defvar-local lsp--have-document-highlights nil
  "Set to `t' on symbol highlighting, cleared on
`lsp--cleanup-highlights-if-needed'. Checking a separately
defined flag is substantially faster than unconditionally
calling `remove-overlays'.")

;; Buffer local variable for storing number of lines.
(defvar lsp--log-lines)

(defvar-local lsp--eldoc-saved-message nil)

(defvar lsp--on-change-timer nil)
(defvar lsp--on-idle-timer nil)

(defvar-local lsp--signature-last nil)
(defvar-local lsp--signature-last-index nil)
(defvar lsp--signature-last-buffer nil)

(defvar-local lsp--virtual-buffer-point-max nil)

(cl-defmethod lsp-execute-command (_server _command _arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(defun lsp-elt (sequence n)
  "Return Nth element of SEQUENCE or nil if N is out of range."
  (cond
   ((listp sequence) (elt sequence n))
   ((arrayp sequence)
    (and (> (length sequence) n) (aref sequence n)))
   (t (and (> (length sequence) n) (elt sequence n)))))

;; define seq-first and seq-rest for older emacs
(defun lsp-seq-first (sequence)
  "Return the first element of SEQUENCE."
  (lsp-elt sequence 0))

(defun lsp-seq-rest (sequence)
  "Return a sequence of the elements of SEQUENCE except the first one."
  (seq-drop sequence 1))

;;;###autoload
(defun lsp--string-listp (sequence)
  "Return t if all elements of SEQUENCE are strings, else nil."
  (not (seq-find (lambda (x) (not (stringp x))) sequence)))

(defun lsp--string-vector-p (candidate)
  "Returns true if CANDIDATE is a vector data structure and
every element of it is of type string, else nil."
  (and
   (vectorp candidate)
   (seq-every-p #'stringp candidate)))

(make-obsolete 'lsp--string-vector-p nil "lsp-mode 8.0.0")

(defun lsp--editable-vector-match (widget value)
  "Function for `lsp-editable-vector' :match."
  ;; Value must be a list or a vector and all the members must match the type.
  (and (or (listp value) (vectorp value))
       (length (cdr (lsp--editable-vector-match-inline widget value)))))

(defun lsp--editable-vector-match-inline (widget value)
  "Value for `lsp-editable-vector' :match-inline."
  (let ((type (nth 0 (widget-get widget :args)))
        (ok t)
        found)
    (while (and value ok)
      (let ((answer (widget-match-inline type value)))
        (if answer
            (let ((head (if (vectorp answer) (aref answer 0) (car answer)))
                  (tail (if (vectorp answer) (seq-drop 1 answer) (cdr answer))))
              (setq found (append found head)
                    value tail))
          (setq ok nil))))
    (cons found value)))

(defun lsp--editable-vector-value-to-external (_widget internal-value)
  "Convert the internal list value to a vector."
  (if (listp internal-value)
      (apply 'vector internal-value)
    internal-value))

(defun lsp--editable-vector-value-to-internal (_widget external-value)
  "Convert the external vector value to a list."
  (if (vectorp external-value)
      (append external-value nil)
    external-value))

(define-widget 'lsp--editable-vector 'editable-list
  "A subclass of `editable-list' that accepts and returns a
vector instead of a list."
  :value-to-external 'lsp--editable-vector-value-to-external
  :value-to-internal 'lsp--editable-vector-value-to-internal
  :match 'lsp--editable-vector-match
  :match-inline 'lsp--editable-vector-match-inline)

(define-widget 'lsp-repeatable-vector 'lsp--editable-vector
  "A variable length homogeneous vector."
  :tag "Repeat"
  :format "%{%t%}:\n%v%i\n")

(define-widget 'lsp-string-vector 'lazy
  "A vector of zero or more elements, every element of which is a string.
Appropriate for any language-specific `defcustom' that needs to
serialize as a JSON array of strings.

Deprecated. Use `lsp-repeatable-vector' instead. "
  :offset 4
  :tag "Vector"
  :type '(lsp-repeatable-vector string))

(make-obsolete 'lsp-string-vector nil "lsp-mode 8.0.0")

(defvar lsp--show-message t
  "If non-nil, show debug message from `lsp-mode'.")

(defun lsp--message  (format &rest args)
  "Wrapper for `message'

We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `lsp--info', `lsp--warn' and `lsp--error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.

See #2049"
  (when lsp--show-message
    (let ((inhibit-message (or inhibit-message
                               (and (minibufferp)
                                    (version< emacs-version "27.0")))))
      (apply #'message format args))))

(defun lsp--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (lsp--message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

(defun lsp--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (lsp--message "%s :: %s" (propertize "LSP" 'face 'warning) (apply #'format format args)))

(defun lsp--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (lsp--message "%s :: %s" (propertize "LSP" 'face 'error) (apply #'format format args)))

(defun lsp-log (format &rest args)
  "Log message to the ’*lsp-log*’ buffer.

FORMAT and ARGS i the same as for `message'."
  (when lsp-log-max
    (let ((log-buffer (get-buffer "*lsp-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-log*"))
        (with-current-buffer log-buffer
          (buffer-disable-undo)
          (view-mode 1)
          (set (make-local-variable 'lsp--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq lsp--log-lines (+ lsp--log-lines newlines))

            (when (and (integerp lsp-log-max) (> lsp--log-lines lsp-log-max))
              (let ((to-delete (- lsp--log-lines lsp-log-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp--log-lines lsp-log-max)))))))))

(defalias 'lsp-message 'lsp-log)

(defalias 'lsp-ht 'ht)

(defalias 'lsp-file-local-name 'file-local-name)

(defun lsp-f-canonical (file-name)
  "Return the canonical FILE-NAME, without a trailing slash."
  (directory-file-name (expand-file-name file-name)))

(defalias 'lsp-canonical-file-name 'lsp-f-canonical)

(defun lsp-f-same? (path-a path-b)
  "Return t if PATH-A and PATH-B are references to the same file.
Symlinks are not followed."
  (when (and (f-exists? path-a)
             (f-exists? path-b))
    (equal
     (lsp-f-canonical (directory-file-name (f-expand path-a)))
     (lsp-f-canonical (directory-file-name (f-expand path-b))))))

(defun lsp-f-parent (path)
  "Return the parent directory to PATH.
Symlinks are not followed."
  (let ((parent (file-name-directory
                 (directory-file-name (f-expand path default-directory)))))
    (unless (lsp-f-same? path parent)
      (if (f-relative? path)
          (f-relative parent)
        (directory-file-name parent)))))

(defun lsp-f-ancestor-of? (path-a path-b)
  "Return t if PATH-A is an ancestor of PATH-B.
Symlinks are not followed."
  (unless (lsp-f-same? path-a path-b)
    (s-prefix? (concat (lsp-f-canonical path-a) (f-path-separator))
               (lsp-f-canonical path-b))))

(defun lsp--merge-results (results method)
  "Merge RESULTS by filtering the empty hash-tables and merging
the lists according to METHOD."
  (pcase (--map (if (vectorp it)
                    (append it nil) it)
                (-filter #'identity results))
    (`() ())
    ;; only one result - simply return it
    (`(,fst) fst)
    ;; multiple results merge it based on strategy
    (results
     (pcase method
       ("textDocument/hover" (pcase (seq-filter
                                     (-compose #'not #'lsp-empty?)
                                     results)
                               (`(,hover) hover)
                               (hovers (lsp-make-hover
                                        :contents
                                        (-mapcat
                                         (-lambda ((&Hover :contents))
                                           (if (and (sequencep contents)
                                                    (not (stringp contents)))
                                               (append contents ())
                                             (list contents)))
                                         hovers)))))
       ("textDocument/completion"
        (lsp-make-completion-list
         :is-incomplete (seq-some
                         #'lsp:completion-list-is-incomplete
                         results)
         :items (cl-mapcan (lambda (it) (append (if (lsp-completion-list? it)
                                                    (lsp:completion-list-items it)
                                                  it)
                                                nil))
                           results)))
       ("completionItem/resolve"
        (let ((item (cl-first results)))
          (when-let ((details (seq-filter #'identity
                                          (seq-map #'lsp:completion-item-detail? results))))
            (lsp:set-completion-item-detail?
             item
             (string-join details " ")))
          (when-let ((docs (seq-filter #'identity
                                       (seq-map #'lsp:completion-item-documentation? results))))
            (lsp:set-completion-item-documentation?
             item
             (lsp-make-markup-content
              :kind (or (seq-some (lambda (it)
                                    (when (equal (lsp:markup-content-kind it)
                                                 lsp/markup-kind-markdown)
                                      lsp/markup-kind-markdown))
                                  docs)
                        lsp/markup-kind-plain-text)
              :value (string-join (seq-map (lambda (doc)
                                             (or (lsp:markup-content-value doc)
                                                 (and (stringp doc) doc)))
                                           docs)
                                  "\n"))))
          (when-let ((edits (seq-filter #'identity
                                        (seq-map #'lsp:completion-item-additional-text-edits? results))))
            (lsp:set-completion-item-additional-text-edits?
             item
             (cl-mapcan (lambda (it) (if (seqp it) it (list it))) edits)))
          item))
       (_ (cl-mapcan (lambda (it) (if (seqp it) it (list it))) results))))))

(defun lsp--spinner-start ()
  "Start spinner indication."
  (condition-case _err (spinner-start (lsp-progress-spinner-type)) (error)))

(defun lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp-workspaces ()
  "Return the lsp workspaces associated with the current project."
  (if lsp--cur-workspace (list lsp--cur-workspace) lsp--buffer-workspaces))

(defun lsp--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide transformation function and disable sort.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((col (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt
                                      (lambda (string pred action)
                                        (if (eq action 'metadata)
                                            `(metadata (display-sort-function . identity))
                                          (complete-with-action action col string pred)))
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion col))))

(defconst lsp--system-arch (lambda ()
                             (setq lsp--system-arch
                                   (pcase system-type
                                     ('windows-nt
                                      (pcase system-configuration
                                        ((rx bol "x86_64-") 'x64)
                                        (_ 'x86)))
                                     ('darwin
                                      (pcase system-configuration
                                        ((rx "aarch64-") 'arm64)
                                        (_ 'x64)))
                                     ('gnu/linux
                                       (pcase system-configuration
                                         ((rx bol "x86_64") 'x64)
                                         ((rx bol (| "i386" "i886")) 'x32)))
                                     (_
                                      (pcase system-configuration
                                        ((rx bol "x86_64") 'x64)
                                        ((rx bol (| "i386" "i886")) 'x32))))))
  "Return the system architecture of `Emacs'.
Special values:
  `x64'       64bit
  `x32'       32bit
  `arm64'     ARM 64bit")

(defmacro lsp-with-current-buffer (buffer-id &rest body)
  (declare (indent 1) (debug t))
  `(if-let ((wcb (plist-get ,buffer-id :with-current-buffer)))
       (with-lsp-workspaces (plist-get ,buffer-id :workspaces)
         (funcall wcb (lambda () ,@body)))
     (with-current-buffer ,buffer-id
       ,@body)))

(defvar lsp--throw-on-input nil
  "Make `lsp-*-while-no-input' throws `input' on interrupted.")

(defmacro lsp--catch (tag bodyform &rest handlers)
  "Catch TAG thrown in BODYFORM.
The return value from TAG will be handled in HANDLERS by `pcase'."
  (declare (debug (form form &rest (pcase-PAT body))) (indent 2))
  (let ((re-sym (make-symbol "re")))
    `(let ((,re-sym (catch ,tag ,bodyform)))
       (pcase ,re-sym
         ,@handlers))))

(defmacro lsp--while-no-input (&rest body)
  "Wrap BODY in `while-no-input' and respecting `non-essential'.
If `lsp--throw-on-input' is set, will throw if input is pending, else
return value of `body' or nil if interrupted."
  (declare (debug t) (indent 0))
  `(if non-essential
       (let ((res (while-no-input ,@body)))
         (cond
          ((and lsp--throw-on-input (equal res t))
           (throw 'input :interrupted))
          ((booleanp res) nil)
          (t res)))
     ,@body))

;; A ‘lsp--client’ object describes the client-side behavior of a language
;; server.  It is used to start individual server processes, each of which is
;; represented by a ‘lsp--workspace’ object.  Client objects are normally
;; created using ‘lsp-define-stdio-client’ or ‘lsp-define-tcp-client’.  Each
;; workspace refers to exactly one client, but there can be multiple workspaces
;; for a single client.
(cl-defstruct lsp--client
  ;; ‘language-id’ is a function that receives a buffer as a single argument
  ;; and should return the language identifier for that buffer.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#textdocumentitem
  ;; for a list of language identifiers.  Also consult the documentation for
  ;; the language server represented by this client to find out what language
  ;; identifiers it supports or expects.
  (language-id nil)

  ;; ‘add-on?’ when set to t the server will be started no matter whether there
  ;; is another server handling the same mode.
  (add-on? nil)
  ;; ‘new-connection’ is a function that should start a language server process
  ;; and return a cons (COMMAND-PROCESS . COMMUNICATION-PROCESS).
  ;; COMMAND-PROCESS must be a process object representing the server process
  ;; just started.  COMMUNICATION-PROCESS must be a process (including pipe and
  ;; network processes) that ‘lsp-mode’ uses to communicate with the language
  ;; server using the language server protocol.  COMMAND-PROCESS and
  ;; COMMUNICATION-PROCESS may be the same process; in that case
  ;; ‘new-connection’ may also return that process as a single
  ;; object. ‘new-connection’ is called with two arguments, FILTER and
  ;; SENTINEL.  FILTER should be used as process filter for
  ;; COMMUNICATION-PROCESS, and SENTINEL should be used as process sentinel for
  ;; COMMAND-PROCESS.
  (new-connection nil)

  ;; ‘ignore-regexps’ is a list of regexps.  When a data packet from the
  ;; language server matches any of these regexps, it will be ignored.  This is
  ;; intended for dealing with language servers that output non-protocol data.
  (ignore-regexps nil)

  ;; ‘ignore-messages’ is a list of regexps.  When a message from the language
  ;; server matches any of these regexps, it will be ignored.  This is useful
  ;; for filtering out unwanted messages; such as servers that send nonstandard
  ;; message types, or extraneous log messages.
  (ignore-messages nil)

  ;; ‘notification-handlers’ is a hash table mapping notification method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a notification, ‘lsp-mode’ will call the associated handler
  ;; function passing two arguments, the ‘lsp--workspace’ object and the
  ;; deserialized notification parameters.
  (notification-handlers (make-hash-table :test 'equal))

  ;; ‘request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a request, ‘lsp-mode’ will call the associated handler function
  ;; passing two arguments, the ‘lsp--workspace’ object and the deserialized
  ;; request parameters.
  (request-handlers (make-hash-table :test 'equal))

  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql))

  ;; ‘prefix-function’ is called for getting the prefix for completion.
  ;; The function takes no parameter and returns a cons (start . end) representing
  ;; the start and end bounds of the prefix. If it's not set, the client uses a
  ;; default prefix function."
  (prefix-function nil)

  ;; Contains mapping of scheme to the function that is going to be used to load
  ;; the file.
  (uri-handlers (make-hash-table :test #'equal))

  ;; ‘action-handlers’ is a hash table mapping action to a handler function. It
  ;; can be used in `lsp-execute-code-action' to determine whether the action
  ;; current client is interested in executing the action instead of sending it
  ;; to the server.
  (action-handlers (make-hash-table :test 'equal))

  ;; major modes supported by the client.
  major-modes
  ;; Function that will be called to decide if this language client
  ;; should manage a particular buffer. The function will be passed
  ;; the file name and major mode to inform the decision. Setting
  ;; `activation-fn' will override `major-modes', if
  ;; present.
  activation-fn
  ;; Break the tie when major-mode is supported by multiple clients.
  (priority 0)
  ;; Unique identifier for representing the client object.
  server-id
  ;; defines whether the client supports multi root workspaces.
  multi-root
  ;; Initialization options or a function that returns initialization options.
  initialization-options
  ;; `semantic-tokens-faces-overrides’ is a plist that can be used to extend, or
  ;; completely replace, the faces used for semantic highlighting on a
  ;; client-by-client basis.
  ;;
  ;; It recognizes four members, all of which are optional: `:types’ and
  ;; `:modifiers’, respectively, should be face definition lists akin to
  ;; `:lsp-semantic-token-faces’. If specified, each of these face lists will be
  ;; merged with the default face definition list.
  ;;
  ;; Alternatively, if the plist members `:discard-default-types’ or
  ;; `:discard-default-modifiers' are non-nil, the default `:type' or `:modifiers'
  ;; face definitions will be replaced entirely by their respective overrides.
  ;;
  ;; For example, setting `:semantic-tokens-faces-overrides' to
  ;; `(:types (("macro" . font-lock-keyword-face)))' will remap "macro" tokens from
  ;; their default face `lsp-face-semhl-macro' to `font-lock-keyword-face'.
  ;;
  ;; `(:types (("macro" . font-lock-keyword-face) ("not-quite-a-macro" . some-face)))'
  ;; will also remap "macro", but on top of that associate the fictional token type
  ;; "not-quite-a-macro" with the face named `some-face'.
  ;;
  ;; `(:types (("macro" . font-lock-keyword-face))
  ;;   :modifiers (("declaration" . lsp-face-semhl-interface))
  ;;   :discard-default-types t
  ;;   :discard-default-modifiers t)'
  ;; will discard all default face definitions, hence leaving the client with
  ;; only one token type "macro", mapped to `font-lock-keyword-face', and one
  ;; modifier type "declaration", mapped to `lsp-face-semhl-interface'.
  semantic-tokens-faces-overrides
  ;; Provides support for registering LSP Server specific capabilities.
  custom-capabilities
  ;; Function which returns the folders that are considered to be not projects but library files.
  ;; The function accepts one parameter currently active workspace.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/225.
  library-folders-fn
  ;; function which will be called when opening file in the workspace to perform
  ;; client specific initialization. The function accepts one parameter
  ;; currently active workspace.
  before-file-open-fn
  ;; Function which will be called right after a workspace has been initialized.
  initialized-fn
  ;; ‘remote?’ indicate whether the client can be used for LSP server over TRAMP.
  (remote? nil)

  ;; ‘completion-in-comments?’ t if the client supports completion in comments.
  (completion-in-comments? nil)

  ;; ‘path->uri-fn’ the function to use for path->uri conversion for the client.
  (path->uri-fn nil)

  ;; ‘uri->path-fn’ the function to use for uri->path conversion for the client.
  (uri->path-fn nil)
  ;; Function that returns an environment structure that will be used
  ;; to set some environment variables when starting the language
  ;; server process. These environment variables enable some
  ;; additional features in the language server. The environment
  ;; structure is an alist of the form (KEY . VALUE), where KEY is a
  ;; string (regularly in all caps), and VALUE may be a string, a
  ;; boolean, or a sequence of strings.
  environment-fn

  ;; ‘after-open-fn’ workspace after open specific hooks.
  (after-open-fn nil)

  ;; ‘async-request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective requests that may take
  ;; time to finish.  Upon receiving a request, ‘lsp-mode’ will call the
  ;; associated handler function passing three arguments, the ‘lsp--workspace’
  ;; object, the deserialized request parameters and the callback which accept
  ;; result as its parameter.
  (async-request-handlers (make-hash-table :test 'equal))
  download-server-fn
  download-in-progress?
  buffers
  synchronize-sections)

(defun lsp-clients-executable-find (find-command &rest args)
  "Finds an executable by invoking a search command.

FIND-COMMAND is the executable finder that searches for the
actual language server executable. ARGS is a list of arguments to
give to FIND-COMMAND to find the language server.  Returns the
output of FIND-COMMAND if it exits successfully, nil otherwise.

Typical uses include finding an executable by invoking `find' in
a project, finding LLVM commands on macOS with `xcrun', or
looking up project-specific language servers for projects written
in the various dynamic languages, e.g. `nvm', `pyenv' and `rbenv'
etc."
  (when-let* ((find-command-path (executable-find find-command))
              (executable-path
               (with-temp-buffer
                 (when (zerop (apply 'call-process find-command-path nil t nil args))
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (string-trim executable-path)))

(defvar lsp--already-widened nil)

(defmacro lsp-save-restriction-and-excursion (&rest form)
  (declare (indent 0) (debug t))
  `(if lsp--already-widened
       (save-excursion ,@form)
     (-let [lsp--already-widened t]
       (save-restriction
         (widen)
         (save-excursion ,@form)))))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp--line-character-to-point (line character)
  "Return the point for character CHARACTER on line LINE."
  (or (lsp-virtual-buffer-call :line/character->point line character)
      (let ((inhibit-field-text-motion t))
        (lsp-save-restriction-and-excursion
          (goto-char (point-min))
          (forward-line line)
          ;; server may send character position beyond the current line and we
          ;; should fallback to line end.
          (-let [line-end (line-end-position)]
            (if (> character (- line-end (point)))
                line-end
              (forward-char character)
              (point)))))))

(lsp-defun lsp--position-to-point ((&Position :line :character))
  "Convert `Position' object in PARAMS to a point."
  (lsp--line-character-to-point line character))

(lsp-defun lsp--range-to-region ((&RangeToPoint :start :end))
  (cons start end))

(lsp-defun lsp--range-text ((&RangeToPoint :start :end))
  (buffer-substring start end))

(lsp-defun lsp--find-wrapping-range ((&SelectionRange :parent? :range (&RangeToPoint :start :end)))
  (cond
   ((and
     (region-active-p)
     (<= start (region-beginning) end)
     (<= start (region-end) end)
     (or (not (= start (region-beginning)))
         (not (= end (region-end)))))
    (cons start end))
   ((and (<= start (point) end)
         (not (region-active-p)))
    (cons start end))
   (parent? (lsp--find-wrapping-range parent?))))

(defun lsp--get-selection-range ()
  (or
   (-when-let ((cache . cache-tick) lsp--document-selection-range-cache)
     (when (= cache-tick (buffer-modified-tick)) cache))
   (let ((response (cl-first
                    (lsp-request
                     "textDocument/selectionRange"
                     (list :textDocument (lsp--text-document-identifier)
                           :positions (vector (lsp--cur-position)))))))
     (setq lsp--document-selection-range-cache
           (cons response (buffer-modified-tick)))
     response)))

(defun lsp-extend-selection ()
  "Extend selection."
  (interactive)
  (unless (lsp-feature? "textDocument/selectionRange")
    (signal 'lsp-capability-not-supported (list "selectionRangeProvider")))
  (-when-let ((start . end) (lsp--find-wrapping-range (lsp--get-selection-range)))
    (goto-char start)
    (set-mark (point))
    (goto-char end)
    (exchange-point-and-mark)))

(defun lsp-warn (message &rest args)
  "Display a warning message made from (`format-message' MESSAGE ARGS...).
This is equivalent to `display-warning', using `lsp-mode' as the type and
`:warning' as the level."
  (display-warning 'lsp-mode (apply #'format-message message args)))

(defun lsp--get-uri-handler (scheme)
  "Get uri handler for SCHEME in the current workspace."
  (--some (gethash scheme (lsp--client-uri-handlers (lsp--workspace-client it)))
          (or (lsp-workspaces) (lsp--session-workspaces (lsp-session)))))

(defun lsp--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp--uri-to-path (uri)
  "Convert URI to a file path."
  (if-let ((fn (->> (lsp-workspaces)
                    (-keep (-compose #'lsp--client-uri->path-fn #'lsp--workspace-client))
                    (cl-first))))
      (funcall fn uri)
    (lsp--uri-to-path-1 uri)))

(defun lsp-remap-path-if-needed (file-name)
  (-if-let ((virtual-buffer &as &plist :buffer) (gethash file-name lsp--virtual-buffer-mappings))
      (propertize (buffer-local-value 'buffer-file-name buffer)
                  'lsp-virtual-buffer virtual-buffer)
    file-name))

(defun lsp--uri-to-path-1 (uri)
  "Convert URI to a file path."
  (let* ((url (url-generic-parse-url (url-unhex-string uri)))
         (type (url-type url))
         (target (url-target url))
         (file
          (concat (decode-coding-string (url-filename url)
                                        (or locale-coding-system 'utf-8))
                  (when (and target
                             (not (s-match
                                   (rx "#" (group (1+ num)) (or "," "#")
                                       (group (1+ num))
                                       string-end)
                                   uri)))
                    (concat "#" target))))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          uri)
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))
    (->> file-name
         (concat (-some #'lsp--workspace-host-root (lsp-workspaces)))
         (lsp-remap-path-if-needed))))

(defun lsp--buffer-uri ()
  "Return URI of the current buffer."
  (or lsp-buffer-uri
      (plist-get lsp--virtual-buffer :buffer-uri)
      (lsp--path-to-uri
       (or (buffer-file-name) (buffer-file-name (buffer-base-buffer))))))

(defun lsp-register-client-capabilities (&rest _args)
  "Implemented only to make `company-lsp' happy.
DELETE when `lsp-mode.el' is deleted.")

(defconst lsp--url-path-allowed-chars
  (url--allowed-chars (append '(?/) url-unreserved-chars))
  "`url-unreserved-chars' with additional delim ?/.
This set of allowed chars is enough for hexifying local file paths.")

(defun lsp--path-to-uri-1 (path)
  (concat lsp--uri-file-prefix
          (--> path
            (expand-file-name it)
            (or (file-remote-p it 'localname t) it)
            (url-hexify-string it lsp--url-path-allowed-chars))))

(defun lsp--path-to-uri (path)
  "Convert PATH to a uri."
  (if-let ((uri-fn (->> (lsp-workspaces)
                        (-keep (-compose #'lsp--client-path->uri-fn #'lsp--workspace-client))
                        (cl-first))))
      (funcall uri-fn path)
    (lsp--path-to-uri-1 path)))

(defun lsp--string-match-any (regex-list str)
  "Return the first regex, if any, within REGEX-LIST matching STR."
  (--first (string-match it str) regex-list))

(cl-defstruct lsp-watch
  (descriptors (make-hash-table :test 'equal))
  root-directory)

(defun lsp--folder-watch-callback (event callback watch ignored-files ignored-directories)
  (let ((file-name (cl-third event))
        (event-type (cl-second event)))
    (cond
     ((and (file-directory-p file-name)
           (equal 'created event-type)
           (not (lsp--string-match-any ignored-directories file-name)))

      (lsp-watch-root-folder (file-truename file-name) callback ignored-files ignored-directories watch)

      ;; process the files that are already present in
      ;; the directory.
      (->> (directory-files-recursively file-name ".*" t)
           (seq-do (lambda (f)
                     (unless (file-directory-p f)
                       (funcall callback (list nil 'created f)))))))
     ((and (memq event-type '(created deleted changed))
           (not (file-directory-p file-name))
           (not (lsp--string-match-any ignored-files file-name)))
      (funcall callback event))
     ((and (memq event-type '(renamed))
           (not (file-directory-p file-name))
           (not (lsp--string-match-any ignored-files file-name)))
      (funcall callback `(,(cl-first event) deleted ,(cl-third event)))
      (funcall callback `(,(cl-first event) created ,(cl-fourth event)))))))

(defun lsp--ask-about-watching-big-repo (number-of-directories dir)
  "Ask the user if they want to watch NUMBER-OF-DIRECTORIES from a repository DIR.
This is useful when there is a lot of files in a repository, as
that may slow Emacs down. Returns t if the user wants to watch
the entire repository, nil otherwise."
  (prog1
      (yes-or-no-p
       (format
        "Watching all the files in %s would require adding watches to %s directories, so watching the repo may slow Emacs down.
Do you want to watch all files in %s? "
        dir
        number-of-directories
        dir))
    (lsp--info
     (concat "You can configure this warning with the `lsp-enable-file-watchers' "
             "and `lsp-file-watch-threshold' variables"))))


(defun lsp--path-is-watchable-directory (path dir ignored-directories)
  "Figure out whether PATH (inside of DIR) is meant to have a file watcher set.
IGNORED-DIRECTORIES is a list of regexes to filter out directories we don't
want to watch."
  (let
      ((full-path (f-join dir path)))
    (and (file-accessible-directory-p full-path)
         (not (equal path "."))
         (not (equal path ".."))
         (not (lsp--string-match-any ignored-directories full-path)))))


(defun lsp--all-watchable-directories (dir ignored-directories)
  "Traverse DIR recursively returning a list of paths that should have watchers.
IGNORED-DIRECTORIES will be used for exclusions"
  (let* ((dir (if (f-symlink? dir)
                  (file-truename dir)
                dir)))
    (apply #'nconc
           ;; the directory itself is assumed to be part of the set
           (list dir)
           ;; collect all subdirectories that are watchable
           (-map
            (lambda (path) (lsp--all-watchable-directories (f-join dir path) ignored-directories))
            ;; but only look at subdirectories that are watchable
            (-filter (lambda (path) (lsp--path-is-watchable-directory path dir ignored-directories))
                     (directory-files dir))))))

(defun lsp-watch-root-folder (dir callback ignored-files ignored-directories &optional watch warn-big-repo?)
  "Create recursive file notification watch in DIR.
CALLBACK will be called when there are changes in any of
the monitored files. WATCHES is a hash table directory->file
notification handle which contains all of the watch that
already have been created. Watches will not be created for
any directory that matches any regex in IGNORED-DIRECTORIES.
Watches will not be created for any file that matches any
regex in IGNORED-FILES."
  (let* ((dir (if (f-symlink? dir)
                  (file-truename dir)
                dir))
         (watch (or watch (make-lsp-watch :root-directory dir)))
         (dirs-to-watch (lsp--all-watchable-directories dir ignored-directories)))
    (lsp-log "Creating watchers for following %s folders:\n  %s"
             (length dirs-to-watch)
             (s-join "\n  " dirs-to-watch))
    (when (or
           (not warn-big-repo?)
           (not lsp-file-watch-threshold)
           (let ((number-of-directories (length dirs-to-watch)))
             (or
              (< number-of-directories lsp-file-watch-threshold)
              (condition-case nil
                  (lsp--ask-about-watching-big-repo number-of-directories dir)
                (quit)))))
      (dolist (current-dir dirs-to-watch)
        (condition-case err
            (progn
              (puthash
               current-dir
               (file-notify-add-watch current-dir
                                      '(change)
                                      (lambda (event)
                                        (lsp--folder-watch-callback event callback watch ignored-files ignored-directories)))
               (lsp-watch-descriptors watch)))
          (error (lsp-log "Failed to create a watch for %s: message" (error-message-string err)))
          (file-missing (lsp-log "Failed to create a watch for %s: message" (error-message-string err))))))
    watch))

(defun lsp-kill-watch (watch)
  "Delete WATCH."
  (-> watch lsp-watch-descriptors hash-table-values (-each #'file-notify-rm-watch))
  (ht-clear! (lsp-watch-descriptors watch)))

(defun lsp-json-bool (val)
  "Convert VAL to JSON boolean."
  (if val t :json-false))

(defmacro with-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--cur-workspace ,workspace)) ,@body))

(defmacro with-lsp-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against multiple WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--buffer-workspaces ,workspaces)) ,@body))



(defmacro lsp-consistency-check (package)
  `(defconst ,(intern (concat (symbol-name package)
                              "-plist-value-when-compiled"))
     (eval-when-compile lsp-use-plists)))


;; loading code-workspace files

;;;###autoload
(defun lsp-load-vscode-workspace (file)
  "Load vscode workspace from FILE"
  (interactive "fSelect file to import: ")
  (mapc #'lsp-workspace-folders-remove (lsp-session-folders (lsp-session)))

  (let ((dir (f-dirname file)))
    (->> file
         (json-read-file)
         (alist-get 'folders)
         (-map (-lambda ((&alist 'path))
                 (lsp-workspace-folders-add (expand-file-name path dir)))))))

;;;###autoload
(defun lsp-save-vscode-workspace (file)
  "Save vscode workspace to FILE"
  (interactive "FSelect file to save to: ")

  (let ((json-encoding-pretty-print t))
    (f-write-text (json-encode
                   `((folders . ,(->> (lsp-session)
                                      (lsp-session-folders)
                                      (--map `((path . ,it)))))))
                  'utf-8
                  file)))


(defmacro lsp-foreach-workspace (&rest body)
  "Execute BODY for each of the current workspaces."
  (declare (debug (form body)))
  `(--map (with-lsp-workspace it ,@body) (lsp-workspaces)))

(defmacro when-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context if present."
  (declare (debug (form body))
           (indent 1))
  `(when-let ((lsp--cur-workspace ,workspace)) ,@body))

(lsp-defun lsp--window-show-quick-pick (_workspace (&ShowQuickPickParams :place-holder :can-pick-many :items))
  (if-let* ((selectfunc (if can-pick-many #'completing-read-multiple #'completing-read))
            (itemLabels (seq-map (-lambda ((item &as &QuickPickItem :label)) (format "%s" label))
                                 items))
            (result (funcall-interactively
                     selectfunc
                     (format "%s%s " place-holder (if can-pick-many " (* for all)" "")) itemLabels))
            (choices (if (listp result)
                         (if (equal result '("*"))
                             itemLabels
                           result)
                       (list result))))
      (vconcat (seq-filter #'identity (seq-map (-lambda ((item &as &QuickPickItem :label :user-data))
                                                 (if (member label choices)
                                                     (lsp-make-quick-pick-item :label label :picked t :user-data user-data)
                                                   nil))
                                               items)))))

(lsp-defun lsp--window-show-input-box (_workspace (&ShowInputBoxParams :prompt :value?))
  (read-string (format "%s: " prompt) (or value? "")))

(lsp-defun lsp--window-show-message (_workspace (&ShowMessageRequestParams :message :type))
  "Send the server's messages to log.
PARAMS - the data sent from _WORKSPACE."
  (funcall (cl-case type
             (1 'lsp--error)
             (2 'lsp--warn)
             (t 'lsp--info))
           "%s"
           message))

(lsp-defun lsp--window-log-message (workspace (&ShowMessageRequestParams :message :type))
  "Send the server's messages to log.
PARAMS - the data sent from WORKSPACE."
  (ignore
   (let ((client (lsp--workspace-client workspace)))
     (when (or (not client)
               (cl-notany (-rpartial #'string-match-p message)
                          (lsp--client-ignore-messages client)))
       (lsp-log "%s" (lsp--propertize message type))))))

(lsp-defun lsp--window-log-message-request ((&ShowMessageRequestParams :message :type :actions?))
  "Display a message request to user sending the user selection back to server."
  (let* ((message (lsp--propertize message type))
         (choices (seq-map #'lsp:message-action-item-title actions?)))
    (if choices
        (completing-read (concat message " ") (seq-into choices 'list) nil t)
      (lsp-log message))))

(lsp-defun lsp--window-show-document ((&ShowDocumentParams :uri :selection?))
  "Show document URI in a buffer and go to SELECTION if any."
  (let ((path (lsp--uri-to-path uri)))
    (when (f-exists? path)
      (with-current-buffer (find-file path)
        (when selection?
          (goto-char (lsp--position-to-point (lsp:range-start selection?))))
        t))))

(defcustom lsp-progress-prefix " ⌛ "
  "Progress prefix."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-progress-function #'lsp-on-progress-modeline
  "Function for handling the progress notifications."
  :group 'lsp-mode
  :type '(choice
          (const :tag "Use modeline" lsp-on-progress-modeline)
          (const :tag "Legacy(uses either `progress-reporter' or `spinner' based on `lsp-progress-via-spinner')"
                 lsp-on-progress-legacy)
          (const :tag "Ignore" ignore)
          (function :tag "Other function"))
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-request-while-no-input-may-block nil
  "Have `lsp-request-while-no-input` block unless `non-essential` is t."
  :group 'lsp-mode
  :type 'boolean)

(defun lsp--progress-status ()
  "Returns the status of the progress for the current workspaces."
  (-let ((progress-status
          (s-join
           "|"
           (-keep
            (lambda (workspace)
              (let ((tokens (lsp--workspace-work-done-tokens workspace)))
                (unless (ht-empty? tokens)
                  (mapconcat
                   (-lambda ((&WorkDoneProgressBegin :message? :title :percentage?))
                     (concat (if percentage?
                                 (if (numberp percentage?)
                                     (format "%.0f%%%% " percentage?)
                                   (format "%s%%%% " percentage?))
                               "")
                             (or message? title)))
                   (ht-values tokens)
                   "|"))))
            (lsp-workspaces)))))
    (unless (s-blank? progress-status)
      (concat lsp-progress-prefix progress-status))))

(lsp-defun lsp-on-progress-modeline (workspace (&ProgressParams :token :value
                                                                (value &as &WorkDoneProgress :kind)))
  "PARAMS contains the progress data.
WORKSPACE is the workspace that contains the progress token."
  (add-to-list 'global-mode-string '(t (:eval (lsp--progress-status))))
  (pcase kind
    ("begin" (lsp-workspace-set-work-done-token token value workspace))
    ("report" (lsp-workspace-set-work-done-token token value workspace))
    ("end" (lsp-workspace-rem-work-done-token token workspace)))
  (force-mode-line-update))

(lsp-defun lsp-on-progress-legacy (workspace (&ProgressParams :token :value
                                                              (value &as &WorkDoneProgress :kind)))
  "PARAMS contains the progress data.
WORKSPACE is the workspace that contains the progress token."
  (pcase kind
    ("begin"
     (-let* (((&WorkDoneProgressBegin :title :percentage?) value)
             (reporter
              (if lsp-progress-via-spinner
                  (let* ((spinner-strings (alist-get (lsp-progress-spinner-type) spinner-types))
                         ;; Set message as a tooltip for the spinner strings
                         (propertized-strings
                          (seq-map (lambda (string) (propertize string 'help-echo title))
                                   spinner-strings))
                         (spinner-type (vconcat propertized-strings)))
                    ;; The progress relates to the server as a whole,
                    ;; display it on all buffers.
                    (mapcar (lambda (buffer)
                              (lsp-with-current-buffer buffer
                                (spinner-start spinner-type))
                              buffer)
                            (lsp--workspace-buffers workspace)))
                (if percentage?
                    (make-progress-reporter title 0 100 percentage?)
                  ;; No percentage, just progress
                  (make-progress-reporter title nil nil)))))
       (lsp-workspace-set-work-done-token token reporter workspace)))
    ("report"
     (when-let ((reporter (lsp-workspace-get-work-done-token token workspace)))
       (unless lsp-progress-via-spinner
         (progress-reporter-update reporter (lsp:work-done-progress-report-percentage? value)))))

    ("end"
     (when-let ((reporter (lsp-workspace-get-work-done-token token workspace)))
       (if lsp-progress-via-spinner
           (mapc (lambda (buffer)
                   (when (lsp-buffer-live-p buffer)
                     (lsp-with-current-buffer buffer
                       (spinner-stop))))
                 reporter)
         (progress-reporter-done reporter))
       (lsp-workspace-rem-work-done-token token workspace)))))


;; diagnostics

(defvar lsp-diagnostic-filter nil
  "A a function which will be called with
  `&PublishDiagnosticsParams' and `workspace' which can be used
  to filter out the diagnostics. The function should return
  `&PublishDiagnosticsParams'.

Common usecase are:
1. Filter the diagnostics for a particular language server.
2. Filter out the diagnostics under specific level.")

(defvar lsp-diagnostic-stats (ht))

(defun lsp-diagnostics (&optional current-workspace?)
  "Return the diagnostics from all workspaces."
  (or (pcase (if current-workspace?
                 (lsp-workspaces)
               (lsp--session-workspaces (lsp-session)))
        (`() ())
        (`(,workspace) (lsp--workspace-diagnostics workspace))
        (`,workspaces (let ((result (make-hash-table :test 'equal)))
                        (mapc (lambda (workspace)
                                (->> workspace
                                     (lsp--workspace-diagnostics)
                                     (maphash (lambda (file-name diagnostics)
                                                (puthash file-name
                                                         (append (gethash file-name result) diagnostics)
                                                         result)))))
                              workspaces)
                        result)))
      (ht)))

(defun lsp-diagnostics-stats-for (path)
  "Get diagnostics statistics for PATH.
The result format is vector [_ errors warnings infos hints] or nil."
  (gethash (lsp--fix-path-casing path) lsp-diagnostic-stats))

(defun lsp-diagnostics--update-path (path new-stats)
  (let ((new-stats (copy-sequence new-stats))
        (path (lsp--fix-path-casing (directory-file-name path))))
    (if-let ((old-data (gethash path lsp-diagnostic-stats)))
        (dotimes (idx 5)
          (cl-callf + (aref old-data idx)
            (aref new-stats idx)))
      (puthash path new-stats lsp-diagnostic-stats))))

(lsp-defun lsp--on-diagnostics-update-stats (workspace
                                             (&PublishDiagnosticsParams :uri :diagnostics))
  (let ((path (lsp--fix-path-casing (lsp--uri-to-path uri)))
        (new-stats (make-vector 5 0)))
    (mapc (-lambda ((&Diagnostic :severity?))
            (cl-incf (aref new-stats (or severity? 1))))
          diagnostics)
    (when-let ((old-diags (gethash path (lsp--workspace-diagnostics workspace))))
      (mapc (-lambda ((&Diagnostic :severity?))
              (cl-decf (aref new-stats (or severity? 1))))
            old-diags))
    (lsp-diagnostics--update-path path new-stats)
    (while (not (string= path (setf path (file-name-directory
                                          (directory-file-name path)))))
      (lsp-diagnostics--update-path path new-stats))))

(defun lsp--on-diagnostics (workspace params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}
PARAMS contains the diagnostics data.
WORKSPACE is the workspace that contains the diagnostics."
  (when lsp-diagnostic-filter
    (setf params (funcall lsp-diagnostic-filter params workspace)))

  (lsp--on-diagnostics-update-stats workspace params)

  (-let* (((&PublishDiagnosticsParams :uri :diagnostics) params)
          (lsp--virtual-buffer-mappings (ht))
          (file (lsp--fix-path-casing (lsp--uri-to-path uri)))
          (workspace-diagnostics (lsp--workspace-diagnostics workspace)))

    (if (seq-empty-p diagnostics)
        (remhash file workspace-diagnostics)
      (puthash file (append diagnostics nil) workspace-diagnostics))

    (run-hooks 'lsp-diagnostics-updated-hook)))

(defun lsp-diagnostics--workspace-cleanup (workspace)
  (->> workspace
       (lsp--workspace-diagnostics)
       (maphash (lambda (key _)
                  (lsp--on-diagnostics-update-stats
                   workspace
                   (lsp-make-publish-diagnostics-params
                    :uri (lsp--path-to-uri key)
                    :diagnostics [])))))
  (clrhash (lsp--workspace-diagnostics workspace)))



;; textDocument/foldingRange support

(cl-defstruct lsp--folding-range beg end kind children)

(defvar-local lsp--cached-folding-ranges nil)
(defvar-local lsp--cached-nested-folding-ranges nil)

(defun lsp--folding-range-width (range)
  (- (lsp--folding-range-end range)
     (lsp--folding-range-beg range)))

(defun lsp--get-folding-ranges ()
  "Get the folding ranges for the current buffer."
  (unless (eq (buffer-chars-modified-tick) (car lsp--cached-folding-ranges))
    (let* ((ranges (lsp-request "textDocument/foldingRange"
                                `(:textDocument ,(lsp--text-document-identifier))))
           (sorted-line-col-pairs (->> ranges
                                       (cl-mapcan (-lambda ((&FoldingRange :start-line
                                                                           :start-character?
                                                                           :end-line
                                                                           :end-character?))
                                                    (list (cons start-line start-character?)
                                                          (cons end-line end-character?))))
                                       (-sort #'lsp--line-col-comparator)))
           (line-col-to-point-map (lsp--convert-line-col-to-points-batch
                                   sorted-line-col-pairs)))
      (setq lsp--cached-folding-ranges
            (cons (buffer-chars-modified-tick)
                  (--> ranges
                    (seq-map (-lambda ((range &as
                                              &FoldingRange :start-line
                                              :start-character?
                                              :end-line
                                              :end-character?
                                              :kind?))
                               (make-lsp--folding-range
                                :beg (ht-get line-col-to-point-map
                                             (cons start-line start-character?))
                                :end (ht-get line-col-to-point-map
                                             (cons end-line end-character?))
                                :kind kind?))
                             it)
                    (seq-filter (lambda (folding-range)
                                  (< (lsp--folding-range-beg folding-range)
                                     (lsp--folding-range-end folding-range)))
                                it)
                    (seq-into it 'list)
                    (delete-dups it))))))
  (cdr lsp--cached-folding-ranges))

(defun lsp--get-nested-folding-ranges ()
  "Get a list of nested folding ranges for the current buffer."
  (-let [(tick . _) lsp--cached-folding-ranges]
    (if (and (eq tick (buffer-chars-modified-tick))
             lsp--cached-nested-folding-ranges)
        lsp--cached-nested-folding-ranges
      (setq lsp--cached-nested-folding-ranges
            (lsp--folding-range-build-trees (lsp--get-folding-ranges))))))

(defun lsp--folding-range-build-trees (ranges)
  (setq ranges (seq-sort #'lsp--range-before-p ranges))
  (let* ((dummy-node (make-lsp--folding-range
                      :beg most-negative-fixnum
                      :end most-positive-fixnum))
         (stack (list dummy-node)))
    (dolist (range ranges)
      (while (not (lsp--range-inside-p range (car stack)))
        (pop stack))
      (push range (lsp--folding-range-children (car stack)))
      (push range stack))
    (lsp--folding-range-children dummy-node)))

(defun lsp--range-inside-p (r1 r2)
  "Return non-nil if folding range R1 lies inside R2"
  (and (>= (lsp--folding-range-beg r1) (lsp--folding-range-beg r2))
       (<= (lsp--folding-range-end r1) (lsp--folding-range-end r2))))

(defun lsp--range-before-p (r1 r2)
  "Return non-nil if folding range R1 ends before R2"
  ;; Ensure r1 comes before r2
  (or (< (lsp--folding-range-beg r1)
         (lsp--folding-range-beg r2))
      ;; If beg(r1) == beg(r2) make sure r2 ends first
      (and (= (lsp--folding-range-beg r1)
              (lsp--folding-range-beg r2))
           (< (lsp--folding-range-end r2)
              (lsp--folding-range-end r1)))))

(defun lsp--point-inside-range-p (point range)
  "Return non-nil if POINT lies inside folding range RANGE."
  (and (>= point (lsp--folding-range-beg range))
       (<= point (lsp--folding-range-end range))))

(cl-defun lsp--get-current-innermost-folding-range (&optional (point (point)))
  "Return the innermost folding range POINT lies in."
  (seq-reduce (lambda (innermost-range curr-range)
                (if (and (lsp--point-inside-range-p point curr-range)
                         (or (null innermost-range)
                             (lsp--range-inside-p curr-range innermost-range)))
                    curr-range
                  innermost-range))
              (lsp--get-folding-ranges)
              nil))

(cl-defun lsp--get-current-outermost-folding-range (&optional (point (point)))
  "Return the outermost folding range POINT lies in."
  (cdr (seq-reduce (-lambda ((best-pair &as outermost-width . _) curr-range)
                     (let ((curr-width (lsp--folding-range-width curr-range)))
                       (if (and (lsp--point-inside-range-p point curr-range)
                                (or (null best-pair)
                                    (> curr-width outermost-width)))
                           (cons curr-width curr-range)
                         best-pair)))
                   (lsp--get-folding-ranges)
                   nil)))

(defun lsp--folding-range-at-point-bounds ()
  (when (and lsp-enable-folding
             (lsp-feature? "textDocument/foldingRange"))
    (if-let ((range (lsp--get-current-innermost-folding-range)))
        (cons (lsp--folding-range-beg range)
              (lsp--folding-range-end range)))))
(put 'lsp--folding-range 'bounds-of-thing-at-point
     #'lsp--folding-range-at-point-bounds)

(defun lsp--get-nearest-folding-range (&optional backward)
  (let ((point (point))
        (found nil))
    (while (not
            (or found
                (if backward
                    (<= point (point-min))
                  (>= point (point-max)))))
      (if backward (cl-decf point) (cl-incf point))
      (setq found (lsp--get-current-innermost-folding-range point)))
    found))

(defun lsp--folding-range-at-point-forward-op (n)
  (when (and lsp-enable-folding
             (not (zerop n))
             (lsp-feature? "textDocument/foldingRange"))
    (cl-block break
      (dotimes (_ (abs n))
        (if-let ((range (lsp--get-nearest-folding-range (< n 0))))
            (goto-char (if (< n 0)
                           (lsp--folding-range-beg range)
                         (lsp--folding-range-end range)))
          (cl-return-from break))))))
(put 'lsp--folding-range 'forward-op
     #'lsp--folding-range-at-point-forward-op)

(defun lsp--folding-range-at-point-beginning-op ()
  (goto-char (car (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'beginning-op
     #'lsp--folding-range-at-point-beginning-op)

(defun lsp--folding-range-at-point-end-op ()
  (goto-char (cdr (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'end-op
     #'lsp--folding-range-at-point-end-op)

(defun lsp--range-at-point-bounds ()
  (or (lsp--folding-range-at-point-bounds)
      (when-let ((range (and
                         (lsp-feature? "textDocument/hover")
                         (->> (lsp--text-document-position-params)
                              (lsp-request "textDocument/hover")
                              (lsp:hover-range?)))))
        (lsp--range-to-region range))))

;; A more general purpose "thing", useful for applications like focus.el
(put 'lsp--range 'bounds-of-thing-at-point
     #'lsp--range-at-point-bounds)

(defun lsp--log-io-p (method)
  "Return non nil if should log for METHOD."
  (and lsp-log-io
       (or (not lsp-log-io-allowlist-methods)
           (member method lsp-log-io-allowlist-methods))))


;; toggles

(defun lsp-toggle-trace-io ()
  "Toggle client-server protocol logging."
  (interactive)
  (setq lsp-log-io (not lsp-log-io))
  (lsp--info "Server logging %s." (if lsp-log-io "enabled" "disabled")))

(defun lsp-toggle-signature-auto-activate ()
  "Toggle signature auto activate."
  (interactive)
  (setq lsp-signature-auto-activate
        (unless lsp-signature-auto-activate '(:on-trigger-char)))
  (lsp--info "Signature autoactivate %s." (if lsp-signature-auto-activate "enabled" "disabled"))
  (lsp--update-signature-help-hook))

(defun lsp-toggle-on-type-formatting ()
  "Toggle on type formatting."
  (interactive)
  (setq lsp-enable-on-type-formatting (not lsp-enable-on-type-formatting))
  (lsp--info "On type formatting is %s." (if lsp-enable-on-type-formatting "enabled" "disabled"))
  (lsp--update-on-type-formatting-hook))

(defun lsp-toggle-symbol-highlight ()
  "Toggle symbol highlighting."
  (interactive)
  (setq lsp-enable-symbol-highlighting (not lsp-enable-symbol-highlighting))

  (cond
   ((and lsp-enable-symbol-highlighting
         (lsp-feature? "textDocument/documentHighlight"))
    (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t)
    (lsp--info "Symbol highlighting enabled in current buffer."))
   ((not lsp-enable-symbol-highlighting)
    (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
    (lsp--remove-overlays 'lsp-highlight)
    (lsp--info "Symbol highlighting disabled in current buffer."))))


;; keybindings
(defvar lsp--binding-descriptions nil
  "List of key binding/short description pair.")

(defmacro lsp-define-conditional-key (keymap key def desc cond &rest bindings)
  "In KEYMAP, define key sequence KEY as DEF conditionally.
This is like `define-key', except the definition disappears
whenever COND evaluates to nil.
DESC is the short-description for the binding.
BINDINGS is a list of (key def desc cond)."
  (declare (indent defun)
           (debug (form form form form form &rest sexp)))
  (->> (cl-list* key def desc cond bindings)
       (-partition 4)
       (-mapcat (-lambda ((key def desc cond))
                  `((define-key ,keymap ,key
                      '(menu-item
                        ,(format "maybe-%s" def)
                        ,def
                        :filter
                        (lambda (item)
                          (when (with-current-buffer (or (when (buffer-live-p lsp--describe-buffer)
                                                           lsp--describe-buffer)
                                                         (current-buffer))
                                  ,cond)
                            item))))
                    (when (stringp ,key)
                      (setq lsp--binding-descriptions
                            (append lsp--binding-descriptions '(,key ,desc)))))))
       macroexp-progn))

(defvar lsp--describe-buffer nil)

(defun lsp-describe-buffer-bindings-advice (fn buffer &optional prefix menus)
  (let ((lsp--describe-buffer buffer))
    (funcall fn buffer prefix menus)))

(advice-add 'describe-buffer-bindings
            :around
            #'lsp-describe-buffer-bindings-advice)

(defun lsp--prepend-prefix (mappings)
  (->> mappings
       (-partition 2)
       (-mapcat (-lambda ((key description))
                  (list (concat lsp-keymap-prefix " " key)
                        description)))))

(defvar lsp-command-map
  (-doto (make-sparse-keymap)
    (lsp-define-conditional-key
      ;; workspaces
      "wD" lsp-disconnect "disconnect" (lsp-workspaces)
      "wd" lsp-describe-session "describe session" t
      "wq" lsp-workspace-shutdown "shutdown server" (lsp-workspaces)
      "wr" lsp-workspace-restart "restart server" (lsp-workspaces)
      "ws" lsp "start server" t

      ;; formatting
      "==" lsp-format-buffer "format buffer" (or (lsp-feature? "textDocument/rangeFormatting")
                                                 (lsp-feature? "textDocument/formatting"))
      "=r" lsp-format-region "format region" (lsp-feature? "textDocument/rangeFormatting")

      ;; folders
      "Fa" lsp-workspace-folders-add "add folder" t
      "Fb" lsp-workspace-blocklist-remove "un-blocklist folder" t
      "Fr" lsp-workspace-folders-remove "remove folder" t

      ;; toggles
      "TD" lsp-modeline-diagnostics-mode "toggle modeline diagnostics" (lsp-feature?
                                                                        "textDocument/publishDiagnostics")
      "TL" lsp-toggle-trace-io "toggle log io" t
      "TS" lsp-ui-sideline-mode "toggle sideline" (featurep 'lsp-ui-sideline)
      "TT" lsp-treemacs-sync-mode "toggle treemacs integration" (featurep 'lsp-treemacs)
      "Ta" lsp-modeline-code-actions-mode "toggle modeline code actions" (lsp-feature?
                                                                          "textDocument/codeAction")
      "Tb" lsp-headerline-breadcrumb-mode "toggle breadcrumb" (lsp-feature?
                                                               "textDocument/documentSymbol")
      "Td" lsp-ui-doc-mode "toggle documentation popup" (featurep 'lsp-ui-doc)
      "Tf" lsp-toggle-on-type-formatting "toggle on type formatting" (lsp-feature?
                                                                      "textDocument/onTypeFormatting")
      "Th" lsp-toggle-symbol-highlight "toggle highlighting" (lsp-feature? "textDocument/documentHighlight")
      "Tl" lsp-lens-mode "toggle lenses" (lsp-feature? "textDocument/codeLens")
      "Ts" lsp-toggle-signature-auto-activate "toggle signature" (lsp-feature? "textDocument/signatureHelp")

      ;; goto
      "ga" xref-find-apropos "find symbol in workspace" (lsp-feature? "workspace/symbol")
      "gd" lsp-find-declaration "find declarations" (lsp-feature? "textDocument/declaration")
      "ge" lsp-treemacs-errors-list "show errors" (fboundp 'lsp-treemacs-errors-list)
      "gg" lsp-find-definition "find definitions" (lsp-feature? "textDocument/definition")
      "gh" lsp-treemacs-call-hierarchy "call hierarchy" (and (lsp-feature? "callHierarchy/incomingCalls")
                                                             (fboundp 'lsp-treemacs-call-hierarchy))
      "gi" lsp-find-implementation "find implementations" (lsp-feature? "textDocument/implementation")
      "gr" lsp-find-references "find references" (lsp-feature? "textDocument/references")
      "gt" lsp-find-type-definition "find type definition" (lsp-feature? "textDocument/typeDefinition")

      ;; help
      "hg" lsp-ui-doc-glance "glance symbol" (and (featurep 'lsp-ui-doc)
                                                  (lsp-feature? "textDocument/hover"))
      "hh" lsp-describe-thing-at-point "describe symbol at point" (lsp-feature? "textDocument/hover")
      "hs" lsp-signature-activate "signature help" (lsp-feature? "textDocument/signatureHelp")

      ;; refactoring
      "ro" lsp-organize-imports "organize imports" (lsp-feature? "textDocument/codeAction")
      "rr" lsp-rename "rename" (lsp-feature? "textDocument/rename")

      ;; actions
      "aa" lsp-execute-code-action "code actions" (lsp-feature? "textDocument/codeAction")
      "ah" lsp-document-highlight "highlight symbol" (lsp-feature? "textDocument/documentHighlight")
      "al" lsp-avy-lens "lens" (and (bound-and-true-p lsp-lens-mode) (featurep 'avy))

      ;; peeks
      "Gg" lsp-ui-peek-find-definitions "peek definitions" (and (lsp-feature? "textDocument/definition")
                                                                (fboundp 'lsp-ui-peek-find-definitions))
      "Gi" lsp-ui-peek-find-implementation "peek implementations" (and
                                                                   (fboundp 'lsp-ui-peek-find-implementation)
                                                                   (lsp-feature? "textDocument/implementation"))
      "Gr" lsp-ui-peek-find-references "peek references" (and (fboundp 'lsp-ui-peek-find-references)
                                                              (lsp-feature? "textDocument/references"))
      "Gs" lsp-ui-peek-find-workspace-symbol "peek workspace symbol" (and (fboundp
                                                                           'lsp-ui-peek-find-workspace-symbol)
                                                                          (lsp-feature? "workspace/symbol")))))


;; which-key integration

(declare-function which-key-add-major-mode-key-based-replacements "ext:which-key")
(declare-function which-key-add-key-based-replacements "ext:which-key")

(defun lsp-enable-which-key-integration (&optional all-modes)
  "Adds descriptions for `lsp-mode-map' to `which-key-mode' for the current
active `major-mode', or for all major modes when ALL-MODES is t."
  (cl-flet ((which-key-fn (if all-modes
                              'which-key-add-key-based-replacements
                            (apply-partially 'which-key-add-major-mode-key-based-replacements major-mode))))
    (apply
     #'which-key-fn
     (lsp--prepend-prefix
      (cl-list*
       ""    "lsp"
       "w"   "workspaces"
       "F"   "folders"
       "="   "formatting"
       "T"   "toggle"
       "g"   "goto"
       "h"   "help"
       "r"   "refactor"
       "a"   "code actions"
       "G"   "peek"
       lsp--binding-descriptions)))))


;; Globbing syntax

;; We port VSCode's glob-to-regexp code
;; (https://github.com/Microsoft/vscode/blob/466da1c9013c624140f6d1473b23a870abc82d44/src/vs/base/common/glob.ts)
;; since the LSP globbing syntax seems to be the same as that of
;; VSCode.

(defconst lsp-globstar "**"
  "Globstar pattern.")

(defconst lsp-glob-split ?/
  "The character by which we split path components in a glob
pattern.")

(defconst lsp-path-regexp "[/\\\\]"
  "Forward or backslash to be used as a path separator in
computed regexps.")

(defconst lsp-non-path-regexp "[^/\\\\]"
  "A regexp matching anything other than a slash.")

(defconst lsp-globstar-regexp
  (format "\\(?:%s\\|%s+%s\\|%s%s+\\)*?"
          lsp-path-regexp
          lsp-non-path-regexp lsp-path-regexp
          lsp-path-regexp lsp-non-path-regexp)
  "Globstar in regexp form.")

(defun lsp-split-glob-pattern (pattern split-char)
  "Split PATTERN at SPLIT-CHAR while respecting braces and brackets."
  (when pattern
    (let ((segments nil)
          (in-braces nil)
          (in-brackets nil)
          (current-segment ""))
      (dolist (char (string-to-list pattern))
        (cl-block 'exit-point
          (if (eq char split-char)
              (when (and (null in-braces)
                         (null in-brackets))
                (push current-segment segments)
                (setq current-segment "")
                (cl-return-from 'exit-point))
            (pcase char
              (?{
               (setq in-braces t))
              (?}
               (setq in-braces nil))
              (?\[
               (setq in-brackets t))
              (?\]
               (setq in-brackets nil))))
          (setq current-segment (concat current-segment
                                        (char-to-string char)))))
      (unless (string-empty-p current-segment)
        (push current-segment segments))
      (nreverse segments))))

(defun lsp--glob-to-regexp (pattern)
  "Helper function to convert a PATTERN from LSP's glob syntax to
an Elisp regexp."
  (if (string-empty-p pattern)
      ""
    (let ((current-regexp "")
          (glob-segments (lsp-split-glob-pattern pattern lsp-glob-split)))
      (if (-all? (lambda (segment) (eq segment lsp-globstar))
                 glob-segments)
          ".*"
        (let ((prev-segment-was-globstar nil))
          (seq-do-indexed
           (lambda (segment index)
             (if (string-equal segment lsp-globstar)
                 (unless prev-segment-was-globstar
                   (setq current-regexp (concat current-regexp
                                                lsp-globstar-regexp))
                   (setq prev-segment-was-globstar t))
               (let ((in-braces nil)
                     (brace-val "")
                     (in-brackets nil)
                     (bracket-val ""))
                 (dolist (char (string-to-list segment))
                   (cond
                    ((and (not (char-equal char ?\}))
                          in-braces)
                     (setq brace-val (concat brace-val
                                             (char-to-string char))))
                    ((and in-brackets
                          (or (not (char-equal char ?\]))
                              (string-empty-p bracket-val)))
                     (let ((curr (cond
                                  ((char-equal char ?-)
                                   "-")
                                  ;; NOTE: ?\^ and ?^ are different characters
                                  ((and (memq char '(?^ ?!))
                                        (string-empty-p bracket-val))
                                   "^")
                                  ((char-equal char lsp-glob-split)
                                   "")
                                  (t
                                   (regexp-quote (char-to-string char))))))
                       (setq bracket-val (concat bracket-val curr))))
                    (t
                     (cl-case char
                       (?{
                        (setq in-braces t))
                       (?\[
                        (setq in-brackets t))
                       (?}
                        (let* ((choices (lsp-split-glob-pattern brace-val ?\,))
                               (brace-regexp (concat "\\(?:"
                                                     (mapconcat #'lsp--glob-to-regexp choices "\\|")
                                                     "\\)")))
                          (setq current-regexp (concat current-regexp
                                                       brace-regexp))
                          (setq in-braces nil)
                          (setq brace-val "")))
                       (?\]
                        (setq current-regexp
                              (concat current-regexp
                                      "[" bracket-val "]"))
                        (setq in-brackets nil)
                        (setq bracket-val ""))
                       (??
                        (setq current-regexp
                              (concat current-regexp
                                      lsp-non-path-regexp)))
                       (?*
                        (setq current-regexp
                              (concat current-regexp
                                      lsp-non-path-regexp "*?")))
                       (t
                        (setq current-regexp
                              (concat current-regexp
                                      (regexp-quote (char-to-string char)))))))))
                 (when (and (< index (1- (length glob-segments)))
                            (or (not (string-equal (nth (1+ index) glob-segments)
                                                   lsp-globstar))
                                (< (+ index 2)
                                   (length glob-segments))))
                   (setq current-regexp
                         (concat current-regexp
                                 lsp-path-regexp)))
                 (setq prev-segment-was-globstar nil))))
           glob-segments)
          current-regexp)))))

;; See https://github.com/emacs-lsp/lsp-mode/issues/2365
(defun lsp-glob-unbrace-at-top-level (glob-pattern)
  "If GLOB-PATTERN does not start with a brace, return a singleton list
containing GLOB-PATTERN.

If GLOB-PATTERN does start with a brace, return a list of the
comma-separated globs within the top-level braces."
  (if (not (string-prefix-p "{" glob-pattern))
      (list glob-pattern)
    (lsp-split-glob-pattern (substring glob-pattern 1 -1) ?\,)))

(defun lsp-glob-convert-to-wrapped-regexp (glob-pattern)
  "Convert GLOB-PATTERN to a regexp wrapped with the beginning-
and end-of-string meta-characters."
  (concat "\\`" (lsp--glob-to-regexp (string-trim glob-pattern)) "\\'"))

(defun lsp-glob-to-regexps (glob-pattern)
  "Convert a GLOB-PATTERN to a list of Elisp regexps."
  (let* ((trimmed-pattern (string-trim glob-pattern))
         (top-level-unbraced-patterns (lsp-glob-unbrace-at-top-level trimmed-pattern)))
    (seq-map #'lsp-glob-convert-to-wrapped-regexp
             top-level-unbraced-patterns)))



(defvar lsp-mode-menu)

(defun lsp-mouse-click (event)
  (interactive "e")
  (let* ((ec (event-start event))
         (choice (x-popup-menu event lsp-mode-menu))
         (action (lookup-key lsp-mode-menu (apply 'vector choice))))

    (select-window (posn-window ec))

    (unless (and (region-active-p) (eq action 'lsp-execute-code-action))
      (goto-char (posn-point ec)))
    (run-with-idle-timer
     0.001 nil
     (lambda ()
       (cl-labels ((check (value) (not (null value))))
         (when choice
           (call-interactively action)))))))

(defvar lsp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<down-mouse-1>") #'lsp-find-definition-mouse)
    (define-key map (kbd "C-<mouse-1>") #'ignore)
    (define-key map (kbd "<mouse-3>") #'lsp-mouse-click)
    (define-key map (kbd "C-S-SPC") #'lsp-signature-activate)
    (when lsp-keymap-prefix
      (define-key map (kbd lsp-keymap-prefix) lsp-command-map))
    map)
  "Keymap for `lsp-mode'.")

(define-minor-mode lsp-mode "Mode for LSP interaction."
  :keymap lsp-mode-map
  :lighter
  (" LSP["
   (lsp--buffer-workspaces
    (:eval (mapconcat #'lsp--workspace-print lsp--buffer-workspaces "]["))
    (:propertize "Disconnected" face warning))
   "]")
  :group 'lsp-mode
  (when (and lsp-mode (not lsp--buffer-workspaces))
    ;; fire up `lsp' when someone calls `lsp-mode' instead of `lsp'
    (lsp)))

(defvar lsp-mode-menu
  (easy-menu-create-menu
   nil
   `(["Go to definition" lsp-find-definition
      :active (lsp-feature? "textDocument/definition")]
     ["Find references" lsp-find-references
      :active (lsp-feature? "textDocument/references")]
     ["Find implementations" lsp-find-implementation
      :active (lsp-feature? "textDocument/implementation")]
     ["Find declarations" lsp-find-declaration
      :active (lsp-feature? "textDocument/declaration")]
     ["Go to type declaration" lsp-find-type-definition
      :active (lsp-feature? "textDocument/typeDefinition")]
     "--"
     ["Describe" lsp-describe-thing-at-point]
     ["Code action" lsp-execute-code-action]
     ["Format" lsp-format-buffer]
     ["Highlight references" lsp-document-highlight]
     ["Type Hierarchy" lsp-java-type-hierarchy
      :visible (lsp-can-execute-command? "java.navigate.resolveTypeHierarchy")]
     ["Type Hierarchy" lsp-treemacs-type-hierarchy
      :visible (and (not (lsp-can-execute-command? "java.navigate.resolveTypeHierarchy"))
                    (functionp 'lsp-treemacs-type-hierarchy)
                    (lsp-feature? "textDocument/typeHierarchy"))]
     ["Call Hierarchy" lsp-treemacs-call-hierarchy
      :visible (and (functionp 'lsp-treemacs-call-hierarchy)
                    (lsp-feature? "textDocument/callHierarchy"))]
     ["Rename" lsp-rename
      :active (lsp-feature? "textDocument/rename")]
     "--"
     ("Session"
      ["View logs" lsp-workspace-show-log]
      ["Describe" lsp-describe-session]
      ["Shutdown" lsp-shutdown-workspace]
      ["Restart" lsp-restart-workspace])
     ("Workspace Folders"
      ["Add" lsp-workspace-folders-add]
      ["Remove" lsp-workspace-folders-remove]
      ["Open" lsp-workspace-folders-open])
     ("Toggle features"
      ["Lenses" lsp-lens-mode]
      ["Headerline breadcrumb" lsp-headerline-breadcrumb-mode]
      ["Modeline code actions" lsp-modeline-code-actions-mode]
      ["Modeline diagnostics" lsp-modeline-diagnostics-mode])
     "---"
     ("Debug"
      :active (bound-and-true-p dap-ui-mode)
      :filter ,(lambda (_)
                 (and (boundp 'dap-ui-menu-items)
                      (nthcdr 3 dap-ui-menu-items))))))
  "Menu for lsp-mode.")

(defalias 'make-lsp-client 'make-lsp--client)

(cl-defstruct lsp--registered-capability
  (id "")
  (method " ")
  (options nil))

;; A ‘lsp--workspace’ object represents exactly one language server process.
(cl-defstruct lsp--workspace
  ;; the `ewoc' object for displaying I/O to and from the server
  (ewoc nil)

  ;; ‘server-capabilities’ is a hash table of the language server capabilities.
  ;; It is the hash table representation of a LSP ServerCapabilities structure;
  ;; cf. https://microsoft.github.io/language-server-protocol/specification#initialize.
  (server-capabilities nil)

  ;; ‘registered-server-capabilities’ is a list of hash tables that represent
  ;; dynamically-registered Registration objects.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#client_registerCapability.
  (registered-server-capabilities nil)

  ;; ‘root’ is a directory name or a directory file name for the workspace
  ;; root.  ‘lsp-mode’ passes this directory to the ‘initialize’ method of the
  ;; language server; see
  ;; https://microsoft.github.io/language-server-protocol/specification#initialize.
  (root nil)

  ;; ‘client’ is the ‘lsp--client’ object associated with this workspace.
  (client nil)

  ;; ‘host-root’ contains the host root info as derived from `file-remote-p'. It
  ;; used to derive the file path in `lsp--uri-to-path' when using tramp
  ;; connection.
  (host-root nil)

  ;; ‘proc’ is a process object; it may represent a regular process, a pipe, or
  ;; a network connection.  ‘lsp-mode’ communicates with ‘proc’ using the
  ;; language server protocol.  ‘proc’ corresponds to the COMMUNICATION-PROCESS
  ;; element of the return value of the client’s ‘get-root’ field, which see.
  (proc nil)

  ;; ‘proc’ is a process object; it must represent a regular process, not a
  ;; pipe or network process.  It represents the actual server process that
  ;; corresponds to this workspace.  ‘cmd-proc’ corresponds to the
  ;; COMMAND-PROCESS element of the return value of the client’s ‘get-root’
  ;; field, which see.
  (cmd-proc nil)

  ;; ‘buffers’ is a list of buffers associated with this workspace.
  (buffers nil)

  ;; if semantic tokens is enabled, `semantic-tokens-faces' contains
  ;; one face (or nil) for each token type supported by the language server.
  (semantic-tokens-faces nil)

  ;; If semantic highlighting is enabled, `semantic-tokens-modifier-faces'
  ;; contains one face (or nil) for each modifier type supported by the language
  ;; server
  (semantic-tokens-modifier-faces nil)

  ;; Extra client capabilities provided by third-party packages using
  ;; `lsp-register-client-capabilities'. It's value is an alist of (PACKAGE-NAME
  ;; . CAPS), where PACKAGE-NAME is a symbol of the third-party package name,
  ;; and CAPS is either a plist of the client capabilities, or a function that
  ;; takes no argument and returns a plist of the client capabilities or nil.
  (extra-client-capabilities nil)

  ;; Workspace status
  (status nil)

  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal))

  ;; contains all the file notification watches that have been created for the
  ;; current workspace in format filePath->file notification handle.
  (watches (make-hash-table :test 'equal))

  ;; list of workspace folders
  (workspace-folders nil)

  ;; ‘last-id’ the last request id for the current workspace.
  (last-id 0)

  ;; ‘status-string’ allows extensions to specify custom status string based on
  ;; the Language Server specific messages.
  (status-string nil)

  ;; ‘shutdown-action’ flag used to mark that workspace should not be restarted (e.g. it
  ;; was stopped).
  shutdown-action

  ;; ‘diagnostics’ a hashmap with workspace diagnostics.
  (diagnostics (make-hash-table :test 'equal))

  ;; contains all the workDone progress tokens that have been created
  ;; for the current workspace.
  (work-done-tokens (make-hash-table :test 'equal)))


(cl-defstruct lsp-session
  ;; contains the folders that are part of the current session
  folders
  ;; contains the folders that must not be imported in the current workspace.
  folders-blocklist
  ;; contains the list of folders that must be imported in a project in case of
  ;; multi root LSP server.
  (server-id->folders (make-hash-table :test 'equal))
  ;; folder to list of the servers that are associated with the folder.
  (folder->servers (make-hash-table :test 'equal))
  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal)))

(defun lsp-workspace-status (status-string &optional workspace)
  "Set current workspace status to STATUS-STRING.
If WORKSPACE is not specified defaults to lsp--cur-workspace."
  (let ((status-string (when status-string (replace-regexp-in-string "%" "%%" status-string))))
    (setf (lsp--workspace-status-string (or workspace lsp--cur-workspace)) status-string)))

(defun lsp-session-set-metadata (key value &optional _workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-set-metadata 'lsp-session-set-metadata)

(defun lsp-session-get-metadata (key &optional _workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-get-metadata 'lsp-session-get-metadata)

(defun lsp-workspace-set-work-done-token (token value workspace)
  "Associate TOKEN with VALUE in the WORKSPACE work-done-tokens."
  (puthash token value (lsp--workspace-work-done-tokens workspace)))

(defun lsp-workspace-get-work-done-token (token workspace)
  "Lookup TOKEN in the WORKSPACE work-done-tokens."
  (gethash token (lsp--workspace-work-done-tokens workspace)))

(defun lsp-workspace-rem-work-done-token (token workspace)
  "Remove TOKEN from the WORKSPACE work-done-tokens."
  (remhash token (lsp--workspace-work-done-tokens workspace)))


(defun lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (list :jsonrpc "2.0" :method method :params params))

(defalias 'lsp--make-request 'lsp--make-notification)
(defalias 'lsp-make-request 'lsp--make-notification)

(defun lsp--make-response (id result)
  "Create response for REQUEST with RESULT."
  `(:jsonrpc "2.0" :id ,id :result ,result))

(defun lsp-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defmacro lsp--json-serialize (params)
  (if (progn
        (require 'json)
        (fboundp 'json-serialize))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let ((body (lsp--json-serialize params)))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(cl-defstruct lsp--log-entry timestamp process-time type method id body)

(defun lsp--make-log-entry (method id body type &optional process-time)
  "Create an outgoing log object from BODY with method METHOD and id ID.
If ID is non-nil, then the body is assumed to be a notification.
TYPE can either be `incoming' or `outgoing'"
  (cl-assert (memq type '(incoming-req outgoing-req incoming-notif
                                       outgoing-notif incoming-resp
                                       outgoing-resp)))
  (make-lsp--log-entry
   :timestamp (format-time-string "%I:%M:%S %p")
   :process-time process-time
   :method method
   :id id
   :type type
   :body body))

(defun lsp--log-font-lock-json (body)
  "Font lock JSON BODY."
  (with-temp-buffer
    (insert body)
    ;; We set the temp buffer file-name extension to .json and call `set-auto-mode'
    ;; so the users configured json mode is used which could be
    ;; `json-mode', `json-ts-mode', `jsonian-mode', etc.
    (let ((buffer-file-name "lsp-log.json"))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))

(defun lsp--log-entry-pp (entry)
  (cl-assert (lsp--log-entry-p entry))
  (pcase-let (((cl-struct lsp--log-entry timestamp method id type process-time
                          body)
               entry)
              (json-false :json-false)
              (json-encoding-pretty-print t)
              (str nil))
    (setq str
          (concat (format "[Trace - %s] " timestamp)
                  (pcase type
                    ('incoming-req (format "Received request '%s - (%s)." method id))
                    ('outgoing-req (format "Sending request '%s - (%s)'." method id))

                    ('incoming-notif (format "Received notification '%s'." method))
                    ('outgoing-notif (format "Sending notification '%s'." method))

                    ('incoming-resp (format "Received response '%s - (%s)' in %dms."
                                            method id process-time))
                    ('outgoing-resp
                     (format
                      "Sending response '%s - (%s)'. Processing request took %dms"
                      method id process-time)))
                  "\n"
                  (if (memq type '(incoming-resp ougoing-resp))
                      "Result: "
                    "Params: ")
                  (lsp--log-font-lock-json (json-encode body))
                  "\n\n\n"))
    (setq str (propertize str 'mouse-face 'highlight 'read-only t))
    (insert str)))

(defvar-local lsp--log-io-ewoc nil)

(defun lsp--get-create-io-ewoc (workspace)
  (if (and (lsp--workspace-ewoc workspace)
           (buffer-live-p (ewoc-buffer (lsp--workspace-ewoc workspace))))
      (lsp--workspace-ewoc workspace)
    (with-current-buffer (lsp--get-log-buffer-create workspace)
      (unless (eq 'lsp-log-io-mode major-mode) (lsp-log-io-mode))
      (setq-local window-point-insertion-type t)
      (setq lsp--log-io-ewoc (ewoc-create #'lsp--log-entry-pp nil nil t))
      (setf (lsp--workspace-ewoc workspace) lsp--log-io-ewoc))
    (lsp--workspace-ewoc workspace)))

(defun lsp--ewoc-count (ewoc)
  (let* ((count 0)
         (count-fn (lambda (_) (setq count (1+ count)))))
    (ewoc-map count-fn ewoc)
    count))

(defun lsp--log-entry-new (entry workspace)
  (let* ((ewoc (lsp--get-create-io-ewoc workspace))
         (count (and (not (eq lsp-io-messages-max t)) (lsp--ewoc-count ewoc)))
         (node (if (or (eq lsp-io-messages-max t)
                       (>= lsp-io-messages-max count))
                   nil
                 (ewoc-nth ewoc (1- lsp-io-messages-max))))
         (prev nil)
         (inhibit-read-only t))
    (while node
      (setq prev (ewoc-prev ewoc node))
      (ewoc-delete ewoc node)
      (setq node prev))
    (ewoc-enter-last ewoc entry)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (lsp-foreach-workspace
   (when (lsp--log-io-p (plist-get body :method))
     (lsp--log-entry-new (lsp--make-log-entry
                          (plist-get body :method)
                          nil (plist-get body :params) 'outgoing-notif)
                         lsp--cur-workspace))
   (lsp--send-no-wait body
                      (lsp--workspace-proc lsp--cur-workspace))))

(defalias 'lsp-send-notification 'lsp--send-notification)

(defun lsp-notify (method params)
  "Send notification METHOD with PARAMS."
  (lsp--send-notification (lsp--make-notification method params)))

(defun lsp--cur-workspace-check ()
  "Check whether buffer lsp workspace(s) are set."
  (cl-assert (lsp-workspaces) nil
             "No language server(s) is associated with this buffer."))

(defun lsp--send-request (body &optional no-wait no-merge)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response.
If NO-MERGE is non-nil, don't merge the results but return an
alist mapping workspace->result."
  (lsp-request (plist-get body :method)
               (plist-get body :params)
               :no-wait no-wait
               :no-merge no-merge))

(defalias 'lsp-send-request 'lsp--send-request
  "Send BODY as a request to the language server and return the response
synchronously.
\n(fn BODY)")

(cl-defun lsp-request (method params &key no-wait no-merge)
  "Send request METHOD with PARAMS.
If NO-MERGE is non-nil, don't merge the results but return alist
workspace->result.
If NO-WAIT is non-nil send the request as notification."
  (if no-wait
      (lsp-notify method params)
    (let* ((send-time (float-time))
           ;; max time by which we must get a response
           (expected-time
            (and
             lsp-response-timeout
             (+ send-time lsp-response-timeout)))
           resp-result resp-error done?)
      (unwind-protect
          (progn
            (lsp-request-async method params
                               (lambda (res) (setf resp-result (or res :finished)) (throw 'lsp-done '_))
                               :error-handler (lambda (err) (setf resp-error err) (throw 'lsp-done '_))
                               :no-merge no-merge
                               :mode 'detached
                               :cancel-token :sync-request)
            (while (not (or resp-error resp-result))
              (if (functionp 'json-rpc-connection)
                  (catch 'lsp-done (sit-for 0.01))
                (catch 'lsp-done
                  (accept-process-output
                   nil
                   (if expected-time (- expected-time send-time) 1))))
              (setq send-time (float-time))
              (when (and expected-time (< expected-time send-time))
                (error "Timeout while waiting for response.  Method: %s" method)))
            (setq done? t)
            (cond
             ((eq resp-result :finished) nil)
             (resp-result resp-result)
             ((lsp-json-error? resp-error) (error (lsp:json-error-message resp-error)))
             ((lsp-json-error? (cl-first resp-error))
              (error (lsp:json-error-message (cl-first resp-error))))))
        (unless done?
          (lsp-cancel-request-by-token :sync-request))))))

(cl-defun lsp-request-while-no-input (method params)
  "Send request METHOD with PARAMS and waits until there is no input.
Return same value as `lsp--while-no-input' and respecting `non-essential'."
  (if (or non-essential (not lsp-request-while-no-input-may-block))
      (let* ((send-time (float-time))
             ;; max time by which we must get a response
             (expected-time
              (and
               lsp-response-timeout
               (+ send-time lsp-response-timeout)))
             resp-result resp-error done?)
        (unwind-protect
            (progn
              (lsp-request-async method params
                                 (lambda (res) (setf resp-result (or res :finished)) (throw 'lsp-done '_))
                                 :error-handler (lambda (err) (setf resp-error err) (throw 'lsp-done '_))
                                 :mode 'detached
                                 :cancel-token :sync-request)
              (while (not (or resp-error resp-result (input-pending-p)))
                (catch 'lsp-done
                  (sit-for
                   (if expected-time (- expected-time send-time) 1)))
                (setq send-time (float-time))
                (when (and expected-time (< expected-time send-time))
                  (error "Timeout while waiting for response.  Method: %s" method)))
              (setq done? (or resp-error resp-result))
              (cond
               ((eq resp-result :finished) nil)
               (resp-result resp-result)
               ((lsp-json-error? resp-error) (error (lsp:json-error-message resp-error)))
               ((lsp-json-error? (cl-first resp-error))
                (error (lsp:json-error-message (cl-first resp-error))))))
          (unless done?
            (lsp-cancel-request-by-token :sync-request))
          (when (and (input-pending-p) lsp--throw-on-input)
            (throw 'input :interrupted))))
    (lsp-request method params)))

(defvar lsp--cancelable-requests (ht))

(cl-defun lsp-request-async (method params callback
                                    &key mode error-handler cancel-handler no-merge cancel-token)
  "Send METHOD with PARAMS as a request to the language server.
Call CALLBACK with the response received from the server
asynchronously.
MODE determines when the callback will be called depending on the
condition of the original buffer.  It could be:
- `detached' which means that the callback will be executed no
matter what has happened to the buffer.
- `alive' - the callback will be executed only if the buffer from
which the call was executed is still alive.
- `current' the callback will be executed only if the original buffer
is still selected.
- `tick' - the callback will be executed only if the buffer was not modified.
- `unchanged' - the callback will be executed only if the buffer hasn't
changed and if the buffer is not modified.

ERROR-HANDLER will be called in case the request has failed.
CANCEL-HANDLER will be called in case the request is being canceled.
If NO-MERGE is non-nil, don't merge the results but return alist
workspace->result.
CANCEL-TOKEN is the token that can be used to cancel request."
  (lsp--send-request-async `(:jsonrpc "2.0" :method ,method :params ,params)
                           callback mode error-handler cancel-handler no-merge cancel-token))

(defun lsp--create-request-cancel (id workspaces hook buf method cancel-callback)
  (lambda (&rest _)
    (unless (and (equal 'post-command-hook hook)
                 (equal (current-buffer) buf))
      (lsp--request-cleanup-hooks id)
      (with-lsp-workspaces workspaces
        (lsp--cancel-request id)
        (when cancel-callback (funcall cancel-callback)))
      (lsp-log "Cancelling %s(%s) in hook %s" method id hook))))

(defun lsp--create-async-callback
    (callback method no-merge workspaces)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method.
If NO-MERGE is non-nil, don't merge the results but return alist
workspace->result. ID is the request id."
  (let (results errors)
    (lambda (result)
      (push (cons lsp--cur-workspace result)
            (if (eq result :error) errors results))
      (when (and (not (eq (length errors) (length workspaces)))
                 (eq (+ (length errors) (length results)) (length workspaces)))
        (funcall callback
                 (if no-merge
                     results
                   (lsp--merge-results (-map #'cl-rest results) method)))))))

(defcustom lsp-default-create-error-handler-fn nil
  "Default error handler customization.
Handler should give METHOD as argument and return function of one argument
ERROR."
  :type 'function
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp--create-default-error-handler (method)
  "Default error handler.
METHOD is the executed method."
  (if lsp-default-create-error-handler-fn
      (funcall lsp-default-create-error-handler-fn method)
    (lambda (error)
      (lsp--warn "%s" (or (lsp--error-string error)
                          (format "%s Request has failed" method))))))

(defvar lsp--request-cleanup-hooks (ht))

(defun lsp--request-cleanup-hooks (request-id)
  (when-let ((cleanup-function (gethash request-id lsp--request-cleanup-hooks)))
    (funcall cleanup-function)
    (remhash request-id lsp--request-cleanup-hooks)))

(defun lsp-cancel-request-by-token (cancel-token)
  "Cancel request using CANCEL-TOKEN."
  (-when-let ((request-id . workspaces) (gethash cancel-token lsp--cancelable-requests))
    (with-lsp-workspaces workspaces
      (lsp--cancel-request request-id))
    (remhash cancel-token lsp--cancelable-requests)
    (lsp--request-cleanup-hooks request-id)))

(defun lsp--send-request-async (body callback
                                     &optional mode error-callback cancel-callback
                                     no-merge cancel-token)
  "Send BODY as a request to the language server.
Call CALLBACK with the response received from the server
asynchronously.
MODE determines when the callback will be called depending on the
condition of the original buffer.  It could be:
- `detached' which means that the callback will be executed no
matter what has happened to the buffer.
- `alive' - the callback will be executed only if the buffer from
which the call was executed is still alive.
- `current' the callback will be executed only if the original buffer
is still selected.
- `tick' - the callback will be executed only if the buffer was not modified.
- `unchanged' - the callback will be executed only if the buffer hasn't
changed and if the buffer is not modified.

ERROR-CALLBACK will be called in case the request has failed.
CANCEL-CALLBACK will be called in case the request is being canceled.
If NO-MERGE is non-nil, don't merge the results but return alist
workspace->result.
CANCEL-TOKEN is the token that can be used to cancel request."
  (when cancel-token
    (lsp-cancel-request-by-token cancel-token))

  (if-let ((target-workspaces (lsp--find-workspaces-for body)))
      (let* ((start-time (current-time))
             (method (plist-get body :method))
             (id (cl-incf lsp-last-id))
             (buf (current-buffer))
             (cancel-callback (when cancel-callback
                                (pcase mode
                                  ((or 'alive 'tick 'unchanged)
                                   (lambda ()
                                     (with-current-buffer buf
                                       (funcall cancel-callback))))
                                  (_ cancel-callback))))
             ;; calculate what are the (hook . local) pairs which will cancel
             ;; the request
             (hooks (pcase mode
                      ('alive     '((kill-buffer-hook . t)))
                      ('tick      '((kill-buffer-hook . t) (after-change-functions . t)))
                      ('unchanged '((after-change-functions . t) (post-command-hook . nil)))
                      ('current   '((post-command-hook . nil)))))
             ;; note: lambdas in emacs can be compared but we should make sure
             ;; that all of the captured arguments are the same - in our case
             ;; `lsp--create-request-cancel' will return the same lambda when
             ;; called with the same params.
             (cleanup-hooks
              (lambda () (mapc
                          (-lambda ((hook . local))
                            (if local
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (remove-hook hook
                                                 (lsp--create-request-cancel
                                                  id target-workspaces hook buf method cancel-callback)
                                                 t)))
                              (remove-hook hook (lsp--create-request-cancel
                                                 id target-workspaces hook buf method cancel-callback))))
                          hooks)
                (remhash cancel-token lsp--cancelable-requests)))
             (callback (pcase mode
                         ((or 'alive 'tick 'unchanged) (lambda (&rest args)
                                                         (with-current-buffer buf
                                                           (apply callback args))))
                         (_ callback)))
             (callback (lsp--create-async-callback callback
                                                   method
                                                   no-merge
                                                   target-workspaces))
             (callback (lambda (result)
                         (lsp--request-cleanup-hooks id)
                         (funcall callback result)))
             (error-callback (lsp--create-async-callback
                              (or error-callback
                                  (lsp--create-default-error-handler method))
                              method
                              nil
                              target-workspaces))
             (error-callback (lambda (error)
                               (funcall callback :error)
                               (lsp--request-cleanup-hooks id)
                               (funcall error-callback error)))
             (body (plist-put body :id id)))

        ;; cancel request in any of the hooks
        (mapc (-lambda ((hook . local))
                (add-hook hook
                          (lsp--create-request-cancel
                           id target-workspaces hook buf method cancel-callback)
                          nil local))
              hooks)
        (puthash id cleanup-hooks lsp--request-cleanup-hooks)

        (setq lsp--last-active-workspaces target-workspaces)

        (when cancel-token
          (puthash cancel-token (cons id target-workspaces) lsp--cancelable-requests))

        (seq-doseq (workspace target-workspaces)
          (when (lsp--log-io-p method)
            (lsp--log-entry-new (lsp--make-log-entry method id
                                                     (plist-get body :params)
                                                     'outgoing-req)
                                workspace))
          (puthash id
                   (list callback error-callback method start-time (current-time))
                   (-> workspace
                       (lsp--workspace-client)
                       (lsp--client-response-handlers)))
          (lsp--send-no-wait body (lsp--workspace-proc workspace)))
        body)
    (error "The connected server(s) does not support method %s.
To find out what capabilities support your server use `M-x lsp-describe-session'
and expand the capabilities section"
           (plist-get body :method))))

;; deprecated, use lsp-request-async.
(defalias 'lsp-send-request-async 'lsp--send-request-async)
(make-obsolete 'lsp-send-request-async 'lsp-request-async "lsp-mode 7.0.1")

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  "Unload working workspaces."
  (lsp-foreach-workspace (lsp--shutdown-workspace)))

(defun lsp--shutdown-workspace (&optional restart)
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (let ((lsp-response-timeout 0.5))
      (condition-case err
          (lsp-request "shutdown" nil)
        (error (lsp--error "%s" err))))
    (lsp-notify "exit" nil))
  (setf (lsp--workspace-shutdown-action lsp--cur-workspace) (or (and restart 'restart) 'shutdown))
  (lsp--uninitialize-workspace))

(defcustom lsp-inlay-hint-enable nil
  "If non-nil it will enable inlay hints."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp--uninitialize-workspace ()
  "Cleanup buffer state.
When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (-let [(&lsp-wks 'cmd-proc 'buffers) lsp--cur-workspace]
    (lsp-process-kill cmd-proc)
    (mapc (lambda (buf)
            (when (lsp-buffer-live-p buf)
              (lsp-with-current-buffer buf
                                       (lsp-managed-mode -1))))
          buffers)
    (lsp-diagnostics--workspace-cleanup lsp--cur-workspace)))

(defun lsp--client-capabilities (&optional custom-capabilities)
  "Return the client capabilities appending CUSTOM-CAPABILITIES."
  (append
   `((general . ((positionEncodings . ["utf-32", "utf-16"])))
     (workspace . ((workspaceEdit . ((documentChanges . t)
                                     (resourceOperations . ["create" "rename" "delete"])))
                   (applyEdit . t)
                   (symbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))))
                   (executeCommand . ((dynamicRegistration . :json-false)))
                   ,@(when lsp-enable-file-watchers '((didChangeWatchedFiles . ((dynamicRegistration . t)))))
                   (workspaceFolders . t)
                   (configuration . t)
                   ,@(when lsp-semantic-tokens-enable
                       `((semanticTokens . ((refreshSupport . ,(or (and (boundp 'lsp-semantic-tokens-honor-refresh-requests)
                                                                        lsp-semantic-tokens-honor-refresh-requests)
                                                                   :json-false))))))
                   ,@(when lsp-lens-enable '((codeLens . ((refreshSupport . t)))))
                   ,@(when lsp-inlay-hint-enable '((inlayHint . ((refreshSupport . :json-false)))))
                   (fileOperations . ((didCreate . :json-false)
                                      (willCreate . :json-false)
                                      (didRename . t)
                                      (willRename . t)
                                      (didDelete . :json-false)
                                      (willDelete . :json-false)))))
     (textDocument . ((declaration . ((dynamicRegistration . t)
                                      (linkSupport . t)))
                      (definition . ((dynamicRegistration . t)
                                     (linkSupport . t)))
                      (references . ((dynamicRegistration . t)))
                      (implementation . ((dynamicRegistration . t)
                                         (linkSupport . t)))
                      (typeDefinition . ((dynamicRegistration . t)
                                         (linkSupport . t)))
                      (synchronization . ((willSave . t) (didSave . t) (willSaveWaitUntil . t)))
                      (documentSymbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))
                                         (hierarchicalDocumentSymbolSupport . t)))
                      (formatting . ((dynamicRegistration . t)))
                      (rangeFormatting . ((dynamicRegistration . t)))
                      (onTypeFormatting . ((dynamicRegistration . t)))
                      ,@(when (and lsp-semantic-tokens-enable
                                   (functionp 'lsp--semantic-tokens-capabilities))
                          (lsp--semantic-tokens-capabilities))
                      (rename . ((dynamicRegistration . t) (prepareSupport . t)))
                      (codeAction . ((dynamicRegistration . t)
                                     (isPreferredSupport . t)
                                     (codeActionLiteralSupport . ((codeActionKind . ((valueSet . [""
                                                                                                  "quickfix"
                                                                                                  "refactor"
                                                                                                  "refactor.extract"
                                                                                                  "refactor.inline"
                                                                                                  "refactor.rewrite"
                                                                                                  "source"
                                                                                                  "source.organizeImports"])))))
                                     (resolveSupport . ((properties . ["edit" "command"])))
                                     (dataSupport . t)))
                      (completion . ((completionItem . ((snippetSupport . ,(cond
                                                                            ((and lsp-enable-snippet (not (fboundp 'yas-minor-mode)))
                                                                             (lsp--warn (concat
                                                                                         "Yasnippet is not installed, but `lsp-enable-snippet' is set to `t'. "
                                                                                         "You must either install yasnippet, or disable snippet support."))
                                                                             :json-false)
                                                                            (lsp-enable-snippet t)
                                                                            (t :json-false)))
                                                        (documentationFormat . ["markdown" "plaintext"])
                                                        ;; Remove this after jdtls support resolveSupport
                                                        (resolveAdditionalTextEditsSupport . t)
                                                        (insertReplaceSupport . t)
                                                        (deprecatedSupport . t)
                                                        (resolveSupport
                                                         . ((properties . ["documentation"
                                                                           "detail"
                                                                           "additionalTextEdits"
                                                                           "command"])))
                                                        (insertTextModeSupport . ((valueSet . [1 2])))))
                                     (contextSupport . t)
                                     (dynamicRegistration . t)))
                      (signatureHelp . ((signatureInformation . ((parameterInformation . ((labelOffsetSupport . t)))))
                                        (dynamicRegistration . t)))
                      (documentLink . ((dynamicRegistration . t)
                                       (tooltipSupport . t)))
                      (hover . ((contentFormat . ["markdown" "plaintext"])
                                (dynamicRegistration . t)))
                      ,@(when lsp-enable-folding
                          `((foldingRange . ((dynamicRegistration . t)
                                             ,@(when lsp-folding-range-limit
                                                 `((rangeLimit . ,lsp-folding-range-limit)))
                                             ,@(when lsp-folding-line-folding-only
                                                 `((lineFoldingOnly . t)))))))
                      (selectionRange . ((dynamicRegistration . t)))
                      (callHierarchy . ((dynamicRegistration . :json-false)))
                      (typeHierarchy . ((dynamicRegistration . t)))
                      (publishDiagnostics . ((relatedInformation . t)
                                             (tagSupport . ((valueSet . [1 2])))
                                             (versionSupport . t)))
                      (linkedEditingRange . ((dynamicRegistration . t)))))
     (window . ((workDoneProgress . t)
                (showDocument . ((support . t))))))
   custom-capabilities))

(defun lsp-find-roots-for-workspace (workspace session)
  "Get all roots for the WORKSPACE."
  (-filter #'identity (ht-map (lambda (folder workspaces)
                                (when (-contains? workspaces workspace)
                                  folder))
                              (lsp-session-folder->servers session))))

(defun lsp-session-watches (&optional session)
  "Get watches created for SESSION."
  (or (gethash "__watches" (lsp-session-metadata (or session (lsp-session))))
      (-let [res (make-hash-table :test 'equal)]
        (puthash "__watches" res (lsp-session-metadata (or session (lsp-session))))
        res)))

(defun lsp--file-process-event (session root-folder event)
  "Process file event."
  (let* ((changed-file (cl-third event))
         (rel-changed-file (f-relative changed-file root-folder))
         (event-numeric-kind (alist-get (cl-second event) lsp--file-change-type))
         (bit-position (1- event-numeric-kind))
         (watch-bit (ash 1 bit-position)))
    (->>
     session
     lsp-session-folder->servers
     (gethash root-folder)
     (seq-do (lambda (workspace)
               (when (->>
                      workspace
                      lsp--workspace-registered-server-capabilities
                      (-any?
                       (lambda (capability)
                         (and
                          (equal (lsp--registered-capability-method capability)
                                 "workspace/didChangeWatchedFiles")
                          (->>
                           capability
                           lsp--registered-capability-options
                           (lsp:did-change-watched-files-registration-options-watchers)
                           (seq-find
                            (-lambda ((fs-watcher &as &FileSystemWatcher :glob-pattern :kind? :_cachedRegexp cached-regexp))
                              (when (or (null kind?)
                                        (> (logand kind? watch-bit) 0))
                                (-let [regexes (or cached-regexp
                                                   (let ((regexp (lsp-glob-to-regexps glob-pattern)))
                                                     (lsp-put fs-watcher :_cachedRegexp regexp)
                                                     regexp))]
                                  (-any? (lambda (re)
                                           (or (string-match re changed-file)
                                               (string-match re rel-changed-file)))
                                         regexes))))))))))
                 (with-lsp-workspace workspace
                   (lsp-notify
                    "workspace/didChangeWatchedFiles"
                    `((changes . [((type . ,event-numeric-kind)
                                   (uri . ,(lsp--path-to-uri changed-file)))]))))))))))

(lsp-defun lsp--server-register-capability ((&Registration :method :id :register-options?))
  "Register capability REG."
  (when (and lsp-enable-file-watchers
             (equal method "workspace/didChangeWatchedFiles"))
    (-let* ((created-watches (lsp-session-watches (lsp-session)))
            (root-folders (cl-set-difference
                           (lsp-find-roots-for-workspace lsp--cur-workspace (lsp-session))
                           (ht-keys created-watches))))
      ;; create watch for each root folder without such
      (dolist (folder root-folders)
        (let* ((watch (make-lsp-watch :root-directory folder))
               (ignored-things (lsp--get-ignored-regexes-for-workspace-root folder))
               (ignored-files-regex-list (car ignored-things))
               (ignored-directories-regex-list (cadr ignored-things)))
          (puthash folder watch created-watches)
          (lsp-watch-root-folder (file-truename folder)
                                 (-partial #'lsp--file-process-event (lsp-session) folder)
                                 ignored-files-regex-list
                                 ignored-directories-regex-list
                                 watch
                                 t)))))

  (push
   (make-lsp--registered-capability :id id :method method :options register-options?)
   (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))

(defmacro lsp--with-workspace-temp-buffer (workspace-root &rest body)
  "With a temp-buffer under `WORKSPACE-ROOT' and evaluate `BODY', useful to
access dir-local variables."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; Set the buffer's name to something under the root so that we can hack the local variables
     ;; This file doesn't need to exist and will not be created due to this.
     (setq-local buffer-file-name (expand-file-name "lsp-mode-temp" (expand-file-name ,workspace-root)))
     (hack-local-variables)
     (prog1 ,@body
       (setq-local buffer-file-name nil))))

(defun lsp--get-ignored-regexes-for-workspace-root (workspace-root)
  "Return a list of the form
(lsp-file-watch-ignored-files lsp-file-watch-ignored-directories) for the given
WORKSPACE-ROOT."
  ;; The intent of this function is to provide per-root workspace-level customization of the
  ;; lsp-file-watch-ignored-directories and lsp-file-watch-ignored-files variables.
  (lsp--with-workspace-temp-buffer workspace-root
    (list lsp-file-watch-ignored-files (lsp-file-watch-ignored-directories))))


(defun lsp--cleanup-hanging-watches ()
  "Cleanup watches in case there are no more workspaces that are interested
in that particular folder."
  (let* ((session (lsp-session))
         (watches (lsp-session-watches session)))
    (dolist (watched-folder (ht-keys watches))
      (when (-none? (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--registered-capability "workspace/didChangeWatchedFiles")))
                    (gethash watched-folder (lsp-session-folder->servers (lsp-session))))
        (lsp-log "Cleaning up watches for folder %s. There is no workspace watching this folder..." watched-folder)
        (lsp-kill-watch (gethash watched-folder watches))
        (remhash watched-folder watches)))))

(lsp-defun lsp--server-unregister-capability ((&Unregistration :id :method))
  "Unregister capability UNREG."
  (setf (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
        (seq-remove (lambda (e) (equal (lsp--registered-capability-id e) id))
                    (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))
  (when (equal method "workspace/didChangeWatchedFiles")
    (lsp--cleanup-hanging-watches)))

(defun lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (->> (lsp-workspaces)
       (-keep #'lsp--workspace-server-capabilities)
       (apply #'lsp-merge)))

(defun lsp--send-open-close-p ()
  "Return whether open and close notifications should be sent to the server."
  (let ((sync (lsp:server-capabilities-text-document-sync? (lsp--server-capabilities))))
    (or (memq sync '(1 2))
        (lsp:text-document-sync-options-open-close? sync))))

(defun lsp--send-will-save-p ()
  "Return whether willSave notifications should be sent to the server."
  (-> (lsp--server-capabilities)
      (lsp:server-capabilities-text-document-sync?)
      (lsp:text-document-sync-options-will-save?)))

(defun lsp--send-will-save-wait-until-p ()
  "Return whether willSaveWaitUntil notifications should be sent to the server."
  (-> (lsp--server-capabilities)
      (lsp:server-capabilities-text-document-sync?)
      (lsp:text-document-sync-options-will-save-wait-until?)))

(defun lsp--send-did-save-p ()
  "Return whether didSave notifications should be sent to the server."
  (let ((sync (lsp:server-capabilities-text-document-sync? (lsp--server-capabilities))))
    (or (memq sync '(1 2))
        (lsp:text-document-sync-options-save? sync))))

(defun lsp--save-include-text-p ()
  "Return whether save notifications should include the text document's contents."
  (->> (lsp--server-capabilities)
       (lsp:server-capabilities-text-document-sync?)
       (lsp:text-document-sync-options-save?)
       (lsp:text-document-save-registration-options-include-text?)))

(defun lsp--send-will-rename-files-p (path)
  "Return whether willRenameFiles request should be sent to the server.
If any filters, checks if it applies for PATH."
  (let* ((will-rename (-> (lsp--server-capabilities)
                          (lsp:server-capabilities-workspace?)
                          (lsp:workspace-server-capabilities-file-operations?)
                          (lsp:workspace-file-operations-will-rename?)))
         (filters (seq-into (lsp:file-operation-registration-options-filters will-rename) 'list)))
    (and will-rename
         (or (seq-empty-p filters)
             (-any? (-lambda ((&FileOperationFilter :scheme? :pattern (&FileOperationPattern :glob)))
                      (-let [regexes (lsp-glob-to-regexps glob)]
                        (and (or (not scheme?)
                                 (string-prefix-p scheme? (lsp--path-to-uri path)))
                             (-any? (lambda (re)
                                      (string-match re path))
                                    regexes))))
                    filters)))))

(defun lsp--send-did-rename-files-p ()
  "Return whether didRenameFiles notification should be sent to the server."
  (-> (lsp--server-capabilities)
      (lsp:server-capabilities-workspace?)
      (lsp:workspace-server-capabilities-file-operations?)
      (lsp:workspace-file-operations-did-rename?)))

(declare-function project-roots "ext:project" (project) t)
(declare-function project-root "ext:project" (project) t)

(defun lsp--suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (if (fboundp 'project-root)
           (project-root project)
         (car (with-no-warnings
                (project-roots project))))))
   default-directory))

(defun lsp--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun lsp--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME.

This function creates the parent directories if they don't exist
yet."
  (let ((print-length nil)
        (print-level nil))
    ;; Create all parent directories:
    (make-directory (f-parent file-name) t)
    (f-write-text (prin1-to-string to-persist) 'utf-8 file-name)))

(defun lsp-workspace-folders-add (project-root)
  "Add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
                              (or (lsp--suggest-project-root) default-directory) nil t)))
  (cl-pushnew (lsp-f-canonical project-root)
              (lsp-session-folders (lsp-session)) :test 'equal)
  (lsp--persist-session (lsp-session))

  (run-hook-with-args 'lsp-workspace-folders-changed-functions (list project-root) nil))

(defun lsp-workspace-folders-remove (project-root)
  "Remove PROJECT-ROOT from the list of workspace folders."
  (interactive (list (completing-read "Select folder to remove: "
                                      (lsp-session-folders (lsp-session))
                                      nil t nil nil
                                      (lsp-find-session-folder (lsp-session) default-directory))))

  (setq project-root (lsp-f-canonical project-root))

  ;; send remove folder to each multiroot workspace associated with the folder
  (dolist (wks (->> (lsp-session)
                    (lsp-session-folder->servers)
                    (gethash project-root)
                    (--filter (lsp--client-multi-root (lsp--workspace-client it)))))
    (with-lsp-workspace wks
      (lsp-notify "workspace/didChangeWorkspaceFolders"
                  (lsp-make-did-change-workspace-folders-params
                   :event (lsp-make-workspace-folders-change-event
                           :removed (vector (lsp-make-workspace-folder
                                             :uri (lsp--path-to-uri project-root)
                                             :name (f-filename project-root)))
                           :added [])))))

  ;; turn off servers in the removed directory
  (let* ((session (lsp-session))
         (folder->servers (lsp-session-folder->servers session))
         (server-id->folders (lsp-session-server-id->folders session))
         (workspaces (gethash project-root folder->servers)))

    (remhash project-root folder->servers)

    ;; turn off the servers without root folders
    (dolist (workspace workspaces)
      (when (--none? (-contains? it workspace) (ht-values folder->servers))
        (lsp--info "Shutdown %s since folder %s is removed..."
                   (lsp--workspace-print workspace) project-root)
        (with-lsp-workspace workspace (lsp--shutdown-workspace))))

    (setf (lsp-session-folders session)
          (-remove-item project-root (lsp-session-folders session)))

    (ht-aeach (puthash key
                       (-remove-item project-root value)
                       server-id->folders)
              server-id->folders)
    (lsp--persist-session (lsp-session)))

  (run-hook-with-args 'lsp-workspace-folders-changed-functions nil (list project-root)))

(defun lsp-workspace-blocklist-remove (project-root)
  "Remove PROJECT-ROOT from the workspace blocklist."
  (interactive (list (completing-read "Select folder to remove:"
                                      (lsp-session-folders-blocklist (lsp-session))
                                      nil t)))
  (setf (lsp-session-folders-blocklist (lsp-session))
        (delete project-root
                (lsp-session-folders-blocklist (lsp-session))))
  (lsp--persist-session (lsp-session)))

(define-obsolete-function-alias 'lsp-workspace-folders-switch
  'lsp-workspace-folders-open "lsp-mode 6.1")

(defun lsp-workspace-folders-open (project-root)
  "Open the directory located at PROJECT-ROOT"
  (interactive (list (completing-read "Open folder: "
                                      (lsp-session-folders (lsp-session))
                                      nil t)))
  (find-file project-root))

(defun lsp--maybe-enable-signature-help (trigger-characters)
  (let ((ch last-command-event))
    (when (cl-find ch trigger-characters :key #'string-to-char)
      (lsp-signature-activate))))

(defun lsp--on-type-formatting-handler-create ()
  (when-let ((provider (lsp--capability-for-method "textDocument/onTypeFormatting" )))
    (-let [(&DocumentOnTypeFormattingOptions :more-trigger-character?
                                             :first-trigger-character) provider]
      (lambda ()
        (lsp--on-type-formatting first-trigger-character
                                 more-trigger-character?)))))

(defun lsp--update-on-type-formatting-hook (&optional cleanup?)
  (let ((on-type-formatting-handler (lsp--on-type-formatting-handler-create)))
    (cond
     ((and lsp-enable-on-type-formatting on-type-formatting-handler (not cleanup?))
      (add-hook 'post-self-insert-hook on-type-formatting-handler nil t))
     ((or cleanup?
          (not lsp-enable-on-type-formatting))
      (remove-hook 'post-self-insert-hook on-type-formatting-handler t)))))

(defun lsp--signature-help-handler-create ()
  (-when-let ((&SignatureHelpOptions? :trigger-characters?)
              (lsp--capability-for-method "textDocument/signatureHelp"))
    (lambda ()
      (lsp--maybe-enable-signature-help trigger-characters?))))

(defun lsp--update-signature-help-hook (&optional cleanup?)
  (let ((signature-help-handler (lsp--signature-help-handler-create)))
    (cond
     ((and (or (equal lsp-signature-auto-activate t)
               (memq :on-trigger-char lsp-signature-auto-activate))
           signature-help-handler)
      (add-hook 'post-self-insert-hook signature-help-handler nil t))

     ((or cleanup?
          (not (or (equal lsp-signature-auto-activate t)
                   (memq :on-trigger-char lsp-signature-auto-activate))))
      (remove-hook 'post-self-insert-hook signature-help-handler t)))))

(defun lsp--after-set-visited-file-name ()
  (lsp-disconnect)
  (lsp))

;; TODO remove those eldoc workarounds when dropping support for Emacs 27
;; https://github.com/emacs-lsp/lsp-mode/issues/3295#issuecomment-1308994099
(defvar eldoc-documentation-default) ; CI
(when (< emacs-major-version 28)
  (unless (boundp 'eldoc-documentation-functions)
    (load "eldoc"))
  (when (memq (default-value 'eldoc-documentation-function) '(nil ignore))
    ;; actually `eldoc-documentation-strategy', but CI was failing
    (setq-default eldoc-documentation-function 'eldoc-documentation-default)))

(define-minor-mode lsp-managed-mode
  "Mode for source buffers managed by lsp-mode."
  :lighter nil
  (cond
   (lsp-managed-mode
    (when (lsp-feature? "textDocument/hover")
      (add-hook 'eldoc-documentation-functions #'lsp-eldoc-function nil t)
      (eldoc-mode 1))

    (add-hook 'after-change-functions #'lsp-on-change nil t)
    (add-hook 'after-revert-hook #'lsp-on-revert nil t)
    (add-hook 'after-save-hook #'lsp-on-save nil t)
    (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
    (add-hook 'before-change-functions #'lsp-before-change nil t)
    (add-hook 'before-save-hook #'lsp--before-save nil t)
    (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)
    (add-hook 'post-command-hook #'lsp--post-command nil t)

    (lsp--update-on-type-formatting-hook)
    (lsp--update-signature-help-hook)

    (when lsp-enable-xref
      (add-hook 'xref-backend-functions #'lsp--xref-backend nil t))

    (lsp-configure-buffer)

    ;; make sure we turn off lsp-mode in case major mode changes, because major
    ;; mode change will wipe the buffer locals.
    (add-hook 'change-major-mode-hook #'lsp-disconnect nil t)
    (add-hook 'after-set-visited-file-name-hook #'lsp--after-set-visited-file-name nil t)

    (let ((buffer (lsp-current-buffer)))
      (run-with-idle-timer
       0.0 nil
       (lambda ()
         (when (lsp-buffer-live-p buffer)
           (lsp-with-current-buffer buffer
             (lsp--on-change-debounce buffer)
             (lsp--on-idle buffer)))))))
   (t
    (lsp-unconfig-buffer)

    (remove-hook 'eldoc-documentation-functions #'lsp-eldoc-function t)
    (remove-hook 'post-command-hook #'lsp--post-command t)
    (remove-hook 'after-change-functions #'lsp-on-change t)
    (remove-hook 'after-revert-hook #'lsp-on-revert t)
    (remove-hook 'after-save-hook #'lsp-on-save t)
    (remove-hook 'auto-save-hook #'lsp--on-auto-save t)
    (remove-hook 'before-change-functions #'lsp-before-change t)
    (remove-hook 'before-save-hook #'lsp--before-save t)
    (remove-hook 'kill-buffer-hook #'lsp--text-document-did-close t)

    (lsp--update-on-type-formatting-hook :cleanup)
    (lsp--update-signature-help-hook :cleanup)

    (when lsp--on-idle-timer
      (cancel-timer lsp--on-idle-timer)
      (setq lsp--on-idle-timer nil))

    (remove-hook 'lsp-on-idle-hook #'lsp--document-links t)
    (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)

    (lsp--remove-overlays 'lsp-highlight)
    (lsp--remove-overlays 'lsp-links)

    (remove-hook 'xref-backend-functions #'lsp--xref-backend t)
    (remove-hook 'change-major-mode-hook #'lsp-disconnect t)
    (remove-hook 'after-set-visited-file-name-hook #'lsp--after-set-visited-file-name t)
    (setq-local lsp-buffer-uri nil))))

(defun lsp-configure-buffer ()
  "Configure LSP features for current buffer."
  ;; make sure the core is running in the context of all available workspaces
  ;; to avoid misconfiguration in case we are running in `with-lsp-workspace' context
  (let ((lsp--buffer-workspaces (cond
                                 (lsp--buffer-workspaces)
                                 (lsp--cur-workspace (list lsp--cur-workspace))))
        lsp--cur-workspace)
    (when lsp-auto-configure
      (lsp--auto-configure)

      (when (and lsp-enable-text-document-color
                 (lsp-feature? "textDocument/documentColor"))
        (add-hook 'lsp-on-change-hook #'lsp--document-color nil t))

      (when (and lsp-enable-imenu
                 (lsp-feature? "textDocument/documentSymbol"))
        (lsp-enable-imenu))

      (when (and lsp-enable-indentation
                 (lsp-feature? "textDocument/rangeFormatting"))
        (add-function :override (local 'indent-region-function) #'lsp-format-region))

      (when (and lsp-enable-symbol-highlighting
                 (lsp-feature? "textDocument/documentHighlight"))
        (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t))

      (when (and lsp-enable-links
                 (lsp-feature? "textDocument/documentLink"))
        (add-hook 'lsp-on-idle-hook #'lsp--document-links nil t))

      (when (and lsp-inlay-hint-enable
                 (lsp-feature? "textDocument/inlayHint"))
        (lsp-inlay-hints-mode))

      (when (and lsp-enable-dap-auto-configure
                 (functionp 'dap-mode))
        (dap-auto-configure-mode 1)))
    (run-hooks 'lsp-configure-hook)))

(defun lsp-unconfig-buffer ()
  "Unconfigure LSP features for buffer."
  (lsp--remove-overlays 'lsp-color)

  (when (advice-function-member-p 'lsp--imenu-create-index imenu-create-index-function)
    (remove-function (local 'imenu-create-index-function) #'lsp--imenu-create-index)
    (setq-local imenu-menubar-modified-tick 0)
    (setq-local imenu--index-alist nil)
    (imenu--cleanup))

  (remove-function (local 'indent-region-function) #'lsp-format-region)

  (remove-hook 'lsp-on-change-hook #'lsp--document-color t)
  (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
  (remove-hook 'lsp-on-idle-hook #'lsp--document-links t)

  (when (and lsp-enable-dap-auto-configure
             (functionp 'dap-mode))
    (dap-auto-configure-mode -1))

  (run-hooks 'lsp-unconfigure-hook))

(defun lsp--buffer-content ()
  (lsp-save-restriction-and-excursion
    (or (lsp-virtual-buffer-call :buffer-string)
        (buffer-substring-no-properties (point-min)
                                        (point-max)))))

(defun lsp--text-document-did-open ()
  "`document/didOpen' event."
  (run-hooks 'lsp-before-open-hook)
  (when (and lsp-auto-touch-files
             (not (f-exists? (lsp--uri-to-path (lsp--buffer-uri)))))
    (lsp--info "Saving file '%s' because it is not present on the disk." (lsp--buffer-uri))
    (save-buffer))

  (setq lsp--cur-version (or lsp--cur-version 0))
  (cl-pushnew (lsp-current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (lsp-buffer-language)
               :version lsp--cur-version
               :text (lsp--buffer-content))))

  (lsp-managed-mode 1)

  (run-hooks 'lsp-after-open-hook)
  (when-let ((client (-some-> lsp--cur-workspace (lsp--workspace-client))))
    (-some-> (lsp--client-after-open-fn client)
      (funcall))
    (-some-> (format "lsp-%s-after-open-hook" (lsp--client-server-id client))
      (intern-soft)
      (run-hooks))))

(defun lsp--text-document-identifier ()
  "Make TextDocumentIdentifier."
  (list :uri (lsp--buffer-uri)))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier."
  (plist-put (lsp--text-document-identifier) :version lsp--cur-version))

(defun lsp--cur-line (&optional point)
  (1- (line-number-at-pos point)))

(defun lsp--cur-position ()
  "Make a Position object for the current point."
  (or (lsp-virtual-buffer-call :cur-position)
      (lsp-save-restriction-and-excursion
        (list :line (lsp--cur-line)
              :character (- (point) (line-beginning-position))))))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (lsp-save-restriction-and-excursion
    (goto-char point)
    (lsp--cur-position)))

(defun lsp--range (start end)
  "Make Range body from START and END."
  ;; make sure start and end are Position objects
  (list :start start :end end))

(defun lsp--region-to-range (start end)
  "Make Range object for the current region."
  (lsp--range (lsp--point-to-position start)
              (lsp--point-to-position end)))

(defun lsp--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (line-beginning-position) (line-end-position))))

(defun lsp--check-document-changes-version (document-changes)
  "Verify that DOCUMENT-CHANGES have the proper version."
  (unless (seq-every-p
           (-lambda ((&TextDocumentEdit :text-document))
             (or
              (not text-document)
              (let* ((filename (-> text-document
                                   lsp:versioned-text-document-identifier-uri
                                   lsp--uri-to-path))
                     (version (lsp:versioned-text-document-identifier-version? text-document)))
                (with-current-buffer (find-file-noselect filename)
                  (or (null version) (zerop version) (= -1 version)
                      (equal version lsp--cur-version))))))
           document-changes)
    (error "Document changes cannot be applied due to different document version")))

(defun lsp--apply-workspace-edit (workspace-edit &optional operation)
  "Apply the WorkspaceEdit object WORKSPACE-EDIT.
OPERATION is symbol representing the source of this text edit."
  (-let (((&WorkspaceEdit :document-changes? :changes?) workspace-edit))
    (if-let ((document-changes (seq-reverse document-changes?)))
        (progn
          (lsp--check-document-changes-version document-changes)
          (->> document-changes
               (seq-filter (-lambda ((&CreateFile :kind)) (equal kind "create")))
               (seq-do (lambda (change) (lsp--apply-text-document-edit change operation))))
          (->> document-changes
               (seq-filter (-lambda ((&CreateFile :kind))
                             (and (or (not kind) (equal kind "edit"))
                                  (not (equal kind "create")))))
               (seq-do (lambda (change) (lsp--apply-text-document-edit change operation))))
          (->> document-changes
               (seq-filter (-lambda ((&CreateFile :kind))
                             (and (not (or (not kind) (equal kind "edit")))
                                  (not (equal kind "create")))))
               (seq-do (lambda (change) (lsp--apply-text-document-edit change operation)))))
      (lsp-map
       (lambda (uri text-edits)
         (with-current-buffer (-> uri lsp--uri-to-path find-file-noselect)
           (lsp--apply-text-edits text-edits operation)))
       changes?))))

(defmacro lsp-with-filename (file &rest body)
  "Execute BODY with FILE as a context.
Need to handle the case when FILE indicates virtual buffer."
  (declare (indent 1) (debug t))
  `(if-let ((lsp--virtual-buffer (get-text-property 0 'lsp-virtual-buffer ,file)))
       (lsp-with-current-buffer lsp--virtual-buffer
         ,@body)
     ,@body))

(defun lsp--apply-text-document-edit (edit &optional operation)
  "Apply the TextDocumentEdit object EDIT.
OPERATION is symbol representing the source of this text edit.
If the file is not being visited by any buffer, it is opened with
`find-file-noselect'.
Because lsp-mode does not store previous document versions, the edit is only
applied if the version of the textDocument matches the version of the
corresponding file.

interface TextDocumentEdit {
  textDocument: VersionedTextDocumentIdentifier;
  edits: TextEdit[];
}"
  (pcase (lsp:edit-kind edit)
    ("create" (-let* (((&CreateFile :uri :options?) edit)
                      (file-name (lsp--uri-to-path uri)))
                (mkdir (f-dirname file-name) t)
                (f-touch file-name)
                (when (lsp:create-file-options-overwrite? options?)
                  (f-write-text "" nil file-name))
                (find-file-noselect file-name)))
    ("delete" (-let (((&DeleteFile :uri :options? (&DeleteFileOptions? :recursive?)) edit))
                (f-delete (lsp--uri-to-path uri) recursive?)))
    ("rename" (-let* (((&RenameFile :old-uri :new-uri :options? (&RenameFileOptions? :overwrite?)) edit)
                      (old-file-name (lsp--uri-to-path old-uri))
                      (new-file-name (lsp--uri-to-path new-uri))
                      (buf (find-buffer-visiting old-file-name)))
                (when buf
                  (lsp-with-current-buffer buf
                    (save-buffer)
                    (lsp--text-document-did-close)))
                (mkdir (f-dirname new-file-name) t)
                (rename-file old-file-name new-file-name overwrite?)
                (when buf
                  (lsp-with-current-buffer buf
                    (set-buffer-modified-p nil)
                    (setq lsp-buffer-uri nil)
                    (set-visited-file-name new-file-name)
                    (lsp)))))
    (_ (let ((file-name (->> edit
                             (lsp:text-document-edit-text-document)
                             (lsp:versioned-text-document-identifier-uri)
                             (lsp--uri-to-path))))
         (lsp-with-current-buffer (find-buffer-visiting file-name)
           (lsp-with-filename file-name
             (lsp--apply-text-edits (lsp:text-document-edit-edits edit) operation)))))))

(lsp-defun lsp--position-compare ((&Position :line left-line
                                             :character left-character)
                                  (&Position :line right-line
                                             :character right-character))
  "Return t if position LEFT is greater than RIGHT."
  (if (= left-line right-line)
      (> left-character right-character)
    (> left-line right-line)))

(lsp-defun lsp-point-in-range? (position (&Range :start :end))
  "Returns if POINT is in RANGE."
  (not (or (lsp--position-compare start position)
           (lsp--position-compare position end))))

(lsp-defun lsp--position-equal ((&Position :line left-line
                                           :character left-character)
                                (&Position :line right-line
                                           :character right-character))
  "Return whether LEFT and RIGHT positions are equal."
  (and (= left-line right-line)
       (= left-character right-character)))

(lsp-defun lsp--text-edit-sort-predicate ((&TextEdit :range (&Range :start left-start :end left-end))
                                          (&TextEdit :range (&Range :start right-start :end right-end)))
  (if (lsp--position-equal left-start right-start)
      (lsp--position-compare left-end right-end)
    (lsp--position-compare left-start right-start)))

(lsp-defun lsp--apply-text-edit ((edit &as &TextEdit :range (&RangeToPoint :start :end) :new-text))
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (setq new-text (s-replace "\r" "" (or new-text "")))
  (lsp:set-text-edit-new-text edit new-text)
  (goto-char start)
  (delete-region start end)
  (insert new-text))

;; WORKAROUND: typescript-language might send -1 when applying code actions.
;; see https://github.com/emacs-lsp/lsp-mode/issues/1582
(lsp-defun lsp--fix-point ((point &as &Position :character :line))
  (-doto point
    (lsp:set-position-line (max 0 line))
    (lsp:set-position-character (max 0 character))))

(lsp-defun lsp--apply-text-edit-replace-buffer-contents ((edit &as
                                                               &TextEdit
                                                               :range (&Range :start :end)
                                                               :new-text))
  "Apply the edits described in the TextEdit object in TEXT-EDIT.
The method uses `replace-buffer-contents'."
  (setq new-text (s-replace "\r" "" (or new-text "")))
  (lsp:set-text-edit-new-text edit new-text)
  (-let* ((source (current-buffer))
          ((beg . end) (lsp--range-to-region (lsp-make-range :start (lsp--fix-point start)
                                                             :end (lsp--fix-point end)))))
    (with-temp-buffer
      (insert new-text)
      (let ((temp (current-buffer)))
        (with-current-buffer source
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)

              ;; On emacs versions < 26.2,
              ;; `replace-buffer-contents' is buggy - it calls
              ;; change functions with invalid arguments - so we
              ;; manually call the change functions here.
              ;;
              ;; See emacs bugs #32237, #32278:
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
              (let ((inhibit-modification-hooks t)
                    (length (- end beg)))
                (run-hook-with-args 'before-change-functions
                                    beg end)
                (replace-buffer-contents temp)
                (run-hook-with-args 'after-change-functions
                                    beg (+ beg (length new-text))
                                    length)))))))))

(defun lsp--to-yasnippet-snippet (snippet)
  "Convert LSP SNIPPET to yasnippet snippet."
  ;; LSP snippet doesn't escape "{" and "`", but yasnippet requires escaping it.
  (replace-regexp-in-string (rx (or bos (not (any "$" "\\"))) (group (or "{" "`")))
                            (rx "\\" (backref 1))
                            snippet
                            nil nil 1))

(defvar-local lsp-enable-relative-indentation nil
  "Enable relative indentation when insert texts, snippets ...
from language server.")

(defun lsp--expand-snippet (snippet &optional start end expand-env)
  "Wrapper of `yas-expand-snippet' with all of it arguments.
The snippet will be convert to LSP style and indent according to
LSP server result."
  (require 'yasnippet nil t)
  (let* ((inhibit-field-text-motion t)
         (yas-wrap-around-region nil)
         (yas-indent-line 'none)
         (yas-also-auto-indent-first-line nil))
    (yas-expand-snippet
     (lsp--to-yasnippet-snippet snippet)
     start end expand-env)))

(defun lsp--indent-lines (start end &optional insert-text-mode?)
  "Indent from START to END based on INSERT-TEXT-MODE? value.
- When INSERT-TEXT-MODE? is provided
  - if it's `lsp/insert-text-mode-as-it', do no editor indentation.
  - if it's `lsp/insert-text-mode-adjust-indentation', adjust leading
    whitespaces to match the line where text is inserted.
- When it's not provided, using `indent-line-function' for each line."
  (save-excursion
    (goto-char end)
    (let* ((end-line (line-number-at-pos))
           (offset (save-excursion
                     (goto-char start)
                     (current-indentation)))
           (indent-line-function
            (cond ((equal insert-text-mode? lsp/insert-text-mode-as-it)
                   #'ignore)
                  ((or (equal insert-text-mode? lsp/insert-text-mode-adjust-indentation)
                       lsp-enable-relative-indentation
                       ;; Indenting snippets is extremely slow in `org-mode' buffers
                       ;; since it has to calculate indentation based on SRC block
                       ;; position.  Thus we use relative indentation as default.
                       (derived-mode-p 'org-mode))
                   (lambda () (save-excursion
                                (beginning-of-line)
                                (indent-to-column offset))))
                  (t indent-line-function))))
      (goto-char start)
      (forward-line)
      (while (and (not (eobp))
                  (<= (line-number-at-pos) end-line))
        (funcall indent-line-function)
        (forward-line)))))

(defun lsp--apply-text-edits (edits &optional operation)
  "Apply the EDITS described in the TextEdit[] object.
OPERATION is symbol representing the source of this text edit."
  (unless (seq-empty-p edits)
    (atomic-change-group
      (run-hooks 'lsp-before-apply-edits-hook)
      (let* ((change-group (prepare-change-group))
             (howmany (length edits))
             (message (format "Applying %s edits to `%s' ..." howmany (current-buffer)))
             (_ (lsp--info message))
             (reporter (make-progress-reporter message 0 howmany))
             (done 0)
             (apply-edit (if (not lsp--virtual-buffer)
                             #'lsp--apply-text-edit-replace-buffer-contents
                           #'lsp--apply-text-edit)))
        (unwind-protect
            (->> edits
                 ;; We sort text edits so as to apply edits that modify latter
                 ;; parts of the document first. Furthermore, because the LSP
                 ;; spec dictates that: "If multiple inserts have the same
                 ;; position, the order in the array defines which edit to
                 ;; apply first."  We reverse the initial list and sort stably
                 ;; to make sure the order among edits with the same position
                 ;; is preserved.
                 (nreverse)
                 (seq-sort #'lsp--text-edit-sort-predicate)
                 (mapc (lambda (edit)
                         (progress-reporter-update reporter (cl-incf done))
                         (funcall apply-edit edit)
                         (when (lsp:snippet-text-edit-insert-text-format? edit)
                           (-when-let ((&SnippetTextEdit :range (&RangeToPoint :start)
                                                         :insert-text-format? :new-text) edit)
                             (when (eq insert-text-format? lsp/insert-text-format-snippet)
                               ;; No `save-excursion' needed since expand snippet will change point anyway
                               (goto-char (+ start (length new-text)))
                               (lsp--indent-lines start (point))
                               (lsp--expand-snippet new-text start (point)))))
                         (run-hook-with-args 'lsp-after-apply-edits-hook operation))))
          (undo-amalgamate-change-group change-group)
          (progress-reporter-done reporter))))))

(defun lsp--create-apply-text-edits-handlers ()
  "Create (handler cleanup-fn) for applying text edits in async request.
Only works when mode is `tick or `alive."
  (let* (first-edited
         (func (lambda (start &rest _)
                 (setq first-edited (if first-edited
                                        (min start first-edited)
                                      start)))))
    (add-hook 'before-change-functions func nil t)
    (list
     (lambda (edits)
       (if (and first-edited
                (seq-find (-lambda ((&TextEdit :range (&RangeToPoint :end)))
                            ;; Text edit region is overlapped
                            (> end first-edited))
                          edits))
           (lsp--warn "TextEdits will not be applied since document has been modified before of them.")
         (lsp--apply-text-edits edits 'completion-cleanup)))
     (lambda ()
       (remove-hook 'before-change-functions func t)))))

(defun lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (when (stringp cap)
    (setq cap (intern (concat ":" cap))))

  (lsp-get (or capabilities
               (lsp--server-capabilities))
           cap))

(defun lsp--registered-capability (method)
  "Check whether there is workspace providing METHOD."
  (->> (lsp-workspaces)
       (--keep (seq-find (lambda (reg)
                           (equal (lsp--registered-capability-method reg) method))
                         (lsp--workspace-registered-server-capabilities it)))
       cl-first))

(defun lsp--capability-for-method (method)
  "Get the value of capability for METHOD."
  (-let* ((reqs (cdr (assoc method lsp-method-requirements)))
          ((&plist :capability) reqs))
    (or (and capability (lsp--capability capability))
        (-some-> (lsp--registered-capability method)
          (lsp--registered-capability-options)))))

(defvar-local lsp--before-change-vals nil
  "Store the positions from the `lsp-before-change' function call, for
validation and use in the `lsp-on-change' function.")

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":5,"character":6}
  ;;             ,"end" :{"line":5,"character":6}}
  ;;             ,"rangeLength":0
  ;;             ,"text":"\nbb = 5"}
  ;;
  ;; And (47 47 7) means delete 7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":6,"character":0}
  ;;            ,"end"  :{"line":7,"character":0}}
  ;;            ,"rangeLength":7
  ;;            ,"text":""}
  ;;
  ;; (208 221 3) means delete 3 chars starting at pos 208, and replace them with
  ;; 13 chars. So it must become
  ;;   {"range":{"start":{"line":5,"character":8}
  ;;             ,"end" :{"line":5,"character":11}}
  ;;             ,"rangeLength":3
  ;;             ,"text":"new-chars-xxx"}
  ;;

  ;; Adding text:
  ;;   lsp-before-change:(start,end)=(33,33)
  ;;   lsp-on-change:(start,end,length)=(33,34,0)
  ;;
  ;; Changing text:
  ;;   lsp-before-change:(start,end)=(208,211)
  ;;   lsp-on-change:(start,end,length)=(208,221,3)
  ;;
  ;; Deleting text:
  ;;   lsp-before-change:(start,end)=(19,27)
  ;;   lsp-on-change:(start,end,length)=(19,19,8)
  (if (zerop length)
      ;; Adding something only, work from start only
      `( :range ,(lsp--range
                  (lsp--point-to-position start)
                  (lsp--point-to-position start))
         :rangeLength 0
         :text ,(buffer-substring-no-properties start end))

    (if (eq start end)
        ;; Deleting something only
        (if (lsp--bracketed-change-p start length)
            ;; The before-change value is bracketed, use it
            `( :range ,(lsp--range
                        (lsp--point-to-position start)
                        (plist-get lsp--before-change-vals :end-pos))
               :rangeLength ,length
               :text "")
          ;; If the change is not bracketed, send a full change event instead.
          (lsp--full-change-event))

      ;; Deleting some things, adding others
      (if (lsp--bracketed-change-p start length)
          ;; The before-change value is valid, use it
          `( :range ,(lsp--range
                      (lsp--point-to-position start)
                      (plist-get lsp--before-change-vals :end-pos))
             :rangeLength ,length
             :text ,(buffer-substring-no-properties start end))
        (lsp--full-change-event)))))

(defun lsp--bracketed-change-p (start length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (-let [(&plist :end before-end :start before-start) lsp--before-change-vals]
    (and (eq start before-start)
         (eq length (- before-end before-start)))))

(defun lsp--full-change-event ()
  `(:text ,(lsp--buffer-content)))

(defun lsp-before-change (start end)
  "Executed before a file is changed.
Added to `before-change-functions'."
  ;; Note:
  ;;
  ;; This variable holds a list of functions to call when Emacs is about to
  ;; modify a buffer. Each function gets two arguments, the beginning and end of
  ;; the region that is about to change, represented as integers. The buffer
  ;; that is about to change is always the current buffer when the function is
  ;; called.
  ;;
  ;; WARNING:
  ;;
  ;; Do not expect the before-change hooks and the after-change hooks be called
  ;; in balanced pairs around each buffer change. Also don't expect the
  ;; before-change hooks to be called for every chunk of text Emacs is about to
  ;; delete. These hooks are provided on the assumption that Lisp programs will
  ;; use either before- or the after-change hooks, but not both, and the
  ;; boundaries of the region where the changes happen might include more than
  ;; just the actual changed text, or even lump together several changes done
  ;; piecemeal.
  (save-match-data
    (lsp-save-restriction-and-excursion
      (setq lsp--before-change-vals
            (list :start start
                  :end end
                  :end-pos (lsp--point-to-position end))))))

(defun lsp--flush-delayed-changes ()
  (let ((inhibit-quit t))
    (when lsp--delay-timer
      (cancel-timer lsp--delay-timer))
    (mapc (-lambda ((workspace buffer document change))
            (with-current-buffer buffer
              (with-lsp-workspace workspace
                (lsp-notify "textDocument/didChange"
                            (list :textDocument document
                                  :contentChanges (vector change))))))
          (prog1 (nreverse lsp--delayed-requests)
            (setq lsp--delayed-requests nil)))))

(defun lsp--workspace-sync-method (workspace)
  (let ((sync (-> workspace
                  (lsp--workspace-server-capabilities)
                  (lsp:server-capabilities-text-document-sync?))))
    (if (lsp-text-document-sync-options? sync)
        (lsp:text-document-sync-options-change? sync)
      sync)))

(defun lsp-on-change (start end length &optional content-change-event-fn)
  "Executed when a file is changed.
Added to `after-change-functions'."
  ;; Note:
  ;;
  ;; Each function receives three arguments: the beginning and end of the region
  ;; just changed, and the length of the text that existed before the change.
  ;; All three arguments are integers. The buffer that has been changed is
  ;; always the current buffer when the function is called.
  ;;
  ;; The length of the old text is the difference between the buffer positions
  ;; before and after that text as it was before the change. As for the
  ;; changed text, its length is simply the difference between the first two
  ;; arguments.
  ;;
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; So (47 47 7) means delete 7 chars starting at pos 47
  (save-match-data
    (let ((inhibit-quit t)
          ;; make sure that `lsp-on-change' is called in multi-workspace context
          ;; see #2901
          lsp--cur-workspace)
      ;; A (revert-buffer) call with the 'preserve-modes parameter (eg, as done
      ;; by auto-revert-mode) will cause this handler to get called with a nil
      ;; buffer-file-name. We need the buffer-file-name to send notifications;
      ;; so we skip handling revert-buffer-caused changes and instead handle
      ;; reverts separately in lsp-on-revert
      (when (not revert-buffer-in-progress-p)
        (cl-incf lsp--cur-version)
        (mapc
         (lambda (workspace)
           (pcase (or lsp-document-sync-method
                      (lsp--workspace-sync-method workspace))
             (1
              (if lsp-debounce-full-sync-notifications
                  (setq lsp--delayed-requests
                        (->> lsp--delayed-requests
                             (-remove (-lambda ((_ buffer))
                                        (equal (current-buffer) buffer)))
                             (cons (list workspace
                                         (current-buffer)
                                         (lsp--versioned-text-document-identifier)
                                         (lsp--full-change-event)))))
                (with-lsp-workspace workspace
                  (lsp-notify "textDocument/didChange"
                              (list :contentChanges (vector (lsp--full-change-event))
                                    :textDocument (lsp--versioned-text-document-identifier))))))
             (2
              (with-lsp-workspace workspace
                (lsp-notify
                 "textDocument/didChange"
                 (list :textDocument (lsp--versioned-text-document-identifier)
                       :contentChanges (vector
                                        (if content-change-event-fn
                                            (funcall content-change-event-fn start end length)
                                          (lsp--text-document-content-change-event
                                           start end length)))))))))
         (lsp-workspaces))
        (when lsp--delay-timer (cancel-timer lsp--delay-timer))
        (setq lsp--delay-timer (run-with-idle-timer
                                lsp-debounce-full-sync-notifications-interval
                                nil
                                #'lsp--flush-delayed-changes))
        ;; force cleanup overlays after each change
        (lsp--remove-overlays 'lsp-highlight)
        (lsp--after-change (current-buffer))
        (setq lsp--signature-last-index nil
              lsp--signature-last nil)
        ;; cleanup diagnostics
        (when lsp-diagnostic-clean-after-change
          (lsp-foreach-workspace
           (-let [diagnostics (lsp--workspace-diagnostics lsp--cur-workspace)]
             (remhash (lsp--fix-path-casing (buffer-file-name)) diagnostics))))))))



;; facilities for on change hooks. We do not want to make lsp calls on each
;; change event so we add debounce to avoid flooding the server with events.
;; Additionally, we want to have a mechanism for stopping the server calls in
;; particular cases like, e. g. when performing completion.

(defvar lsp-inhibit-lsp-hooks nil
  "Flag to control.")

(defcustom lsp-on-change-hook nil
  "Hooks to run when buffer has changed."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-idle-delay 0.500
  "Debounce interval for `after-change-functions'."
  :type 'number
  :group 'lsp-mode)

(defcustom lsp-on-idle-hook nil
  "Hooks to run after `lsp-idle-delay'."
  :type 'hook
  :group 'lsp-mode)

(defun lsp--idle-reschedule (buffer)
  (when lsp--on-idle-timer
    (cancel-timer lsp--on-idle-timer))

  (setq lsp--on-idle-timer (run-with-idle-timer
                            lsp-idle-delay
                            nil
                            #'lsp--on-idle
                            buffer)))

(defun lsp--post-command ()
  (lsp--cleanup-highlights-if-needed)
  (lsp--idle-reschedule (current-buffer)))

(defun lsp--on-idle (buffer)
  "Start post command loop."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (not lsp-inhibit-lsp-hooks)
             lsp-managed-mode)
    (run-hooks 'lsp-on-idle-hook)))

(defun lsp--on-change-debounce (buffer)
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (not lsp-inhibit-lsp-hooks)
             lsp-managed-mode)
    (run-hooks 'lsp-on-change-hook)))

(defun lsp--after-change (buffer)
  (when (fboundp 'lsp--semantic-tokens-refresh-if-enabled)
    (lsp--semantic-tokens-refresh-if-enabled buffer))
  (when lsp--on-change-timer
    (cancel-timer lsp--on-change-timer))
  (setq lsp--on-change-timer (run-with-idle-timer
                              lsp-idle-delay
                              nil
                              #'lsp--on-change-debounce
                              buffer))
  (lsp--idle-reschedule buffer))


(defcustom lsp-trim-trailing-whitespace t
  "Trim trailing whitespace on a line."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-insert-final-newline t
  "Insert a newline character at the end of the file if one does not exist."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-trim-final-newlines t
  "Trim all newlines after the final newline at the end of the file."
  :group 'lsp-mode
  :type 'boolean)


(defun lsp--on-type-formatting (first-trigger-characters more-trigger-characters)
  "Self insert handling.
Applies on type formatting."
  (let ((ch last-command-event))
    (when (or (eq (string-to-char first-trigger-characters) ch)
              (cl-find ch more-trigger-characters :key #'string-to-char))
      (lsp-request-async "textDocument/onTypeFormatting"
                         (lsp-make-document-on-type-formatting-params
                          :text-document (lsp--text-document-identifier)
                          :options (lsp-make-formatting-options
                                    :tab-size (symbol-value (lsp--get-indent-width major-mode))
                                    :insert-spaces (lsp-json-bool (not indent-tabs-mode))
                                    :trim-trailing-whitespace? (lsp-json-bool lsp-trim-trailing-whitespace)
                                    :insert-final-newline? (lsp-json-bool lsp-insert-final-newline)
                                    :trim-final-newlines? (lsp-json-bool lsp-trim-final-newlines))
                          :ch (char-to-string ch)
                          :position (lsp--cur-position))
                         (lambda (data) (lsp--apply-text-edits data 'format))
                         :mode 'tick))))


;; links
(defun lsp--document-links ()
  (when (lsp-feature? "textDocument/documentLink")
    (lsp-request-async
     "textDocument/documentLink"
     `(:textDocument ,(lsp--text-document-identifier))
     (lambda (links)
       (lsp--remove-overlays 'lsp-link)
       (seq-do
        (-lambda ((link &as &DocumentLink :range (&Range :start :end)))
          (-doto (make-button (lsp--position-to-point start)
                              (lsp--position-to-point end)
                              'action (lsp--document-link-keymap link)
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [M-return] 'push-button)
                                        (define-key map [mouse-2] 'push-button)
                                        map)
                              'help-echo "mouse-2, M-RET: Visit this link")
            (overlay-put 'lsp-link t)))
        links))
     :mode 'unchanged)))

(defun lsp--document-link-handle-target (url)
  (let* ((parsed-url (url-generic-parse-url (url-unhex-string url)))
         (type (url-type parsed-url)))
    (pcase type
      ("file"
       (xref-push-marker-stack)
       (find-file (lsp--uri-to-path url))
       (-when-let ((_ line column) (s-match (rx "#" (group (1+ num)) (or "," "#") (group (1+ num))) url))
         (goto-char (lsp--position-to-point
                     (lsp-make-position :character (1- (string-to-number column))
                                        :line (1- (string-to-number line)))))))
      ((or "http" "https") (browse-url url))
      (type (if-let ((handler (lsp--get-uri-handler type)))
                (funcall handler url)
              (signal 'lsp-file-scheme-not-supported (list url)))))))

(lsp-defun lsp--document-link-keymap ((link &as &DocumentLink :target?))
  (if target?
      (lambda (_)
        (interactive)
        (lsp--document-link-handle-target target?))
    (lambda (_)
      (interactive)
      (when (lsp:document-link-registration-options-resolve-provider?
             (lsp--capability-for-method "textDocument/documentLink"))
        (lsp-request-async
         "documentLink/resolve"
         link
         (-lambda ((&DocumentLink :target?))
           (lsp--document-link-handle-target target?)))))))



(defcustom lsp-warn-no-matched-clients t
  "Whether to show messages when there are no supported clients."
  :group 'lsp-mode
  :type 'boolean)

(defun lsp-buffer-language--configured-id ()
  "Return nil when not registered."
  (->> lsp-language-id-configuration
       (-first
        (-lambda ((mode-or-pattern . language))
          (cond
           ((and (stringp mode-or-pattern)
                 (s-matches? mode-or-pattern (buffer-file-name)))
            language)
           ((eq mode-or-pattern major-mode) language))))
       cl-rest))

(defvar-local lsp--buffer-language nil
  "Locally cached returned value of `lsp-buffer-language'.")

(defun lsp-buffer-language ()
  "Get language corresponding current buffer."
  (or lsp--buffer-language
      (let* ((configured-language (lsp-buffer-language--configured-id)))
        (setq lsp--buffer-language
              (or configured-language
                  ;; ensure non-nil
                  (string-remove-suffix "-mode" (symbol-name major-mode))))
        (when (and lsp-warn-no-matched-clients
                   (null configured-language))
          (lsp-warn "Unable to calculate the languageId for buffer `%s'. \
Take a look at `lsp-language-id-configuration'. The `major-mode' is %s"
                    (buffer-name)
                    major-mode))
        lsp--buffer-language)))

(defun lsp-activate-on (&rest languages)
  "Returns language activation function.
The function will return t when the `lsp-buffer-language' returns
one of the LANGUAGES."
  (lambda (_file-name _mode)
    (-contains? languages (lsp-buffer-language))))

(defun lsp-workspace-root (&optional path)
  "Find the workspace root for the current file or PATH."
  (-when-let* ((file-name (or path (buffer-file-name)))
               (file-name (lsp-f-canonical file-name)))
    (->> (lsp-session)
         (lsp-session-folders)
         (--filter (and (lsp--files-same-host it file-name)
                        (or (lsp-f-ancestor-of? it file-name)
                            (equal it file-name))))
         (--max-by (> (length it) (length other))))))

(defun lsp-on-revert ()
  "Executed when a file is reverted.
Added to `after-revert-hook'."
  (let ((n (buffer-size))
        (revert-buffer-in-progress-p nil))
    (lsp-on-change 0 n n)))

(defun lsp--text-document-did-close (&optional keep-workspace-alive)
  "Executed when the file is closed, added to `kill-buffer-hook'.

If KEEP-WORKSPACE-ALIVE is non-nil, do not shutdown the workspace
if it's closing the last buffer in the workspace."
  (lsp-foreach-workspace
   (cl-callf2 delq (lsp-current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
   (with-demoted-errors "Error sending didClose notification in ‘lsp--text-document-did-close’: %S"
     (lsp-notify "textDocument/didClose"
                 `(:textDocument ,(lsp--text-document-identifier))))
   (when (and (not lsp-keep-workspace-alive)
              (not keep-workspace-alive)
              (not (lsp--workspace-buffers lsp--cur-workspace)))
     (lsp--shutdown-workspace))))

(defun lsp--will-save-text-document-params (reason)
  (list :textDocument (lsp--text-document-identifier)
        :reason reason))

(defun lsp--before-save ()
  "Before save handler."
  (with-demoted-errors "Error in ‘lsp--before-save’: %S"
    (let ((params (lsp--will-save-text-document-params 1)))
      (when (lsp--send-will-save-p)
        (lsp-notify "textDocument/willSave" params))
      (when (and (lsp--send-will-save-wait-until-p) lsp-before-save-edits)
        (let ((lsp-response-timeout 0.1))
          (condition-case nil
              (lsp--apply-text-edits
               (lsp-request "textDocument/willSaveWaitUntil"
                            params)
               'before-save)
            (error)))))))

(defun lsp--on-auto-save ()
  "Handler for auto-save."
  (when (lsp--send-will-save-p)
    (with-demoted-errors "Error in ‘lsp--on-auto-save’: %S"
      (lsp-notify "textDocument/willSave" (lsp--will-save-text-document-params 2)))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when (lsp--send-did-save-p)
    (with-demoted-errors "Error on ‘lsp--text-document-did-save: %S’"
      (lsp-notify "textDocument/didSave"
                  `( :textDocument ,(lsp--versioned-text-document-identifier)
                     ,@(when (lsp--save-include-text-p)
                         (list :text (lsp--buffer-content))))))))

(defun lsp--text-document-position-params (&optional identifier position)
  "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document
identifier and the position respectively."
  (list :textDocument (or identifier (lsp--text-document-identifier))
        :position (or position (lsp--cur-position))))

(defun lsp--get-buffer-diagnostics ()
  "Return buffer diagnostics."
  (gethash (or
            (plist-get lsp--virtual-buffer :buffer-file-name)
            (lsp--fix-path-casing (buffer-file-name)))
           (lsp-diagnostics t)))

(defun lsp-cur-line-diagnostics ()
  "Return any diagnostics that apply to the current line."
  (-let [(&plist :start (&plist :line start) :end (&plist :line end)) (lsp--region-or-line)]
    (cl-coerce (-filter
                (-lambda ((&Diagnostic :range (&Range :start (&Position :line))))
                  (and (>= line start) (<= line end)))
                (lsp--get-buffer-diagnostics))
               'vector)))

(lsp-defun lsp-range-overlapping?((left &as &Range :start left-start :end left-end)
                                  (right &as &Range :start right-start :end right-end))
  (or (lsp-point-in-range? right-start left)
      (lsp-point-in-range? right-end left)
      (lsp-point-in-range? left-start right)
      (lsp-point-in-range? left-end right)))

(defun lsp-make-position-1 (position)
  (lsp-make-position :line (plist-get position :line)
                     :character (plist-get position :character)))

(defun lsp-cur-possition-diagnostics ()
  "Return any diagnostics that apply to the current line."
  (-let* ((start (if (use-region-p) (region-beginning) (point)))
          (end (if (use-region-p) (region-end) (point)))
          (current-range (lsp-make-range :start (lsp-make-position-1 (lsp-point-to-position start))
                                         :end (lsp-make-position-1 (lsp-point-to-position end)))))
    (->> (lsp--get-buffer-diagnostics)
         (-filter
          (-lambda ((&Diagnostic :range))
            (lsp-range-overlapping? range current-range)))
         (apply 'vector))))

(defalias 'lsp--cur-line-diagnotics 'lsp-cur-line-diagnostics)

(defun lsp--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring (line-beginning-position) (line-end-position)))))

(lsp-defun lsp--xref-make-item (filename (&Range :start (start &as &Position :character start-char :line start-line)
                                                 :end (end &as &Position :character end-char)))
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((line (lsp--extract-line-from-buffer start))
         (len (length line)))
    (add-face-text-property (max (min start-char len) 0)
                            (max (min end-char len) 0)
                            'xref-match t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make (or line filename)
               (xref-make-file-location
                filename
                (lsp-translate-line (1+ start-line))
                (lsp-translate-column start-char)))))

(defun lsp--location-uri (loc)
  (if (lsp-location? loc)
      (lsp:location-uri loc)
    (lsp:location-link-target-uri loc)))

(lsp-defun lsp-goto-location ((loc &as &Location :uri :range (&Range :start)))
  "Go to location."
  (let ((path (lsp--uri-to-path uri)))
    (if (f-exists? path)
        (with-current-buffer (find-file path)
          (goto-char (lsp--position-to-point start)))
      (error "There is no file %s" path))))

(defun lsp--location-range (loc)
  (if (lsp-location? loc)
      (lsp:location-range loc)
    (lsp:location-link-target-selection-range loc)))

(defun lsp--locations-to-xref-items (locations)
  "Return a list of `xref-item' given LOCATIONS, which can be of
type Location, LocationLink, Location[] or LocationLink[]."
  (setq locations
        (pcase locations
          ((seq (or (Location)
                    (LocationLink)))
           (append locations nil))
          ((or (Location)
               (LocationLink))
           (list locations))))

  (cl-labels ((get-xrefs-in-file
               (file-locs)
               (-let [(filename . matches) file-locs]
                 (condition-case err
                     (let ((visiting (find-buffer-visiting filename))
                           (fn (lambda (loc)
                                 (lsp-with-filename filename
                                   (lsp--xref-make-item filename
                                                        (lsp--location-range loc))))))
                       (if visiting
                           (with-current-buffer visiting
                             (seq-map fn matches))
                         (when (file-readable-p filename)
                           (with-temp-buffer
                             (insert-file-contents-literally filename)
                             (seq-map fn matches)))))
                   (error (lsp-warn "Failed to process xref entry for filename '%s': %s"
                                    filename (error-message-string err)))
                   (file-error (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
                                         filename (error-message-string err)))))))

    (->> locations
         (seq-sort #'lsp--location-before-p)
         (seq-group-by (-compose #'lsp--uri-to-path #'lsp--location-uri))
         (seq-map #'get-xrefs-in-file)
         (apply #'nconc))))

(defun lsp--location-before-p (left right)
  "Sort first by file, then by line, then by column."
  (let ((left-uri (lsp--location-uri left))
        (right-uri (lsp--location-uri right)))
    (if (not (string= left-uri right-uri))
        (string< left-uri right-uri)
      (-let (((&Range :start left-start) (lsp--location-range left))
             ((&Range :start right-start) (lsp--location-range right)))
        (lsp--position-compare right-start left-start)))))

(defun lsp--make-reference-params (&optional td-position exclude-declaration)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead.
If EXCLUDE-DECLARATION is non-nil, request the server to include declarations."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
               :context `(:includeDeclaration ,(lsp-json-bool (not exclude-declaration))))))

(defun lsp--cancel-request (id)
  "Cancel request with ID in all workspaces."
  (lsp-foreach-workspace
   (->> lsp--cur-workspace lsp--workspace-client lsp--client-response-handlers (remhash id))
   (lsp-notify "$/cancelRequest" `(:id ,id))))

(defvar-local lsp--hover-saved-bounds nil)

(defun lsp-eldoc-function (cb &rest _ignored)
  "`lsp-mode' eldoc function to display hover info (based on `textDocument/hover')."
  (if (and lsp--hover-saved-bounds
           (lsp--point-in-bounds-p lsp--hover-saved-bounds))
      lsp--eldoc-saved-message
    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil)
    (if (looking-at-p "[[:space:]\n]")
        (setq lsp--eldoc-saved-message nil) ; And returns nil.
      (when (and lsp-eldoc-enable-hover (lsp--capability :hoverProvider))
        (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (-lambda ((hover &as &Hover? :range? :contents))
           (setq lsp--hover-saved-bounds (when range?
                                           (lsp--range-to-region range?)))
           (funcall cb (setq lsp--eldoc-saved-message
                             (when contents
                               (lsp--render-on-hover-content
                                contents
                                lsp-eldoc-render-all)))))
         :error-handler #'ignore
         :mode 'tick
         :cancel-token :eldoc-hover)))))

(defun lsp--point-on-highlight? ()
  (-some? (lambda (overlay)
            (overlay-get overlay 'lsp-highlight))
          (overlays-at (point))))

(defun lsp--cleanup-highlights-if-needed ()
  (when (and lsp-enable-symbol-highlighting
             lsp--have-document-highlights
             (not (lsp--point-on-highlight?)))
    (lsp--remove-overlays 'lsp-highlight)
    (setq lsp--have-document-highlights nil)
    (lsp-cancel-request-by-token :highlights)))

(defvar-local lsp--symbol-bounds-of-last-highlight-invocation nil
  "The bounds of the symbol from which `lsp--document-highlight'
  most recently requested highlights.")

(defun lsp--document-highlight ()
  (when (lsp-feature? "textDocument/documentHighlight")
    (let ((curr-sym-bounds (bounds-of-thing-at-point 'symbol)))
      (unless (or (looking-at-p "[[:space:]\n]")
                  (not lsp-enable-symbol-highlighting)
                  (and lsp--have-document-highlights
                       curr-sym-bounds
                       (equal curr-sym-bounds
                              lsp--symbol-bounds-of-last-highlight-invocation)))
        (setq lsp--symbol-bounds-of-last-highlight-invocation
              curr-sym-bounds)
        (lsp-request-async "textDocument/documentHighlight"
                           (lsp--text-document-position-params)
                           #'lsp--document-highlight-callback
                           :mode 'tick
                           :cancel-token :highlights)))))

(defun lsp--help-open-link (&rest _)
  "Open markdown link at point via mouse or keyboard."
  (interactive "P")
  (let ((buffer-list-update-hook nil))
    (-let [(buffer point) (if-let* ((valid (and (listp last-input-event)
                                                (eq (car last-input-event) 'mouse-2)))
                                    (event (cadr last-input-event))
                                    (win (posn-window event))
                                    (buffer (window-buffer win)))
                              `(,buffer ,(posn-point event))
                            `(,(current-buffer) ,(point)))]
      (with-current-buffer buffer
        (when-let* ((face (get-text-property point 'face))
                    (url (or (and (eq face 'markdown-link-face)
                                  (get-text-property point 'help-echo))
                             (and (memq face '(markdown-url-face markdown-plain-url-face))
                                  (nth 3 (markdown-link-at-pos point))))))
          (lsp--document-link-handle-target url))))))

(defvar lsp-help-mode-map
  (-doto (make-sparse-keymap)
    (define-key [remap markdown-follow-link-at-point] #'lsp--help-open-link))
  "Keymap for `lsp-help-mode'.")

(define-derived-mode lsp-help-mode help-mode "LspHelp"
  "Major mode for displaying lsp help.")

(defun lsp-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (if (and contents (not (equal contents "")))
        (let ((lsp-help-buf-name "*lsp-help*"))
          (with-current-buffer (get-buffer-create lsp-help-buf-name)
            (delay-mode-hooks
              (lsp-help-mode)
              (with-help-window lsp-help-buf-name
                (insert (string-trim-right (lsp--render-on-hover-content contents t)))))
            (run-mode-hooks)))
      (lsp--info "No content at point."))))

(defun lsp--point-in-bounds-p (bounds)
  "Return whether the current point is within BOUNDS."
  (and (<= (car bounds) (point)) (< (point) (cdr bounds))))

(defun lsp-get-renderer (language)
  "Get renderer for LANGUAGE."
  (lambda (str)
    (lsp--render-string str language)))

(defun lsp--setup-markdown (mode)
  "Setup the ‘markdown-mode’ in the frame.
MODE is the mode used in the parent frame."
  (make-local-variable 'markdown-code-lang-modes)
  (dolist (mark (alist-get mode lsp-custom-markup-modes))
    (add-to-list 'markdown-code-lang-modes (cons mark mode)))
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-fontify-code-block-default-mode mode)
  (setq-local markdown-hide-markup t)

  ;; Render some common HTML entities.
  ;; This should really happen in markdown-mode instead,
  ;; but it doesn't, so we do it here for now.
  (setq prettify-symbols-alist
        (cl-loop for i from 0 to 255
                 collect (cons (format "&#x%02X;" i) i)))
  (push '("&lt;" . ?<) prettify-symbols-alist)
  (push '("&gt;" . ?>) prettify-symbols-alist)
  (push '("&amp;" . ?&) prettify-symbols-alist)
  (push '("&nbsp;" . ? ) prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate
        (lambda (_start _end _match) t))
  (prettify-symbols-mode 1))

(defvar lsp-help-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'lsp--help-open-link)
    (define-key map "\r" #'lsp--help-open-link)
    map)
  "Keymap active on links in *lsp-help* mode.")

(defun lsp--fix-markdown-links ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (prop))
    (save-restriction
      (goto-char (point-min))
      (while (setq prop (markdown-find-next-prop 'face))
        (let ((end (or (next-single-property-change (car prop) 'face)
                       (point-max))))
          (when (memq (get-text-property (car prop) 'face)
                      '(markdown-link-face
                        markdown-url-face
                        markdown-plain-url-face))
            (add-text-properties (car prop) end
                                 (list 'button t
                                       'category 'lsp-help-link
                                       'follow-link t
                                       'keymap lsp-help-link-keymap)))
          (goto-char end))))))

(defun lsp--buffer-string-visible ()
  "Return visible buffer string.
Stolen from `org-copy-visible'."
  (let ((temp (generate-new-buffer " *temp*"))
        (beg (point-min))
        (end (point-max)))
    (while (/= beg end)
      (when (get-char-property beg 'invisible)
        (setq beg (next-single-char-property-change beg 'invisible nil end)))
      (let* ((next (next-single-char-property-change beg 'invisible nil end))
             (substring (buffer-substring beg next)))
        (with-current-buffer temp (insert substring))
        ;; (setq result (concat result substring))
        (setq beg next)))
    (setq deactivate-mark t)
    (prog1 (with-current-buffer temp
             (s-chop-suffix "\n" (buffer-string)))
      (kill-buffer temp))))

(defvar lsp-buffer-major-mode nil
  "Holds the major mode when fontification function is running.
See #2588")

(defvar view-inhibit-help-message)

(defun lsp--render-markdown ()
  "Render markdown."

  (let ((markdown-enable-math nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx (and "\\" (group (or "\\" "`" "*" "_" ":" "/"
                                     "{" "}" "[" "]" "(" ")"
                                     "#" "+" "-" "." "!" "|"))))
            nil t)
      (replace-match (rx (backref 1))))

    ;; markdown-mode v2.3 does not yet provide gfm-view-mode
    (if (fboundp 'gfm-view-mode)
        (let ((view-inhibit-help-message t))
          (gfm-view-mode))
      (gfm-mode))

    (lsp--setup-markdown lsp-buffer-major-mode)))

(defvar lsp--display-inline-image-alist
  '((lsp--render-markdown
     (:regexp
      "!\\[.*?\\](data:image/[a-zA-Z]+;base64,\\([A-Za-z0-9+/\n]+?=*?\\)\\(|[^)]+\\)?)"
      :sexp
      (create-image
       (base64-decode-string
        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
       nil t))))
  "Replaced string regexp and function returning image.
Each element should have the form (MODE . (PROPERTY-LIST...)).
MODE (car) is function which is defined in `lsp-language-id-configuration'.
Cdr should be list of PROPERTY-LIST.

Each PROPERTY-LIST should have properties:
:regexp  Regexp which determines what string is relpaced to image.
         You should also get information of image, by parenthesis constructs.
         By default, all matched string is replaced to image, but you can
         change index of replaced string by keyword :replaced-index.

:sexp    Return image when evaluated. You can use information of regexp
         by using (match-beggining N), (match-end N) or (match-substring N).

In addition, each can have property:
:replaced-index  Determine index which is used to replace regexp to image.
                 The value means first argument of `match-beginning' and
                 `match-end'. If omitted, interpreted as index 0.")

(defcustom lsp-display-inline-image t
  "Showing inline image or not."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-suggest-server-download t
  "When non-nil enable server downloading suggestions."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-auto-register-remote-clients t
  "When non-nil register remote when registering the local one."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp--display-inline-image (mode)
  "Add image property if available."
  (let ((plist-list (cdr (assq mode lsp--display-inline-image-alist))))
    (when (and (display-images-p) lsp-display-inline-image)
      (cl-loop
       for plist in plist-list
       with regexp with replaced-index
       do
       (setq regexp (plist-get plist :regexp))
       (setq replaced-index (or (plist-get plist :replaced-index) 0))

       (font-lock-remove-keywords nil (list regexp replaced-index))
       (let ((inhibit-read-only t))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward regexp nil t)
             (set-text-properties
              (match-beginning replaced-index) (match-end replaced-index)
              nil)
             (add-text-properties
              (match-beginning replaced-index) (match-end replaced-index)
              `(display ,(eval (plist-get plist :sexp)))))))))))

(defun lsp--fontlock-with-mode (str mode)
  "Fontlock STR with MODE."
  (let ((lsp-buffer-major-mode major-mode))
    (with-temp-buffer
      (with-demoted-errors "Error during doc rendering: %s"
        (insert str)
        (delay-mode-hooks (funcall mode))
        (cl-flet ((window-body-width () lsp-window-body-width))
          ;; This can go wrong in some cases, and the fontification would
          ;; not work as expected.
          ;;
          ;; See #2984
          (ignore-errors (font-lock-ensure))
          (lsp--display-inline-image mode)
          (when (eq mode 'lsp--render-markdown)
            (lsp--fix-markdown-links))))
      (lsp--buffer-string-visible))))

(defun lsp--render-string (str language)
  "Render STR using `major-mode' corresponding to LANGUAGE.
When language is nil render as markup if `markdown-mode' is loaded."
  (setq str (s-replace "\r" "" (or str "")))
  (if-let* ((modes (-keep (-lambda ((mode . lang))
                            (when (and (equal lang language) (functionp mode))
                              mode))
                          lsp-language-id-configuration))
            (mode (car (or (member major-mode modes) modes))))
      (lsp--fontlock-with-mode str mode)
    str))

(defun lsp--render-element (content)
  "Render CONTENT element."
  (let ((inhibit-message t))
    (or
     (pcase content
       ((MarkedString :value :language)
        (lsp--render-string value language))
       ((MarkupContent :value :kind)
        (lsp--render-string value kind))
       ;; plain string
       ((pred stringp) (lsp--render-string content "markdown"))
       ((pred null) "")
       (_ (error "Failed to handle %s" content)))
     "")))

(defun lsp--create-unique-string-fn ()
  (let (elements)
    (lambda (element)
      (let ((count (cl-count element elements :test #'string=)))
        (prog1 (if (zerop count)
                   element
                 (format "%s (%s)" element count))
          (push element elements))))))

(defun lsp--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
   ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
    (lsp-seq-first actions))
   (t (let ((completion-ignore-case t))
        (lsp--completing-read "Select code action: "
                              (seq-into actions 'list)
                              (-compose (lsp--create-unique-string-fn)
                                        #'lsp:code-action-title)
                              nil t)))))

(defun lsp--workspace-server-id (workspace)
  "Return the server ID of WORKSPACE."
  (-> workspace lsp--workspace-client lsp--client-server-id))

(defun lsp--handle-rendered-for-echo-area (contents)
  "Return a single line from RENDERED, appropriate for display in the echo area."
  (pcase (lsp-workspaces)
    (`(,workspace)
     (lsp-clients-extract-signature-on-hover contents (lsp--workspace-server-id workspace)))
    ;; For projects with multiple active workspaces we also default to
    ;; render the first line.
    (_ (lsp-clients-extract-signature-on-hover contents nil))))

(cl-defmethod lsp-clients-extract-signature-on-hover (contents _server-id)
  "Extract a representative line from CONTENTS, to show in the echo area."
  (car (s-lines (s-trim (lsp--render-element contents)))))

(defun lsp--render-on-hover-content (contents render-all)
  "Render the content received from `document/onHover' request.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL - nil if only the signature should be rendered."
  (cond
   ((lsp-markup-content? contents)
    ;; MarkupContent.
    ;; It tends to be long and is not suitable to display fully in the echo area.
    ;; Just display the first line which is typically the signature.
    (if render-all
        (lsp--render-element contents)
      (lsp--handle-rendered-for-echo-area contents)))
   ((and (stringp contents) (not (string-match-p "\n" contents)))
    ;; If the contents is a single string containing a single line,
    ;; render it always.
    (lsp--render-element contents))
   (t
    ;; MarkedString -> MarkedString[]
    (when (or (lsp-marked-string? contents) (stringp contents))
      (setq contents (list contents)))
    ;; Consider the signature consisting of the elements who have a renderable
    ;; "language" property. When render-all is nil, ignore other elements.
    (string-join
     (seq-map
      #'lsp--render-element
      (if render-all
          contents
        ;; Only render contents that have an available renderer.
        (seq-take
         (seq-filter
          (-andfn #'lsp-marked-string?
                  (-compose #'lsp-get-renderer #'lsp:marked-string-language))
          contents)
         1)))
     (if (bound-and-true-p page-break-lines-mode)
         "\n\n"
       "\n")))))



(defvar lsp-signature-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "M-n") #'lsp-signature-next)
    (define-key (kbd "M-p") #'lsp-signature-previous)
    (define-key (kbd "M-a") #'lsp-signature-toggle-full-docs)
    (define-key (kbd "C-c C-k") #'lsp-signature-stop)
    (define-key (kbd "C-g") #'lsp-signature-stop))
  "Keymap for `lsp-signature-mode'.")

(define-minor-mode lsp-signature-mode
  "Mode used to show signature popup."
  :keymap lsp-signature-mode-map
  :lighter ""
  :group 'lsp-mode)

(defun lsp-signature-stop ()
  "Stop showing current signature help."
  (interactive)
  (lsp-cancel-request-by-token :signature)
  (remove-hook 'post-command-hook #'lsp-signature)
  (funcall lsp-signature-function nil)
  (lsp-signature-mode -1))

(declare-function page-break-lines--update-display-tables "ext:page-break-lines")

(defun lsp--setup-page-break-mode-if-present ()
  "Enable `page-break-lines-mode' in current buffer."
  (when (fboundp 'page-break-lines-mode)
    (page-break-lines-mode)
    ;; force page-break-lines-mode to update the display tables.
    (page-break-lines--update-display-tables)))

(defun lsp-lv-message (message)
  (add-hook 'lv-window-hook #'lsp--setup-page-break-mode-if-present)
  (if message
      (progn
        (setq lsp--signature-last-buffer (current-buffer))
        (let ((lv-force-update t))
          (lv-message "%s" message)))
    (lv-delete-window)
    (remove-hook 'lv-window-hook #'lsp--setup-page-break-mode-if-present)))

(declare-function posframe-show "ext:posframe")
(declare-function posframe-hide "ext:posframe")
(declare-function posframe-poshandler-point-bottom-left-corner-upward "ext:posframe")

(defface lsp-signature-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-signature-posframe'."
  :group 'lsp-mode)

(defvar lsp-signature-posframe-params
  (list :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
        :height 10
        :width 60
        :border-width 1
        :min-width 60)
  "Params for signature and `posframe-show'.")

(defun lsp-signature-posframe (str)
  "Use posframe to show the STR signatureHelp string."
  (if str
      (apply #'posframe-show
             (with-current-buffer (get-buffer-create " *lsp-signature*")
               (erase-buffer)
               (insert str)
               (visual-line-mode 1)
               (lsp--setup-page-break-mode-if-present)
               (current-buffer))
             (append
              lsp-signature-posframe-params
              (list :position (point)
                    :background-color (face-attribute 'lsp-signature-posframe :background nil t)
                    :foreground-color (face-attribute 'lsp-signature-posframe :foreground nil t)
                    :border-color (face-attribute 'font-lock-comment-face :foreground nil t))))
    (posframe-hide " *lsp-signature*")))

(defun lsp--handle-signature-update (signature)
  (let ((message
         (if (lsp-signature-help? signature)
             (lsp--signature->message signature)
           (mapconcat #'lsp--signature->message signature "\n"))))
    (if (s-present? message)
        (funcall lsp-signature-function message)
      (lsp-signature-stop))))

(defun lsp-signature-activate ()
  "Activate signature help.
It will show up only if current point has signature help."
  (interactive)
  (setq lsp--signature-last nil
        lsp--signature-last-index nil
        lsp--signature-last-buffer (current-buffer))
  (add-hook 'post-command-hook #'lsp-signature)
  (lsp-signature-mode t))

(defcustom lsp-signature-cycle t
  "Whether `lsp-signature-next' and prev should cycle."
  :type 'boolean
  :group 'lsp-mode)

(defun lsp-signature-next ()
  "Show next signature."
  (interactive)
  (let ((nsigs (length (lsp:signature-help-signatures lsp--signature-last))))
    (when (and lsp--signature-last-index
               lsp--signature-last
               (or lsp-signature-cycle (< (1+ lsp--signature-last-index) nsigs)))
      (setq lsp--signature-last-index (% (1+ lsp--signature-last-index) nsigs))
      (funcall lsp-signature-function (lsp--signature->message lsp--signature-last)))))

(defun lsp-signature-previous ()
  "Next signature."
  (interactive)
  (when (and lsp--signature-last-index
             lsp--signature-last
             (or lsp-signature-cycle (not (zerop lsp--signature-last-index))))
    (setq lsp--signature-last-index (1- (if (zerop lsp--signature-last-index)
                                            (length (lsp:signature-help-signatures lsp--signature-last))
                                          lsp--signature-last-index)))
    (funcall lsp-signature-function (lsp--signature->message lsp--signature-last))))

(defun lsp-signature-toggle-full-docs ()
  "Toggle full/partial signature documentation."
  (interactive)
  (let ((all? (not (numberp lsp-signature-doc-lines))))
    (setq lsp-signature-doc-lines (if all?
                                      (or (car-safe lsp-signature-doc-lines)
                                          20)
                                    (list lsp-signature-doc-lines))))
  (lsp-signature-activate))

(defun lsp--signature->message (signature-help)
  "Generate eldoc message from SIGNATURE-HELP response."
  (setq lsp--signature-last signature-help)

  (when (and signature-help (not (seq-empty-p (lsp:signature-help-signatures signature-help))))
    (-let* (((&SignatureHelp :active-signature?
                             :active-parameter?
                             :signatures) signature-help)
            (active-signature? (or lsp--signature-last-index active-signature? 0))
            (_ (setq lsp--signature-last-index active-signature?))
            ((signature &as &SignatureInformation? :label :parameters?) (seq-elt signatures active-signature?))
            (prefix (if (= (length signatures) 1)
                        ""
                      (concat (propertize (format " %s/%s"
                                                  (1+ active-signature?)
                                                  (length signatures))
                                          'face 'success)
                              " ")))
            (method-docs (when
                             (and lsp-signature-render-documentation
                                  (or (not (numberp lsp-signature-doc-lines)) (< 0 lsp-signature-doc-lines)))
                           (let ((docs (lsp--render-element
                                        (lsp:parameter-information-documentation? signature))))
                             (when (s-present? docs)
                               (concat
                                "\n"
                                (if (fboundp 'page-break-lines-mode)
                                    "\n"
                                  "")
                                (if (and (numberp lsp-signature-doc-lines)
                                         (> (length (s-lines docs)) lsp-signature-doc-lines))
                                    (concat (s-join "\n" (-take lsp-signature-doc-lines (s-lines docs)))
                                            (propertize "\nTruncated..." 'face 'highlight))
                                  docs)))))))
      (when (and active-parameter? (not (seq-empty-p parameters?)))
        (-when-let* ((param (when (and (< -1 active-parameter? (length parameters?)))
                              (seq-elt parameters? active-parameter?)))
                     (selected-param-label (let ((label (lsp:parameter-information-label param)))
                                             (if (stringp label) label (append label nil))))
                     (start (if (stringp selected-param-label)
                                (s-index-of selected-param-label label)
                              (cl-first selected-param-label)))
                     (end (if (stringp selected-param-label)
                              (+ start (length selected-param-label))
                            (cl-second selected-param-label))))
          (add-face-text-property start end 'eldoc-highlight-function-argument nil label)))
      (concat prefix label method-docs))))

(defun lsp-signature ()
  "Display signature info (based on `textDocument/signatureHelp')"
  (if (and lsp--signature-last-buffer
           (not (equal (current-buffer) lsp--signature-last-buffer)))
      (lsp-signature-stop)
    (lsp-request-async "textDocument/signatureHelp"
                       (lsp--text-document-position-params)
                       #'lsp--handle-signature-update
                       :cancel-token :signature)))


(defcustom lsp-overlay-document-color-char "■"
  "Display the char represent the document color in overlay"
  :type 'string
  :group 'lsp-mode)

;; color presentation
(defun lsp--color-create-interactive-command (color range)
  (lambda ()
    (interactive)
    (-let [(&ColorPresentation? :text-edit?
                                :additional-text-edits?)
           (lsp--completing-read
            "Select color presentation: "
            (lsp-request
             "textDocument/colorPresentation"
             `( :textDocument ,(lsp--text-document-identifier)
                :color ,color
                :range ,range))
            #'lsp:color-presentation-label
            nil
            t)]
      (when text-edit?
        (lsp--apply-text-edit text-edit?))
      (when additional-text-edits?
        (lsp--apply-text-edits additional-text-edits? 'color-presentation)))))

(defun lsp--number->color (number)
  (let ((result (format "%x"
                        (round (* (or number 0) 255.0)))))
    (if (= 1 (length result))
        (concat "0" result)
      result)))

(defun lsp--document-color ()
  "Document color handler."
  (when (lsp-feature? "textDocument/documentColor")
    (lsp-request-async
     "textDocument/documentColor"
     `(:textDocument ,(lsp--text-document-identifier))
     (lambda (result)
       (lsp--remove-overlays 'lsp-color)
       (seq-do
        (-lambda ((&ColorInformation :color (color &as &Color :red :green :blue)
                                     :range))
          (-let* (((beg . end) (lsp--range-to-region range))
                  (overlay (make-overlay beg end))
                  (command (lsp--color-create-interactive-command color range)))
            (overlay-put overlay 'lsp-color t)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay
                         'before-string
                         (propertize
                          lsp-overlay-document-color-char
                          'face `((:foreground ,(format
                                                 "#%s%s%s"
                                                 (lsp--number->color red)
                                                 (lsp--number->color green)
                                                 (lsp--number->color blue))))
                          'action command
                          'mouse-face 'lsp-lens-mouse-face
                          'local-map (-doto (make-sparse-keymap)
                                       (define-key [mouse-1] command))))))
        result))
     :mode 'unchanged
     :cancel-token :document-color-token)))



(defun lsp--action-trigger-parameter-hints (_command)
  "Handler for editor.action.triggerParameterHints."
  (when (member :on-server-request lsp-signature-auto-activate)
    (lsp-signature-activate)))

(defun lsp--action-trigger-suggest (_command)
  "Handler for editor.action.triggerSuggest."
  (cond
   ((and (bound-and-true-p company-mode)
         (fboundp 'company-auto-begin)
         (fboundp 'company-post-command))
    (run-at-time 0 nil
                 (lambda ()
                   (let ((this-command 'company-idle-begin)
                         (company-minimum-prefix-length 0))
                     (company-auto-begin)
                     (company-post-command)))))
   (t
    (completion-at-point))))

(defconst lsp--default-action-handlers
  (ht ("editor.action.triggerParameterHints" #'lsp--action-trigger-parameter-hints)
      ("editor.action.triggerSuggest" #'lsp--action-trigger-suggest))
  "Default action handlers.")

(defun lsp--find-action-handler (command)
  "Find action handler for particular COMMAND."
  (or
   (--some (-some->> it
             (lsp--workspace-client)
             (lsp--client-action-handlers)
             (gethash command))
           (lsp-workspaces))
   (gethash command lsp--default-action-handlers)))

(defun lsp--text-document-code-action-params (&optional kind)
  "Code action params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))
        :context `( :diagnostics ,(lsp-cur-possition-diagnostics)
                    ,@(when kind (list :only (vector kind))))))

(defun lsp-code-actions-at-point (&optional kind)
  "Retrieve the code actions for the active region or the current line.
It will filter by KIND if non nil."
  (lsp-request "textDocument/codeAction" (lsp--text-document-code-action-params kind)))

(defun lsp-execute-code-action-by-kind (command-kind)
  "Execute code action by COMMAND-KIND."
  (if-let ((action (->> (lsp-get-or-calculate-code-actions command-kind)
                        (-filter (-lambda ((&CodeAction :kind?))
                                   (and kind? (s-prefix? command-kind kind?))))
                        lsp--select-action)))
      (lsp-execute-code-action action)
    (signal 'lsp-no-code-actions '(command-kind))))

(defalias 'lsp-get-or-calculate-code-actions 'lsp-code-actions-at-point)

(lsp-defun lsp--execute-command ((action &as &Command :command :arguments?))
  "Parse and execute a code ACTION represented as a Command LSP type."
  (let ((server-id (->> (lsp-workspaces)
                        (cl-first)
                        (or lsp--cur-workspace)
                        (lsp--workspace-client)
                        (lsp--client-server-id))))
    (condition-case nil
        (with-no-warnings
          (lsp-execute-command server-id (intern command) arguments?))
      (cl-no-applicable-method
       (if-let ((action-handler (lsp--find-action-handler command)))
           (funcall action-handler action)
         (lsp-send-execute-command command arguments?))))))

(lsp-defun lsp-execute-code-action ((action &as &CodeAction :command? :edit?))
  "Execute code action ACTION. For example, when text under the
caret has a suggestion to apply a fix from an lsp-server, calling
this function will do so.
If ACTION is not set it will be selected from `lsp-code-actions-at-point'.
Request codeAction/resolve for more info if server supports."
  (interactive (list (lsp--select-action (lsp-code-actions-at-point))))
  (if (and (lsp-feature? "codeAction/resolve")
           (not command?)
           (not edit?))
      (lsp--execute-code-action (lsp-request "codeAction/resolve" action))
    (lsp--execute-code-action action)))

(lsp-defun lsp--execute-code-action ((action &as &CodeAction :command? :edit?))
  "Execute code action ACTION."
  (when edit?
    (lsp--apply-workspace-edit edit? 'code-action))

  (cond
   ((stringp command?) (lsp--execute-command action))
   ((lsp-command? command?) (lsp--execute-command command?))))

(defvar lsp--formatting-indent-alist
  ;; Taken from `dtrt-indent-mode'
  '(
    (ada-mode                   . ada-indent)                       ; Ada
    (ada-ts-mode                . ada-ts-mode-indent-offset)
    (c++-mode                   . c-basic-offset)                   ; C++
    (c++-ts-mode                . c-ts-mode-indent-offset)
    (c-mode                     . c-basic-offset)                   ; C
    (c-ts-mode                  . c-ts-mode-indent-offset)
    (cperl-mode                 . cperl-indent-level)               ; Perl
    (crystal-mode               . crystal-indent-level)             ; Crystal (Ruby)
    (csharp-mode                . c-basic-offset)                   ; C#
    (csharp-tree-sitter-mode    . csharp-tree-sitter-indent-offset) ; C#
    (csharp-ts-mode             . csharp-ts-mode-indent-offset)     ; C# (tree-sitter, Emacs29)
    (css-mode                   . css-indent-offset)                ; CSS
    (d-mode                     . c-basic-offset)                   ; D
    (enh-ruby-mode              . enh-ruby-indent-level)            ; Ruby
    (erlang-mode                . erlang-indent-level)              ; Erlang
    (ess-mode                   . ess-indent-offset)                ; ESS (R)
    (go-ts-mode                 . go-ts-mode-indent-offset)
    (hack-mode                  . hack-indent-offset)               ; Hack
    (java-mode                  . c-basic-offset)                   ; Java
    (java-ts-mode               . java-ts-mode-indent-offset)
    (jde-mode                   . c-basic-offset)                   ; Java (JDE)
    (js-mode                    . js-indent-level)                  ; JavaScript
    (js-ts-mode                 . js-indent-level)
    (js2-mode                   . js2-basic-offset)                 ; JavaScript-IDE
    (js3-mode                   . js3-indent-level)                 ; JavaScript-IDE
    (json-mode                  . js-indent-level)                  ; JSON
    (json-ts-mode               . json-ts-mode-indent-offset)
    (lua-mode                   . lua-indent-level)                 ; Lua
    (lua-ts-mode                . lua-ts-indent-offset)
    (nxml-mode                  . nxml-child-indent)                ; XML
    (objc-mode                  . c-basic-offset)                   ; Objective C
    (pascal-mode                . pascal-indent-level)              ; Pascal
    (perl-mode                  . perl-indent-level)                ; Perl
    (php-mode                   . c-basic-offset)                   ; PHP
    (php-ts-mode                . php-ts-mode-indent-offset)        ; PHP
    (powershell-mode            . powershell-indent)                ; PowerShell
    (powershell-ts-mode         . powershell-ts-mode-indent-offset) ; PowerShell
    (raku-mode                  . raku-indent-offset)               ; Perl6/Raku
    (ruby-mode                  . ruby-indent-level)                ; Ruby
    (rust-mode                  . rust-indent-offset)               ; Rust
    (rust-ts-mode               . rust-ts-mode-indent-offset)
    (rustic-mode                . rustic-indent-offset)             ; Rust
    (scala-mode                 . scala-indent:step)                ; Scala
    (sgml-mode                  . sgml-basic-offset)                ; SGML
    (sh-mode                    . sh-basic-offset)                  ; Shell Script
    (toml-ts-mode               . toml-ts-mode-indent-offset)
    (typescript-mode            . typescript-indent-level)          ; Typescript
    (typescript-ts-mode         . typescript-ts-mode-indent-offset) ; Typescript (tree-sitter, Emacs29)
    (yaml-mode                  . yaml-indent-offset)               ; YAML
    (yang-mode                  . c-basic-offset)                   ; YANG (yang-mode)

    (default                    . standard-indent))                 ; default fallback
  "A mapping from `major-mode' to its indent variable.")

(defun lsp--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp--formatting-indent-alist)
      (lsp--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(defun lsp--make-document-formatting-params ()
  "Create document formatting params."
  (lsp-make-document-formatting-params
   :text-document (lsp--text-document-identifier)
   :options (lsp-make-formatting-options
             :tab-size (symbol-value (lsp--get-indent-width major-mode))
             :insert-spaces (lsp-json-bool (not indent-tabs-mode))
             :trim-trailing-whitespace? (lsp-json-bool lsp-trim-trailing-whitespace)
             :insert-final-newline? (lsp-json-bool lsp-insert-final-newline)
             :trim-final-newlines? (lsp-json-bool lsp-trim-final-newlines))))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (cond ((lsp-feature? "textDocument/formatting")
         (let ((edits (lsp-request "textDocument/formatting"
                                   (lsp--make-document-formatting-params))))
           (if (seq-empty-p edits)
               (lsp--info "No formatting changes provided")
             (lsp--apply-text-edits edits 'format))))
        ((lsp-feature? "textDocument/rangeFormatting")
         (save-restriction
           (widen)
           (lsp-format-region (point-min) (point-max))))
        (t (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))))

(defun lsp-format-region (s e)
  "Ask the server to format the region, or if none is selected, the current line."
  (interactive "r")
  (let ((edits (lsp-request
                "textDocument/rangeFormatting"
                (lsp--make-document-range-formatting-params s e))))
    (if (seq-empty-p edits)
        (lsp--info "No formatting changes provided")
      (lsp--apply-text-edits edits 'format))))

(defmacro lsp-make-interactive-code-action (func-name code-action-kind)
  "Define an interactive function FUNC-NAME that attempts to
execute a CODE-ACTION-KIND action."
  `(defun ,(intern (concat "lsp-" (symbol-name func-name))) ()
     ,(format "Perform the %s code action, if available." code-action-kind)
     (interactive)
     ;; Even when `lsp-auto-execute-action' is nil, it still makes sense to
     ;; auto-execute here: the user has specified exactly what they want.
     (let ((lsp-auto-execute-action t))
       (condition-case nil
           (lsp-execute-code-action-by-kind ,code-action-kind)
         (lsp-no-code-actions
          (when (called-interactively-p 'any)
            (lsp--info ,(format "%s action not available" code-action-kind))))))))

(lsp-make-interactive-code-action organize-imports "source.organizeImports")

(defun lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region."
  (lsp:set-document-range-formatting-params-range (lsp--make-document-formatting-params)
                                                  (lsp--region-to-range start end)))

(defconst lsp--highlight-kind-face
  '((1 . lsp-face-highlight-textual)
    (2 . lsp-face-highlight-read)
    (3 . lsp-face-highlight-write)))

(defun lsp--remove-overlays (name)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) name t)))

(defun lsp-document-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (lsp--remove-overlays 'lsp-highlight) ;; clear any previous highlights
  (setq lsp--have-document-highlights nil
        lsp--symbol-bounds-of-last-highlight-invocation nil)
  (let ((lsp-enable-symbol-highlighting t))
    (lsp--document-highlight)))

(defun lsp--document-highlight-callback (highlights)
  "Create a callback to process the reply of a
`textDocument/documentHighlight' message for the buffer BUF.
A reference is highlighted only if it is visible in a window."
  (lsp--remove-overlays 'lsp-highlight)

  (let* ((wins-visible-pos (-map (lambda (win)
                                   (cons (1- (line-number-at-pos (window-start win) t))
                                         (1+ (line-number-at-pos (window-end win) t))))
                                 (get-buffer-window-list nil nil 'visible))))
    (setq lsp--have-document-highlights t)
    (-map
     (-lambda ((&DocumentHighlight :range (&Range :start (start &as &Position :line start-line)
                                                  :end (end &as &Position :line end-line))
                                   :kind?))
       (-map
        (-lambda ((start-window . end-window))
          ;; Make the overlay only if the reference is visible
          (let ((start-point (lsp--position-to-point start))
                (end-point (lsp--position-to-point end)))
            (when (and (> (1+ start-line) start-window)
                       (< (1+ end-line) end-window)
                       (not (and lsp-symbol-highlighting-skip-current
                                 (<= start-point (point) end-point))))
              (-doto (make-overlay start-point end-point)
                (overlay-put 'face (cdr (assq (or kind? 1) lsp--highlight-kind-face)))
                (overlay-put 'lsp-highlight t)))))
        wins-visible-pos))
     highlights)))

(defcustom lsp-symbol-kinds
  '((1 . "File")
    (2 . "Module")
    (3 . "Namespace")
    (4 . "Package")
    (5 . "Class")
    (6 . "Method")
    (7 . "Property")
    (8 . "Field")
    (9 . "Constructor")
    (10 . "Enum")
    (11 . "Interface")
    (12 . "Function")
    (13 . "Variable")
    (14 . "Constant")
    (15 . "String")
    (16 . "Number")
    (17 . "Boolean")
    (18 . "Array")
    (19 . "Object")
    (20 . "Key")
    (21 . "Null")
    (22 . "Enum Member")
    (23 . "Struct")
    (24 . "Event")
    (25 . "Operator")
    (26 . "Type Parameter"))
  "Alist mapping SymbolKinds to human-readable strings.
Various Symbol objects in the LSP protocol have an integral type,
specifying what they are. This alist maps such type integrals to
readable representations of them. See
`https://microsoft.github.io/language-server-protocol/specifications/specification-current/',
namespace SymbolKind."
  :group 'lsp-mode
  :type '(alist :key-type integer :value-type string))
(defalias 'lsp--symbol-kind 'lsp-symbol-kinds)

(lsp-defun lsp--symbol-information-to-xref
  ((&SymbolInformation :kind :name
                       :location (&Location :uri :range (&Range :start
                                                                (&Position :line :character)))))
  "Return a `xref-item' from SYMBOL information."
  (xref-make (format "[%s] %s" (alist-get kind lsp-symbol-kinds) name)
             (xref-make-file-location (lsp--uri-to-path uri)
                                      line
                                      character)))

(defun lsp--get-document-symbols ()
  "Get document symbols.

If the buffer has not been modified since symbols were last
retrieved, simply return the latest result.

Else, if the request was initiated by Imenu updating its menu-bar
entry, perform it asynchronously; i.e., give Imenu the latest
result and then force a refresh when a new one is available.

Else (e.g., due to interactive use of `imenu' or `xref'),
perform the request synchronously."
  (if (= (buffer-chars-modified-tick) lsp--document-symbols-tick)
      lsp--document-symbols
    (let ((method "textDocument/documentSymbol")
          (params `(:textDocument ,(lsp--text-document-identifier)))
          (tick (buffer-chars-modified-tick)))
      (if (not lsp--document-symbols-request-async)
          (prog1
              (setq lsp--document-symbols (lsp-request method params))
            (setq lsp--document-symbols-tick tick))
        (lsp-request-async method params
                           (lambda (document-symbols)
                             (setq lsp--document-symbols document-symbols
                                   lsp--document-symbols-tick tick)
                             (lsp--imenu-refresh))
                           :mode 'alive
                           :cancel-token :document-symbols)
        lsp--document-symbols))))

(advice-add 'imenu-update-menubar :around
            (lambda (oldfun &rest r)
              (let ((lsp--document-symbols-request-async t))
                (apply oldfun r))))

(defun lsp--document-symbols->document-symbols-hierarchy (document-symbols current-position)
  "Convert DOCUMENT-SYMBOLS to symbols hierarchy on CURRENT-POSITION."
  (-let (((symbol &as &DocumentSymbol? :children?)
          (seq-find (-lambda ((&DocumentSymbol :range))
                      (lsp-point-in-range? current-position range))
                    document-symbols)))
    (if children?
        (cons symbol (lsp--document-symbols->document-symbols-hierarchy children? current-position))
      (when symbol
        (list symbol)))))

(lsp-defun lsp--symbol-information->document-symbol ((&SymbolInformation :name :kind :location :container-name? :deprecated?))
  "Convert a SymbolInformation to a DocumentInformation"
  (lsp-make-document-symbol :name name
                            :kind kind
                            :range (lsp:location-range location)
                            :children? nil
                            :deprecated? deprecated?
                            :selection-range (lsp:location-range location)
                            :detail? container-name?))

(defun lsp--symbols-informations->document-symbols-hierarchy (symbols-informations current-position)
  "Convert SYMBOLS-INFORMATIONS to symbols hierarchy on CURRENT-POSITION."
  (--> symbols-informations
    (-keep (-lambda ((symbol &as &SymbolInformation :location (&Location :range)))
             (when (lsp-point-in-range? current-position range)
               (lsp--symbol-information->document-symbol symbol)))
           it)
    (sort it (-lambda ((&DocumentSymbol :range (&Range :start a-start-position :end a-end-position))
                       (&DocumentSymbol :range (&Range :start b-start-position :end b-end-position)))
               (and (lsp--position-compare b-start-position a-start-position)
                    (lsp--position-compare a-end-position b-end-position))))))

(defun lsp--symbols->document-symbols-hierarchy (symbols)
  "Convert SYMBOLS to symbols-hierarchy."
  (when-let ((first-symbol (lsp-seq-first symbols)))
    (let ((cur-position (lsp-make-position :line (plist-get (lsp--cur-position) :line)
                                           :character (plist-get (lsp--cur-position) :character))))
      (if (lsp-symbol-information? first-symbol)
          (lsp--symbols-informations->document-symbols-hierarchy symbols cur-position)
        (lsp--document-symbols->document-symbols-hierarchy symbols cur-position)))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(defun lsp--xref-elements-index (symbols path)
  (-mapcat
   (-lambda (sym)
     (pcase-exhaustive sym
       ((DocumentSymbol :name :children? :selection-range (Range :start))
        (cons (cons (concat path name)
                    (lsp--position-to-point start))
              (lsp--xref-elements-index children? (concat path name " / "))))
       ((SymbolInformation :name :location (Location :range (Range :start)))
        (list (cons (concat path name)
                    (lsp--position-to-point start))))))
   symbols))

(defvar-local lsp--symbols-cache nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (if (lsp--find-workspaces-for "textDocument/documentSymbol")
      (progn
        (setq lsp--symbols-cache (lsp--xref-elements-index
                                  (lsp--get-document-symbols) nil))
        lsp--symbols-cache)
    (list (propertize (or (thing-at-point 'symbol) "")
                      'identifier-at-point t))))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (save-excursion
    (unless (get-text-property 0 'identifier-at-point identifier)
      (goto-char (cl-rest (or (assoc identifier lsp--symbols-cache)
                              (user-error "Unable to find symbol %s in current document" identifier)))))
    (lsp--locations-to-xref-items (lsp-request "textDocument/definition"
                                               (lsp--text-document-position-params)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (save-excursion
    (unless (get-text-property 0 'identifier-at-point identifier)
      (goto-char (cl-rest (or (assoc identifier lsp--symbols-cache)
                              (user-error "Unable to find symbol %s" identifier)))))
    (lsp--locations-to-xref-items (lsp-request "textDocument/references"
                                               (lsp--make-reference-params nil lsp-references-exclude-definition)))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (seq-map #'lsp--symbol-information-to-xref
           (lsp-request "workspace/symbol" `(:query ,pattern))))

(defcustom lsp-rename-use-prepare t
  "Whether `lsp-rename' should do a prepareRename first.
For some language servers, textDocument/prepareRename might be
too slow, in which case this variable may be set to nil.
`lsp-rename' will then use `thing-at-point' `symbol' to determine
the symbol to rename at point."
  :group 'lsp-mode
  :type 'boolean)

(defun lsp--get-symbol-to-rename ()
  "Get a symbol to rename and placeholder at point.
Returns a cons ((START . END) . PLACEHOLDER?), and nil if
renaming is generally supported but cannot be done at point.
START and END are the bounds of the identifiers being renamed,
while PLACEHOLDER?, is either nil or a string suggested by the
language server as the initial input of a new-name prompt."
  (unless (lsp-feature? "textDocument/rename")
    (error "The connected server(s) doesn't support renaming"))
  (if (and lsp-rename-use-prepare (lsp-feature? "textDocument/prepareRename"))
      (when-let ((response
                  (lsp-request "textDocument/prepareRename"
                               (lsp--text-document-position-params))))
        (let* ((bounds (lsp--range-to-region
                        (if (lsp-range? response)
                            response
                          (lsp:prepare-rename-result-range response))))
               (placeholder
                (and (not (lsp-range? response))
                     (lsp:prepare-rename-result-placeholder response))))
          (cons bounds placeholder)))
    (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
      (cons bounds nil))))

(defface lsp-face-rename '((t :underline t))
  "Face used to highlight the identifier being renamed.
Renaming can be done using `lsp-rename'."
  :group 'lsp-mode)

(defface lsp-rename-placeholder-face '((t :inherit font-lock-variable-name-face))
  "Face used to display the rename placeholder in.
When calling `lsp-rename' interactively, this will be the face of
the new name."
  :group 'lsp-mode)

(defvar lsp-rename-history '()
  "History for `lsp--read-rename'.")

(defun lsp--read-rename (at-point)
  "Read a new name for a `lsp-rename' at `point' from the user.
AT-POINT shall be a structure as returned by
`lsp--get-symbol-to-rename'.

Returns a string, which should be the new name for the identifier
at point. If renaming cannot be done at point (as determined from
AT-POINT), throw a `user-error'.

This function is for use in `lsp-rename' only, and shall not be
relied upon."
  (unless at-point
    (user-error "`lsp-rename' is invalid here"))
  (-let* ((((start . end) . placeholder?) at-point)
          ;; Do the `buffer-substring' first to not include `lsp-face-rename'
          (rename-me (buffer-substring start end))
          (placeholder (or placeholder? rename-me))
          (placeholder (propertize placeholder 'face 'lsp-rename-placeholder-face))

          overlay)
    ;; We need unwind protect, as the user might cancel here, causing the
    ;; overlay to linger.
    (unwind-protect
        (progn
          (setq overlay (make-overlay start end))
          (overlay-put overlay 'face 'lsp-face-rename)

          (read-string (format "Rename %s to: " rename-me) placeholder
                       'lsp-rename-history))
      (and overlay (delete-overlay overlay)))))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (lsp--read-rename (lsp--get-symbol-to-rename))))
  (when-let ((edits (lsp-request "textDocument/rename"
                                 `( :textDocument ,(lsp--text-document-identifier)
                                    :position ,(lsp--cur-position)
                                    :newName ,newname))))
    (lsp--apply-workspace-edit edits 'rename)))

(defun lsp--on-rename-file (old-func old-name new-name &optional ok-if-already-exists?)
  "Advice around function `rename-file'.
Applies OLD-FUNC with OLD-NAME, NEW-NAME and OK-IF-ALREADY-EXISTS?.

This advice sends workspace/willRenameFiles before renaming file
to check if server wants to apply any workspaceEdits after renamed."
  (if (and lsp-apply-edits-after-file-operations
           (lsp--send-will-rename-files-p old-name))
      (let ((params (lsp-make-rename-files-params
                     :files (vector (lsp-make-file-rename
                                     :oldUri (lsp--path-to-uri old-name)
                                     :newUri (lsp--path-to-uri new-name))))))
        (when-let ((edits (lsp-request "workspace/willRenameFiles" params)))
          (lsp--apply-workspace-edit edits 'rename-file)
          (funcall old-func old-name new-name ok-if-already-exists?)
          (when (lsp--send-did-rename-files-p)
            (lsp-notify "workspace/didRenameFiles" params))))
    (funcall old-func old-name new-name ok-if-already-exists?)))

(advice-add 'rename-file :around #'lsp--on-rename-file)

(defcustom lsp-xref-force-references nil
  "If non-nil threat everything as references(e. g. jump if only one item.)"
  :group 'lsp-mode
  :type 'boolean)

(defun lsp-show-xrefs (xrefs display-action references?)
  (unless (region-active-p) (push-mark nil t))
  (if (boundp 'xref-show-definitions-function)
      (with-no-warnings
        (xref-push-marker-stack)
        (funcall (if (and references? (not lsp-xref-force-references))
                     xref-show-xrefs-function
                   xref-show-definitions-function)
                 (-const xrefs)
                 `((window . ,(selected-window))
                   (display-action . ,display-action)
                   ,(if (and references? (not lsp-xref-force-references))
                        `(auto-jump . ,xref-auto-jump-to-first-xref)
                      `(auto-jump . ,xref-auto-jump-to-first-definition)))))
    (xref--show-xrefs xrefs display-action)))

(cl-defmethod seq-empty-p ((ht hash-table))
  "Function `seq-empty-p' for hash-table."
  (hash-table-empty-p ht))

(cl-defun lsp-find-locations (method &optional extra &key display-action references?)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters.
REFERENCES? t when METHOD returns references."
  (let ((loc (lsp-request method
                          (append (lsp--text-document-position-params) extra))))
    (if (seq-empty-p loc)
        (lsp--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
      (lsp-show-xrefs (lsp--locations-to-xref-items loc) display-action references?))))

(cl-defun lsp-find-declaration (&key display-action)
  "Find declarations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/declaration" nil :display-action display-action))

(cl-defun lsp-find-definition (&key display-action)
  "Find definitions of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/definition" nil :display-action display-action))

(defun lsp-find-definition-mouse (click)
  "Click to start `lsp-find-definition' at clicked point."
  (interactive "e")
  (let* ((ec (event-start click))
         (p1 (posn-point ec))
         (w1 (posn-window ec)))
    (select-window w1)
    (goto-char p1)
    (lsp-find-definition)))

(cl-defun lsp-find-implementation (&key display-action)
  "Find implementations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/implementation"
                      nil
                      :display-action display-action
                      :references? t))

(cl-defun lsp-find-references (&optional exclude-declaration &key display-action)
  "Find references of the symbol under point."
  (interactive "P")
  (lsp-find-locations "textDocument/references"
                      (list :context `(:includeDeclaration ,(lsp-json-bool (not (or exclude-declaration lsp-references-exclude-definition)))))
                      :display-action display-action
                      :references? t))

(cl-defun lsp-find-type-definition (&key display-action)
  "Find type definitions of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/typeDefinition" nil :display-action display-action))

(defalias 'lsp-find-custom #'lsp-find-locations)
(defalias 'lsp-goto-implementation #'lsp-find-implementation)
(defalias 'lsp-goto-type-definition #'lsp-find-type-definition)

(with-eval-after-load 'evil
  (evil-set-command-property 'lsp-find-definition :jump t)
  (evil-set-command-property 'lsp-find-implementation :jump t)
  (evil-set-command-property 'lsp-find-references :jump t)
  (evil-set-command-property 'lsp-find-type-definition :jump t))

(defun lsp--workspace-method-supported? (check-command method capability workspace)
  (with-lsp-workspace workspace
    (if check-command
        (funcall check-command workspace)
      (or
       (when capability (lsp--capability capability))
       (lsp--registered-capability method)
       (and (not capability) (not check-command))))))

(defun lsp-disable-method-for-server (method server-id)
  "Disable METHOD for SERVER-ID."
  (cl-callf
      (lambda (reqs)
        (-let (((&plist :check-command :capability) reqs))
          (list :check-command
                (lambda (workspace)
                  (unless (-> workspace
                              lsp--workspace-client
                              lsp--client-server-id
                              (eq server-id))
                    (lsp--workspace-method-supported? check-command
                                                      method
                                                      capability
                                                      workspace))))))
      (alist-get method lsp-method-requirements nil nil 'string=)))

(defun lsp--find-workspaces-for (msg-or-method)
  "Find all workspaces in the current project that can handle MSG."
  (let ((method (if (stringp msg-or-method)
                    msg-or-method
                  (plist-get msg-or-method :method))))
    (-if-let (reqs (cdr (assoc method lsp-method-requirements)))
        (-let (((&plist :capability :check-command) reqs))
          (-filter
           (-partial #'lsp--workspace-method-supported?
                     check-command method capability)
           (lsp-workspaces)))
      (lsp-workspaces))))

(defun lsp-can-execute-command? (command-name)
  "Returns non-nil if current language server(s) can execute COMMAND-NAME.
The command is executed via `workspace/executeCommand'"
  (cl-position
   command-name
   (lsp:execute-command-options-commands
    (lsp:server-capabilities-execute-command-provider?
     (lsp--server-capabilities)))
   :test #'equal))

(defalias 'lsp-feature? 'lsp--find-workspaces-for)

(cl-defmethod lsp-execute-command (_server _command _arguments)
  "Dispatch COMMAND execution."
  (signal 'cl-no-applicable-method nil))

(defun lsp-workspace-command-execute (command &optional args)
  "Execute workspace COMMAND with ARGS."
  (condition-case-unless-debug err
      (let ((params (if args
                        (list :command command :arguments args)
                      (list :command command))))
        (lsp-request "workspace/executeCommand" params))
    (error
     (error "`workspace/executeCommand' with `%s' failed.\n\n%S"
            command err))))

(defun lsp-send-execute-command (command &optional args)
  "Create and send a `workspace/executeCommand' message having command COMMAND
and optional ARGS."
  (lsp-workspace-command-execute command args))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp--send-execute-command #'lsp-send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)

(defun lsp--set-configuration (settings)
  "Set the SETTINGS for the lsp server."
  (lsp-notify "workspace/didChangeConfiguration" `(:settings ,settings)))

(defun lsp-current-buffer ()
  (or lsp--virtual-buffer
      (current-buffer)))

(defun lsp-buffer-live-p (buffer-id)
  (if-let ((buffer-live (plist-get buffer-id :buffer-live?)))
      (funcall buffer-live buffer-id)
    (buffer-live-p buffer-id)))

(defun lsp--on-set-visited-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp--cur-workspace
    (lsp--text-document-did-close t))
  (prog1 (apply old-func args)
    (when lsp--cur-workspace
      (lsp--text-document-did-open))))

(advice-add 'set-visited-file-name :around #'lsp--on-set-visited-file-name)

(defvar lsp--flushing-delayed-changes nil)

(defun lsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."

  (unless lsp--flushing-delayed-changes
    (let ((lsp--flushing-delayed-changes t))
      (lsp--flush-delayed-changes)))
  (lsp-process-send proc message))

(define-error 'lsp-parse-error
  "Error parsing message from language server" 'lsp-error)
(define-error 'lsp-unknown-message-type
  "Unknown message type" '(lsp-error lsp-parse-error))
(define-error 'lsp-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(lsp-error lsp-parse-error))
(define-error 'lsp-no-content-length
  "Content-Length header missing in message" '(lsp-error lsp-parse-error))
(define-error 'lsp-invalid-header-name
  "Invalid header name" '(lsp-error lsp-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification
(defun lsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (lsp:json-message-id? json-data)
      (if (lsp:json-message-error? json-data)
          'response-error
        (if (lsp:json-message-method? json-data)
            'request
          'response))
    'notification))

(defconst lsp--default-notification-handlers
  (ht ("window/showMessage" #'lsp--window-show-message)
      ("window/logMessage" #'lsp--window-log-message)
      ("window/showInputBox" #'lsp--window-show-input-box)
      ("window/showQuickPick" #'lsp--window-show-quick-pick)
      ("textDocument/publishDiagnostics" #'lsp--on-diagnostics)
      ("textDocument/diagnosticsEnd" #'ignore)
      ("textDocument/diagnosticsBegin" #'ignore)
      ("telemetry/event" #'ignore)
      ("$/progress" (lambda (workspace params)
                      (funcall lsp-progress-function workspace params)))))

(lsp-defun lsp--on-notification (workspace (&JSONNotification :params :method))
  "Call the appropriate handler for NOTIFICATION."
  (-let ((client (lsp--workspace-client workspace)))
    (when (lsp--log-io-p method)
      (lsp--log-entry-new (lsp--make-log-entry method nil params 'incoming-notif)
                          lsp--cur-workspace))
    (if-let ((handler (or (gethash method (lsp--client-notification-handlers client))
                          (gethash method lsp--default-notification-handlers))))
        (funcall handler workspace params)
      (when (and method (not (string-prefix-p "$" method)))
        (lsp-warn "Unknown notification: %s" method)))))

(lsp-defun lsp--build-workspace-configuration-response ((&ConfigurationParams :items))
  "Get section configuration.
PARAMS are the `workspace/configuration' request params"
  (->> items
       (-map (-lambda ((&ConfigurationItem :section?))
               (-let* ((path-parts (split-string section? "\\."))
                       (path-without-last (s-join "." (-slice path-parts 0 -1)))
                       (path-parts-len (length path-parts)))
                 (cond
                  ((<= path-parts-len 1)
                   (ht-get (lsp-configuration-section section?)
                           (car-safe path-parts)
                           (ht-create)))
                  ((> path-parts-len 1)
                   (when-let ((section (lsp-configuration-section path-without-last))
                              (keys path-parts))
                     (while (and keys section)
                       (setf section (ht-get section (pop keys))))
                     section))))))
       (apply #'vector)))

(defun lsp--ms-since (timestamp)
  "Integer number of milliseconds since TIMESTAMP.  Fractions discarded."
  (floor (* 1000 (float-time (time-since timestamp)))))

(defun lsp--send-request-response (workspace recv-time request response)
  "Send the RESPONSE for REQUEST in WORKSPACE and log if needed."
  (-let* (((&JSONResponse :params :method :id) request)
          (process (lsp--workspace-proc workspace))
          (response (lsp--make-response id response))
          (req-entry (and lsp-log-io
                          (lsp--make-log-entry method id params 'incoming-req)))
          (resp-entry (and lsp-log-io
                           (lsp--make-log-entry method id response 'outgoing-resp
                                                (lsp--ms-since recv-time)))))
    ;; Send response to the server.
    (when (lsp--log-io-p method)
      (lsp--log-entry-new req-entry workspace)
      (lsp--log-entry-new resp-entry workspace))
    (lsp--send-no-wait response process)))

(lsp-defun lsp--on-request (workspace (request &as &JSONRequest :params :method))
  "Call the appropriate handler for REQUEST, and send the return value to the
server. WORKSPACE is the active workspace."
  (-let* ((recv-time (current-time))
          (client (lsp--workspace-client workspace))
          (buffers (lsp--workspace-buffers workspace))
          handler
          (response (cond
                     ((setq handler (gethash method (lsp--client-request-handlers client) nil))
                      (funcall handler workspace params))
                     ((setq handler (gethash method (lsp--client-async-request-handlers client) nil))
                      (funcall handler workspace params
                               (-partial #'lsp--send-request-response
                                         workspace recv-time request))
                      'delay-response)
                     ((equal method "client/registerCapability")
                      (mapc #'lsp--server-register-capability
                            (lsp:registration-params-registrations params))
                      (mapc (lambda (buf)
                              (when (lsp-buffer-live-p buf)
                                (lsp-with-current-buffer buf
                                  (lsp-unconfig-buffer)
                                  (lsp-configure-buffer))))
                            buffers)
                      nil)
                     ((equal method "window/showMessageRequest")
                      (let ((choice (lsp--window-log-message-request params)))
                        `(:title ,choice)))
                     ((equal method "window/showDocument")
                      (let ((success? (lsp--window-show-document params)))
                        (lsp-make-show-document-result :success (or success?
                                                                    :json-false))))
                     ((equal method "client/unregisterCapability")
                      (mapc #'lsp--server-unregister-capability
                            (lsp:unregistration-params-unregisterations params))
                      (mapc (lambda (buf)
                              (when (lsp-buffer-live-p buf)
                                (lsp-with-current-buffer buf
                                  (lsp-unconfig-buffer)
                                  (lsp-configure-buffer))))
                            buffers)
                      nil)
                     ((equal method "workspace/applyEdit")
                      (list :applied (condition-case err
                                         (prog1 t
                                           (lsp--apply-workspace-edit (lsp:apply-workspace-edit-params-edit params) 'server-requested))
                                       (error
                                        (lsp--error "Failed to apply edits with message %s"
                                                    (error-message-string err))
                                        :json-false))))
                     ((equal method "workspace/configuration")
                      (with-lsp-workspace workspace
                        (if-let ((buf (car buffers)))
                            (lsp-with-current-buffer buf
                              (lsp--build-workspace-configuration-response params))
                          (lsp--with-workspace-temp-buffer (lsp--workspace-root workspace)
                            (lsp--build-workspace-configuration-response params)))))
                     ((equal method "workspace/workspaceFolders")
                      (let ((folders (or (-> workspace
                                             (lsp--workspace-client)
                                             (lsp--client-server-id)
                                             (gethash (lsp-session-server-id->folders (lsp-session))))
                                         (lsp-session-folders (lsp-session)))))
                        (->> folders
                             (-distinct)
                             (-map (lambda (folder)
                                     (list :uri (lsp--path-to-uri folder))))
                             (apply #'vector))))
                     ((equal method "window/workDoneProgress/create")
                      nil ;; no specific reply, no processing required
                      )
                     ((equal method "workspace/semanticTokens/refresh")
                      (when (and lsp-semantic-tokens-enable
                                 (fboundp 'lsp--semantic-tokens-on-refresh))
                        (lsp--semantic-tokens-on-refresh workspace))
                      nil)
                     ((equal method "workspace/codeLens/refresh")
                      (when (and lsp-lens-enable
                                 (fboundp 'lsp--lens-on-refresh))
                        (lsp--lens-on-refresh workspace))
                      nil)
                     (t (lsp-warn "Unknown request method: %s" method) nil))))
    ;; Send response to the server.
    (unless (eq response 'delay-response)
      (lsp--send-request-response workspace recv-time request response))))

(lsp-defun lsp--error-string ((&JSONError :message :code))
  "Format ERR as a user friendly string."
  (format "Error from the Language Server: %s (%s)"
          message
          (or (car (alist-get code lsp--errors)) "Unknown error")))

(defun lsp--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server or our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defmacro lsp--read-json (str)
  "Read json string STR."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :object-type (if lsp-use-plists
                                           'plist
                                         'hash-table)
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type (if lsp-use-plists
                                 'plist
                               'hash-table))
           (json-false nil))
       (json-read-from-string ,str))))

(defmacro lsp-json-read-buffer ()
  "Read json from the current buffer."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-buffer))
      `(json-parse-buffer :object-type (if lsp-use-plists
                                           'plist
                                         'hash-table)
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type (if lsp-use-plists
                                 'plist
                               'hash-table))
           (json-false nil))
       (json-read))))

(defun lsp--read-json-file (file-path)
  "Read json file."
  (-> file-path
    (f-read-text)
    (lsp--read-json)))

(defun lsp--parser-on-message (json-data workspace)
  "Called when the parser P read a complete MSG from the server."
  (with-demoted-errors "Error processing message %S."
    (with-lsp-workspace workspace
      (let* ((client (lsp--workspace-client workspace))
             (id (--when-let (lsp:json-response-id json-data)
                   (if (stringp it) (string-to-number it) it)))
             (data (lsp:json-response-result json-data)))
        (pcase (lsp--get-message-type json-data)
          ('response
           (cl-assert id)
           (-let [(callback _ method _ before-send) (gethash id (lsp--client-response-handlers client))]
             (when (lsp--log-io-p method)
               (lsp--log-entry-new
                (lsp--make-log-entry method id data 'incoming-resp
                                     (lsp--ms-since before-send))
                workspace))
             (when callback
               (remhash id (lsp--client-response-handlers client))
               (funcall callback (lsp:json-response-result json-data)))))
          ('response-error
           (cl-assert id)
           (-let [(_ callback method _ before-send) (gethash id (lsp--client-response-handlers client))]
             (when (lsp--log-io-p method)
               (lsp--log-entry-new
                (lsp--make-log-entry method id (lsp:json-response-error-error json-data)
                                     'incoming-resp (lsp--ms-since before-send))
                workspace))
             (when callback
               (remhash id (lsp--client-response-handlers client))
               (funcall callback (lsp:json-response-error-error json-data)))))
          ('notification
           (lsp--on-notification workspace json-data))
          ('request (lsp--on-request workspace json-data)))))))

(defun lsp--create-filter-function (workspace)
  "Make filter for the workspace."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (if (s-blank? leftovers)
                      input
                    (concat leftovers input)))

      (let (messages)
        (while (not (s-blank? chunk))
          (if (not body-length)
              ;; Read headers
              (if-let ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                  ;; We've got all the headers, handle them all at once:
                  (setf body-length (lsp--get-body-length
                                     (mapcar #'lsp--parse-header
                                             (split-string
                                              (substring-no-properties chunk
                                                                       (or (string-match-p "Content-Length" chunk)
                                                                           (error "Unable to find Content-Length header."))
                                                                       body-sep-pos)
                                              "\r\n")))
                        body-received 0
                        leftovers nil
                        chunk (substring-no-properties chunk (+ body-sep-pos 4)))

                ;; Haven't found the end of the headers yet. Save everything
                ;; for when the next chunk arrives and await further input.
                (setf leftovers chunk
                      chunk nil))
            (let* ((chunk-length (string-bytes chunk))
                   (left-to-receive (- body-length body-received))
                   (this-body (if (< left-to-receive chunk-length)
                                  (prog1 (substring-no-properties chunk 0 left-to-receive)
                                    (setf chunk (substring-no-properties chunk left-to-receive)))
                                (prog1 chunk
                                  (setf chunk nil))))
                   (body-bytes (string-bytes this-body)))
              (push this-body body)
              (setf body-received (+ body-received body-bytes))
              (when (>= chunk-length left-to-receive)
                (condition-case err
                    (with-temp-buffer
                      (apply #'insert
                             (nreverse
                              (prog1 body
                                (setf leftovers nil
                                      body-length nil
                                      body-received nil
                                      body nil))))
                      (decode-coding-region (point-min)
                                            (point-max)
                                            'utf-8)
                      (goto-char (point-min))
                      (push (lsp-json-read-buffer) messages))

                  (error
                   (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                             (concat leftovers input)
                             err)))))))
        (mapc (lambda (msg)
                (lsp--parser-on-message msg workspace))
              (nreverse messages))))))

(defvar-local lsp--line-col-to-point-hash-table nil
  "Hash table with keys (line . col) and values that are either point positions
or markers.")

(defcustom lsp-imenu-detailed-outline t
  "Whether `lsp-imenu' should include signatures.
This will be ignored if the server doesn't provide the necessary
information, for example if it doesn't support DocumentSymbols."
  :group 'lsp-imenu
  :type 'boolean)

(defcustom lsp-imenu-hide-parent-details t
  "Whether `lsp-imenu' should hide signatures of parent nodes."
  :group 'lsp-imenu
  :type 'boolean)

(defface lsp-details-face '((t :height 0.8 :inherit shadow))
  "Used to display additional information throughout `lsp'.
Things like line numbers, signatures, ... are considered
additional information. Often, additional faces are defined that
inherit from this face by default, like `lsp-signature-face', and
they may be customized for finer control."
  :group 'lsp-mode)

(defface lsp-signature-face '((t :inherit lsp-details-face))
  "Used to display signatures in `imenu', ...."
  :group 'lsp-mode)

(lsp-defun lsp-render-symbol ((&DocumentSymbol :name :detail? :deprecated?)
                              show-detail?)
  "Render INPUT0, an `&DocumentSymbol', to a string.
If SHOW-DETAIL? is set, make use of its `:detail?' field (often
the signature)."
  (let ((detail (and show-detail? (s-present? detail?)
                     (propertize (concat " " (s-trim-left detail?))
                                 'face 'lsp-signature-face)))
        (name (if deprecated?
                  (propertize name 'face 'lsp-face-semhl-deprecated) name)))
    (concat name detail)))

(lsp-defun lsp-render-symbol-information ((&SymbolInformation :name :deprecated? :container-name?)
                                          separator)
  "Render a piece of SymbolInformation.
Handle :deprecated?. If SEPARATOR is non-nil, the
symbol's (optional) parent, SEPARATOR and the symbol itself are
concatenated."
  (when (and separator container-name? (not (string-empty-p container-name?)))
    (setq name (concat name separator container-name?)))
  (if deprecated? (propertize name 'face 'lsp-face-semhl-deprecated) name))

(defun lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (let ((start-point (ht-get lsp--line-col-to-point-hash-table
                             (lsp--get-line-and-col sym))))
    (cons (lsp-render-symbol-information
           sym (and lsp-imenu-show-container-name
                    lsp-imenu-container-name-separator))
          start-point)))

(lsp-defun lsp--symbol-to-hierarchical-imenu-elem ((sym &as &DocumentSymbol :children?))
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (let ((filtered-children (lsp--imenu-filter-symbols children?))
        (signature (lsp-render-symbol sym lsp-imenu-detailed-outline)))
    (if (seq-empty-p filtered-children)
        (cons signature
              (ht-get lsp--line-col-to-point-hash-table
                      (lsp--get-line-and-col sym)))
      (cons signature
            (lsp--imenu-create-hierarchical-index filtered-children)))))

(lsp-defun lsp--symbol-ignore ((&SymbolInformation :kind))
  "Determine if SYM is for the current document and is to be shown."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the
  ;; current buffer file.
  (and lsp-imenu-index-symbol-kinds
       (numberp kind)
       (let ((clamped-kind (if (< 0 kind (length lsp/symbol-kind-lookup))
                               kind
                             0)))
         (not (memql (aref lsp/symbol-kind-lookup clamped-kind)
                     lsp-imenu-index-symbol-kinds)))))

(lsp-defun lsp--get-symbol-type ((&SymbolInformation :kind))
  "The string name of the kind of SYM."
  (alist-get kind lsp-symbol-kinds "Other"))

(defun lsp--get-line-and-col (sym)
  "Obtain the line and column corresponding to SYM."
  (-let* ((location (lsp:symbol-information-location sym))
          (name-range (or (and location (lsp:location-range location))
                          (lsp:document-symbol-selection-range sym)))
          ((&Range :start (&Position :line :character)) name-range))
    (cons line character)))

(defun lsp--collect-lines-and-cols (symbols)
  "Return a sorted list ((line . col) ...) of the locations of SYMBOLS."
  (let ((stack (mapcar 'identity symbols))
        line-col-list)
    (while stack
      (let ((sym (pop stack)))
        (push (lsp--get-line-and-col sym) line-col-list)
        (unless (seq-empty-p (lsp:document-symbol-children? sym))
          (setf stack (nconc (lsp--imenu-filter-symbols (lsp:document-symbol-children? sym)) stack)))))
    (-sort #'lsp--line-col-comparator line-col-list)))

(defun lsp--convert-line-col-to-points-batch (line-col-list)
  "Convert a sorted list of positions from line-column
representation to point representation."
  (let ((line-col-to-point-map (ht-create))
        (inhibit-field-text-motion t)
        (curr-line 0))
    (lsp-save-restriction-and-excursion
      (goto-char (point-min))
      (cl-loop for (line . col) in line-col-list do
               (forward-line (- line curr-line))
               (setq curr-line line)
               (let ((line-end (line-end-position)))
                 (if (or (not col) (> col (- line-end (point))))
                     (goto-char line-end)
                   (forward-char col)))
               (ht-set! line-col-to-point-map (cons line col) (if imenu-use-markers
                                                                  (point-marker)
                                                                (point)))))
    line-col-to-point-map))

(cl-defun lsp--line-col-comparator ((l1 . c1) (l2 . c2))
  (or (< l1 l2)
      (and (= l1 l2)
           (cond ((and c1 c2)
                  (< c1 c2))
                 (c1 t)))))

(defun lsp-imenu-create-uncategorized-index (symbols)
  "Create imenu index from document SYMBOLS.
This function, unlike `lsp-imenu-create-categorized-index', does
not categorize by type, but instead returns an `imenu' index
corresponding to the symbol hierarchy returned by the server
directly."
  (let* ((lsp--line-col-to-point-hash-table (-> symbols
                                                lsp--collect-lines-and-cols
                                                lsp--convert-line-col-to-points-batch)))
    (if (lsp--imenu-hierarchical-p symbols)
        (lsp--imenu-create-hierarchical-index symbols)
      (lsp--imenu-create-non-hierarchical-index symbols))))

(defcustom lsp-imenu-symbol-kinds
  '((1 . "Files")
    (2 . "Modules")
    (3 . "Namespaces")
    (4 . "Packages")
    (5 . "Classes")
    (6 . "Methods")
    (7 . "Properties")
    (8 . "Fields")
    (9 . "Constructors")
    (10 . "Enums")
    (11 . "Interfaces")
    (12 . "Functions")
    (13 . "Variables")
    (14 . "Constants")
    (15 . "Strings")
    (16 . "Numbers")
    (17 . "Booleans")
    (18 . "Arrays")
    (19 . "Objects")
    (20 . "Keys")
    (21 . "Nulls")
    (22 . "Enum Members")
    (23 . "Structs")
    (24 . "Events")
    (25 . "Operators")
    (26 . "Type Parameters"))
  "`lsp-symbol-kinds', but only used by `imenu'.
A new variable is needed, as it is `imenu' convention to use
pluralized categories, which `lsp-symbol-kinds' doesn't. If the
non-pluralized names are preferred, this can be set to
`lsp-symbol-kinds'."
  :type '(alist :key-type integer :value-type string))

(defun lsp--imenu-kind->name (kind)
  (alist-get kind lsp-imenu-symbol-kinds "?"))

(defun lsp-imenu-create-top-level-categorized-index (symbols)
  "Create an `imenu' index categorizing SYMBOLS by type.
Only root symbols are categorized.

See `lsp-symbol-kinds' to customize the category naming. SYMBOLS
shall be a list of DocumentSymbols or SymbolInformation."
  (mapcan
   (-lambda ((type . symbols))
     (let ((cat (lsp--imenu-kind->name type))
           (symbols (lsp-imenu-create-uncategorized-index symbols)))
       ;; If there is no :kind (this is being defensive), or we couldn't look it
       ;; up, just display the symbols inline, without categories.
       (if cat (list (cons cat symbols)) symbols)))
   (sort (seq-group-by #'lsp:document-symbol-kind symbols)
         (-lambda ((kinda) (kindb)) (< kinda kindb)))))

(lsp-defun lsp--symbol->imenu ((sym &as &DocumentSymbol :selection-range (&RangeToPoint :start)))
  "Convert an `&DocumentSymbol' to an `imenu' entry."
  (cons (lsp-render-symbol sym lsp-imenu-detailed-outline) start))

(defun lsp--imenu-create-categorized-index-1 (symbols)
  "Returns an `imenu' index from SYMBOLS categorized by type.
The result looks like this: ((\"Variables\" . (...)))."
  (->>
   symbols
   (mapcan
    (-lambda ((sym &as &DocumentSymbol :kind :children?))
      (if (seq-empty-p children?)
          (list (list kind (lsp--symbol->imenu sym)))
        (let ((parent (lsp-render-symbol sym (and lsp-imenu-detailed-outline
                                                  (not lsp-imenu-hide-parent-details)))))
          (cons
           (list kind (lsp--symbol->imenu sym))
           (mapcar (-lambda ((type .  imenu-items))
                     (list type (cons parent (mapcan #'cdr imenu-items))))
                   (-group-by #'car (lsp--imenu-create-categorized-index-1 children?))))))))
   (-group-by #'car)
   (mapcar
    (-lambda ((kind . syms))
      (cons kind (mapcan #'cdr syms))))))

(defun lsp--imenu-create-categorized-index (symbols)
  (let ((syms (lsp--imenu-create-categorized-index-1 symbols)))
    (dolist (sym syms)
      (setcar sym (lsp--imenu-kind->name (car sym))))
    syms))

(lsp-defun lsp--symbol-information->imenu ((sym &as &SymbolInformation :location (&Location :range (&RangeToPoint :start))))
  (cons (lsp-render-symbol-information sym nil) start))

(defun lsp--imenu-create-categorized-index-flat (symbols)
  "Create a kind-categorized index for SymbolInformation."
  (mapcar (-lambda ((kind . syms))
            (cons (lsp--imenu-kind->name kind)
                  (mapcan (-lambda ((parent . children))
                            (let ((children (mapcar #'lsp--symbol-information->imenu children)))
                              (if parent (list (cons parent children)) children)))
                          (-group-by #'lsp:symbol-information-container-name? syms))))
          (seq-group-by #'lsp:symbol-information-kind symbols)))

(defun lsp-imenu-create-categorized-index (symbols)
  (if (lsp--imenu-hierarchical-p symbols)
      (lsp--imenu-create-categorized-index symbols)
    (lsp--imenu-create-categorized-index-flat symbols)))

(defcustom lsp-imenu-index-function #'lsp-imenu-create-uncategorized-index
  "Function that should create an `imenu' index.
It will be called with a list of SymbolInformation or
DocumentSymbols, whose first level is already filtered. It shall
then return an appropriate `imenu' index (see
`imenu-create-index-function').

Note that this interface is not stable, and subject to change any
time."
  :group 'lsp-imenu
  :type '(radio
          (const :tag "Categorize by type"
                 lsp-imenu-create-categorized-index)
          (const :tag "Categorize root symbols by type"
                 lsp-imenu-create-top-level-categorized-index)
          (const :tag "Uncategorized, inline entries"
                 lsp-imenu-create-uncategorized-index)
          (function :tag "Custom function")))

(defun lsp--imenu-create-index ()
  "Create an `imenu' index based on the language server.
Respects `lsp-imenu-index-function'."
  (let ((symbols (lsp--imenu-filter-symbols (lsp--get-document-symbols))))
    (funcall lsp-imenu-index-function symbols)))

(defun lsp--imenu-filter-symbols (symbols)
  "Filter out unsupported symbols from SYMBOLS."
  (seq-remove #'lsp--symbol-ignore symbols))

(defun lsp--imenu-hierarchical-p (symbols)
  "Determine whether any element in SYMBOLS has children."
  (seq-some #'lsp-document-symbol? symbols))

(defun lsp--imenu-create-non-hierarchical-index (symbols)
  "Create imenu index for non-hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (seq-map (lambda (nested-alist)
             (cons (car nested-alist)
                   (seq-map #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
           (seq-group-by #'lsp--get-symbol-type symbols)))

(defun lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (seq-map #'lsp--symbol-to-hierarchical-imenu-elem
           (seq-sort #'lsp--imenu-symbol-lessp symbols)))

(defun lsp--imenu-symbol-lessp (sym1 sym2)
  (let* ((compare-results (mapcar (lambda (method)
                                    (funcall (alist-get method lsp--imenu-compare-function-alist)
                                             sym1 sym2))
                                  lsp-imenu-sort-methods))
         (result (seq-find (lambda (result)
                             (not (= result 0)))
                           compare-results
                           0)))
    (and (numberp result) (< result 0))))

(lsp-defun lsp--imenu-compare-kind ((&SymbolInformation :kind left)
                                    (&SymbolInformation :kind right))
  "Compare SYM1 and SYM2 by kind."
  (- left right))

(defun lsp--imenu-compare-line-col (sym1 sym2)
  (if (lsp--line-col-comparator
       (lsp--get-line-and-col sym1)
       (lsp--get-line-and-col sym2))
      -1
    1))

(lsp-defun lsp--imenu-compare-name ((&SymbolInformation :name name1)
                                    (&SymbolInformation :name name2))
  "Compare SYM1 and SYM2 by name."
  (let ((result (compare-strings name1 0 (length name1) name2 0 (length name2))))
    (if (numberp result) result 0)))

(defun lsp--imenu-refresh ()
  "Force Imenu to refresh itself."
  (imenu--menubar-select imenu--rescan-item))

(defun lsp-enable-imenu ()
  "Use lsp-imenu for the current buffer."
  (imenu--cleanup)
  (add-function :override (local 'imenu-create-index-function) #'lsp--imenu-create-index)
  (setq-local imenu-menubar-modified-tick -1)
  (setq-local imenu--index-alist nil)
  (when menu-bar-mode
    (lsp--imenu-refresh)))

(defun lsp-resolve-final-command (command &optional test?)
  "Resolve final function COMMAND."
  (let* ((command (lsp-resolve-value command))
         (command (cl-etypecase command
                    (list
                     (cl-assert (seq-every-p (apply-partially #'stringp) command) nil
                                "Invalid command list")
                     command)
                    (string (list command)))))
    (if (and (file-remote-p default-directory) (not test?))
        (list shell-file-name "-c"
              (string-join (cons "stty raw > /dev/null;"
                                 (mapcar #'shell-quote-argument command))
                           " "))
      command)))

(defun lsp-server-present? (final-command)
  "Check whether FINAL-COMMAND is present."
  (let ((binary-found? (executable-find (cl-first final-command) t)))
    (if binary-found?
        (lsp-log "Command \"%s\" is present on the path." (s-join " " final-command))
      (lsp-log "Command \"%s\" is not present on the path." (s-join " " final-command)))
    binary-found?))

(defun lsp--value-to-string (value)
  "Convert VALUE to a string that can be set as value in an environment
variable."
  (cond
   ((stringp value) value)
   ((booleanp value) (if value
                         "1"
                       "0"))
   ((and (sequencep value)
         (seq-every-p #'stringp value)) (string-join value ":"))
   (t (user-error "Only strings, booleans, and sequences of strings are supported as environment variables"))))

(defun lsp--compute-process-environment (environment-fn)
  "Append a list of KEY=VALUE from the alist ENVIRONMENT to `process-environment'.
Ignore non-boolean keys whose value is nil."
  (let ((environment (if environment-fn
                         (funcall environment-fn)
                       nil)))
    (-flatten (cons (cl-loop for (key . value) in environment
                             if (or (eval value)
                                    (eq (get value 'custom-type) 'boolean))
                             collect (concat key "=" (lsp--value-to-string
                                                      (eval value))))
                    process-environment))))

(defun lsp--default-directory-for-connection (&optional path)
  "Return path to be used for the working directory of a LSP process.

If `lsp-use-workspace-root-for-server-default-directory' is
non-nil, uses `lsp-workspace-root' to find the directory
corresponding to PATH, else returns `default-directory'."
  (if lsp-use-workspace-root-for-server-default-directory
      (lsp-workspace-root path)
    default-directory))

(defun lsp--fix-remote-cmd (program)
  "Helper for `lsp-stdio-connection'.
Originally coppied from eglot."

  (if (file-remote-p default-directory)
      (list shell-file-name "-c"
            (string-join (cons "stty raw > /dev/null;"
                               (mapcar #'shell-quote-argument program))
                         " "))
    program))

(defvar tramp-use-ssh-controlmaster-options)
(defvar tramp-ssh-controlmaster-options)

(defun lsp-stdio-connection (command &optional test-command)
  "Returns a connection property list using COMMAND.
COMMAND can be: A string, denoting the command to launch the
language server. A list of strings, denoting an executable with
its command line arguments. A function, that either returns a
string or a list of strings. In all cases, the launched language
server should send and receive messages on standard I/O.
TEST-COMMAND is a function with no arguments which returns
whether the command is present or not. When not specified
`lsp-mode' will check whether the first element of the list
returned by COMMAND is available via `executable-find'"
  (cl-check-type command (or string
                             function
                             (and list
                                  (satisfies (lambda (l)
                                               (seq-every-p (lambda (el)
                                                              (stringp el))
                                                            l))))))
  (list :connect (lambda (filter sentinel name environment-fn workspace)
                   (if (and (functionp 'json-rpc-connection)
                            (not (file-remote-p default-directory)))
                       (lsp-json-rpc-connection workspace (lsp-resolve-final-command command))
                     (let ((final-command (lsp-resolve-final-command command))
                           (process-name (generate-new-buffer-name name))
                           (process-environment
                            (lsp--compute-process-environment environment-fn)))
                       (let* ((stderr-buf (get-buffer-create (format "*%s::stderr*" process-name)))
                              (default-directory (lsp--default-directory-for-connection))
                              (tramp-use-ssh-controlmaster-options 'suppress)
                              (tramp-ssh-controlmaster-options "-o ControlMaster=no -o ControlPath=none")
                              (proc (make-process
                                     :name process-name
                                     :connection-type 'pipe
                                     :buffer (format "*%s*" process-name)
                                     :coding 'no-conversion
                                     :command final-command
                                     :filter filter
                                     :sentinel sentinel
                                     :stderr stderr-buf
                                     :noquery t
                                     :file-handler t)))
                         (set-process-query-on-exit-flag proc nil)
                         (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)
                         (with-current-buffer (get-buffer stderr-buf)
                           ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
                           (special-mode))
                         (cons proc proc)))))
        :test? (or
                test-command
                (lambda ()
                  (lsp-server-present? (lsp-resolve-final-command command t))))))

(defun lsp--open-network-stream (host port name)
  "Open network stream to HOST:PORT.
  NAME will be passed to `open-network-stream'.
  RETRY-COUNT is the number of the retries.
  SLEEP-INTERVAL is the sleep interval between each retry."
  (let* ((retries 0)
         (sleep-interval 0.01)
         (number-of-retries (/ lsp-tcp-connection-timeout sleep-interval))
         connection)
    (while (and (not connection) (< retries number-of-retries))
      (condition-case err
          (setq connection (open-network-stream name nil host port
                                                :type 'plain
                                                :coding 'no-conversion))
        (file-error
         (let ((inhibit-message t))
           (lsp--warn "Failed to connect to %s:%s with error message %s"
                      host
                      port
                      (error-message-string err))
           (sleep-for sleep-interval)
           (cl-incf retries)))))
    (or connection (error "Port %s was never taken. Consider increasing `lsp-tcp-connection-timeout'." port))))

(defun lsp--port-available (host port)
  "Return non-nil if HOST and PORT are available."
  (condition-case _err
      (delete-process (open-network-stream "*connection-test*" nil host port :type 'plain))
    (file-error t)))

(defun lsp--find-available-port (host starting-port)
  "Find available port on HOST starting from STARTING-PORT."
  (let ((port starting-port))
    (while (not (lsp--port-available host port))
      (cl-incf port))
    port))

(defun lsp-tcp-connection (command-fn)
  "Returns a connection property list similar to `lsp-stdio-connection'.
COMMAND-FN can only be a function that takes a single argument, a
port number. It should return a command for launches a language server
process listening for TCP connections on the provided port."
  (cl-check-type command-fn function)
  (list
   :connect (lambda (filter sentinel name environment-fn _workspace)
              (let* ((host "localhost")
                     (port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (funcall command-fn port))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (lsp-server-present? final-command)
                          (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag proc nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () (lsp-server-present? (funcall command-fn 0)))))

(defalias 'lsp-tcp-server 'lsp-tcp-server-command)

(defun lsp-tcp-server-command (command-fn)
  "Create tcp server connection.
In this mode Emacs is TCP server and the language server connects
to it. COMMAND is function with one parameter(the port) and it
should return the command to start the LS server."
  (cl-check-type command-fn function)
  (list
   :connect (lambda (filter sentinel name environment-fn _workspace)
              (let* (tcp-client-connection
                     (tcp-server (make-network-process :name (format "*tcp-server-%s*" name)
                                                       :buffer (format "*tcp-server-%s*" name)
                                                       :family 'ipv4
                                                       :service lsp--tcp-server-port
                                                       :sentinel (lambda (proc _string)
                                                                   (lsp-log "Language server %s is connected." name)
                                                                   (setf tcp-client-connection proc))
                                                       :server 't))
                     (port (process-contact tcp-server :service))
                     (final-command (funcall command-fn port))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (cmd-proc (make-process :name name
                                             :connection-type 'pipe
                                             :coding 'no-conversion
                                             :command final-command
                                             :stderr (format "*tcp-server-%s*::stderr" name)
                                             :noquery t)))
                (let ((retries 0))
                  ;; wait for the client to connect (we sit-for 500 ms, so have to double lsp--tcp-server-wait-seconds)
                  (while (and (not tcp-client-connection) (< retries (* 2 lsp--tcp-server-wait-seconds)))
                    (lsp--info "Waiting for connection for %s, retries: %s" name retries)
                    (sit-for 0.500)
                    (cl-incf retries)))

                (unless tcp-client-connection
                  (condition-case nil (delete-process tcp-server) (error))
                  (condition-case nil (delete-process cmd-proc) (error))
                  (error "Failed to create connection to %s on port %s" name port))
                (lsp--info "Successfully connected to %s" name)

                (set-process-query-on-exit-flag cmd-proc nil)
                (set-process-query-on-exit-flag tcp-client-connection nil)
                (set-process-query-on-exit-flag tcp-server nil)

                (set-process-filter tcp-client-connection filter)
                (set-process-sentinel tcp-client-connection sentinel)
                (cons tcp-client-connection cmd-proc)))
   :test? (lambda () (lsp-server-present? (funcall command-fn 0)))))

(defalias 'lsp-tramp-connection 'lsp-stdio-connection)

(defun lsp--auto-configure ()
  "Autoconfigure `company', `flycheck', `lsp-ui', etc if they are installed."
  (when (functionp 'lsp-ui-mode)
    (lsp-ui-mode))

  (if lsp-headerline-breadcrumb-enable
      (add-hook 'lsp-configure-hook 'lsp-headerline-breadcrumb-mode)
    (remove-hook 'lsp-configure-hook 'lsp-headerline-breadcrumb-mode))
  (if lsp-modeline-code-actions-enable
      (add-hook 'lsp-configure-hook 'lsp-modeline-code-actions-mode)
    (remove-hook 'lsp-configure-hook 'lsp-modeline-code-actions-mode))
  (if lsp-modeline-diagnostics-enable
      (add-hook 'lsp-configure-hook 'lsp-modeline-diagnostics-mode)
    (remove-hook 'lsp-configure-hook 'lsp-modeline-diagnostics-mode))
  (if lsp-modeline-workspace-status-enable
      (add-hook 'lsp-configure-hook 'lsp-modeline-workspace-status-mode)
    (remove-hook 'lsp-configure-hook 'lsp-modeline-workspace-status-mode))
  (if lsp-lens-enable
      (add-hook 'lsp-configure-hook 'lsp-lens--enable)
    (remove-hook 'lsp-configure-hook 'lsp-lens--enable))
  (if lsp-semantic-tokens-enable
      (add-hook 'lsp-configure-hook 'lsp-semantic-tokens--enable)
    (remove-hook 'lsp-configure-hook 'lsp-semantic-tokens--enable))

  ;; yas-snippet config
  (setq-local yas-inhibit-overlay-modification-protection t))

(defvar-local lsp--buffer-deferred nil
  "Whether buffer was loaded via `lsp-deferred'.")

(defun lsp--restart-if-needed (workspace)
  "Handler restart for WORKSPACE."
  (when (or (eq lsp-restart 'auto-restart)
            (eq (lsp--workspace-shutdown-action workspace) 'restart)
            (and (eq lsp-restart 'interactive)
                 (let ((query (format
                               "Server %s exited (check corresponding stderr buffer for details). Do you want to restart it?"
                               (lsp--workspace-print workspace))))
                   (y-or-n-p query))))
    (--each (lsp--workspace-buffers workspace)
      (when (lsp-buffer-live-p it)
        (lsp-with-current-buffer it
          (if lsp--buffer-deferred
              (lsp-deferred)
            (lsp--info "Restarting LSP in buffer %s" (buffer-name))
            (lsp)))))))

(defun lsp--update-key (table key fn)
  "Apply FN on value corresponding to KEY in TABLE."
  (let ((existing-value (gethash key table)))
    (if-let ((new-value (funcall fn existing-value)))
        (puthash key new-value table)
      (remhash key table))))

(defun lsp--process-sentinel (workspace process exit-str)
  "Create the sentinel for WORKSPACE."
  (unless (process-live-p process)
    (lsp--handle-process-exit workspace exit-str)))

(defun lsp--handle-process-exit (workspace exit-str)
  (let* ((folder->workspaces (lsp-session-folder->servers (lsp-session)))
         (proc (lsp--workspace-proc workspace)))
    (lsp--warn "%s has exited (%s)"
               (lsp-process-name proc)
               (string-trim-right (or exit-str "")))
    (with-lsp-workspace workspace
      ;; Clean workspace related data in each of the buffers
      ;; in the workspace.
      (--each (lsp--workspace-buffers workspace)
        (when (lsp-buffer-live-p it)
          (lsp-with-current-buffer it
            (setq lsp--buffer-workspaces (delete workspace lsp--buffer-workspaces))
            (lsp--uninitialize-workspace)
            (lsp--spinner-stop)
            (lsp--remove-overlays 'lsp-highlight))))

      ;; Cleanup session from references to the closed workspace.
      (--each (hash-table-keys folder->workspaces)
        (lsp--update-key folder->workspaces it (apply-partially 'delete workspace)))

      (lsp-process-cleanup proc))

    (run-hook-with-args 'lsp-after-uninitialized-functions workspace)

    (if (eq (lsp--workspace-shutdown-action workspace) 'shutdown)
        (lsp--info "Workspace %s shutdown." (lsp--workspace-print workspace))
      (lsp--restart-if-needed workspace))
    (lsp--cleanup-hanging-watches)))

(defun lsp-workspace-folders (workspace)
  "Return all folders associated with WORKSPACE."
  (let (result)
    (->> (lsp-session)
         (lsp-session-folder->servers)
         (maphash (lambda (folder workspaces)
                    (when (-contains? workspaces workspace)
                      (push folder result)))))
    result))

(defun lsp--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (lsp--spinner-start)
  (-let* ((default-directory root)
          (client (copy-lsp--client client-template))
          (workspace (make-lsp--workspace
                      :root root
                      :client client
                      :status 'starting
                      :buffers (list (lsp-current-buffer))
                      :host-root (file-remote-p root)))
          ((&lsp-cln 'server-id 'environment-fn 'new-connection 'custom-capabilities
                     'multi-root 'initialized-fn) client)
          ((proc . cmd-proc) (funcall
                              (or (plist-get new-connection :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (lsp--create-filter-function workspace)
                              (apply-partially #'lsp--process-sentinel workspace)
                              (format "%s" server-id)
                              environment-fn
                              workspace))
          (workspace-folders (gethash server-id (lsp-session-server-id->folders session))))
    (setf (lsp--workspace-proc workspace) proc
          (lsp--workspace-cmd-proc workspace) cmd-proc)

    ;; update (lsp-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (mapc (lambda (project-root)
            (->> session
                 (lsp-session-folder->servers)
                 (gethash project-root)
                 (cl-pushnew workspace)))
          (or workspace-folders (list root)))

    (with-lsp-workspace workspace
      (run-hooks 'lsp-before-initialize-hook)
      (lsp-request-async
       "initialize"
       (append
        (list :processId (unless (file-remote-p (buffer-file-name))
                           (emacs-pid))
              :rootPath (lsp-file-local-name (expand-file-name root))
              :clientInfo (list :name "emacs"
                                :version (emacs-version))
              :rootUri (lsp--path-to-uri root)
              :capabilities (lsp--client-capabilities custom-capabilities)
              :initializationOptions initialization-options
              :workDoneToken "1")
        (when lsp-server-trace
          (list :trace lsp-server-trace))
        (when multi-root
          (->> workspace-folders
               (-distinct)
               (-map (lambda (folder)
                       (list :uri (lsp--path-to-uri folder)
                             :name (f-filename folder))))
               (apply 'vector)
               (list :workspaceFolders))))
       (-lambda ((&InitializeResult :capabilities))
         ;; we know that Rust Analyzer will send {} which will be parsed as null
         ;; when using plists
         (when (equal 'rust-analyzer server-id)
           (-> capabilities
               (lsp:server-capabilities-text-document-sync?)
               (lsp:set-text-document-sync-options-save? t)))

         (setf (lsp--workspace-server-capabilities workspace) capabilities
               (lsp--workspace-status workspace) 'initialized)

         (with-lsp-workspace workspace
           (lsp-notify "initialized" lsp--empty-ht))

         (when initialized-fn (funcall initialized-fn workspace))

         (cl-callf2 -filter #'lsp-buffer-live-p (lsp--workspace-buffers workspace))
         (->> workspace
              (lsp--workspace-buffers)
              (mapc (lambda (buffer)
                      (lsp-with-current-buffer buffer
                        (lsp--open-in-workspace workspace)))))

         (with-lsp-workspace workspace
           (run-hooks 'lsp-after-initialize-hook))
         (lsp--info "%s initialized successfully in folders: %s"
                    (lsp--workspace-print workspace)
                    (lsp-workspace-folders workspace)))
       :mode 'detached))
    workspace))

(defun lsp--load-default-session ()
  "Load default session."
  (setq lsp--session (or (condition-case err
                             (lsp--read-from-file lsp-session-file)
                           (error (lsp--error "Failed to parse the session %s, starting with clean one."
                                              (error-message-string err))
                                  nil))
                         (make-lsp-session))))

(defun lsp-session ()
  "Get the session associated with the current buffer."
  (or lsp--session (setq lsp--session (lsp--load-default-session))))

(defun lsp--client-disabled-p (buffer-major-mode client)
  (seq-some
   (lambda (entry)
     (pcase entry
       ((pred symbolp) (eq entry client))
       (`(,mode . ,client-or-list)
        (and (eq mode buffer-major-mode)
             (if (listp client-or-list)
                 (memq client client-or-list)
               (eq client client-or-list))))))
   lsp-disabled-clients))


;; download server

(defcustom lsp-server-install-dir (expand-file-name
                                   (locate-user-emacs-file (f-join ".cache" "lsp")))
  "Directory in which the servers will be installed."
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.3")
  :group 'lsp-mode)

(defcustom lsp-verify-signature t
  "Whether to check GPG signatures of downloaded files."
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0")
  :group 'lsp-mode)

(defvar lsp--dependencies (ht))

(defun lsp-dependency (name &rest definitions)
  "Used to specify a language server DEPENDENCY, the server
executable or other required file path. Typically, the
DEPENDENCY is found by locating it on the system path using
`executable-find'.

You can explicitly call lsp-dependency in your environment to
specify the absolute path to the DEPENDENCY. For example, the
typescript-language-server requires both the server and the
typescript compiler. If you have installed them in a team shared
read-only location, you can instruct lsp-mode to use them via

 (eval-after-load `lsp-mode
   `(progn
      (require lsp-javascript)
      (lsp-dependency typescript-language-server (:system ,tls-exe))
      (lsp-dependency typescript (:system ,ts-js))))

where tls-exe is the absolute path to the typescript-language-server
executable and ts-js is the absolute path to the typescript compiler
JavaScript file, tsserver.js (the *.js is required for Windows)."
  (ht-set lsp--dependencies name definitions))

(defun lsp--server-binary-present? (client)
  (unless (equal (lsp--client-server-id client) 'lsp-pwsh)
    (condition-case ()
        (-some-> client lsp--client-new-connection (plist-get :test?) funcall)
      (error nil)
      (args-out-of-range nil))))

(define-minor-mode lsp-installation-buffer-mode
  "Mode used in *lsp-installation* buffers.
It can be used to set-up keybindings, etc. Disabling this mode
detaches the installation buffer from commands like
`lsp-select-installation-buffer'."
  :init-value nil
  :lighter nil)

(defface lsp-installation-finished-buffer-face '((t :foreground "orange"))
  "Face used for finished installation buffers.
Used in `lsp-select-installation-buffer'."
  :group 'lsp-mode)

(defface lsp-installation-buffer-face '((t :foreground "green"))
  "Face used for installation buffers still in progress.
Used in `lsp-select-installation-buffer'."
  :group 'lsp-mode)

(defun lsp--installation-buffer? (buf)
  "Check whether BUF is an `lsp-async-start-process' buffer."
  (buffer-local-value 'lsp-installation-buffer-mode buf))

(defun lsp-select-installation-buffer (&optional show-finished)
  "Interactively choose an installation buffer.
If SHOW-FINISHED is set, leftover (finished) installation buffers
are still shown."
  (interactive "P")
  (let ((bufs (--filter (and (lsp--installation-buffer? it)
                             (or show-finished (get-buffer-process it)))
                        (buffer-list))))
    (pcase bufs
      (`nil (user-error "No installation buffers"))
      (`(,buf) (pop-to-buffer buf))
      (bufs (pop-to-buffer (completing-read "Select installation buffer: "
                                            (--map (propertize (buffer-name it) 'face
                                                               (if (get-buffer-process it)
                                                                   'lsp-installation-buffer-face
                                                                 'lsp-installation-finished-buffer-face))
                                                   bufs)))))))

(defun lsp-cleanup-installation-buffers ()
  "Delete finished *lsp-installation* buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (and (lsp--installation-buffer? buf) (not (get-buffer-process buf)))
      (kill-buffer buf))))

(defun lsp--download-status ()
  (-some--> #'lsp--client-download-in-progress?
    (lsp--filter-clients it)
    (-map (-compose #'symbol-name #'lsp--client-server-id) it)
    (format "%s" it)
    (propertize it 'face 'success)
    (format " Installing following servers: %s" it)
    (propertize it
                'local-map (make-mode-line-mouse-map
                            'mouse-1 #'lsp-select-installation-buffer)
                'mouse-face 'highlight)))

(defun lsp--install-server-internal (client &optional update?)
  (unless (lsp--client-download-server-fn client)
    (user-error "There is no automatic installation for `%s', you have to install it manually following lsp-mode's documentation."
                (lsp--client-server-id client)))

  (setf (lsp--client-download-in-progress? client) t)
  (add-to-list 'global-mode-string '(t (:eval (lsp--download-status))))
  (cl-flet ((done
             (success? &optional error-message)
             ;; run with idle timer to make sure the lsp command is executed in
             ;; the main thread, see #2739.
             (run-with-timer
              0.0
              nil
              (lambda ()
                (-let [(&lsp-cln 'server-id 'buffers) client]
                  (setf (lsp--client-download-in-progress? client) nil
                        (lsp--client-buffers client) nil)
                  (if success?
                      (lsp--info "Server %s downloaded, auto-starting in %s buffers." server-id
                                 (length buffers))
                    (lsp--error "Server %s install process failed with the following error message: %s.
Check `*lsp-install*' and `*lsp-log*' buffer."
                                server-id
                                error-message))
                  (seq-do
                   (lambda (buffer)
                     (when (lsp-buffer-live-p buffer)
                       (lsp-with-current-buffer buffer
                         (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                                    global-mode-string)
                         (when success? (lsp)))))
                   buffers)
                  (unless (lsp--filter-clients #'lsp--client-download-in-progress?)
                    (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                               global-mode-string)))))))
    (lsp--info "Download %s started." (lsp--client-server-id client))
    (condition-case err
        (funcall
         (lsp--client-download-server-fn client)
         client
         (lambda () (done t))
         (lambda (msg) (done nil msg))
         update?)
      (error
       (done nil (error-message-string err))))))

(defun lsp--require-packages ()
  "Load `lsp-client-packages' if needed."
  (when (and lsp-auto-configure (not lsp--client-packages-required))
    (seq-do (lambda (package)
              ;; loading client is slow and `lsp' can be called repeatedly
              (unless (featurep package)
                (require package nil t)))
            lsp-client-packages)
    (setq lsp--client-packages-required t)))

;;;###autoload
(defun lsp-install-server (update? &optional server-id)
  "Interactively install or re-install server.
When prefix UPDATE? is t force installation even if the server is present."
  (interactive "P")
  (lsp--require-packages)
  (let* ((chosen-client (or (gethash server-id lsp-clients)
                            (lsp--completing-read
                             "Select server to install/re-install: "
                             (or (->> lsp-clients
                                      (ht-values)
                                      (-filter (-andfn
                                                (-not #'lsp--client-download-in-progress?)
                                                #'lsp--client-download-server-fn)))
                                 (user-error "There are no servers with automatic installation"))
                             (lambda (client)
                               (let ((server-name (-> client lsp--client-server-id symbol-name)))
                                 (if (lsp--server-binary-present? client)
                                     (concat server-name " (Already installed)")
                                   server-name)))
                             nil
                             t)))
         (update? (or update?
                      (and (not (lsp--client-download-in-progress? chosen-client))
                           (lsp--server-binary-present? chosen-client)))))
    (lsp--install-server-internal chosen-client update?)))

;;;###autoload
(defun lsp-uninstall-server (dir)
  "Delete a LSP server from `lsp-server-install-dir'."
  (interactive
   (list (read-directory-name "Uninstall LSP server: " (f-slash lsp-server-install-dir))))
  (unless (file-directory-p dir)
    (user-error "Couldn't find %s directory" dir))
  (delete-directory dir 'recursive)
  (message "Server `%s' uninstalled." (file-name-nondirectory (directory-file-name dir))))

;;;###autoload
(defun lsp-uninstall-servers ()
  "Uninstall all installed servers."
  (interactive)
  (let* ((dir lsp-server-install-dir)
         (servers (ignore-errors
                    (directory-files dir t
                                     directory-files-no-dot-files-regexp))))
    (if (or (not (file-directory-p dir)) (zerop (length servers)))
        (user-error "No servers to uninstall")
      (when (yes-or-no-p
             (format "Servers to uninstall: %d (%s), proceed? "
                     (length servers)
                     (mapconcat (lambda (server)
                                  (file-name-nondirectory (directory-file-name server)))
                                servers " ")))
        (mapc #'lsp-uninstall-server servers)
        (message "All servers uninstalled")))))

;;;###autoload
(defun lsp-update-server (&optional server-id)
  "Interactively update (reinstall) a server."
  (interactive)
  (lsp--require-packages)
  (let ((chosen-client (or (gethash server-id lsp-clients)
                           (lsp--completing-read
                            "Select server to update (if not on the list, probably you need to `lsp-install-server`): "
                            (or (->> lsp-clients
                                     (ht-values)
                                     (-filter (-andfn
                                               (-not #'lsp--client-download-in-progress?)
                                               #'lsp--client-download-server-fn
                                               #'lsp--server-binary-present?)))
                                (user-error "There are no servers to update"))
                            (lambda (client)
                              (-> client lsp--client-server-id symbol-name))
                            nil
                            t))))
    (lsp--install-server-internal chosen-client t)))

;;;###autoload
(defun lsp-update-servers ()
  "Update (reinstall) all installed servers."
  (interactive)
  (lsp--require-packages)
  (mapc (lambda (client) (lsp--install-server-internal client t))
        (-filter (-andfn
                  (-not #'lsp--client-download-in-progress?)
                  #'lsp--client-download-server-fn
                  #'lsp--server-binary-present?) (hash-table-values lsp-clients))))

;;;###autoload
(defun lsp-ensure-server (server-id)
  "Ensure server SERVER-ID"
  (lsp--require-packages)
  (if-let ((client (gethash server-id lsp-clients)))
      (unless (lsp--server-binary-present? client)
        (lsp--info "Server `%s' is not preset, installing..." server-id)
        (lsp-install-server nil server-id))
    (warn "Unable to find server registration with id %s" server-id)))

(defun lsp-async-start-process (callback error-callback &rest command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (let ((name (cl-first command)))
    (with-current-buffer (compilation-start (mapconcat #'shell-quote-argument (-filter (lambda (cmd)
                                                                                         (not (null cmd)))
                                                                                       command)
                                                       " ") t
                                            (lambda (&rest _)
                                              (generate-new-buffer-name (format "*lsp-install: %s*" name))))
      (lsp-installation-buffer-mode +1)
      (view-mode +1)
      (add-hook
       'compilation-finish-functions
       (lambda (_buf status)
         (if (string= "finished\n" status)
             (condition-case err
                 (funcall callback)
               (error
                (funcall error-callback (error-message-string err))))
           (funcall error-callback (s-trim-right status))))
       nil t))))

(defun lsp-resolve-value (value)
  "Resolve VALUE's value.
If it is function - call it.
If it is a variable - return it's value
Otherwise returns value itself."
  (cond
   ((functionp value) (funcall value))
   ((and (symbolp value) (boundp value)) (symbol-value value))
   (value)))

(defvar lsp-deps-providers
  (list :npm (list :path #'lsp--npm-dependency-path
                   :install #'lsp--npm-dependency-install)
        :cargo (list :path #'lsp--cargo-dependency-path
                     :install #'lsp--cargo-dependency-install)
        :system (list :path #'lsp--system-path)
        :download (list :path #'lsp-download-path
                        :install #'lsp-download-install)))

(defun lsp--system-path (path)
  "If PATH is absolute and exists return it as is. Otherwise,
return the absolute path to the executable defined by PATH or
nil."
  ;; For node.js 'sub-packages' PATH may point to a *.js file. Consider the
  ;; typescript-language-server. When lsp invokes the server, lsp needs to
  ;; supply the path to the typescript compiler, tsserver.js, as an argument. To
  ;; make code platform independent, one must pass the absolute path to the
  ;; tsserver.js file (Windows requires a *.js file - see help on the JavaScript
  ;; child process spawn command that is invoked by the
  ;; typescript-language-server). This is why we check for existence and not
  ;; that the path is executable.
  (let ((path (lsp-resolve-value path)))
    (cond
     ((and (f-absolute? path)
           (f-exists? path))
      path)
     ((executable-find path t) path))))

(defun lsp-package-path (dependency)
  "Path to the DEPENDENCY each of the registered providers."
  (let (path)
    (-first (-lambda ((provider . rest))
              (setq path (-some-> lsp-deps-providers
                           (plist-get provider)
                           (plist-get :path)
                           (apply rest))))
            (gethash dependency lsp--dependencies))
    path))

(defun lsp-package-ensure (dependency callback error-callback)
  "Asynchronously ensure a package."
  (or (-first (-lambda ((provider . rest))
                (-some-> lsp-deps-providers
                  (plist-get provider)
                  (plist-get :install)
                  (apply (cl-list* callback error-callback rest))))
              (gethash dependency lsp--dependencies))
      (funcall error-callback (format "Unable to find a way to install %s" dependency))))


;; npm handling

;; https://docs.npmjs.com/files/folders#executables
(cl-defun lsp--npm-dependency-path (&key package path &allow-other-keys)
  "Return npm dependency PATH for PACKAGE."
  (let ((path (executable-find
               (f-join lsp-server-install-dir "npm" package
                       (cond ((eq system-type 'windows-nt) "")
                             (t "bin"))
                       path)
               t)))
    (unless (and path (f-exists? path))
      (error "The package %s is not installed.  Unable to find %s" package path))
    path))

(cl-defun lsp--npm-dependency-install (callback error-callback &key package &allow-other-keys)
  (if-let ((npm-binary (executable-find "npm")))
      (progn
        ;; Explicitly `make-directory' to work around NPM bug in
        ;; versions 7.0.0 through 7.4.1. See
        ;; https://github.com/emacs-lsp/lsp-mode/issues/2364 for
        ;; discussion.
        (make-directory (f-join lsp-server-install-dir "npm" package "lib") 'parents)
        (lsp-async-start-process (lambda ()
                                   (if (string-empty-p
                                        (string-trim (shell-command-to-string
                                                      (mapconcat #'shell-quote-argument `(,npm-binary "view" ,package "peerDependencies") " "))))
                                       (funcall callback)
                                     (let ((default-directory (f-dirname (car (last (directory-files-recursively (f-join lsp-server-install-dir "npm" package) "package.json")))))
                                           (process-environment (append '("npm_config_yes=true") process-environment))) ;; Disable prompting for older versions of npx
                                       (when (f-dir-p default-directory)
                                         (lsp-async-start-process callback
                                                                  error-callback
                                                                  (executable-find "npx")
                                                                  "npm-install-peers")))))
                                 error-callback
                                 npm-binary
                                 "-g"
                                 "--prefix"
                                 (f-join lsp-server-install-dir "npm" package)
                                 "install"
                                 package))
    (lsp-log "Unable to install %s via `npm' because it is not present" package)
    nil))


;; Cargo dependency handling
(cl-defun lsp--cargo-dependency-path (&key package path &allow-other-keys)
  (let ((path (executable-find
               (f-join lsp-server-install-dir
                       "cargo"
                       package
                       "bin"
                       path)
               t)))
    (unless (and path (f-exists? path))
      (error "The package %s is not installed.  Unable to find %s" package path))
    path))

(cl-defun lsp--cargo-dependency-install (callback error-callback &key package git &allow-other-keys)
  (if-let ((cargo-binary (executable-find "cargo")))
      (lsp-async-start-process
       callback
       error-callback
       cargo-binary
       "install"
       package
       (when git
         "--git")
       git
       "--root"
       (f-join lsp-server-install-dir "cargo" package))
    (lsp-log "Unable to install %s via `cargo' because it is not present" package)
    nil))



;; Download URL handling
(cl-defun lsp-download-install (callback error-callback &key url asc-url pgp-key store-path decompress &allow-other-keys)
  (let* ((url (lsp-resolve-value url))
         (store-path (lsp-resolve-value store-path))
         ;; (decompress (lsp-resolve-value decompress))
         (download-path
          (pcase decompress
            (:gzip (concat store-path ".gz"))
            (:zip (concat store-path ".zip"))
            (:targz (concat store-path ".tar.gz"))
            (`nil store-path)
            (_ (error ":decompress must be `:gzip', `:zip', `:targz' or `nil'")))))
    (make-thread
     (lambda ()
       (condition-case err
           (progn
             (when (f-exists? download-path)
               (f-delete download-path))
             (when (f-exists? store-path)
               (f-delete store-path))
             (lsp--info "Starting to download %s to %s..." url download-path)
             (mkdir (f-parent download-path) t)
             (url-copy-file url download-path)
             (lsp--info "Finished downloading %s..." download-path)
             (when (and lsp-verify-signature asc-url pgp-key)
               (if (executable-find epg-gpg-program)
                   (let ((asc-download-path (concat download-path ".asc"))
                         (context (epg-make-context))
                         (fingerprint)
                         (signature))
                     (when (f-exists? asc-download-path)
                       (f-delete asc-download-path))
                     (lsp--info "Starting to download %s to %s..." asc-url asc-download-path)
                     (url-copy-file asc-url asc-download-path)
                     (lsp--info "Finished downloading %s..." asc-download-path)
                     (epg-import-keys-from-string context pgp-key)
                     (setq fingerprint (epg-import-status-fingerprint
                                        (car
                                         (epg-import-result-imports
                                          (epg-context-result-for context 'import)))))
                     (lsp--info "Verifying signature %s..." asc-download-path)
                     (epg-verify-file context asc-download-path download-path)
                     (setq signature (car (epg-context-result-for context 'verify)))
                     (unless (and
                              (eq (epg-signature-status signature) 'good)
                              (equal (epg-signature-fingerprint signature) fingerprint))
                       (error "Failed to verify GPG signature: %s" (epg-signature-to-string signature))))
                 (lsp--warn "GPG is not installed, skipping the signature check.")))
             (when decompress
               (lsp--info "Decompressing %s..." download-path)
               (pcase decompress
                 (:gzip
                  (lsp-gunzip download-path))
                 (:zip (lsp-unzip download-path (f-parent store-path)))
                 (:targz (lsp-tar-gz-decompress download-path (f-parent store-path))))
               (lsp--info "Decompressed %s..." store-path))
             (funcall callback))
         (error (funcall error-callback err)))))))

(cl-defun lsp-download-path (&key store-path binary-path set-executable? &allow-other-keys)
  "Download URL and store it into STORE-PATH.

SET-EXECUTABLE? when non-nil change the executable flags of
STORE-PATH to make it executable. BINARY-PATH can be specified
when the binary to start does not match the name of the
archive (e.g. when the archive has multiple files)"
  (let ((store-path (or (lsp-resolve-value binary-path)
                        (lsp-resolve-value store-path))))
    (cond
     ((executable-find store-path) store-path)
     ((and set-executable? (f-exists? store-path))
      (set-file-modes store-path #o0700)
      store-path)
     ((f-exists? store-path) store-path))))

(defun lsp--find-latest-gh-release-url (url regex)
  "Fetch the latest version in the releases given by URL by using REGEX."
  (let ((url-request-method "GET"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'noerror)
      (delete-region (point-min) (point))
      (let* ((json-result (lsp-json-read-buffer)))
        (message "Latest version found: %s" (lsp-get json-result :tag_name))
        (--> json-result
             (lsp-get it :assets)
             (seq-find (lambda (entry) (string-match-p regex (lsp-get entry :name))) it)
             (lsp-get it :browser_download_url))))))

;; unzip

(defconst lsp-ext-pwsh-script "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst lsp-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defcustom lsp-unzip-script (lambda ()
                              (cond ((executable-find "unzip") lsp-ext-unzip-script)
                                    ((executable-find "powershell") lsp-ext-pwsh-script)
                                    (t nil)))
  "The script to unzip."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-unzip (zip-file dest)
  "Unzip ZIP-FILE to DEST."
  (unless lsp-unzip-script
    (error "Unable to find `unzip' or `powershell' on the path, please customize `lsp-unzip-script'"))
  (shell-command (format (lsp-resolve-value lsp-unzip-script) zip-file dest)))

;; gunzip

(defconst lsp-ext-gunzip-script "gzip -d %1$s"
  "Script to decompress a gzippped file with gzip.")

(defcustom lsp-gunzip-script (lambda ()
                               (cond ((executable-find "gzip") lsp-ext-gunzip-script)
                                     (t nil)))
  "The script to decompress a gzipped file.
Should be a format string with one argument for the file to be decompressed
in place."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-gunzip (gz-file)
  "Decompress GZ-FILE in place."
  (unless lsp-gunzip-script
    (error "Unable to find `gzip' on the path, please either customize `lsp-gunzip-script' or manually decompress %s" gz-file))
  (shell-command (format (lsp-resolve-value lsp-gunzip-script) gz-file)))

;; tar.gz decompression

(defconst lsp-ext-tar-script "bash -c 'mkdir -p %2$s; tar xf %1$s --directory=%2$s'"
  "Script to decompress a .tar.gz file.")

(defcustom lsp-tar-script (lambda ()
                            (cond ((executable-find "tar") lsp-ext-tar-script)
                                  (t nil)))
  "The script to decompress a .tar.gz file.
Should be a format string with one argument for the file to be decompressed
in place."
  :group 'lsp-mode
  :type 'string)

(defun lsp-tar-gz-decompress (targz-file dest)
  "Decompress TARGZ-FILE in DEST."
  (unless lsp-tar-script
    (error "Unable to find `tar' on the path, please either customize `lsp-tar-script' or manually decompress %s" targz-file))
  (shell-command (format (lsp-resolve-value lsp-tar-script) targz-file dest)))


;; VSCode marketplace

(defcustom lsp-vscode-ext-url
  "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/%s/vsextensions/%s/%s/vspackage%s"
  "Vscode extension template url."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(defun lsp-vscode-extension-url (publisher name version &optional targetPlatform)
  "Return the URL to vscode extension.
PUBLISHER is the extension publisher.
NAME is the name of the extension.
VERSION is the version of the extension.
TARGETPLATFORM is the targetPlatform of the extension."
  (format lsp-vscode-ext-url publisher name version (or targetPlatform "")))



;; Queueing prompts

(defvar lsp--question-queue nil
  "List of questions yet to be asked by `lsp-ask-question'.")

(defun lsp-ask-question (question options callback)
  "Prompt the user to answer the QUESTION with one of the OPTIONS from the
minibuffer. Once the user selects an option, the CALLBACK function will be
called, passing the selected option to it.

If the user is currently being shown a question, the question will be stored in
`lsp--question-queue', and will be asked once the user has answered the current
question."
  (add-to-list 'lsp--question-queue `(("question" . ,question)
                                      ("options" . ,options)
                                      ("callback" . ,callback)) t)
  (when (eq (length lsp--question-queue) 1)
    (lsp--process-question-queue)))

(defun lsp--process-question-queue ()
  "Take the first question from `lsp--question-queue', process it, then process
the next question until the queue is empty."
  (-let* (((&alist "question" "options" "callback") (car lsp--question-queue))
          (answer (completing-read question options nil t)))
    (pop lsp--question-queue)
    (funcall callback answer)
    (when lsp--question-queue
      (lsp--process-question-queue))))

(defun lsp--supports-buffer? (client)
  (and
   ;; both file and client remote or both local
   (eq (---truthy? (file-remote-p (buffer-file-name)))
       (---truthy? (lsp--client-remote? client)))

   ;; activation function or major-mode match.
   (if-let ((activation-fn (lsp--client-activation-fn client)))
       (funcall activation-fn (buffer-file-name) major-mode)
     (-contains? (lsp--client-major-modes client) major-mode))

   ;; check whether it is enabled if `lsp-enabled-clients' is not null
   (or (null lsp-enabled-clients)
       (or (member (lsp--client-server-id client) lsp-enabled-clients)
           (ignore (lsp--info "Client %s is not in lsp-enabled-clients"
                              (lsp--client-server-id client)))))

   ;; check whether it is not disabled.
   (not (lsp--client-disabled-p major-mode (lsp--client-server-id client)))))

(defun lsp--filter-clients (pred)
  (->> lsp-clients hash-table-values (-filter pred)))

(defun lsp--find-clients ()
  "Find clients which can handle current buffer."
  (-when-let (matching-clients (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                            #'lsp--server-binary-present?)))
    (lsp-log "Found the following clients for %s: %s"
             (buffer-file-name)
             (s-join ", "
                     (-map (lambda (client)
                             (format "(server-id %s, priority %s)"
                                     (lsp--client-server-id client)
                                     (lsp--client-priority client)))
                           matching-clients)))
    (-let* (((add-on-clients main-clients) (-separate #'lsp--client-add-on? matching-clients))
            (selected-clients (if-let ((main-client (and main-clients
                                                         (--max-by (> (lsp--client-priority it)
                                                                      (lsp--client-priority other))
                                                                   main-clients))))
                                  (cons main-client add-on-clients)
                                add-on-clients)))
      (lsp-log "The following clients were selected based on priority: %s"
               (s-join ", "
                       (-map (lambda (client)
                               (format "(server-id %s, priority %s)"
                                       (lsp--client-server-id client)
                                       (lsp--client-priority client)))
                             selected-clients)))
      selected-clients)))

(defun lsp-workspace-remove-all-folders()
  "Delete all lsp tracked folders."
  (interactive)
  (--each (lsp-session-folders (lsp-session))
    (lsp-workspace-folders-remove it)))

(defun lsp-register-client (client)
  "Registers LSP client CLIENT."
  (let ((client-id (lsp--client-server-id client)))
    (puthash client-id client lsp-clients)
    (setplist (intern (format "lsp-%s-after-open-hook" client-id))
              `( standard-value (nil) custom-type hook
                 custom-package-version (lsp-mode . "7.0.1")
                 variable-documentation ,(format "Hooks to run after `%s' server is run." client-id)
                 custom-requests nil)))
  (when (and lsp-auto-register-remote-clients
             (not (lsp--client-remote? client)))
    (let ((remote-client (copy-lsp--client client)))
      (setf (lsp--client-remote? remote-client) t
            (lsp--client-server-id remote-client) (intern
                                                   (format "%s-tramp"
                                                           (lsp--client-server-id client)))
            ;; disable automatic download
            (lsp--client-download-server-fn remote-client) nil)
      (lsp-register-client remote-client))))

(defun lsp--create-initialization-options (_session client)
  "Create initialization-options from SESSION and CLIENT.
Add workspace folders depending on server being multiroot and
session workspace folder configuration for the server."
  (let* ((initialization-options-or-fn (lsp--client-initialization-options client)))
    (if (functionp initialization-options-or-fn)
        (funcall initialization-options-or-fn)
      initialization-options-or-fn)))

(defvar lsp-client-settings (make-hash-table :test 'equal)
  "For internal use, any external users please use
  `lsp-register-custom-settings' function instead")

(defun lsp-register-custom-settings (props)
  "Register PROPS.
PROPS is list of triple (path value boolean?) where PATH is the path to the
property; VALUE can be a literal value, symbol to be evaluated, or either a
function or lambda function to be called without arguments; BOOLEAN? is an
optional flag that should be non-nil for boolean settings, when it is nil the
property will be ignored if the VALUE is nil.

Example: `(lsp-register-custom-settings `((\"foo.bar.buzz.enabled\" t t)))'
\(note the double parentheses)"
  (mapc
   (-lambda ((path . rest))
     (puthash path rest lsp-client-settings))
   props))

(defun lsp-region-text (region)
  "Get the text for REGION in current buffer."
  (-let (((start . end) (lsp--range-to-region region)))
    (buffer-substring-no-properties start end)))

(defun lsp-ht-set (tbl paths value)
  "Set nested hash table value.
TBL - a hash table, PATHS is the path to the nested VALUE."
  (pcase paths
    (`(,path) (ht-set! tbl path value))
    (`(,path . ,rst) (let ((nested-tbl (or (gethash path tbl)
                                           (let ((temp-tbl (ht)))
                                             (ht-set! tbl path temp-tbl)
                                             temp-tbl))))
                       (lsp-ht-set nested-tbl rst value)))))

;; sections

(defalias 'defcustom-lsp 'lsp-defcustom)

(defmacro lsp-defcustom (symbol standard doc &rest args)
  "Defines `lsp-mode' server property."
  (declare (doc-string 3) (debug (name body))
           (indent defun))
  (let ((path (plist-get args :lsp-path)))
    (cl-remf args :lsp-path)
    `(progn
       (lsp-register-custom-settings
        (quote ((,path ,symbol ,(equal ''boolean (plist-get args :type))))))

       (defcustom ,symbol ,standard ,doc
         :set (lambda (sym val)
                (lsp--set-custom-property sym val ,path))
         ,@args))))

(defun lsp--set-custom-property (sym val path)
  (set sym val)
  (let ((section (cl-first (s-split "\\." path))))
    (mapc (lambda (workspace)
            (when (-contains? (lsp--client-synchronize-sections (lsp--workspace-client workspace))
                              section)
              (with-lsp-workspace workspace
                (lsp--set-configuration (lsp-configuration-section section)))))
          (lsp--session-workspaces (lsp-session)))))

(defun lsp-configuration-section (section)
  "Get settings for SECTION."
  (let ((ret (ht-create)))
    (maphash (-lambda (path (variable boolean?))
               (when (s-matches? (concat (regexp-quote section) "\\..*") path)
                 (let* ((symbol-value (-> variable
                                          lsp-resolve-value
                                          lsp-resolve-value))
                        (value (if (and boolean? (not symbol-value))
                                   :json-false
                                 symbol-value)))
                   (when (or boolean? value)
                     (lsp-ht-set ret (s-split "\\." path) value)))))
             lsp-client-settings)
    ret))


(defun lsp--start-connection (session client project-root)
  "Initiates connection created from CLIENT for PROJECT-ROOT.
SESSION is the active session."
  (when (lsp--client-multi-root client)
    (cl-pushnew project-root (gethash (lsp--client-server-id client)
                                      (lsp-session-server-id->folders session))))
  (run-hook-with-args 'lsp-workspace-folders-changed-functions (list project-root) nil)

  (unwind-protect
      (lsp--start-workspace session client project-root (lsp--create-initialization-options session client))
    (lsp--spinner-stop)))

;; lsp-log-io-mode

(defvar lsp-log-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'lsp-log-io-next)
    (define-key map (kbd "M-p") #'lsp-log-io-prev)
    (define-key map (kbd "k") #'lsp--erase-log-buffer)
    (define-key map (kbd "K") #'lsp--erase-session-log-buffers)
    map)
  "Keymap for lsp log buffer mode.")

(define-derived-mode lsp-log-io-mode special-mode "LspLogIo"
  "Special mode for viewing IO logs.")

(defun lsp-workspace-show-log (workspace)
  "Display the log buffer of WORKSPACE."
  (interactive
   (list (if lsp-log-io
             (if (eq (length (lsp-workspaces)) 1)
                 (cl-first (lsp-workspaces))
               (lsp--completing-read "Workspace: " (lsp-workspaces)
                                     #'lsp--workspace-print nil t))
           (user-error "IO logging is disabled"))))
  (pop-to-buffer (lsp--get-log-buffer-create workspace)))

(defalias 'lsp-switch-to-io-log-buffer 'lsp-workspace-show-log)

(defun lsp--get-log-buffer-create (workspace)
  "Return the lsp log buffer of WORKSPACE, creating a new one if needed."
  (let* ((server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name))
         (pid (-> workspace lsp--workspace-cmd-proc lsp-process-id)))
    (get-buffer-create (format "*lsp-log: %s:%s*" server-id pid))))

(defun lsp--erase-log-buffer (&optional all)
  "Delete contents of current lsp log buffer.
When ALL is t, erase all log buffers of the running session."
  (interactive)
  (let* ((workspaces (lsp--session-workspaces (lsp-session)))
         (current-log-buffer (current-buffer)))
    (dolist (w workspaces)
      (let ((b (lsp--get-log-buffer-create w)))
        (when (or all (eq b current-log-buffer))
          (with-current-buffer b
            (let ((inhibit-read-only t))
              (erase-buffer))))))))

(defun lsp--erase-session-log-buffers ()
  "Erase log buffers of the running session."
  (interactive)
  (lsp--erase-log-buffer t))

(defun lsp-log-io-next (arg)
  "Move to next log entry."
  (interactive "P")
  (ewoc-goto-next lsp--log-io-ewoc (or arg 1)))

(defun lsp-log-io-prev (arg)
  "Move to previous log entry."
  (interactive "P")
  (ewoc-goto-prev lsp--log-io-ewoc (or arg 1)))



(cl-defmethod lsp-process-id ((process process))
  (process-id process))

(cl-defmethod lsp-process-name ((process process)) (process-name process))

(cl-defmethod lsp-process-status ((process process)) (process-status process))

(cl-defmethod lsp-process-kill ((process process))
  (when (process-live-p process)
    (kill-process process)))

(cl-defmethod lsp-process-send ((process process) message)
  (condition-case err
      (process-send-string process (lsp--make-message message))
    (error (lsp--error "Sending to process failed with the following error: %s"
                       (error-message-string err)))))

(cl-defmethod lsp-process-cleanup (process)
  ;; Kill standard error buffer only if the process exited normally.
  ;; Leave it intact otherwise for debugging purposes.
  (let ((buffer (-> process process-name get-buffer)))
    (when (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process))
               (buffer-live-p buffer))
      (kill-buffer buffer))))


;; native JSONRPC

(declare-function json-rpc "ext:json")
(declare-function json-rpc-connection "ext:json")
(declare-function json-rpc-send "ext:json")
(declare-function json-rpc-shutdown "ext:json")
(declare-function json-rpc-stderr "ext:json")
(declare-function json-rpc-pid "ext:json")

(defvar lsp-json-rpc-thread nil)
(defvar lsp-json-rpc-queue nil)
(defvar lsp-json-rpc-done nil)
(defvar lsp-json-rpc-mutex (make-mutex))
(defvar lsp-json-rpc-condition (make-condition-variable lsp-json-rpc-mutex))

(defun lsp-json-rpc-process-queue ()
  (while (not lsp-json-rpc-done)
    (while lsp-json-rpc-queue
      (-let (((proc . message) (pop lsp-json-rpc-queue)))
        (json-rpc-send
         proc message
         :null-object nil
         :false-object :json-false)))
    (with-mutex lsp-json-rpc-mutex
      (condition-wait lsp-json-rpc-condition))))

(cl-defmethod lsp-process-id (process) (json-rpc-pid process))

(cl-defmethod lsp-process-name (_process) "TBD")

(cl-defmethod lsp-process-kill (process) (json-rpc-shutdown process))

(cl-defmethod lsp-process-send (proc message)
  (unless lsp-json-rpc-thread
    (with-current-buffer (get-buffer-create " *json-rpc*")
      (setq lsp-json-rpc-thread (make-thread #'lsp-json-rpc-process-queue "*json-rpc-queue*"))))

  (with-mutex lsp-json-rpc-mutex
    (setq lsp-json-rpc-queue (append lsp-json-rpc-queue
                                     (list (cons proc message))))
    (condition-notify lsp-json-rpc-condition)))

(cl-defmethod lsp-process-cleanup (_proc))

(defun lsp-json-rpc-connection (workspace command)
  (let ((con (apply #'json-rpc-connection command))
        (object-type (if lsp-use-plists 'plist 'hash-table)))
    (with-current-buffer (get-buffer-create " *json-rpc*")
      (make-thread
       (lambda ()
         (json-rpc
          con
          (lambda (result err done)
            (run-with-timer
             0.0
             nil
             (lambda ()
               (cond
                (result (lsp--parser-on-message result workspace))
                (err (warn "Json parsing failed with the following error: %s" err))
                (done (lsp--handle-process-exit workspace ""))))))
          :object-type object-type
          :null-object nil
          :false-object nil))
       "*json-rpc-connection*"))
    (cons con con)))

(defun lsp-json-rpc-stderr ()
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (let ((content (json-rpc-stderr (lsp--workspace-cmd-proc it)))
          (buffer (format "*stderr-%s*" (lsp--workspace-print it)) ))
      (with-current-buffer (get-buffer-create buffer)
        (with-help-window buffer
          (insert content))))))


(defun lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
         (status (lsp--workspace-status workspace))
         (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name))
         (pid (lsp-process-id proc)))

    (if (eq 'initialized status)
        (format "%s:%s" server-id pid)
      (format "%s:%s/%s" server-id pid status))))

(defun lsp--map-tree-widget (m)
  "Build `tree-widget' from a hash-table or plist M."
  (when (lsp-structure-p m)
    (let (nodes)
      (lsp-map (lambda (k v)
                 (push `(tree-widget
                         :tag ,(if (lsp-structure-p v)
                                   (format "%s:" k)
                                 (format "%s: %s" k
                                         (propertize (format "%s" v)
                                                     'face
                                                     'font-lock-string-face)))
                         :open t
                         ,@(lsp--map-tree-widget v))
                       nodes))
               m)
      nodes)))

(defun lsp-buffer-name (buffer-id)
  (if-let ((buffer-name (plist-get buffer-id :buffer-name)))
      (funcall buffer-name buffer-id)
    (buffer-name buffer-id)))

(defun lsp--render-workspace (workspace)
  "Tree node representation of WORKSPACE."
  `(tree-widget :tag ,(lsp--workspace-print workspace)
                :open t
                (tree-widget :tag ,(propertize "Buffers" 'face 'font-lock-function-name-face)
                             :open t
                             ,@(->> workspace
                                    (lsp--workspace-buffers)
                                    (--map `(tree-widget
                                             :tag ,(when (lsp-buffer-live-p it)
                                                     (let ((buffer-name (lsp-buffer-name it)))
                                                       (if (lsp-with-current-buffer it buffer-read-only)
                                                           (propertize buffer-name 'face 'font-lock-constant-face)
                                                         buffer-name)))))))
                (tree-widget :tag ,(propertize "Capabilities" 'face 'font-lock-function-name-face)
                             ,@(-> workspace lsp--workspace-server-capabilities lsp--map-tree-widget))))

(define-derived-mode lsp-browser-mode special-mode "LspBrowser"
  "Define mode for displaying lsp sessions."
  (setq-local display-buffer-base-action '(nil . ((inhibit-same-window . t)))))

(defun lsp-describe-session ()
  "Describes current `lsp-session'."
  (interactive)
  (let ((session (lsp-session))
        (buf (get-buffer-create "*lsp session*"))
        (root (lsp-workspace-root)))
    (with-current-buffer buf
      (lsp-browser-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (--each (lsp-session-folders session)
          (widget-create
           `(tree-widget
             :tag ,(propertize it 'face 'font-lock-keyword-face)
             :open t
             ,@(->> session
                    (lsp-session-folder->servers)
                    (gethash it)
                    (-map 'lsp--render-workspace)))))))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (cl-loop for tag = (widget-get (widget-get (widget-at) :node) :tag)
             until (or (and root (string= tag root)) (eobp))
             do (goto-char (next-overlay-change (point))))))

(defun lsp--session-workspaces (session)
  "Get all workspaces that are part of the SESSION."
  (-> session lsp-session-folder->servers hash-table-values -flatten -uniq))

(defun lsp--find-multiroot-workspace (session client project-root)
  "Look for a multiroot connection in SESSION created from CLIENT for
PROJECT-ROOT and BUFFER-MAJOR-MODE."
  (when (lsp--client-multi-root client)
    (-when-let (multi-root-workspace (->> session
                                          (lsp--session-workspaces)
                                          (--first (eq (-> it lsp--workspace-client lsp--client-server-id)
                                                       (lsp--client-server-id client)))))
      (with-lsp-workspace multi-root-workspace
        (lsp-notify "workspace/didChangeWorkspaceFolders"
                    (lsp-make-did-change-workspace-folders-params
                     :event (lsp-make-workspace-folders-change-event
                             :added (vector (lsp-make-workspace-folder
                                             :uri (lsp--path-to-uri project-root)
                                             :name (f-filename project-root)))
                             :removed []))))

      (->> session (lsp-session-folder->servers) (gethash project-root) (cl-pushnew multi-root-workspace))
      (->> session (lsp-session-server-id->folders) (gethash (lsp--client-server-id client)) (cl-pushnew project-root))

      (lsp--persist-session session)

      (lsp--info "Opened folder %s in workspace %s" project-root (lsp--workspace-print multi-root-workspace))
      (lsp--open-in-workspace multi-root-workspace)

      multi-root-workspace)))

(defun lsp--ensure-lsp-servers (session clients project-root ignore-multi-folder)
  "Ensure that SESSION contain server CLIENTS created for PROJECT-ROOT.
IGNORE-MULTI-FOLDER to ignore multi folder server."
  (-map (lambda (client)
          (or
           (lsp--find-workspace session client project-root)
           (unless ignore-multi-folder
             (lsp--find-multiroot-workspace session client project-root))
           (lsp--start-connection session client project-root)))
        clients))

(defun lsp--spinner-stop ()
  "Stop the spinner in case all of the workspaces are started."
  (when (--all? (eq (lsp--workspace-status it) 'initialized)
                lsp--buffer-workspaces)
    (spinner-stop)))

(defun lsp--open-in-workspace (workspace)
  "Open in existing WORKSPACE."
  (if (eq 'initialized (lsp--workspace-status workspace))
      ;; when workspace is initialized just call document did open.
      (progn
        (with-lsp-workspace workspace
          (when-let ((before-document-open-fn (-> workspace
                                                  lsp--workspace-client
                                                  lsp--client-before-file-open-fn)))
            (funcall before-document-open-fn workspace))
          (lsp--text-document-did-open))
        (lsp--spinner-stop))
    ;; when it is not initialized
    (lsp--spinner-start)
    (cl-pushnew (lsp-current-buffer) (lsp--workspace-buffers workspace))))

(defun lsp--find-workspace (session client project-root)
  "Find server connection created with CLIENT in SESSION for PROJECT-ROOT."
  (when-let ((workspace (->> session
                             (lsp-session-folder->servers)
                             (gethash project-root)
                             (--first (eql (-> it lsp--workspace-client lsp--client-server-id)
                                           (lsp--client-server-id client))))))
    (lsp--open-in-workspace workspace)
    workspace))

(defun lsp--read-char (prompt &optional options)
  "Wrapper for `read-char-from-minibuffer' if Emacs +27.
Fallback to `read-key' otherwise.
PROMPT is the message and OPTIONS the available options."
  (if (fboundp 'read-char-from-minibuffer)
      (read-char-from-minibuffer prompt options)
    (read-key prompt)))

(defun lsp--find-root-interactively (session)
  "Find project interactively.
Returns nil if the project should not be added to the current SESSION."
  (condition-case nil
      (let* ((project-root-suggestion (or (lsp--suggest-project-root) default-directory))
             (action (lsp--read-char
                      (format
                       "%s is not part of any project.

%s ==> Import project root %s
%s ==> Import project by selecting root directory interactively
%s ==> Import project at current directory %s
%s ==> Do not ask again for the current project by adding %s to lsp-session-folders-blocklist
%s ==> Do not ask again for the current project by selecting ignore path interactively
%s ==> Do nothing: ask again when opening other files from the current project

Select action: "
                       (propertize (buffer-name) 'face 'bold)
                       (propertize "i" 'face 'success)
                       (propertize project-root-suggestion 'face 'bold)
                       (propertize "I" 'face 'success)
                       (propertize "." 'face 'success)
                       (propertize default-directory 'face 'bold)
                       (propertize "d" 'face 'warning)
                       (propertize project-root-suggestion 'face 'bold)
                       (propertize "D" 'face 'warning)
                       (propertize "n" 'face 'warning))
                      '(?i ?\r ?I ?. ?d ?D ?n))))
        (cl-case action
          (?i project-root-suggestion)
          (?\r project-root-suggestion)
          (?I (read-directory-name "Select workspace folder to add: "
                                   (or project-root-suggestion default-directory)
                                   nil
                                   t))
          (?. default-directory)
          (?d (push project-root-suggestion (lsp-session-folders-blocklist session))
              (lsp--persist-session session)
              nil)
          (?D (push (read-directory-name "Select folder to blocklist: "
                                         (or project-root-suggestion default-directory)
                                         nil
                                         t)
                    (lsp-session-folders-blocklist session))
              (lsp--persist-session session)
              nil)
          (t nil)))
    (quit)))

(declare-function tramp-file-name-host "ext:tramp" (file) t)
(declare-function tramp-dissect-file-name "ext:tramp" (file &optional nodefault))

(defun lsp--files-same-host (f1 f2)
  "Predicate on whether or not two files are on the same host."
  (or (not (or (file-remote-p f1) (file-remote-p f2)))
      (and (file-remote-p f1)
           (file-remote-p f2)
           (progn (require 'tramp)
                  (equal (tramp-file-name-host (tramp-dissect-file-name f1))
                         (tramp-file-name-host (tramp-dissect-file-name f2)))))))

(defun lsp-find-session-folder (session file-name)
  "Look in the current SESSION for folder containing FILE-NAME."
  (let ((file-name-canonical (lsp-f-canonical file-name)))
    (->> session
         (lsp-session-folders)
         (--filter (and (lsp--files-same-host it file-name-canonical)
                        (or (lsp-f-same? it file-name-canonical)
                            (and (f-dir? it)
                                 (lsp-f-ancestor-of? it file-name-canonical)))))
         (--max-by (> (length it)
                      (length other))))))

(defun lsp-find-workspace (server-id &optional file-name)
  "Find workspace for SERVER-ID for FILE-NAME."
  (-when-let* ((session (lsp-session))
               (folder->servers (lsp-session-folder->servers session))
               (workspaces (if file-name
                               (gethash (lsp-find-session-folder session file-name) folder->servers)
                             (lsp--session-workspaces session))))

    (--first (eq (lsp--client-server-id (lsp--workspace-client it)) server-id) workspaces)))

(defun lsp--calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blocklist)
        (--first (and (lsp--files-same-host it file-name)
                      (lsp-f-ancestor-of? it file-name)
                      (prog1 t
                        (lsp--info "File %s is in blocklisted directory %s" file-name it))))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (unless lsp-guess-root-without-session
      (lsp-find-session-folder session file-name))
    (unless lsp-auto-guess-root
      (when-let ((root-folder (lsp--find-root-interactively session)))
        (if (or (not (f-equal? root-folder (expand-file-name "~/")))
                (yes-or-no-p
                 (concat
                  (propertize "[WARNING] " 'face 'warning)
                  "You are trying to import your home folder as project root. This may cause performance issue because some language servers (python, lua, etc) will try to scan all files under project root. To avoid that you may:

1. Use `I' option from the interactive project import to select subfolder(e. g. `~/foo/bar' instead of `~/').
2. If your file is under `~/' then create a subfolder and move that file in this folder.

Type `No' to go back to project selection.
Type `Yes' to confirm `HOME' as project root.
Type `C-g' to cancel project import process and stop `lsp'")))
            root-folder
          (lsp--calculate-root session file-name)))))))

(defun lsp--try-open-in-library-workspace ()
  "Try opening current file as library file in any of the active workspace.
The library folders are defined by each client for each of the active workspace."
  (when-let ((workspace (->> (lsp-session)
                             (lsp--session-workspaces)
                             ;; Sort the last active workspaces first as they are more likely to be
                             ;; the correct ones, especially when jumping to a definition.
                             (-sort (lambda (a _b)
                                      (-contains? lsp--last-active-workspaces a)))
                             (--first
                              (and (-> it lsp--workspace-client lsp--supports-buffer?)
                                   (when-let ((library-folders-fn
                                               (-> it lsp--workspace-client lsp--client-library-folders-fn)))
                                     (-first (lambda (library-folder)
                                               (lsp-f-ancestor-of? library-folder (buffer-file-name)))
                                             (funcall library-folders-fn it))))))))
    (lsp--open-in-workspace workspace)
    (view-mode t)
    (lsp--info "Opening read-only library file %s." (buffer-file-name))
    (list workspace)))

(defun lsp--persist-session (session)
  "Persist SESSION to `lsp-session-file'."
  (lsp--persist lsp-session-file (make-lsp-session
                                  :folders (lsp-session-folders session)
                                  :folders-blocklist (lsp-session-folders-blocklist session)
                                  :server-id->folders (lsp-session-server-id->folders session))))

(defun lsp--try-project-root-workspaces (ask-for-client ignore-multi-folder)
  "Try create opening file as a project file.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be opened in multi folder language server if there is
such."
  (-let ((session (lsp-session)))
    (-if-let (clients (if ask-for-client
                          (list (lsp--completing-read "Select server to start: "
                                                      (ht-values lsp-clients)
                                                      (-compose 'symbol-name 'lsp--client-server-id) nil t))
                        (lsp--find-clients)))
        (-if-let (project-root (-some-> session
                                 (lsp--calculate-root (buffer-file-name))
                                 (lsp-f-canonical)))
            (progn
              ;; update project roots if needed and persist the lsp session
              (unless (-contains? (lsp-session-folders session) project-root)
                (cl-pushnew project-root (lsp-session-folders session))
                (lsp--persist-session session))
              (lsp--ensure-lsp-servers session clients project-root ignore-multi-folder))
          (lsp--warn "%s not in project or it is blocklisted." (buffer-name))
          nil)
      (lsp--warn "No LSP server for %s(check *lsp-log*)." major-mode)
      nil)))

(defun lsp-shutdown-workspace ()
  "Shutdown language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) (when (y-or-n-p (format "Are you sure you want to stop the server %s?"
                                                       (lsp--workspace-print workspace)))
                                 workspace))
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp-workspace-shutdown it)))

(make-obsolete 'lsp-shutdown-workspace 'lsp-workspace-shutdown "lsp-mode 6.1")

(defcustom lsp-auto-select-workspace t
  "Shutdown or restart a single workspace.
If set and the current buffer has only a single workspace
associated with it, `lsp-shutdown-workspace' and
`lsp-restart-workspace' will act on it without asking."
  :type 'boolean
  :group 'lsp-mode)

(defun lsp--read-workspace ()
  "Ask the user to select a workspace.
Errors if there are none."
  (pcase (lsp-workspaces)
    (`nil (error "No workspaces associated with the current buffer"))
    ((and `(,workspace) (guard lsp-auto-select-workspace)) workspace)
    (workspaces (lsp--completing-read "Select workspace: " workspaces
                                      #'lsp--workspace-print nil t))))

(defun lsp-workspace-shutdown (workspace)
  "Shut the workspace WORKSPACE and the language server associated with it"
  (interactive (list (lsp--read-workspace)))
  (lsp--warn "Stopping %s" (lsp--workspace-print workspace))
  (with-lsp-workspace workspace (lsp--shutdown-workspace)))

(defun lsp-disconnect ()
  "Disconnect the buffer from the language server."
  (interactive)
  (lsp--text-document-did-close t)
  (lsp-managed-mode -1)
  (lsp-mode -1)
  (setq lsp--buffer-workspaces nil)
  (lsp--info "Disconnected"))

(defun lsp-restart-workspace ()
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp-workspace-restart it)))

(make-obsolete 'lsp-restart-workspace 'lsp-workspace-restart "lsp-mode 6.1")

(defun lsp-workspace-restart (workspace)
  "Restart the workspace WORKSPACE and the language server associated with it"
  (interactive (list (lsp--read-workspace)))
  (lsp--warn "Restarting %s" (lsp--workspace-print workspace))
  (with-lsp-workspace workspace (lsp--shutdown-workspace t)))

;;;###autoload
(defun lsp (&optional arg)
  "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start."
  (interactive "P")

  (lsp--require-packages)

  (when (buffer-file-name)
    (let (clients
          (matching-clients (lsp--filter-clients
                             (-andfn #'lsp--supports-buffer?
                                     #'lsp--server-binary-present?))))
      (cond
       (matching-clients
        (when (setq lsp--buffer-workspaces
                    (or (and
                         ;; Don't open as library file if file is part of a project.
                         (not (lsp-find-session-folder (lsp-session) (buffer-file-name)))
                         (lsp--try-open-in-library-workspace))
                        (lsp--try-project-root-workspaces (equal arg '(4))
                                                          (and arg (not (equal arg 1))))))
          (lsp-mode 1)
          (when lsp-auto-configure (lsp--auto-configure))
          (setq lsp-buffer-uri (lsp--buffer-uri))
          (lsp--info "Connected to %s."
                     (apply 'concat (--map (format "[%s %s]"
                                                   (lsp--workspace-print it)
                                                   (lsp--workspace-root it))
                                           lsp--buffer-workspaces)))))
       ;; look for servers which are currently being downloaded.
       ((setq clients (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                   #'lsp--client-download-in-progress?)))
        (lsp--info "There are language server(%s) installation in progress.
The server(s) will be started in the buffer when it has finished."
                   (-map #'lsp--client-server-id clients))
        (seq-do (lambda (client)
                  (cl-pushnew (current-buffer) (lsp--client-buffers client)))
                clients))
       ;; look for servers to install
       ((setq clients (lsp--filter-clients
                       (-andfn #'lsp--supports-buffer?
                               (-const lsp-enable-suggest-server-download)
                               #'lsp--client-download-server-fn
                               (-not #'lsp--client-download-in-progress?))))
        (let ((client (lsp--completing-read
                       (concat "Unable to find installed server supporting this file. "
                               "The following servers could be installed automatically: ")
                       clients
                       (-compose #'symbol-name #'lsp--client-server-id)
                       nil
                       t)))
          (cl-pushnew (current-buffer) (lsp--client-buffers client))
          (lsp--install-server-internal client)))
       ;; ignore other warnings
       ((not lsp-warn-no-matched-clients)
        nil)
       ;; automatic installation disabled
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                     #'lsp--client-download-server-fn
                                                     (-not (-const lsp-enable-suggest-server-download))
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but automatic download is disabled: %s
\(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no clients present
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but do not have automatic installation: %s
You may find the installation instructions at https://emacs-lsp.github.io/lsp-mode/page/languages.
\(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no matches
       ((-> #'lsp--supports-buffer? lsp--filter-clients not)
        (lsp--error "There are no language servers supporting current mode `%s' registered with `lsp-mode'.
This issue might be caused by:
1. The language you are trying to use does not have built-in support in `lsp-mode'. You must install the required support manually. Examples of this are `lsp-java' or `lsp-metals'.
2. The language server that you expect to run is not configured to run for major mode `%s'. You may check that by checking the `:major-modes' that are passed to `lsp-register-client'.
3. `lsp-mode' doesn't have any integration for the language behind `%s'. Refer to https://emacs-lsp.github.io/lsp-mode/page/languages and https://langserver.org/ .
4. You are over `tramp'. In this case follow https://emacs-lsp.github.io/lsp-mode/page/remote/.
5. You have disabled the `lsp-mode' clients for that file. (Check `lsp-enabled-clients' and `lsp-disabled-clients').
You can customize `lsp-warn-no-matched-clients' to disable this message."
                    major-mode major-mode major-mode))))))

(defun lsp--buffer-visible-p ()
  "Return non nil if current buffer is visible."
  (or (buffer-modified-p) (get-buffer-window nil t)))

(defun lsp--init-if-visible ()
  "Run `lsp' for the current buffer if the buffer is visible.
Returns non nil if `lsp' was run for the buffer."
  (when (lsp--buffer-visible-p)
    (remove-hook 'window-configuration-change-hook #'lsp--init-if-visible t)
    (lsp)
    t))

;;;###autoload
(defun lsp-deferred ()
  "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs."
  ;; Workspace may not be initialized yet. Use a buffer local variable to
  ;; remember that we deferred loading of this buffer.
  (setq lsp--buffer-deferred t)
  (let ((buffer (current-buffer)))
    ;; Avoid false positives as desktop-mode restores buffers by deferring
    ;; visibility check until the stack clears.
    (run-with-idle-timer 0 nil (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (unless (lsp--init-if-visible)
                                       (add-hook 'window-configuration-change-hook #'lsp--init-if-visible nil t))))))))



(defvar lsp-file-truename-cache (ht))

(defmacro lsp-with-cached-filetrue-name (&rest body)
  "Executes BODY caching the `file-truename' calls."
  `(let ((old-fn (symbol-function 'file-truename)))
     (unwind-protect
         (progn
           (fset 'file-truename
                 (lambda (file-name &optional counter prev-dirs)
                   (or (gethash file-name lsp-file-truename-cache)
                       (puthash file-name (apply old-fn (list file-name counter prev-dirs))
                                lsp-file-truename-cache))))
           ,@body)
       (fset 'file-truename old-fn))))


(defun lsp-virtual-buffer-call (key &rest args)
  (when lsp--virtual-buffer
    (when-let ((fn (plist-get lsp--virtual-buffer key)))
      (apply fn args))))

(defun lsp-translate-column (column)
  "Translate COLUMN taking into account virtual buffers."
  (or (lsp-virtual-buffer-call :real->virtual-char column)
      column))

(defun lsp-translate-line (line)
  "Translate LINE taking into account virtual buffers."
  (or (lsp-virtual-buffer-call :real->virtual-line line)
      line))


;; lsp internal validation.

(defmacro lsp--doctor (&rest checks)
  `(-let [buf (current-buffer)]
     (with-current-buffer (get-buffer-create "*lsp-performance*")
       (with-help-window (current-buffer)
         ,@(-map (-lambda ((msg form))
                   `(insert (format "%s: %s\n" ,msg
                                    (let ((res (with-current-buffer buf
                                                 ,form)))
                                      (cond
                                       ((eq res :optional) (propertize "OPTIONAL" 'face 'warning))
                                       (res (propertize "OK" 'face 'success))
                                       (t (propertize "ERROR" 'face 'error)))))))
                 (-partition 2 checks))))))

(define-obsolete-function-alias 'lsp-diagnose
  'lsp-doctor "lsp-mode 8.0.0")

(defun lsp-doctor ()
  "Validate performance settings."
  (interactive)
  (lsp--doctor
   "Checking for Native JSON support" (functionp 'json-serialize)
   "Check emacs supports `read-process-output-max'" (boundp 'read-process-output-max)
   "Check `read-process-output-max' default has been changed from 4k"
   (and (boundp 'read-process-output-max)
        (> read-process-output-max 4096))
   "Byte compiled against Native JSON (recompile lsp-mode if failing when Native JSON available)"
   (condition-case _err
       (progn (lsp--make-message (list "a" "b"))
              nil)
     (error t))
   "`gc-cons-threshold' increased?" (> gc-cons-threshold 800000)
   "Using `plist' for deserialized objects? (refer to https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)" (or lsp-use-plists :optional)
   "Using emacs 28+ with native compilation?"
   (or (and (fboundp 'native-comp-available-p)
            (native-comp-available-p))
       :optional)))

(declare-function package-version-join "ext:package")
(declare-function package-desc-version "ext:package")
(declare-function package--alist "ext:package")

(defun lsp-version ()
  "Return string describing current version of `lsp-mode'."
  (interactive)
  (unless (featurep 'package)
    (require 'package))
  (let ((ver (format "lsp-mode %s, Emacs %s, %s"
                     (package-version-join
                      (package-desc-version
                       (car (alist-get 'lsp-mode (package--alist)))))
                     emacs-version
                     system-type)))
    (if (called-interactively-p 'interactive)
        (lsp--info "%s" ver)
      ver)))



;; org-mode/virtual-buffer

(declare-function org-babel-get-src-block-info "ext:ob-core")
(declare-function org-do-remove-indentation "ext:org-macs")
(declare-function org-src-get-lang-mode "ext:org-src")
(declare-function org-element-context "ext:org-element")

(defun lsp--virtual-buffer-update-position ()
  (-if-let (virtual-buffer (-first (-lambda ((&plist :in-range))
                                     (funcall in-range))
                                   lsp--virtual-buffer-connections))
      (unless (equal virtual-buffer lsp--virtual-buffer)
        (lsp-org))
    (when lsp-managed-mode
      (lsp-managed-mode -1)
      (lsp-mode -1)
      (setq lsp--buffer-workspaces nil)
      (setq lsp--virtual-buffer nil)
      (setq lsp-buffer-uri nil)

      ;; force refresh of diagnostics
      (run-hooks 'lsp-after-diagnostics-hook))))

(defun lsp-virtual-buffer-on-change (start end length)
  "Adjust on change event to be executed against the proper language server."
  (let ((max-point (max end
                        (or (plist-get lsp--before-change-vals :end) 0)
                        (+ start length))))
    (when-let ((virtual-buffer (-first (lambda (vb)
                                         (let ((lsp--virtual-buffer vb))
                                           (and (lsp-virtual-buffer-call :in-range start)
                                                (lsp-virtual-buffer-call :in-range max-point))))
                                       lsp--virtual-buffer-connections)))
      (lsp-with-current-buffer virtual-buffer
        (lsp-on-change start end length
                       (lambda (&rest _)
                         (list :range (lsp--range (list :character 0 :line 0)
                                                  lsp--virtual-buffer-point-max)
                               :text (lsp--buffer-content))))))))

(defun lsp-virtual-buffer-before-change (start _end)
  (when-let ((virtual-buffer (-first (lambda (vb)
                                       (lsp-with-current-buffer vb
                                         (lsp-virtual-buffer-call :in-range start)))
                                     lsp--virtual-buffer-connections)))
    (lsp-with-current-buffer virtual-buffer
      (setq lsp--virtual-buffer-point-max
            (lsp--point-to-position (lsp-virtual-buffer-call :last-point))))))

(defun lsp-patch-on-change-event ()
  (remove-hook 'after-change-functions #'lsp-on-change t)
  (add-hook 'after-change-functions #'lsp-virtual-buffer-on-change nil t)
  (add-hook 'before-change-functions #'lsp-virtual-buffer-before-change nil t))

(defun lsp-kill-virtual-buffers ()
  (mapc #'lsp-virtual-buffer-disconnect lsp--virtual-buffer-connections))

(defun lsp--move-point-in-indentation (point indentation)
  (save-excursion
    (goto-char point)
    (if (<= point (+ (line-beginning-position) indentation))
        (line-beginning-position)
      point)))

(declare-function flycheck-checker-supports-major-mode-p "ext:flycheck")
(declare-function flycheck-add-mode "ext:flycheck")
(declare-function lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics")

(defalias 'lsp-client-download-server-fn 'lsp--client-download-server-fn)

(defun lsp-flycheck-add-mode (mode)
  "Register flycheck support for MODE."
  (lsp-diagnostics-lsp-checker-if-needed)
  (unless (flycheck-checker-supports-major-mode-p 'lsp mode)
    (flycheck-add-mode 'lsp mode)))

(defun lsp-progress-spinner-type ()
  "Retrieve the spinner type value, if value is not a symbol of `spinner-types
defaults to `progress-bar."
  (or (car (assoc lsp-progress-spinner-type spinner-types)) 'progress-bar))

(defun lsp-org ()
  (interactive)
  (-if-let ((virtual-buffer &as &plist :workspaces) (-first (-lambda ((&plist :in-range))
                                                              (funcall in-range))
                                                            lsp--virtual-buffer-connections))
      (unless (equal lsp--virtual-buffer virtual-buffer)
        (setq lsp--buffer-workspaces workspaces)
        (setq lsp--virtual-buffer virtual-buffer)
        (setq lsp-buffer-uri nil)
        (lsp-mode 1)
        (lsp-managed-mode 1)
        (lsp-patch-on-change-event))

    (save-excursion
      (-let* (virtual-buffer
              (wcb (lambda (f)
                     (with-current-buffer (plist-get virtual-buffer :buffer)
                       (-let* (((&plist :major-mode :buffer-file-name
                                        :goto-buffer :workspaces) virtual-buffer)
                               (lsp--virtual-buffer virtual-buffer)
                               (lsp--buffer-workspaces workspaces))
                         (save-excursion
                           (funcall goto-buffer)
                           (funcall f))))))
              ((&plist :begin :end :post-blank :language) (cl-second (org-element-context)))
              ((&alist :tangle file-name) (cl-third (org-babel-get-src-block-info 'light)))

              (file-name (if file-name
                             (f-expand file-name)
                           (user-error "You should specify file name in the src block header.")))
              (begin-marker (progn
                              (goto-char begin)
                              (forward-line)
                              (set-marker (make-marker) (point))))
              (end-marker (progn
                            (goto-char end)
                            (forward-line (1- (- post-blank)))
                            (set-marker (make-marker) (1+ (point)))))
              (buf (current-buffer))
              (src-block (buffer-substring-no-properties begin-marker
                                                         (1- end-marker)))
              (indentation (with-temp-buffer
                             (insert src-block)

                             (goto-char (point-min))
                             (let ((indentation (current-indentation)))
                               (plist-put lsp--virtual-buffer :indentation indentation)
                               (org-do-remove-indentation)
                               (goto-char (point-min))
                               (- indentation (current-indentation))))))
        (add-hook 'post-command-hook #'lsp--virtual-buffer-update-position nil t)

        (when (fboundp 'flycheck-add-mode)
          (lsp-flycheck-add-mode 'org-mode))

        (setq lsp--virtual-buffer
              (list
               :in-range (lambda (&optional point)
                           (<= begin-marker (or point (point)) (1- end-marker)))
               :goto-buffer (lambda () (goto-char begin-marker))
               :buffer-string
               (lambda ()
                 (let ((src-block (buffer-substring-no-properties
                                   begin-marker
                                   (1- end-marker))))
                   (with-temp-buffer
                     (insert src-block)

                     (goto-char (point-min))
                     (while (not (eobp))
                       (delete-region (point) (if (> (+ (point) indentation) (line-end-position))
                                                  (line-end-position)
                                                (+ (point) indentation)))
                       (forward-line))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
               :buffer buf
               :begin begin-marker
               :end end-marker
               :indentation indentation
               :last-point (lambda () (1- end-marker))
               :cur-position (lambda ()
                               (lsp-save-restriction-and-excursion
                                 (list :line (- (lsp--cur-line)
                                                (lsp--cur-line begin-marker))
                                       :character (let ((character (- (point)
                                                                      (line-beginning-position)
                                                                      indentation)))
                                                    (if (< character 0)
                                                        0
                                                      character)))))
               :line/character->point (-lambda (line character)
                                        (-let [inhibit-field-text-motion t]
                                          (+ indentation
                                             (lsp-save-restriction-and-excursion
                                               (goto-char begin-marker)
                                               (forward-line line)
                                               (-let [line-end (line-end-position)]
                                                 (if (> character (- line-end (point)))
                                                     line-end
                                                   (forward-char character)
                                                   (point)))))))
               :major-mode (org-src-get-lang-mode language)
               :buffer-file-name file-name
               :buffer-uri (lsp--path-to-uri file-name)
               :with-current-buffer wcb
               :buffer-live? (lambda (_) (buffer-live-p buf))
               :buffer-name (lambda (_)
                              (propertize (format "%s(%s:%s)%s"
                                                  (buffer-name buf)
                                                  begin-marker
                                                  end-marker
                                                  language)
                                          'face 'italic))
               :real->virtual-line (lambda (line)
                                     (+ line (line-number-at-pos begin-marker) -1))
               :real->virtual-char (lambda (char) (+ char indentation))
               :cleanup (lambda ()
                          (set-marker begin-marker nil)
                          (set-marker end-marker nil))))
        (setf virtual-buffer lsp--virtual-buffer)
        (puthash file-name virtual-buffer lsp--virtual-buffer-mappings)
        (push virtual-buffer lsp--virtual-buffer-connections)

        ;; TODO: tangle only connected sections
        (add-hook 'after-save-hook 'org-babel-tangle nil t)
        (add-hook 'lsp-after-open-hook #'lsp-patch-on-change-event nil t)
        (add-hook 'kill-buffer-hook #'lsp-kill-virtual-buffers nil t)

        (setq lsp--buffer-workspaces
              (lsp-with-current-buffer virtual-buffer
                (lsp)
                (plist-put virtual-buffer :workspaces (lsp-workspaces))
                (lsp-workspaces)))))))

(defun lsp-virtual-buffer-disconnect (virtual-buffer)
  (interactive (list (or
                      lsp--virtual-buffer
                      (when lsp--virtual-buffer-connections
                        (lsp--completing-read "Select virtual buffer to disconnect: "
                                              lsp--virtual-buffer-connections
                                              (-lambda ((&plist :buffer-file-name))
                                                buffer-file-name))))))
  (-if-let ((&plist :buffer-file-name file-name :cleanup) virtual-buffer)
      (progn
        (lsp-with-current-buffer virtual-buffer
          (lsp--text-document-did-close))
        (setq lsp--virtual-buffer-connections (-remove-item virtual-buffer lsp--virtual-buffer-connections))
        (when (eq virtual-buffer lsp--virtual-buffer)
          (setf lsp--virtual-buffer nil))
        (when cleanup (funcall cleanup))
        (remhash file-name lsp--virtual-buffer-mappings)

        (lsp--virtual-buffer-update-position)
        (lsp--info "Disconnected from buffer %s" file-name))
    (lsp--error "Nothing to disconnect from?")))


;; inlay hints

(defface lsp-inlay-hint-face
  '((t :inherit font-lock-comment-face))
  "The face to use for the JavaScript inlays."
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defface lsp-inlay-hint-type-face
  '((t :inherit lsp-inlay-hint-face))
  "Face for inlay type hints (e.g. inferred variable types)."
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-inlay-hint-type-format "%s"
  "Format string for variable inlays (part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defface lsp-inlay-hint-parameter-face
  '((t :inherit lsp-inlay-hint-face))
  "Face for inlay parameter hints (e.g. function parameter names at
call-site)."
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-inlay-hint-param-format "%s"
  "Format string for parameter inlays (part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-update-inlay-hints-on-scroll t
  "If non-nil update inlay hints immediately when scrolling or
modifying window sizes."
  :type 'boolean
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp--format-inlay (text kind)
  (cond
   ((eql kind lsp/inlay-hint-kind-type-hint) (format lsp-inlay-hint-type-format text))
   ((eql kind lsp/inlay-hint-kind-parameter-hint) (format lsp-inlay-hint-param-format text))
   (t text)))

(defun lsp--face-for-inlay (kind)
  (cond
   ((eql kind lsp/inlay-hint-kind-type-hint) 'lsp-inlay-hint-type-face)
   ((eql kind lsp/inlay-hint-kind-parameter-hint) 'lsp-inlay-hint-parameter-face)
   (t 'lsp-inlay-hint-face)))

(defun lsp--update-inlay-hints-scroll-function (window start)
  (lsp-update-inlay-hints start (window-end window t)))

(defun lsp--update-inlay-hints ()
  (lsp-update-inlay-hints (window-start) (window-end nil t)))

(defun lsp--label-from-inlay-hints-response (label)
  "Returns a string label built from an array of
InlayHintLabelParts or the argument itself if it's already a
string."
  (cl-typecase label
    (string label)
    (vector
     (string-join (mapcar (lambda (part)
                            (-let (((&InlayHintLabelPart :value) part))
                              value))
                          label)))))

(defun lsp-update-inlay-hints (start end)
  (lsp-request-async
   "textDocument/inlayHint"
   (lsp-make-inlay-hints-params
    :text-document (lsp--text-document-identifier)
    :range (lsp-make-range :start
                           (lsp-point-to-position start)
                           :end
                           (lsp-point-to-position end)))
   (lambda (res)
     (lsp--remove-overlays 'lsp-inlay-hint)
     (dolist (hint res)
       (-let* (((&InlayHint :label :position :kind? :padding-left? :padding-right?) hint)
               (kind (or kind? lsp/inlay-hint-kind-type-hint))
               (label (lsp--label-from-inlay-hints-response label))
               (pos (lsp--position-to-point position))
               (overlay (make-overlay pos pos nil 'front-advance 'end-advance)))
         (when (stringp label)
           (overlay-put overlay 'lsp-inlay-hint t)
           (overlay-put overlay 'before-string
                        (format "%s%s%s"
                                (if padding-left? " " "")
                                (propertize (lsp--format-inlay label kind)
                                            'font-lock-face (lsp--face-for-inlay kind))
                                (if padding-right? " " "")))))))
   :mode 'tick))

(define-minor-mode lsp-inlay-hints-mode
  "Mode for displaying inlay hints."
  :lighter nil
  (cond
   ((and lsp-inlay-hints-mode lsp--buffer-workspaces)
    (add-hook 'lsp-on-idle-hook #'lsp--update-inlay-hints nil t)
    (when lsp-update-inlay-hints-on-scroll
      (add-to-list (make-local-variable 'window-scroll-functions)
                   #'lsp--update-inlay-hints-scroll-function)))
   (t
    (lsp--remove-overlays 'lsp-inlay-hint)
    (remove-hook 'lsp-on-idle-hook #'lsp--update-inlay-hints t)
    (setf window-scroll-functions
          (delete #'lsp--update-inlay-hints-scroll-function window-scroll-functions)))))



;;;###autoload
(defun lsp-start-plain ()
  "Start `lsp-mode' using minimal configuration using the latest `melpa' version
of the packages.

In case the major-mode that you are using for "
  (interactive)
  (let ((start-plain (make-temp-file "plain" nil ".el")))
    (url-copy-file "https://raw.githubusercontent.com/emacs-lsp/lsp-mode/master/scripts/lsp-start-plain.el"
                   start-plain t)
    (async-shell-command
     (format "%s -q -l %s %s"
             (expand-file-name invocation-name invocation-directory)
             start-plain
             (or (buffer-file-name) ""))
     (generate-new-buffer " *lsp-start-plain*"))))



(provide 'lsp-mode)
;;; lsp-mode.el ends here
