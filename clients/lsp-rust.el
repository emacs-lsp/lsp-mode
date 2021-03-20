;;; lsp-rust.el --- Rust Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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

;; lsp-rust client

;;; Code:

(require 'lsp-mode)
(require 'ht)
(require 'dash)

(defgroup lsp-rust nil
  "LSP support for Rust, using Rust Language Server or rust-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rust-lang/rls")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-server 'rust-analyzer
  "Choose LSP server."
  :type '(choice (const :tag "rls" rls)
                 (const :tag "rust-analyzer" rust-analyzer))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2"))

;; RLS

(defcustom lsp-rust-rls-server-command '("rls")
  "Command to start RLS."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-library-directories '("~/.cargo/registry/src" "~/.rustup/toolchains")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-sysroot nil
  "If non-nil, use the given path as the sysroot for all rustc invocations
instead of trying to detect the sysroot automatically."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Sysroot"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-target nil
  "If non-nil, use the given target triple for all rustc invocations."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Target"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-rustflags nil
  "Flags added to RUSTFLAGS."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Flags"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-clear-env-rust-log t
  "Clear the RUST_LOG environment variable before running rustc or cargo."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-lib nil
  "If non-nil, checks the project as if you passed the `--lib' argument to
cargo.

Mutually exclusive with, and preferred over, `lsp-rust-build-bin'. (Unstable)"
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-bin nil
  "If non-nil, checks the project as if you passed `-- bin <build_bin>'
argument to cargo.

Mutually exclusive with `lsp-rust-build-lib'. (Unstable)"
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Binary"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-cfg-test nil
  "If non-nil, checks the project as if you were running `cargo test' rather
than cargo build.

I.e., compiles (but does not run) test code."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-unstable-features nil
  "Enable unstable features."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-wait-to-build nil
  "Time in milliseconds between receiving a change notification
and starting build. If not specified, automatically inferred by
the latest build duration."
  :type '(choice
          (const :tag "Auto" nil)
          (number :tag "Time"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-show-warnings t
  "Show warnings."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-crate-blacklist  [
                                      "cocoa"
                                      "gleam"
                                      "glium"
                                      "idna"
                                      "libc"
                                      "openssl"
                                      "rustc_serialize"
                                      "serde"
                                      "serde_json"
                                      "typenum"
                                      "unicode_normalization"
                                      "unicode_segmentation"
                                      "winapi"
                                      ]
  "A list of Cargo crates to blacklist."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-on-save nil
  "Only index the project when a file is saved and not on change."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-features []
  "List of Cargo features to enable."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-all-features nil
  "Enable all Cargo features."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-no-default-features nil
  "Do not enable default Cargo features."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-racer-completion t
  "Enables code completion using racer."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-clippy-preference "opt-in"
  "Controls eagerness of clippy diagnostics when available.
Valid values are (case-insensitive):\n - \"off\": Disable clippy
lints.\n - \"opt-in\": Clippy lints are shown when crates
specify `#![warn(clippy)]`.\n - \"on\": Clippy lints enabled
for all crates in workspace.\nYou need to install clippy via
rustup if you haven't already."
  :type '(choice
          (const "on")
          (const "opt-in")
          (const "off"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-jobs nil
  "Number of Cargo jobs to be run in parallel."
  :type '(choice
          (const :tag "Auto" nil)
          (number :tag "Jobs"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-all-targets t
  "Checks the project as if you were running cargo check --all-targets.
I.e., check all targets and integration tests too."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-target-dir nil
  "When specified, it places the generated analysis files at the
specified target directory. By default it is placed target/rls
directory."
  :type '(choice
          (const :tag "Default" nil)
          (string :tag "Directory"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-rustfmt-path nil
  "When specified, RLS will use the Rustfmt pointed at the path
instead of the bundled one"
  :type '(choice
          (const :tag "Bundled" nil)
          (string :tag "Path"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-command nil
  "EXPERIMENTAL (requires `unstable_features`)\nIf set, executes
a given program responsible for rebuilding save-analysis to be
loaded by the RLS. The program given should output a list of
resulting .json files on stdout. \nImplies `rust.build_on_save`:
true."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Command"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-full-docs nil
  "Instructs cargo to enable full documentation extraction during
save-analysis while building the crate."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-show-hover-context t
  "Show additional context in hover tooltips when available. This
is often the type local variable declaration."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(lsp-register-custom-settings
 '(("rust.show_hover_context" lsp-rust-show-hover-context t)
   ("rust.full_docs" lsp-rust-full-docs t)
   ("rust.build_command" lsp-rust-build-command)
   ("rust.rustfmt_path" lsp-rust-rustfmt-path)
   ("rust.target_dir" lsp-rust-target-dir)
   ("rust.all_targets" lsp-rust-all-targets t)
   ("rust.jobs" lsp-rust-jobs)
   ("rust.clippy_preference" lsp-rust-clippy-preference)
   ("rust.racer_completion" lsp-rust-racer-completion t)
   ("rust.no_default_features" lsp-rust-no-default-features t)
   ("rust.all_features" lsp-rust-all-features t)
   ("rust.features" lsp-rust-features)
   ("rust.build_on_save" lsp-rust-build-on-save t)
   ("rust.crate_blacklist" lsp-rust-crate-blacklist)
   ("rust.show_warnings" lsp-rust-show-warnings t)
   ("rust.wait_to_build" lsp-rust-wait-to-build)
   ("rust.unstable_features" lsp-rust-unstable-features t)
   ("rust.cfg_test" lsp-rust-cfg-test t)
   ("rust.build_bin" lsp-rust-build-bin)
   ("rust.build_lib" lsp-rust-build-lib t)
   ("rust.clear_env_rust_log" lsp-rust-clear-env-rust-log t)
   ("rust.rustflags" lsp-rust-rustflags)
   ("rust.target" lsp-rust-target)
   ("rust.sysroot" lsp-rust-sysroot)))

(defun lsp-clients--rust-window-progress (workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  (-let [(&v1:ProgressParams :done? :message? :title) params]
    (if (or done? (s-blank-str? message?))
        (lsp-workspace-status nil workspace)
      (lsp-workspace-status (format "%s - %s" title (or message? "")) workspace))))

(lsp-defun lsp-rust--rls-run ((&Command :arguments? params))
  (-let* (((&rls:Cmd :env :binary :args :cwd) (lsp-seq-first params))
          (default-directory (or cwd (lsp-workspace-root) default-directory) ))
    (compile
     (format "%s %s %s"
             (s-join " " (ht-amap (format "%s=%s" key value) env))
             binary
             (s-join " " args)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-rust-rls-server-command))
                  :major-modes '(rust-mode rustic-mode)
                  :priority (if (eq lsp-rust-server 'rls) 1 -1)
                  :initialization-options '((omitInitBuild . t)
                                            (cmdRun . t))
                  :notification-handlers (ht ("window/progress" 'lsp-clients--rust-window-progress))
                  :action-handlers (ht ("rls.run" 'lsp-rust--rls-run))
                  :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "rust"))))
                  :server-id 'rls))


;; rust-analyzer
(defcustom lsp-rust-analyzer-server-command '("rust-analyzer")
  "Command to start rust-analyzer."
  :type '(repeat string)
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-rust-analyzer-server-args '()
  "Args to start rust-analyzer with when rust-analyzer is downloaded \
automatically."
  :type '(repeat string)
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-server-display-inlay-hints nil
  "Show inlay hints."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-rust-analyzer-max-inlay-hint-length nil
  "Max inlay hint length."
  :type 'integer
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-display-parameter-hints nil
  "Whether to show function parameter name inlay hints at the call site."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-display-chaining-hints nil
  "Whether to show inlay type hints for method chains."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-lru-capacity nil
  "Number of syntax trees rust-analyzer keeps in memory."
  :type 'integer
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-watch-enable t
  "Enable Cargo watch."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-watch-command "check"
  "Cargo watch command."
  :type 'string
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-watch-args []
  "Cargo watch args."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-override-command []
  "Advanced option, fully override the command rust-analyzer uses for checking.
The command should include `--message=format=json` or similar option."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-all-targets t
  "Cargo watch all targets or not."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-use-client-watching t
  "Use client watching"
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-exclude-globs []
  "Exclude globs"
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default
  "Use a different function if you want formatted macro expansion results and
syntax highlighting."
  :type 'function
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-diagnostics-enable t
  "Whether to show native rust-analyzer diagnostics."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-diagnostics-enable-experimental t
  "Whether to show native rust-analyzer diagnostics that are still experimental
(might have more false positives than usual)."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-rust-analyzer-diagnostics-disabled []
  "List of native rust-analyzer diagnostics to disable."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1.0"))

(define-obsolete-variable-alias
  'lsp-rust-analyzer-cargo-load-out-dirs-from-check
  'lsp-rust-analyzer-cargo-run-build-scripts
  "7.1.0")

(defcustom lsp-rust-analyzer-cargo-run-build-scripts t
  "Whether to run build scripts (`build.rs`) for more precise code analysis."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-rust-analyzer-rustfmt-extra-args []
  "Additional arguments to rustfmt."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-rustfmt-override-command []
  "Advanced option, fully override the command rust-analyzer uses
for formatting."
  :type 'lsp-string-vector
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-completion-add-call-parenthesis t
  "Whether to add parenthesis when completing functions."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-completion-add-call-argument-snippets t
  "Whether to add argument snippets when completing functions."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-completion-postfix-enable t
  "Whether to show postfix snippets like `dbg`, `if`, `not`, etc."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-call-info-full t
  "Whether to show function name and docs in parameter hints."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-proc-macro-enable nil
  "Enable Proc macro support; implies lsp-rust-analyzer-cargo-run-build-scripts"
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-import-merge-behaviour "full"
  "The strategy to use when inserting new imports or merging imports.
Valid values are:\n - \"none\": No merging\n - \"full\": Merge all layers of
the import trees\n - \"last\": Only merge the last layer of the import trees"
  :type '(choice
          (const "none")
          (const "full")
          (const "last"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-rust-analyzer-import-prefix "plain"
  "The path structure for newly inserted paths to use.
Valid values are:\n - \"plain\": Insert import paths relative to the
current module, using up to one `super` prefix if the parent module
contains the requested item.\n - \"by_self\": Prefix all import paths
with `self` if they don't begin with `self`, `super`, `crate` or a crate
name\n - \"by_crate\": Force import paths to be absolute by always starting
them with `crate` or the crate name they refer to."
  :type '(choice
          (const "plain")
          (const "by_self")
          (const "by_crate"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1.0"))

(defun lsp-rust-analyzer--make-init-options ()
  "Init options for rust-analyzer"
  `(:diagnostics (:enable ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable)
                  :enableExperimental ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable-experimental)
                  :disabled ,lsp-rust-analyzer-diagnostics-disabled)
    :assist (:importMergeBehaviour ,lsp-rust-analyzer-import-merge-behaviour
             :importPrefix ,lsp-rust-analyzer-import-prefix)
    :lruCapacity ,lsp-rust-analyzer-lru-capacity
    :checkOnSave (:enable ,(lsp-json-bool lsp-rust-analyzer-cargo-watch-enable)
                  :command ,lsp-rust-analyzer-cargo-watch-command
                  :extraArgs ,lsp-rust-analyzer-cargo-watch-args
                  :allTargets ,(lsp-json-bool lsp-rust-analyzer-cargo-all-targets)
                  :overrideCommand ,lsp-rust-analyzer-cargo-override-command)
    :files (:exclude ,lsp-rust-analyzer-exclude-globs
            :watcher ,(lsp-json-bool (if lsp-rust-analyzer-use-client-watching
                                         "client"
                                       "notify")))
    :cargo (:allFeatures ,(lsp-json-bool lsp-rust-all-features)
            :noDefaultFeatures ,(lsp-json-bool lsp-rust-no-default-features)
            :features ,lsp-rust-features
            :runBuildScripts ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
            ; Obsolete, but used by old Rust-Analyzer versions
            :loadOutDirsFromCheck ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts))
    :rustfmt (:extraArgs ,lsp-rust-analyzer-rustfmt-extra-args
              :overrideCommand ,lsp-rust-analyzer-rustfmt-override-command)
    :inlayHints (:typeHints ,(lsp-json-bool lsp-rust-analyzer-server-display-inlay-hints)
                 :chainingHints ,(lsp-json-bool lsp-rust-analyzer-display-chaining-hints)
                 :parameterHints ,(lsp-json-bool lsp-rust-analyzer-display-parameter-hints)
                 :maxLength ,lsp-rust-analyzer-max-inlay-hint-length)
    :completion (:addCallParenthesis ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-parenthesis)
                 :addCallArgumentSnippets ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-argument-snippets)
                 :postfix (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-postfix-enable)))
    :callInfo (:full ,(lsp-json-bool lsp-rust-analyzer-call-info-full))
    :procMacro (:enable ,(lsp-json-bool lsp-rust-analyzer-proc-macro-enable))))

(defconst lsp-rust-notification-handlers
  '(("rust-analyzer/publishDecorations" . (lambda (_w _p)))))

(defconst lsp-rust-action-handlers
  '())

(define-derived-mode lsp-rust-analyzer-syntax-tree-mode special-mode "Rust-Analyzer-Syntax-Tree"
  "Mode for the rust-analyzer syntax tree buffer.")

(defun lsp-rust-analyzer-syntax-tree ()
  "Display syntax tree for current buffer."
  (interactive)
  (-let* ((root (lsp-workspace-root default-directory))
          (params (lsp-make-rust-analyzer-syntax-tree-params
                   :text-document (lsp--text-document-identifier)
                   :range? (if (use-region-p)
                               (lsp--region-to-range (region-beginning) (region-end))
                             (lsp--region-to-range (point-min) (point-max)))))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/syntaxTree"
                                      params))))
    (let ((buf (get-buffer-create (format "*rust-analyzer syntax tree %s*" root)))
          (inhibit-read-only t))
      (with-current-buffer buf
        (lsp-rust-analyzer-syntax-tree-mode)
        (erase-buffer)
        (insert results)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

(define-derived-mode lsp-rust-analyzer-status-mode special-mode "Rust-Analyzer-Status"
  "Mode for the rust-analyzer status buffer.")

(defun lsp-rust-analyzer-status ()
  "Displays status information for rust-analyzer."
  (interactive)
  (-let* ((root (lsp-workspace-root default-directory))
          (params (lsp-make-rust-analyzer-analyzer-status-params
                   :text-document (lsp--text-document-identifier)))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/analyzerStatus"
                                      params))))
    (let ((buf (get-buffer-create (format "*rust-analyzer status %s*" root)))
          (inhibit-read-only t))
      (with-current-buffer buf
        (lsp-rust-analyzer-status-mode)
        (erase-buffer)
        (insert results)
        (pop-to-buffer buf)))))

(defun lsp-rust-analyzer-join-lines ()
  "Join selected lines into one, smartly fixing up whitespace and trailing commas."
  (interactive)
  (let* ((params (lsp-make-rust-analyzer-join-lines-params
                  :text-document (lsp--text-document-identifier)
                  :ranges (vector (if (use-region-p)
                                      (lsp--region-to-range (region-beginning) (region-end))
                                    (lsp--region-to-range (point) (point))))))
         (result (lsp-send-request (lsp-make-request "experimental/joinLines" params))))
    (lsp--apply-text-edits result 'code-action)))

(defun lsp-rust-analyzer-reload-workspace ()
  "Reload workspace, picking up changes from Cargo.toml"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-send-request (lsp-make-request "rust-analyzer/reloadWorkspace")))

(defcustom lsp-rust-analyzer-download-url
  (format "https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/%s"
          (pcase system-type
            ('gnu/linux "rust-analyzer-linux")
            ('darwin "rust-analyzer-mac")
            ('windows-nt "rust-analyzer-windows.exe")))
  "Automatic download url for Rust Analyzer"
  :type 'string
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-store-path (f-join lsp-server-install-dir
                                                "rust"
                                                (if (eq system-type 'windows-nt)
                                                    "rust-analyzer.exe"
                                                  "rust-analyzer"))
  "The path to the file in which `rust-analyzer' will be stored."
  :type 'file
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(lsp-dependency
 'rust-analyzer
 `(:download :url lsp-rust-analyzer-download-url
             :store-path lsp-rust-analyzer-store-path
             :set-executable? t)
 '(:system "rust-analyzer"))

(lsp-defun lsp-rust--analyzer-run-single ((&Command :arguments?))
  (lsp-rust-analyzer-run (lsp-seq-first arguments?)))

(lsp-defun lsp-rust--analyzer-show-references
  ((&Command :title :arguments? [_uri _filepos references]))
  (lsp-show-xrefs (lsp--locations-to-xref-items references) nil
                  (s-contains-p "reference" title)))

(declare-function dap-debug "ext:dap-mode" (template) t)

(lsp-defun lsp-rust--analyzer-debug-lens ((&Command :arguments? [args]))
  (lsp-rust-analyzer-debug args))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-rust-analyzer-server-command))
                            (lsp-package-path 'rust-analyzer)
                            "rust-analyzer")
                       ,@(cl-rest lsp-rust-analyzer-server-args))))
  :major-modes '(rust-mode rustic-mode)
  :priority (if (eq lsp-rust-server 'rust-analyzer) 1 -1)
  :initialization-options 'lsp-rust-analyzer--make-init-options
  :notification-handlers (ht<-alist lsp-rust-notification-handlers)
  :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single)
                       ("rust-analyzer.debugSingle" #'lsp-rust--analyzer-debug-lens)
                       ("rust-analyzer.showReferences" #'lsp-rust--analyzer-show-references))
  :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
  :after-open-fn (lambda ()
                   (when lsp-rust-analyzer-server-display-inlay-hints
                     (lsp-rust-analyzer-inlay-hints-mode)))
  :ignore-messages nil
  :server-id 'rust-analyzer
  :custom-capabilities `((experimental . ((snippetTextEdit . ,(and lsp-enable-snippet (featurep 'yasnippet))))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'rust-analyzer callback error-callback))))

(defun lsp-rust-switch-server (&optional lsp-server)
  "Switch priorities of lsp servers, unless LSP-SERVER is already active."
  (interactive)
  (let ((current-server (if (> (lsp--client-priority (gethash 'rls lsp-clients)) 0)
                            'rls
                          'rust-analyzer)))
    (unless (eq lsp-server current-server)
      (dolist (server '(rls rust-analyzer))
        (when (natnump (setf (lsp--client-priority (gethash server lsp-clients))
                             (* (lsp--client-priority (gethash server lsp-clients)) -1)))
          (message (format "Switched to server %s." server)))))))

;; inlay hints

(defvar-local lsp-rust-analyzer-inlay-hints-timer nil)

(defface lsp-rust-analyzer-inlay-face
  '((t :inherit font-lock-comment-face))
  "The face to use for the Rust Analyzer inlays."
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.0"))

(defface lsp-rust-analyzer-inlay-type-face
  '((t :inherit lsp-rust-analyzer-inlay-face))
  "Face for inlay type hints (e.g. inferred variable types)."
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-type-space-format "%s"
  "Format string for spacing around variable inlays
(not part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-type-format ": %s"
  "Format string for variable inlays (part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defface lsp-rust-analyzer-inlay-param-face
  '((t :inherit lsp-rust-analyzer-inlay-face))
  "Face for inlay parameter hints (e.g. function parameter names at call-site)."
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-param-space-format "%s "
  "Format string for spacing around parameter inlays
(not part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-param-format "%s:"
  "Format string for parameter inlays (part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defface lsp-rust-analyzer-inlay-chain-face
  '((t :inherit lsp-rust-analyzer-inlay-face))
  "Face for inlay chaining hints (e.g. inferred chain intermediate types)."
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-chain-space-format "%s"
  "Format string for spacing around chain inlays (not part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-rust-analyzer-inlay-chain-format ": %s"
  "Format string for chain inlays (part of the inlay face)."
  :type '(string :tag "String")
  :group 'lsp-rust
  :package-version '(lsp-mode . "7.1"))

(defun lsp-rust-analyzer-update-inlay-hints (buffer)
  (if (and (lsp-rust-analyzer-initialized?)
           (eq buffer (current-buffer)))
      (lsp-request-async
       "rust-analyzer/inlayHints"
       (lsp-make-rust-analyzer-inlay-hints-params
        :text-document (lsp--text-document-identifier))
       (lambda (res)
         (remove-overlays (point-min) (point-max) 'lsp-rust-analyzer-inlay-hint t)
         (dolist (hint res)
           (-let* (((&rust-analyzer:InlayHint :range :label :kind) hint)
                   ((&RangeToPoint :start :end) range)
                   (overlay (make-overlay start end nil 'front-advance 'end-advance)))
             (overlay-put overlay 'lsp-rust-analyzer-inlay-hint t)
             (overlay-put overlay 'evaporate t)
             (cond
              ((equal kind lsp/rust-analyzer-inlay-hint-kind-type-hint)
               (overlay-put overlay 'after-string
                (format lsp-rust-analyzer-inlay-type-space-format
                 (propertize (format lsp-rust-analyzer-inlay-type-format label)
                  'font-lock-face 'lsp-rust-analyzer-inlay-type-face))))

              ((equal kind lsp/rust-analyzer-inlay-hint-kind-param-hint)
               (overlay-put overlay 'before-string
                (format lsp-rust-analyzer-inlay-param-space-format
                 (propertize (format lsp-rust-analyzer-inlay-param-format label)
                  'font-lock-face 'lsp-rust-analyzer-inlay-param-face))))

              ((equal kind lsp/rust-analyzer-inlay-hint-kind-chaining-hint)
               (overlay-put overlay 'after-string
                (format lsp-rust-analyzer-inlay-chain-space-format
                 (propertize (format lsp-rust-analyzer-inlay-chain-format label)
                  'font-lock-face 'lsp-rust-analyzer-inlay-chain-face))))))))
       :mode 'tick))
 nil)

(defun lsp-rust-analyzer-initialized? ()
  (when-let ((workspace (lsp-find-workspace 'rust-analyzer (buffer-file-name))))
    (eq 'initialized (lsp--workspace-status workspace))))

(defun lsp-rust-analyzer-inlay-hints-change-handler (&rest _rest)
  (when lsp-rust-analyzer-inlay-hints-timer
    (cancel-timer lsp-rust-analyzer-inlay-hints-timer))
  (setq lsp-rust-analyzer-inlay-hints-timer
        (run-with-idle-timer 0.1 nil #'lsp-rust-analyzer-update-inlay-hints (current-buffer))))

(define-minor-mode lsp-rust-analyzer-inlay-hints-mode
  "Mode for displaying inlay hints."
  nil nil nil
  (cond
   (lsp-rust-analyzer-inlay-hints-mode
    (lsp-rust-analyzer-update-inlay-hints (current-buffer))
    (add-hook 'lsp-on-change-hook #'lsp-rust-analyzer-inlay-hints-change-handler nil t))
   (t
    (remove-overlays (point-min) (point-max) 'lsp-rust-analyzer-inlay-hint t)
    (remove-hook 'lsp-on-change-hook #'lsp-rust-analyzer-inlay-hints-change-handler t))))

(defun lsp-rust-analyzer-expand-macro ()
  "Expands the macro call at point recursively."
  (interactive)
  (-if-let* ((params (lsp-make-rust-analyzer-expand-macro-params
                      :text-document (lsp--text-document-identifier)
                      :position (lsp--cur-position)))
             (response (lsp-send-request (lsp-make-request
                                          "rust-analyzer/expandMacro"
                                          params)))
             ((&rust-analyzer:ExpandedMacro :expansion) response))
      (funcall lsp-rust-analyzer-macro-expansion-method expansion)
    (message "No macro found at point, or it could not be expanded.")))

(defun lsp-rust-analyzer-macro-expansion-default (result)
  "Default method for displaying macro expansion."
  (let* ((root (lsp-workspace-root default-directory))
         (buf (get-buffer-create (get-buffer-create (format "*rust-analyzer macro expansion %s*" root)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert result)
        (special-mode)))
    (display-buffer buf)))

;; runnables
(defvar lsp-rust-analyzer--last-runnable nil)

(defun lsp-rust-analyzer--runnables ()
  (lsp-send-request (lsp-make-request
                     "experimental/runnables"
                     (lsp-make-rust-analyzer-runnables-params
                      :text-document (lsp--text-document-identifier)
                      :position? (lsp--cur-position)))))

(defun lsp-rust-analyzer--select-runnable ()
  (lsp--completing-read
   "Select runnable:"
   (if lsp-rust-analyzer--last-runnable
       (cons lsp-rust-analyzer--last-runnable
             (-remove (-lambda ((&rust-analyzer:Runnable :label))
                        (equal label (lsp-get lsp-rust-analyzer--last-runnable :label)))
                      (lsp-rust-analyzer--runnables)))
     (lsp-rust-analyzer--runnables))
   (-lambda ((&rust-analyzer:Runnable :label)) label)))

(defun lsp-rust-analyzer-run (runnable)
  "Select and run a runnable action."
  (interactive (list (lsp-rust-analyzer--select-runnable)))
  (-let* (((&rust-analyzer:Runnable :kind :label :args) runnable)
          ((&rust-analyzer:RunnableArgs :cargo-args :executable-args :workspace-root?) args)
          (default-directory (or workspace-root? default-directory)))
    (if (not (string-equal kind "cargo"))
        (lsp--error "'%s' runnable is not supported" kind)
      (compilation-start
       (string-join (append (list "cargo") cargo-args (when executable-args '("--")) executable-args '()) " ")
       ;; cargo-process-mode is nice, but try to work without it...
       (if (functionp 'cargo-process-mode) 'cargo-process-mode nil)
       (lambda (_) (concat "*" label "*")))
      (setq lsp-rust-analyzer--last-runnable runnable))))

(defun lsp-rust-analyzer-debug (runnable)
  "Select and run a runnable action."
  (interactive (list (lsp-rust-analyzer--select-runnable)))
  (unless (featurep 'dap-cpptools)
    (user-error "You must require `dap-cpptools'."))
  (-let (((&rust-analyzer:Runnable
           :args (&rust-analyzer:RunnableArgs :cargo-args :workspace-root? :executable-args)
           :label) runnable))
    (cl-case (aref cargo-args 0)
      ("run" (aset cargo-args 0 "build"))
      ("test" (when (-contains? (append cargo-args ()) "--no-run")
                (cl-callf append cargo-args (list "--no-run")))))
    (->> (append (list (executable-find "cargo"))
                 cargo-args
                 (list "--message-format=json"))
         (s-join " ")
         (shell-command-to-string)
         (s-lines)
         (-keep (lambda (s)
                  (condition-case nil
                      (-let* ((json-object-type 'plist)
                              ((msg &as &plist :reason :executable) (json-read-from-string s)))
                        (when (and executable (string= "compiler-artifact" reason))
                          executable))
                    (error))))
         (funcall
          (lambda (artifact-spec)
            (pcase artifact-spec
              (`() (user-error "No compilation artifacts or obtaining the runnable artifacts failed."))
              (`(,spec) spec)
              (_ (user-error "Multiple compilation artifacts are not supported.")))))
         (list :type "cppdbg"
               :request "launch"
               :name label
               :args executable-args
               :cwd workspace-root?
               :sourceLanguages ["rust"]
               :program)
         (dap-debug))))

(defun lsp-rust-analyzer-rerun (&optional runnable)
  (interactive (list (or lsp-rust-analyzer--last-runnable
                         (lsp-rust-analyzer--select-runnable))))
  (lsp-rust-analyzer-run (or runnable lsp-rust-analyzer--last-runnable)))

;; goto parent module
(cl-defun lsp-rust-find-parent-module (&key display-action)
  "Find parent module of current module."
  (interactive)
  (lsp-find-locations "experimental/parentModule" nil :display-action display-action))

(provide 'lsp-rust)
;;; lsp-rust.el ends here
