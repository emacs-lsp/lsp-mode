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
(require 'lsp-semantic-tokens)
(require 's)

(defgroup lsp-rust nil
  "LSP support for Rust, using Rust Language Server or rust-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rust-lang/rls")
  :package-version '(lsp-mode . "6.1"))

(defgroup lsp-rust-rls nil
  "LSP support for Rust, using Rust Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rust-lang/rls")
  :package-version '(lsp-mode . "8.0.0"))

(defgroup lsp-rust-analyzer nil
  "LSP support for Rust, using rust-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rust-lang/rust-analyzer")
  :package-version '(lsp-mode . "8.0.0"))

(defgroup lsp-rust-analyzer-semantic-tokens nil
  "LSP semantic tokens support for rust-analyzer."
  :group 'lsp-rust-analyzer
  :link '(url-link "https://github.com/rust-lang/rust-analyzer")
  :package-version '(lsp-mode . "9.0.0"))

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
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-library-directories
  '("~/.cargo/registry/src" "~/.rustup/toolchains")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-sysroot nil
  "If non-nil, use the given path as the sysroot for all rustc invocations
instead of trying to detect the sysroot automatically."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Sysroot"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-target nil
  "If non-nil, use the given target triple for all rustc invocations."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Target"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-rustflags nil
  "Flags added to RUSTFLAGS."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Flags"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-clear-env-rust-log t
  "Clear the RUST_LOG environment variable before running rustc or cargo."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-lib nil
  "If non-nil, checks the project as if you passed the `--lib' argument to
cargo.

Mutually exclusive with, and preferred over, `lsp-rust-build-bin'. (Unstable)"
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-bin nil
  "If non-nil, checks the project as if you passed `-- bin <build_bin>'
argument to cargo.

Mutually exclusive with `lsp-rust-build-lib'. (Unstable)"
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Binary"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-cfg-test nil
  "If non-nil, checks the project as if you were running `cargo test' rather
than cargo build.

I.e., compiles (but does not run) test code."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-unstable-features nil
  "Enable unstable features."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-wait-to-build nil
  "Time in milliseconds between receiving a change notification
and starting build. If not specified, automatically inferred by
the latest build duration."
  :type '(choice
          (const :tag "Auto" nil)
          (number :tag "Time"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-show-warnings t
  "Show warnings."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-crate-blocklist  [
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
  "A list of Cargo crates to blocklist."
  :type 'lsp-string-vector
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-on-save nil
  "Only index the project when a file is saved and not on change."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-features []
  "List of features to activate.
Corresponds to the `rust-analyzer` setting `rust-analyzer.cargo.features`.
Set this to `\"all\"` to pass `--all-features` to cargo."
  :type 'lsp-string-vector
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-all-features nil
  "Enable all Cargo features."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-no-default-features nil
  "Do not enable default Cargo features."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-racer-completion t
  "Enables code completion using racer."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-clippy-preference "opt-in"
  "Controls eagerness of clippy diagnostics when available.
Valid values are (case-insensitive):
 - \"off\": Disable clippy lints.
 - \"opt-in\": Clippy lints are shown when crates specify `#![warn(clippy)]'.
 - \"on\": Clippy lints enabled for all crates in workspace.

You need to install clippy via rustup if you haven't already."
  :type '(choice
          (const "on")
          (const "opt-in")
          (const "off"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-jobs nil
  "Number of Cargo jobs to be run in parallel."
  :type '(choice
          (const :tag "Auto" nil)
          (number :tag "Jobs"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-all-targets t
  "Checks the project as if you were running cargo check --all-targets.
I.e., check all targets and integration tests too."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-target-dir nil
  "When specified, it places the generated analysis files at the
specified target directory. By default it is placed target/rls
directory."
  :type '(choice
          (const :tag "Default" nil)
          (string :tag "Directory"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-rustfmt-path nil
  "When specified, RLS will use the Rustfmt pointed at the path
instead of the bundled one"
  :type '(choice
          (const :tag "Bundled" nil)
          (string :tag "Path"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-command nil
  "EXPERIMENTAL (requires `rust.unstable_features')
If set, executes a given program responsible for rebuilding save-analysis to be
loaded by the RLS. The program given should output a list of resulting .json
files on stdout.

Implies `rust.build_on_save': true."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Command"))
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-full-docs nil
  "Instructs cargo to enable full documentation extraction during
save-analysis while building the crate."
  :type 'boolean
  :group 'lsp-rust-rls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-show-hover-context t
  "Show additional context in hover tooltips when available. This
is often the type local variable declaration."
  :type 'boolean
  :group 'lsp-rust-rls
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
   ("rust.crate_blocklist" lsp-rust-crate-blocklist)
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
                  :activation-fn (lsp-activate-on "rust")
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
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-rust-analyzer-library-directories
  '("~/.cargo/git" "~/.cargo/registry/src" "~/.rustup/toolchains")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-server-format-inlay-hints t
  "Whether to ask rust-analyzer to format inlay hints itself.  If
active, the various inlay format settings are not used."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-hide-closure-initialization nil
  "Whether to hide inlay type hints for `let` statements that initialize
to a closure. Only applies to closures with blocks, same as
`#rust-analyzer.inlayHints.closureReturnTypeHints.enable#`."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-highlight-breakpoints t
  "Enables highlighting of related references while the cursor is on
`break`, `loop`, `while`, or `for` keywords."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-highlight-closure-captures t
  "Enables highlighting of all captures of a closure while the
cursor is on the `|` or move keyword of a closure."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-highlight-exit-points t
  "Enables highlighting of all exit points while the cursor is on
any `return`, `?`, `fn`, or return type arrow (`->`)."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-highlight-references t
  "Enables highlighting of related references while the cursor is on
any identifier."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-highlight-yield-points t
  "Enables highlighting of all break points for a loop or block
context while the cursor is on any `async` or `await` keywords."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-closure-return-type-hints "never"
  "Whether to show inlay type hints for return types of closures."
  :type '(choice
          (const "never")
          (const "always")
          (const "with_block"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-discriminants-hints "never"
  "Whether to show enum variant discriminant hints."
  :type '(choice
          (const "never")
          (const "always")
          (const "fieldless"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-expression-adjustment-hints "never"
  "Whether to show inlay hints for type adjustments.."
  :type '(choice
          (const "never")
          (const "always")
          (const "reborrow"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-expression-adjustment-hints-mode "prefix"
  "Whether to show inlay hints as postfix ops (`.*` instead of `*`, etc)."
  :type '(choice
          (const "prefix")
          (const "postfix")
          (const "prefer_prefix")
          (const "prefer_postfix"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-expression-adjustment-hide-unsafe nil
  "Whether to hide inlay hints for type adjustments outside of
`unsafe` blocks."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-implicit-drops nil
  "Whether to show implicit drop hints."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))


(defcustom lsp-rust-analyzer-closure-capture-hints nil
  "Whether to show inlay hints for closure captures."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-closure-style "impl_fn"
  "Closure notation in type and chaining inlay hints."
  :type 'string
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-hide-named-constructor nil
  "Whether to hide inlay type hints for constructors."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-max-inlay-hint-length nil
  "Max inlay hint length."
  :type 'integer
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-display-chaining-hints nil
  "Whether to show inlay type hints for method chains.  These
hints will be formatted with the type hint formatting options, if
the mode is not configured to ask the server to format them."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-display-lifetime-elision-hints-enable "never"
  "Whether to show elided lifetime inlay hints."
  :type '(choice
          (const "never")
          (const "always")
          (const "skip_trivial"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
  "When showing elided lifetime inlay hints, whether to use
parameter names or numeric placeholder names for the lifetimes."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-display-closure-return-type-hints nil
  "Whether to show closure return type inlay hints for closures
with block bodies."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-display-parameter-hints nil
  "Whether to show function parameter name inlay hints at the call site."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-display-reborrow-hints "never"
  "Whether to show inlay type hints for compiler inserted reborrows."
  :type '(choice
          (const "always")
          (const "never")
          (const "mutable"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-binding-mode-hints nil
  "Whether to show inlay type hints for binding modes."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-closing-brace-hints t
  "Whether to show inlay hints after a closing `}` to indicate what item it
belongs to."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-closing-brace-hints-min-lines 25
  "Minimum number of lines required before the `}` until the hint is shown
\(set to 0 or 1 to always show them)."
  :type 'integer
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lru-capacity nil
  "Number of syntax trees rust-analyzer keeps in memory."
  :type 'integer
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-target nil
  "Compilation target (target triple)."
  :type '(choice
          (string :tag "Target")
          (const :tag "None" nil))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-cargo-target-dir nil
  "Optional path to a rust-analyzer specific target directory.
This prevents rust-analyzer's `cargo check` and initial build-script and
proc-macro building from locking the `Cargo.lock` at the expense of
duplicating build artifacts.

Set to `true` to use a subdirectory of the existing target directory or
set to a path relative to the workspace to use that path."
  :type '(choice
          (string :tag "Directory")
          boolean)
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-cargo-watch-enable t
  "Enable Cargo watch."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-watch-command "check"
  "Cargo watch command."
  :type 'string
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-watch-args []
  "Extra arguments for `cargo check`."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-cargo-override-command []
  "Advanced option, fully override the command rust-analyzer uses for checking.
The command should include `--message=format=json` or similar option."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-check-all-targets t
  "Enables --all-targets for `cargo check`."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.2"))

(defcustom lsp-rust-analyzer-checkonsave-features nil
  "List of features to activate.
Corresponds to the `rust-analyzer` setting `rust-analyzer.check.features`.
When set to `nil` (default), the value of `lsp-rust-features' is inherited.
Set this to `\"all\"` to pass `--all-features` to cargo.
Note: setting this to `nil` means \"unset\", whereas setting this
to `[]` (empty vector) means \"set to empty list of features\",
which overrides any value that would otherwise be inherited from
`lsp-rust-features'."
  :type 'lsp-string-vector
  :group 'lsp-rust-rust-analyzer
  :package-version '(lsp-mode . "8.0.2"))

(defcustom lsp-rust-analyzer-check-invocation-location "workspace"
  "Specifies the working directory for running checks.
- \"workspace\": run checks for workspaces in the corresponding workspaces' root directories.  This falls back to \"root\" if rust-analyzer.check.invocationStrategy is set to once.
- \"root\": run checks in the project’s root directory.
This config only has an effect when rust-analyzer.check.overrideCommand is set."
  :type '(choice
          (const "workspace")
          (const "root"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-check-invocation-strategy "per_workspace"
  "Specifies the invocation strategy to use when running the check command.
If per_workspace is set, the command will be executed for each workspace.
If once is set, the command will be executed once.
This config only has an effect when rust-analyzer.check.overrideCommand is set."
  :type '(choice
          (const "per_workspace")
          (const "once"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-cargo-unset-test []
  "force rust-analyzer to unset `#[cfg(test)]` for the specified crates."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-cfg-set-test t
  "force rust-analyzer to set `#[cfg(test)]` for the current crate / workspace."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-use-client-watching t
  "Use client watching"
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-exclude-globs []
  "Exclude globs"
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-exclude-dirs []
  "These directories will be ignored by rust-analyzer."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default
  "Use a different function if you want formatted macro expansion results and
syntax highlighting."
  :type 'function
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.2.2"))

(defcustom lsp-rust-analyzer-diagnostics-enable t
  "Whether to show native rust-analyzer diagnostics."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-diagnostics-enable-experimental nil
  "Whether to show native rust-analyzer diagnostics that are still experimental
\(might have more false positives than usual)."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-diagnostics-disabled []
  "List of native rust-analyzer diagnostics to disable."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-diagnostics-warnings-as-hint []
  "List of warnings that should be displayed with hint severity."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-diagnostics-warnings-as-info []
  "List of warnings that should be displayed with info severity."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(define-obsolete-variable-alias
  'lsp-rust-analyzer-cargo-load-out-dirs-from-check
  'lsp-rust-analyzer-cargo-run-build-scripts
  "8.0.0")

(defcustom lsp-rust-analyzer-cargo-run-build-scripts t
  "Whether to run build scripts (`build.rs`) for more precise code analysis."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-cargo-build-scripts-invocation-location "workspace"
  "Specifies the working directory for running checks.
- \"workspace\": run checks for workspaces in the corresponding workspaces' root directories.  This falls back to \"root\" if rust-analyzer.check.invocationStrategy is set to once.
- \"root\": run checks in the project’s root directory.
This config only has an effect when rust-analyzer.check.overrideCommand is set."
  :type '(choice
          (const "workspace")
          (const "root"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-cargo-build-scripts-invocation-strategy "per_workspace"
  "Specifies the invocation strategy to use when running the check command.
If per_workspace is set, the command will be executed for each workspace.
If once is set, the command will be executed once.
This config only has an effect when rust-analyzer.check.overrideCommand is set."
  :type '(choice
          (const "per_workspace")
          (const "once"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-cargo-build-scripts-override-command []
  "Override the command rust-analyzer uses to run build scripts and build procedural macros."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-rustfmt-extra-args []
  "Additional arguments to rustfmt."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-rustfmt-override-command []
  "Advanced option, fully override the command rust-analyzer uses
for formatting."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-rustfmt-rangeformatting-enable nil
  "Enables the use of rustfmt's unstable range formatting command for the
`textDocument/rangeFormatting` request. The rustfmt option is unstable and only
available on a nightly build."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-assist-prefer-self nil
  "Prefer to use `Self` over the type name when inserting a type (e.g. in “fill
match arms” assist)."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-completion-add-call-parenthesis t
  "Whether to add parenthesis when completing functions."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-completion-add-call-argument-snippets t
  "Whether to add argument snippets when completing functions."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-completion-postfix-enable t
  "Whether to show postfix snippets like `dbg`, `if`, `not`, etc."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-call-info-full t
  "Whether to show function name and docs in parameter hints."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-proc-macro-enable t
  "Enable Proc macro support.
Implies `lsp-rust-analyzer-cargo-run-build-scripts'"
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-rust-analyzer-proc-macro-server nil
  "Internal config, path to proc-macro server executable."
  :type 'string
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-import-prefix "plain"
  "The path structure for newly inserted paths to use.
Valid values are:
 - \"plain\": Insert import paths relative to the current module, using up to
one `super' prefix if the parent module contains the requested item.
 - \"by_self\": Prefix all import paths with `self' if they don't begin with
`self', `super', `crate' or a crate name.
 - \"by_crate\": Force import paths to be absolute by always starting
them with `crate' or the crate name they refer to."
  :type '(choice
          (const "plain")
          (const "by_self")
          (const "by_crate"))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-import-granularity "crate"
  "How imports should be grouped into use statements."
  :type '(choice
          (const :tag "Merge imports from the same crate into a single use statement. This kind of nesting is only supported in Rust versions later than 1.24." "crate" )
          (const :tag "Merge imports from the same module into a single use statement." "module" )
          (const :tag "Don’t merge imports at all, creating one import per item." "item" )
          (const :tag "Do not change the granularity of any imports. For auto-import this has the same effect as `\"item\"'" "preserve" ))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-cargo-auto-reload t
  "Automatically refresh project info via `cargo metadata' on `Cargo.toml' changes."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-use-rustc-wrapper-for-build-scripts t
  "Use `RUSTC_WRAPPER=rust-analyzer' when running build scripts to avoid
compiling unnecessary things."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-completion-auto-import-enable t
  "Toggles the additional completions that automatically add imports when
completed. `lsp-completion-enable-additional-text-edit' must be non-nil
 for this feature to be fully enabled."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-completion-auto-self-enable t
  "Toggles the additional completions that automatically show method calls
and field accesses with self prefixed to them when inside a method."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-completion-term-search-enable nil
  "Enable term search based snippets like `Some(foo.bar().baz())`."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-rust-analyzer-import-enforce-granularity nil
  "Whether to enforce the import granularity setting for all files.
 If set to nil rust-analyzer will try to keep import styles consistent per file."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-imports-merge-glob t
  "Whether to allow import insertion to merge new imports into single path
glob imports like `use std::fmt::*;`."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-import-group t
  "Group inserted imports by the following order:
https://rust-analyzer.github.io/manual.html#auto-import.
 Groups are separated by newlines."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-highlighting-strings t
  "Use semantic tokens for strings."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-rustc-source nil
  "Path to the Cargo.toml of the rust compiler workspace."
  :type '(choice
          (file :tag "Path")
          (const :tag "None" nil))
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-linked-projects []
  "Disable project auto-discovery in favor of explicitly specified set of
projects. Elements must be paths pointing to `Cargo.toml`, `rust-project.json`,
or JSON objects in `rust-project.json` format."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-experimental-proc-attr-macros t
  "Whether to enable experimental support for expanding proc macro attributes."
  :type 'boolean
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

;; https://rust-analyzer.github.io/book/configuration#cargo.cfgs
(defcustom lsp-rust-analyzer-cargo-cfgs ["debug_assertions" "miri"]
  "Extra configurations that are passed to every cargo invocation."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-cargo-extra-args []
  "Extra arguments that are passed to every cargo invocation."
  :type 'lsp-string-vector
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-cargo-extra-env #s(hash-table)
  "Extra environment variables that will be set when running cargo, rustc or
other commands within the workspace.  Useful for setting RUSTFLAGS."
  :type 'alist
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-cargo-sysroot-src nil
  "Relative path to the sysroot library sources.
If left unset, this will default to {cargo.sysroot}/lib/rustlib/src/rust/library.
This option does not take effect until rust-analyzer is restarted."
  :type 'string
  :group 'lsp-rust-analyzer
  :package-version '(lsp-version . "9.0.1"))

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

(defun lsp-rust-analyzer-view-item-tree ()
  "Show item tree of rust file."
  (interactive)
  (-let* ((params (lsp-make-rust-analyzer-view-item-tree
                   :text-document (lsp--text-document-identifier)))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/viewItemTree"
                                      params))))
    (let ((buf (get-buffer-create "*rust-analyzer item tree*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (special-mode)
        (erase-buffer)
        (insert (lsp--render-string results "rust"))
        (pop-to-buffer buf)))))

(defun lsp-rust-analyzer-view-hir ()
  "View Hir of function at point."
  (interactive)
  (-let* ((params (lsp-make-rust-analyzer-expand-macro-params
                   :text-document (lsp--text-document-identifier)
                   :position (lsp--cur-position)))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/viewHir"
                                      params))))
    (let ((buf (get-buffer-create "*rust-analyzer hir*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (special-mode)
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
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (format "https://github.com/rust-lang/rust-analyzer/releases/latest/download/%s"
            (pcase system-type
              ('gnu/linux (format "rust-analyzer-%s-unknown-linux-gnu.gz" arch))
              ('darwin (format "rust-analyzer-%s-apple-darwin.gz" arch))
              ('windows-nt (format "rust-analyzer-%s-pc-windows-msvc.zip" arch)))))
  "Automatic download url for Rust Analyzer"
  :type 'string
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-rust-analyzer-store-path (f-join lsp-server-install-dir "rust"
                                                (pcase system-type
                                                  ('windows-nt "rust-analyzer.exe")
                                                  (_ "rust-analyzer")))
  "The path to the file in which `rust-analyzer' will be stored."
  :type 'file
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency
 'rust-analyzer
 `(:download :url lsp-rust-analyzer-download-url
             :decompress ,(pcase system-type ('windows-nt :zip) (_ :gzip))
             :store-path lsp-rust-analyzer-store-path
             :set-executable? t)
 `(:system ,(file-name-nondirectory lsp-rust-analyzer-store-path)))

(lsp-defun lsp-rust--analyzer-run-single ((&Command :arguments?))
  (lsp-rust-analyzer-run (lsp-seq-first arguments?)))

(lsp-defun lsp-rust--analyzer-show-references
  ((&Command :title :arguments? [_uri _filepos references]))
  (lsp-show-xrefs (lsp--locations-to-xref-items references) nil
                  (s-contains-p "reference" title)))

(declare-function dap-debug "ext:dap-mode" (template) t)

(lsp-defun lsp-rust--analyzer-debug-lens ((&Command :arguments? [args]))
  (lsp-rust-analyzer-debug args))

;; Semantic tokens

;; Modifier faces
(defface lsp-rust-analyzer-documentation-modifier-face
  '((t nil))
  "The face modification to use for documentation items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-declaration-modifier-face
  '((t nil))
  "The face modification to use for declaration items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-definition-modifier-face
  '((t nil))
  "The face modification to use for definition items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-static-modifier-face
  '((t nil))
  "The face modification to use for static items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-abstract-modifier-face
  '((t nil))
  "The face modification to use for abstract items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-deprecated-modifier-face
  '((t nil))
  "The face modification to use for deprecated items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-readonly-modifier-face
  '((t nil))
  "The face modification to use for readonly items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-default-library-modifier-face
  '((t nil))
  "The face modification to use for default-library items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-async-modifier-face
  '((t nil))
  "The face modification to use for async items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-attribute-modifier-face
  '((t nil))
  "The face modification to use for attribute items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-callable-modifier-face
  '((t nil))
  "The face modification to use for callable items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-constant-modifier-face
  '((t nil))
  "The face modification to use for constant items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-consuming-modifier-face
  '((t nil))
  "The face modification to use for consuming items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-control-flow-modifier-face
  '((t nil))
  "The face modification to use for control-flow items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-crate-root-modifier-face
  '((t nil))
  "The face modification to use for crate-root items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-injected-modifier-face
  '((t nil))
  "The face modification to use for injected items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-intra-doc-link-modifier-face
  '((t nil))
  "The face modification to use for intra-doc-link items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-library-modifier-face
  '((t nil))
  "The face modification to use for library items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-mutable-modifier-face
  '((t :underline t))
  "The face modification to use for mutable items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-public-modifier-face
  '((t nil))
  "The face modification to use for public items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-reference-modifier-face
  '((t :bold t))
  "The face modification to use for reference items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-trait-modifier-face
  '((t nil))
  "The face modification to use for trait items."
  :group 'lsp-rust-analyzer-semantic-tokens)

(defface lsp-rust-analyzer-unsafe-modifier-face
  '((t nil))
  "The face modification to use for unsafe items."
  :group 'lsp-rust-analyzer-semantic-tokens)


;; ---------------------------------------------------------------------
;; Semantic token modifier face customization

(defcustom lsp-rust-analyzer-documentation-modifier 'lsp-rust-analyzer-documentation-modifier-face
  "Face for semantic token modifier for `documentation' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-declaration-modifier 'lsp-rust-analyzer-declaration-modifier-face
  "Face for semantic token modifier for `declaration' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-definition-modifier 'lsp-rust-analyzer-definition-modifier-face
  "Face for semantic token modifier for `definition' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-static-modifier 'lsp-rust-analyzer-static-modifier-face
  "Face for semantic token modifier for `static' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-abstract-modifier 'lsp-rust-analyzer-abstract-modifier-face
  "Face for semantic token modifier for `abstract' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-deprecated-modifier 'lsp-rust-analyzer-deprecated-modifier-face
  "Face for semantic token modifier for `deprecated' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-readonly-modifier 'lsp-rust-analyzer-readonly-modifier-face
  "Face for semantic token modifier for `readonly' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-default-library-modifier 'lsp-rust-analyzer-default-library-modifier-face
  "Face for semantic token modifier for `default' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-async-modifier 'lsp-rust-analyzer-async-modifier-face
  "Face for semantic token modifier for `async' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-attribute-modifier 'lsp-rust-analyzer-attribute-modifier-face
  "Face for semantic token modifier for `attribute' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-callable-modifier 'lsp-rust-analyzer-callable-modifier-face
  "Face for semantic token modifier for `callable' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-constant-modifier 'lsp-rust-analyzer-constant-modifier-face
  "Face for semantic token modifier for `constant' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-consuming-modifier 'lsp-rust-analyzer-consuming-modifier-face
  "Face for semantic token modifier for `consuming' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-control-flow-modifier 'lsp-rust-analyzer-control-flow-modifier-face
  "Face for semantic token modifier for `control_flow' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-crate-root-modifier 'lsp-rust-analyzer-crate-root-modifier-face
  "Face for semantic token modifier for `crate_root' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-injected-modifier 'lsp-rust-analyzer-injected-modifier-face
  "Face for semantic token modifier for `injected' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-intra-doc-link-modifier 'lsp-rust-analyzer-intra-doc-link-modifier-face
  "Face for semantic token modifier for `intra_doc_link' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-library-modifier 'lsp-rust-analyzer-library-modifier-face
  "Face for semantic token modifier for `library' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-mutable-modifier 'lsp-rust-analyzer-mutable-modifier-face
  "Face for semantic token modifier for `mutable' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-public-modifier 'lsp-rust-analyzer-public-modifier-face
  "Face for semantic token modifier for `public' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-reference-modifier 'lsp-rust-analyzer-reference-modifier-face
  "Face for semantic token modifier for `reference' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-trait-modifier 'lsp-rust-analyzer-trait-modifier-face
  "Face for semantic token modifier for `trait' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-unsafe-modifier 'lsp-rust-analyzer-unsafe-modifier-face
  "Face for semantic token modifier for `unsafe' attribute."
  :type 'face
  :group 'lsp-rust-analyzer-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

;; ---------------------------------------------------------------------

(defun lsp-rust-analyzer--semantic-modifiers ()
  "Mapping between rust-analyzer keywords and fonts to apply.
The keywords are sent in the initialize response, in the semantic
tokens legend."
  `(("documentation"   . ,lsp-rust-analyzer-documentation-modifier)
    ("declaration"     . ,lsp-rust-analyzer-declaration-modifier)
    ("definition"      . ,lsp-rust-analyzer-definition-modifier)
    ("static"          . ,lsp-rust-analyzer-static-modifier)
    ("abstract"        . ,lsp-rust-analyzer-abstract-modifier)
    ("deprecated"      . ,lsp-rust-analyzer-deprecated-modifier)
    ("readonly"        . ,lsp-rust-analyzer-readonly-modifier)
    ("default_library" . ,lsp-rust-analyzer-default-library-modifier)
    ("async"           . ,lsp-rust-analyzer-async-modifier)
    ("attribute"       . ,lsp-rust-analyzer-attribute-modifier)
    ("callable"        . ,lsp-rust-analyzer-callable-modifier)
    ("constant"        . ,lsp-rust-analyzer-constant-modifier)
    ("consuming"       . ,lsp-rust-analyzer-consuming-modifier)
    ("control_flow"    . ,lsp-rust-analyzer-control-flow-modifier)
    ("crate_root"      . ,lsp-rust-analyzer-crate-root-modifier)
    ("injected"        . ,lsp-rust-analyzer-injected-modifier)
    ("intra_doc_link"  . ,lsp-rust-analyzer-intra-doc-link-modifier)
    ("library"         . ,lsp-rust-analyzer-library-modifier)
    ("mutable"         . ,lsp-rust-analyzer-mutable-modifier)
    ("public"          . ,lsp-rust-analyzer-public-modifier)
    ("reference"       . ,lsp-rust-analyzer-reference-modifier)
    ("trait"           . ,lsp-rust-analyzer-trait-modifier)
    ("unsafe"          . ,lsp-rust-analyzer-unsafe-modifier)))

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

;;
;;; Inlay hints

(defcustom lsp-rust-analyzer-debug-lens-extra-dap-args
  '(:MIMode "gdb" :miDebuggerPath "gdb" :stopAtEntry t :externalConsole :json-false)
  "Extra arguments to pass to DAP template when debugging a test from code lens.

As a rule of the thumb, do not add extra keys to this plist unless you exactly
what you are doing, it might break the \"Debug test\" lens otherwise.

See dap-mode documentation and cpptools documentation for the extra variables
meaning."
  :type 'plist
  :group 'lsp-rust-analyzer
  :package-version '(lsp-mode . "8.0.0"))

;;
;;; Lenses

(defgroup lsp-rust-analyzer-lens nil
  "LSP lens support for Rust when using rust-analyzer.

Lenses are (depending on your configuration) clickable links to
the right of function definitions and the like. These display
some useful information in their own right and/or perform a
shortcut action when clicked such as displaying uses of that
function or running an individual test.
"
  :prefix "lsp-rust-analyzer-lens-"
  :group 'lsp-rust-analyzer
  :link '(url-link "https://emacs-lsp.github.io/lsp-mode/")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-debug-enable t
  "Enable or disable the Debug lens."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-enable t
  "Master-enable of lenses in Rust files."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

;; This customisation "works" in that it works as described, but the default is fine and changing it
;; from the default will either stop lenses working or do nothing.
;;
;; If this is ever uncommented to re-enable the option, don't forget to also uncomment it in defun
;; lsp-rust-analyzer--make-init-options too or it'll not do anything.

;; (defcustom lsp-rust-analyzer-lens-force-custom-commands t
;;   "Internal config: use custom client-side commands even when the
;; client doesn't set the corresponding capability."
;;   :type 'boolean
;;   :group 'lsp-rust-analyzer-lens
;;   :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-implementations-enable t
  "Enable or disable the Implementations lens.

The Implementations lens shows `NN implementations' to the right
of the first line of an enum, struct, or union declaration. This
is the count of impl blocks, including derived traits. Clicking
on it gives a list of the impls of that type.
"
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

;; The valid range of values for this is documented in the rust-lang/rust-analyzer repository at the
;; path "editors/code/package.json"; the TL:DR is that it's "above_name" or "above_whole_item".
;; However, setting it to "above_whole_item" causes lenses to disappear in Emacs. I suspect this
;; feature has only ever been tested in some other IDE and it's broken in Emacs. So I've disabled it
;; for now.
;;
;; If this is ever uncommented to re-enable the option, don't forget to also uncomment it in defun
;; lsp-rust-analyzer--make-init-options too or it'll not do anything.

;; (defcustom lsp-rust-analyzer-lens-location "above_name"
;;   "Where to render annotations."
;;    :type '(choice
;;            (const :tag "Above name" "above_name")
;;            (const :tag "Above whole item" "above_whole_item")
;;    :group 'lsp-rust-analyzer-lens
;;    :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-references-adt-enable nil
  "Enable or disable the References lens on enums, structs, and traits.

The References lens shows `NN references` to the right of the
first line of each enum, struct, or union declaration. This is
the count of uses of that type. Clicking on it gives a list of
where that type is used."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-references-enum-variant-enable nil
  "Enable or disable the References lens on enum variants.

The References lens shows `NN references` to the right of the
first (or only) line of each enum variant. This is the count of
uses of that enum variant. Clicking on it gives a list of where
that enum variant is used."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-references-method-enable nil
  "Enable or disable the References lens on functions.

The References lens shows `NN references` to the right of the
first line of each function declaration. This is the count of
uses of that function. Clicking on it gives a list of where that
function is used."

  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-references-trait-enable nil
  "Enable or disable the References lens on traits.

The References lens shows `NN references` to the right of the
first line of each trait declaration. This is a count of uses of
that trait. Clicking on it gives a list of where that trait is
used.

There is some overlap with the Implementations lens which slows
all of the trait's impl blocks, but this also shows other uses
such as imports and dyn traits."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-rust-analyzer-lens-run-enable t
  "Enable or disable the Run lens."
  :type 'boolean
  :group 'lsp-rust-analyzer-lens
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-rust-analyzer-initialized? ()
  (when-let* ((workspace (lsp-find-workspace 'rust-analyzer (buffer-file-name))))
    (eq 'initialized (lsp--workspace-status workspace))))

(defun lsp-rust-analyzer-expand-macro ()
  "Expands the macro call at point recursively."
  (interactive)
  (-if-let* ((params (lsp-make-rust-analyzer-expand-macro-params
                      :text-document (lsp--text-document-identifier)
                      :position (lsp--cur-position)))
             (response (lsp-request
                        "rust-analyzer/expandMacro"
                        params))
             ((&rust-analyzer:ExpandedMacro :expansion) response))
      (funcall lsp-rust-analyzer-macro-expansion-method expansion)
    (lsp--error "No macro found at point, or it could not be expanded.")))

(defun lsp-rust-analyzer-macro-expansion-default (result)
  "Default method for displaying macro expansion."
  (let* ((root (lsp-workspace-root default-directory))
         (buf (get-buffer-create (get-buffer-create (format "*rust-analyzer macro expansion %s*" root)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (lsp--render-string result "rust"))
        (special-mode)))
    (pop-to-buffer buf)))

;;
;;; Runnables

(defvar lsp-rust-analyzer--last-runnable nil
  "Record the last runnable.")

(defun lsp-rust-analyzer--runnables ()
  "Return list of runnables."
  (lsp-send-request (lsp-make-request
                     "experimental/runnables"
                     (lsp-make-rust-analyzer-runnables-params
                      :text-document (lsp--text-document-identifier)
                      :position? (lsp--cur-position)))))

(defun lsp-rust-analyzer--select-runnable ()
  "Select runnable."
  (lsp--completing-read
   "Select runnable:"
   (if lsp-rust-analyzer--last-runnable
       (cons lsp-rust-analyzer--last-runnable
             (-remove (-lambda ((&rust-analyzer:Runnable :label))
                        (equal label (lsp-get lsp-rust-analyzer--last-runnable :label)))
                      (lsp-rust-analyzer--runnables)))
     (lsp-rust-analyzer--runnables))
   (-lambda ((&rust-analyzer:Runnable :label)) label)))

(defun lsp-rust-analyzer--common-runner (runnable)
  "Execute a given RUNNABLE.

Extract the arguments, prepare the minor mode (cargo-process-mode if possible)
and run a compilation"
  (-let* (((&rust-analyzer:Runnable :kind :label :args) runnable)
          ((&rust-analyzer:RunnableArgs :cargo-args :executable-args :workspace-root? :expect-test? :environment?) args)
          (default-directory (or workspace-root? default-directory)))
    (if (not (string-equal kind "cargo"))
        (lsp--error "'%s' runnable is not supported" kind)
      (compilation-start
       (string-join (append (when expect-test? '("env" "UPDATE_EXPECT=1"))
                            (when environment? (lsp-rust-analyzer--to-bash-env environment?))
                            (list "cargo") cargo-args
                            (when executable-args '("--")) executable-args '()) " ")

       ;; cargo-process-mode is nice, but try to work without it...
       (if (functionp 'cargo-process-mode) 'cargo-process-mode nil)
       (lambda (_) (concat "*" label "*"))))))

(defun lsp-rust-analyzer--to-bash-env (env-vars)
  "Extract the environment variables from plist ENV-VARS."
  (cl-loop for (key value) on env-vars by 'cddr
           collect (format "%s=%s" (substring (symbol-name key) 1) value)))

(defun lsp-rust-analyzer-run (runnable)
  "Select and run a RUNNABLE action."
  (interactive (list (lsp-rust-analyzer--select-runnable)))
  (when (lsp-rust-analyzer--common-runner runnable)
    (setq lsp-rust-analyzer--last-runnable runnable)))

(defun lsp-rust-analyzer-debug (runnable)
  "Select and debug a RUNNABLE action."
  (interactive (list (lsp-rust-analyzer--select-runnable)))
  (unless (or (featurep 'dap-cpptools) (featurep 'dap-gdb))
    (user-error "You must require `dap-cpptools' or 'dap-gdb'"))
  (-let (((&rust-analyzer:Runnable
           :args (&rust-analyzer:RunnableArgs :cargo-args :workspace-root? :executable-args)
           :label) runnable))
    (pcase (aref cargo-args 0)
      ("run" (aset cargo-args 0 "build"))
      ("test" (unless (-contains? (append cargo-args ()) "--no-run")
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
              (`() (user-error "No compilation artifacts or obtaining the runnable artifacts failed"))
              (`(,spec) spec)
              (_ (user-error "Multiple compilation artifacts are not supported")))))
         (list :type (if (featurep 'dap-gdb) "gdb" "cppdbg")
               :request "launch"
               :name label
               :args executable-args
               :cwd workspace-root?
               :sourceLanguages ["rust"]
               :program)
         (append lsp-rust-analyzer-debug-lens-extra-dap-args)
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

(defun lsp-rust-analyzer-open-cargo-toml (&optional new-window)
  "Open the closest Cargo.toml from the current file.

Rust-Analyzer LSP protocol documented here and added in November 2020
https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#open-cargotoml

If NEW-WINDOW (interactively the prefix argument) is non-nil,
open in a new window."
  (interactive "P")
  (-if-let (workspace (lsp-find-workspace 'rust-analyzer (buffer-file-name)))
      (-if-let* ((response (with-lsp-workspace workspace
                             (lsp-send-request (lsp-make-request
                                                "experimental/openCargoToml"
                                                (lsp-make-rust-analyzer-open-cargo-toml-params
                                                 :text-document (lsp--text-document-identifier))))))
                 ((&Location :uri :range) response))
          (funcall (if new-window #'find-file-other-window #'find-file)
                   (lsp--uri-to-path uri))
        (lsp--warn "Couldn't find a Cargo.toml file or your version of rust-analyzer doesn't support this extension"))
    (lsp--error "OpenCargoToml is an extension available only with rust-analyzer")))

(defun lsp-rust-analyzer-open-external-docs ()
  "Open a URL for documentation related to the current TextDocumentPosition.

Rust-Analyzer LSP protocol documented here
https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#open-external-documentation"
  (interactive)
  (-if-let* ((params (lsp-make-rust-analyzer-open-external-docs-params
                      :text-document (lsp--text-document-identifier)
                      :position (lsp--cur-position)))
             (url (lsp-request "experimental/externalDocs" params)))
      (browse-url url)
    (lsp--warn "Couldn't find documentation URL or your version of rust-analyzer doesn't support this extension")))

(defun lsp-rust-analyzer--related-tests ()
  "Get runnable test items related to the current TextDocumentPosition.
Calls a rust-analyzer LSP extension endpoint that returns a wrapper over
Runnable[]."
  (lsp-send-request (lsp-make-request
                     "rust-analyzer/relatedTests"
                     (lsp--text-document-position-params))))

(defun lsp-rust-analyzer--select-related-test ()
  "Call the endpoint and ask for user selection.

Cannot reuse `lsp-rust-analyzer--select-runnable' because the runnables endpoint
responds with Runnable[], while relatedTests responds with TestInfo[],
which is a wrapper over runnable. Also, this method doesn't set
the `lsp-rust-analyzer--last-runnable' variable."
  (-if-let* ((resp (lsp-rust-analyzer--related-tests))
             (runnables (seq-map
                         #'lsp:rust-analyzer-related-tests-runnable
                         resp)))
      (lsp--completing-read
       "Select test: "
       runnables
       #'lsp:rust-analyzer-runnable-label)))

(defun lsp-rust-analyzer-related-tests (runnable)
  "Execute a RUNNABLE test related to the current document position.

Rust-Analyzer LSP protocol extension
https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#related-tests"
  (interactive (list (lsp-rust-analyzer--select-related-test)))
  (if runnable
      (lsp-rust-analyzer--common-runner runnable)
    (lsp--info "There are no tests related to the symbol at point")))

(defun lsp-rust-analyzer-move-item (direction)
  "Move item under cursor or selection in some DIRECTION"
  (let* ((params (lsp-make-rust-analyzer-move-item-params
                  :text-document (lsp--text-document-identifier)
                  :range (if (use-region-p)
                             (lsp--region-to-range (region-beginning) (region-end))
                           (lsp--region-to-range (point) (point)))
                  :direction direction))
         (edits (lsp-request "experimental/moveItem" params)))
    (lsp--apply-text-edits edits 'code-action)))

(defun lsp-rust-analyzer-move-item-up ()
  "Move item under cursor or selection up"
  (interactive)
  (lsp-rust-analyzer-move-item "Up"))

(defun lsp-rust-analyzer-move-item-down ()
  "Move item under cursor or selection down"
  (interactive)
  (lsp-rust-analyzer-move-item "Down"))

(defun lsp-rust-analyzer--make-init-options ()
  "Init options for rust-analyzer"
  `(:diagnostics
    ( :enable ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable)
      :enableExperimental ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable-experimental)
      :disabled ,lsp-rust-analyzer-diagnostics-disabled
      :warningsAsHint ,lsp-rust-analyzer-diagnostics-warnings-as-hint
      :warningsAsInfo ,lsp-rust-analyzer-diagnostics-warnings-as-info)
    :imports ( :granularity ( :enforce ,(lsp-json-bool lsp-rust-analyzer-import-enforce-granularity)
                              :group ,lsp-rust-analyzer-import-granularity)
               :group ,(lsp-json-bool lsp-rust-analyzer-import-group)
               :merge (:glob ,(lsp-json-bool lsp-rust-analyzer-imports-merge-glob))
               :prefix ,lsp-rust-analyzer-import-prefix)
    :lruCapacity ,lsp-rust-analyzer-lru-capacity
    ;; This `checkOnSave` is called `check` in the `rust-analyzer` docs, not
    ;; `checkOnSave`, but the `rust-analyzer` source code shows that both names
    ;; work. The `checkOnSave` name has been supported by `rust-analyzer` for a
    ;; long time, whereas the `check` name was introduced here in 2023:
    ;; https://github.com/rust-lang/rust-analyzer/commit/d2bb62b6a81d26f1e41712e04d4ac760f860d3b3
    :checkOnSave ( :enable ,(lsp-json-bool lsp-rust-analyzer-cargo-watch-enable)
                   :command ,lsp-rust-analyzer-cargo-watch-command
                   :extraArgs ,lsp-rust-analyzer-cargo-watch-args
                   :allTargets ,(lsp-json-bool lsp-rust-analyzer-check-all-targets)
                   ;; We need to distinguish between setting this to the empty
                   ;; vector, and not setting it at all, which `rust-analyzer`
                   ;; interprets as "inherit from
                   ;; `rust-analyzer.cargo.features`". We use `nil` to mean
                   ;; "unset".
                   ,@(when (vectorp lsp-rust-analyzer-checkonsave-features)
                       `(:features ,lsp-rust-analyzer-checkonsave-features))
                   :overrideCommand ,lsp-rust-analyzer-cargo-override-command
                   :invocationLocation ,lsp-rust-analyzer-check-invocation-location
                   :invocationStrategy ,lsp-rust-analyzer-check-invocation-strategy)
    :highlightRelated ( :breakPoints (:enable ,(lsp-json-bool lsp-rust-analyzer-highlight-breakpoints))
                        :closureCaptures (:enable ,(lsp-json-bool lsp-rust-analyzer-highlight-closure-captures))
                        :exitPoints (:enable ,(lsp-json-bool lsp-rust-analyzer-highlight-exit-points))
                        :references (:enable ,(lsp-json-bool lsp-rust-analyzer-highlight-references))
                        :yieldPoints (:enable ,(lsp-json-bool lsp-rust-analyzer-highlight-yield-points)))
    :files ( :exclude ,lsp-rust-analyzer-exclude-globs
             :watcher ,(if lsp-rust-analyzer-use-client-watching "client" "notify")
             :excludeDirs ,lsp-rust-analyzer-exclude-dirs)
    :cfg ( :setTest ,(lsp-json-bool lsp-rust-analyzer-cfg-set-test) )
    :cargo ( :allFeatures ,(lsp-json-bool lsp-rust-all-features)
             :noDefaultFeatures ,(lsp-json-bool lsp-rust-no-default-features)
             :features ,lsp-rust-features
             :cfgs ,lsp-rust-analyzer-cargo-cfgs
             :extraArgs ,lsp-rust-analyzer-cargo-extra-args
             :extraEnv ,lsp-rust-analyzer-cargo-extra-env
             :target ,lsp-rust-analyzer-cargo-target
             :targetDir ,lsp-rust-analyzer-cargo-target-dir
             :runBuildScripts ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
             ;; Obsolete, but used by old Rust-Analyzer versions
             :loadOutDirsFromCheck ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
             :autoreload ,(lsp-json-bool lsp-rust-analyzer-cargo-auto-reload)
             :useRustcWrapperForBuildScripts ,(lsp-json-bool lsp-rust-analyzer-use-rustc-wrapper-for-build-scripts)
             :unsetTest ,lsp-rust-analyzer-cargo-unset-test
             :sysrootSrc ,lsp-rust-analyzer-cargo-sysroot-src
             :buildScripts (:enable ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
                            :overrideCommand ,lsp-rust-analyzer-cargo-build-scripts-override-command
                            :invocationLocation ,lsp-rust-analyzer-cargo-build-scripts-invocation-location
                            :invocationStrategy ,lsp-rust-analyzer-cargo-build-scripts-invocation-strategy))
    :rustfmt ( :extraArgs ,lsp-rust-analyzer-rustfmt-extra-args
               :overrideCommand ,lsp-rust-analyzer-rustfmt-override-command
               :rangeFormatting (:enable ,(lsp-json-bool lsp-rust-analyzer-rustfmt-rangeformatting-enable)))
    :lens ( :debug (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-debug-enable))
            :enable ,(lsp-json-bool lsp-rust-analyzer-lens-enable)
            ;; :forceCustomCommands ,(lsp-json-bool lsp-rust-analyzer-lens-force-custom-commands)
            :implementations (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-implementations-enable))
            ;; :location ,lsp-rust-analyzer-lens-location
            :references ( :adt (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-references-adt-enable))
                          :enumVariant (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-references-enum-variant-enable))
                          :method (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-references-method-enable))
                          :trait (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-references-trait-enable)))
            :run (:enable ,(lsp-json-bool lsp-rust-analyzer-lens-run-enable)))

    :inlayHints ( :bindingModeHints (:enable ,(lsp-json-bool lsp-rust-analyzer-binding-mode-hints))
                  :chainingHints (:enable ,(lsp-json-bool lsp-rust-analyzer-display-chaining-hints))
                  :closingBraceHints ( :enable ,(lsp-json-bool lsp-rust-analyzer-closing-brace-hints)
                                       :minLines ,lsp-rust-analyzer-closing-brace-hints-min-lines)
                  :closureCaptureHints (:enable ,(lsp-json-bool lsp-rust-analyzer-closure-capture-hints))
                  :closureReturnTypeHints (:enable ,lsp-rust-analyzer-closure-return-type-hints)
                  :closureStyle ,lsp-rust-analyzer-closure-style
                  :discriminantHints (:enable ,lsp-rust-analyzer-discriminants-hints)

                  :expressionAdjustmentHints ( :enable ,lsp-rust-analyzer-expression-adjustment-hints
                                               :hideOutsideUnsafe ,(lsp-json-bool lsp-rust-analyzer-expression-adjustment-hide-unsafe)
                                               :mode ,lsp-rust-analyzer-expression-adjustment-hints-mode)
                  :implicitDrops (:enable ,(lsp-json-bool lsp-rust-analyzer-implicit-drops))
                  :lifetimeElisionHints ( :enable ,lsp-rust-analyzer-display-lifetime-elision-hints-enable
                                          :useParameterNames ,(lsp-json-bool lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names))
                  :maxLength ,lsp-rust-analyzer-max-inlay-hint-length
                  :parameterHints (:enable ,(lsp-json-bool lsp-rust-analyzer-display-parameter-hints))
                  :reborrowHints (:enable ,lsp-rust-analyzer-display-reborrow-hints)
                  :renderColons ,(lsp-json-bool lsp-rust-analyzer-server-format-inlay-hints)
                  :typeHints ( :enable ,(lsp-json-bool lsp-inlay-hint-enable)
                               :hideClosureInitialization ,(lsp-json-bool lsp-rust-analyzer-hide-closure-initialization)
                               :hideNamedConstructor ,(lsp-json-bool lsp-rust-analyzer-hide-named-constructor)))
    :assist ( :preferSelf ,(lsp-json-bool lsp-rust-analyzer-assist-prefer-self))
    :completion ( :addCallParenthesis ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-parenthesis)
                  :addCallArgumentSnippets ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-argument-snippets)
                  :postfix (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-postfix-enable))
                  :autoimport (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-auto-import-enable))
                  :autoself (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-auto-self-enable))
                  :termSearch (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-term-search-enable)))
    :callInfo (:full ,(lsp-json-bool lsp-rust-analyzer-call-info-full))
    :procMacro (:enable ,(lsp-json-bool lsp-rust-analyzer-proc-macro-enable)
                :server ,lsp-rust-analyzer-proc-macro-server)
    :rustcSource ,lsp-rust-analyzer-rustc-source
    :linkedProjects ,lsp-rust-analyzer-linked-projects
    :highlighting (:strings ,(lsp-json-bool lsp-rust-analyzer-highlighting-strings))
    :experimental (:procAttrMacros ,(lsp-json-bool lsp-rust-analyzer-experimental-proc-attr-macros))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-rust-analyzer-server-command)
                             t)
                            (lsp-package-path 'rust-analyzer)
                            "rust-analyzer")
                       ,@(cl-rest lsp-rust-analyzer-server-command)))
                   (lambda () t))
  :activation-fn (lsp-activate-on "rust")
  :priority (if (eq lsp-rust-server 'rust-analyzer) 1 -1)
  :initialization-options 'lsp-rust-analyzer--make-init-options
  :notification-handlers (ht<-alist lsp-rust-notification-handlers)
  :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single)
                       ("rust-analyzer.debugSingle" #'lsp-rust--analyzer-debug-lens)
                       ("rust-analyzer.showReferences" #'lsp-rust--analyzer-show-references)
                       ("rust-analyzer.triggerParameterHints" #'lsp--action-trigger-parameter-hints))
  :library-folders-fn (lambda (_workspace) lsp-rust-analyzer-library-directories)
  :semantic-tokens-faces-overrides `( :discard-default-modifiers t
                                      :modifiers ,(lsp-rust-analyzer--semantic-modifiers))
  :server-id 'rust-analyzer
  :custom-capabilities `((experimental .
                                       ((snippetTextEdit . ,(and lsp-enable-snippet (fboundp 'yas-minor-mode)))
                                        (commands . ((commands .
                                                               [
                                                                "rust-analyzer.runSingle"
                                                                "rust-analyzer.debugSingle"
                                                                "rust-analyzer.showReferences"
                                                                ;; "rust-analyzer.gotoLocation"
                                                                "rust-analyzer.triggerParameterHints"
                                                                ;; "rust-analyzer.rename"
                                                                ]))))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'rust-analyzer callback error-callback))))

(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  "Extract first non-comment line from rust-analyzer's hover CONTENTS.
The first line of the hover contents is usally about memory layout or notable
traits starting with //, with the actual signature follows."
  (let* ((lines (s-lines (s-trim (lsp--render-element contents))))
         (non-comment-lines (--filter (not (s-prefix? "//" it)) lines)))
    (if non-comment-lines
        (car non-comment-lines)
      (car lines))))

(lsp-consistency-check lsp-rust)

(provide 'lsp-rust)
;;; lsp-rust.el ends here
