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

(defgroup lsp-rust nil
  "LSP support for Rust, using Rust Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/rust-lang/rls")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-library-directories '("~/.cargo/registry/src" "~/.rustup/toolchains")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-sysroot nil
  "If non-nil, use the given path as the sysroot for all rustc invocations instead of trying to detect the sysroot automatically."
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
  "Clear the RUST_LOG environment variable before running rustc
or cargo."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-lib nil
  "If non-nil, checks the project as if you passed the `--lib' argument to cargo.
 Mutually exclusive with, and preferred over, `lsp-rust-build-bin'. (Unstable)"
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-bin nil
  "If non-nil, checks the project as if you passed `-- bin <build_bin>' argument to cargo.
 Mutually exclusive with `lsp-rust-build-lib'. (Unstable)"
  :type '(choice
           (const :tag "None" nil)
           (string :tag "Binary"))
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-cfg-test nil
  "If non-nil, checks the project as if you were running `cargo test' rather than cargo build. I.e., compiles (but does not run) test code."
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

(defcustom lsp-rust-use-crate-blacklist t
  "Don't index crates on the crate blacklist."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-build-on-save nil
  "Only index the project when a file is saved and not on
change."
  :type 'boolean
  :group 'lsp-rust
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-rust-features []
  "A list of Cargo features to enable."
  :type '(restricted-sexp :match-alternatives (lambda (xs)
                                                (and
                                                  (vectorp xs)
                                                  (seq-every-p #'stringp xs))))
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
  "Controls eagerness of clippy diagnostics when available. Valid
  values are (case-insensitive):\n - \"off\": Disable clippy
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
  "Checks the project as if you were running cargo check
--all-targets (I.e., check all targets and integration tests
too)."
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
   ("rust.use_crate_blacklist" lsp-rust-use-crate-blacklist t)
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

(defvar lsp-clients-rust-progress-string ""
  "Rust progress status as reported by the RLS server.")

(put 'lsp-rust-progress-string 'risky-local-variable t)
(add-to-list 'global-mode-string (list '(t lsp-clients-rust-progress-string)))

(defun lsp-clients--rust-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  (-let (((&hash "done" "message" "title") params))
    (if (or done (s-blank-str? message))
        (setq lsp-clients-rust-progress-string nil)
      (setq lsp-clients-rust-progress-string (format "%s - %s" title (or message "")))
      (lsp-log lsp-clients-rust-progress-string))))

(defcustom lsp-rust-rls-server-command '("rls")
  "Command to start RLS."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(cl-defmethod lsp-execute-command
  (_server (command (eql rls.run)) params)
  (-let* (((&hash "env" "binary" "args" "cwd") (seq-first params))
          (default-directory (or cwd (lsp-workspace-root) default-directory) ))
    (compile
     (format "%s %s %s"
             (s-join " " (ht-amap (format "%s=%s" key value) env))
             binary
             (s-join " " args)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-rust-rls-server-command))
                  :major-modes '(rust-mode rustic-mode)
                  :priority -1
                  :server-id 'rls
                  :initialization-options '((omitInitBuild . t)
                                            (cmdRun . t))
                  :notification-handlers (ht ("window/progress" 'lsp-clients--rust-window-progress))
                  :action-handlers (ht ("rls.run" 'lsp-rust--rls-run))
                  :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "rust"))))))

(provide 'lsp-rust)
;;; lsp-rust.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
