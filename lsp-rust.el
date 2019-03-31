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
  "Settings for rls."
  :group 'tools
  :tag "Language Server")

(defcustom lsp-rust-sysroot nil
  "--sysroot"
  :type '(repeat string))

(defcustom lsp-rust-target nil
  "--target"
  :type '(repeat string)
  :group 'lsp-rust)

(defcustom lsp-rust-rustflags nil
  "Flags added to RUSTFLAGS."
  :type '(repeat string))

(defcustom lsp-rust-clear-env-rust-log t
  "Clear the RUST_LOG environment variable before running rustc
or cargo."
  :type 'boolean)

(defcustom lsp-rust-build-lib nil
  "Specify to run analysis as if running `cargo check --lib`. Use
`null` to auto-detect. (unstable)"
  :type '(repeat boolean))

(defcustom lsp-rust-build-bin nil
  "Specify to run analysis as if running `cargo check --bin
<name>`. Use `null` to auto-detect. (unstable)"
  :type '(repeat string))

(defcustom lsp-rust-cfg-test nil
  "Build cfg(test) code. (unstable)"
  :type 'boolean)

(defcustom lsp-rust-unstable-features nil
  "Enable unstable features."
  :type 'boolean)

(defcustom lsp-rust-wait-to-build 1500
  "Time in milliseconds between receiving a change notification
and starting build."
  :type 'number)

(defcustom lsp-rust-show-warnings t
  "Show warnings."
  :type 'boolean)

(defcustom lsp-rust-use-crate-blacklist t
  "Don't index crates on the crate blacklist."
  :type 'boolean)

(defcustom lsp-rust-build-on-save nil
  "Only index the project when a file is saved and not on
change."
  :type 'boolean)

(defcustom lsp-rust-features nil
  "A list of Cargo features to enable."
  :type '(repeat string))

(defcustom lsp-rust-all-features nil
  "Enable all Cargo features."
  :type 'boolean)

(defcustom lsp-rust-no-default-features nil
  "Do not enable default Cargo features."
  :type 'boolean)

(defcustom lsp-rust-racer-completion t
  "Enables code completion using racer."
  :type 'boolean)

(defcustom lsp-rust-clippy-preference "opt-in"
  "Controls eagerness of clippy diagnostics when available. Valid
  values are (case-insensitive):\n - \"off\": Disable clippy
  lints.\n - \"opt-in\": Clippy lints are shown when crates
  specify `#![warn(clippy)]`.\n - \"on\": Clippy lints enabled
  for all crates in workspace.\nYou need to install clippy via
  rustup if you haven't already."
  :type '(choice (const :tag "on" "opt-in" "off")))

(defcustom lsp-rust-jobs nil
  "Number of Cargo jobs to be run in parallel."
  :type '(repeat number))

(defcustom lsp-rust-all-targets t
  "Checks the project as if you were running cargo check
--all-targets (I.e., check all targets and integration tests
too)."
  :type 'boolean)

(defcustom lsp-rust-target-dir nil
  "When specified, it places the generated analysis files at the
specified target directory. By default it is placed target/rls
directory."
  :type '(repeat string))

(defcustom lsp-rust-rustfmt-path nil
  "When specified, RLS will use the Rustfmt pointed at the path
instead of the bundled one"
  :type '(repeat string))

(defcustom lsp-rust-build-command nil
  "EXPERIMENTAL (requires `unstable_features`)\nIf set, executes
a given program responsible for rebuilding save-analysis to be
loaded by the RLS. The program given should output a list of
resulting .json files on stdout. \nImplies `rust.build_on_save`:
true."
  :type '(repeat string))

(defcustom lsp-rust-full-docs nil
  "Instructs cargo to enable full documentation extraction during
save-analysis while building the crate."
  :type '(repeat boolean))

(defcustom lsp-rust-show-hover-context t
  "Show additional context in hover tooltips when available. This
is often the type local variable declaration."
  :type 'boolean)

(lsp-register-custom-settings
 '(("rust.show_hover_context" lsp-rust-show-hover-context t)
   ("rust.full_docs" lsp-rust-full-docs)
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
   ("rust.build_lib" lsp-rust-build-lib)
   ("rust.clear_env_rust_log" lsp-rust-clear-env-rust-log t)
   ("rust.rustflags" lsp-rust-rustflags)
   ("rust.target" lsp-rust-target)
   ("rust.sysroot" lsp-rust-sysroot)))

(defun lsp-clients--rust-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rls"))
                  :major-modes '(rust-mode rustic-mode)
                  :priority -1
                  :server-id 'rls
                  :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "rust"))))))

(provide 'lsp-rust)
;;; lsp-rust.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
