;;; lsp-zig.el --- lsp-mode Zig integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Riccardo Binetti
;; Copyright (C) 2021-2026 lsp-mode maintainers

;; Author: Riccardo Binetti <rbino@gmx.com>
;; Keywords: languages,tools

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

;;  client for zls, the Zig language server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-zig nil
  "LSP support for Zig via zls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/zigtools/zls"))

(defcustom lsp-zig-zls-executable "zls"
  "The zls executable to use.

Leave as just the executable name to use the default behavior of finding the
executable with variable `exec-path'."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-trace-server "off"
  "Traces the communication between Emacs and the language server."
  :group 'lsp-zig
  :type '(choice (const "off")
                 (const "messages")
                 (const "verbose")))

(defcustom lsp-zls-enable-snippets t
  "Enables snippet completions when the client also supports them."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-enable-argument-placeholders t
  "Whether to enable function argument placeholder completions."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-enable-build-on-save nil
  "Whether to enable build-on-save diagnostics."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-build-on-save-step "install"
  "Select which step should be executed on build-on-save."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-enable-autofix nil
  "Whether to automatically fix errors on save.
Currently supports adding and removing discards."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-semantic-tokens "partial"
  "Traces the communication between Emacs and the language server."
  :group 'lsp-zig
  :type '(choice (const "off")
                 (const "messages")
                 (const "verbose")))

(defcustom lsp-zig-enable-inlay-hints t
  "Enables inlay hint support when the client also supports it."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-show-variable-type-hints t
  "Enable inlay hints for variable type."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-show-parameter-name t
  "Enable inlay hints for parameter names."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-exclude-single-argument t
  "Don't show inlay hints for single argument calls."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-show-builtin t
  "Don't show inlay hints for single argument calls."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-hide-redundant-param-names nil
  "Hides inlay hints when parameter name matches the identifier (e.g. foo: foo)."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-inlay-hints-hide-redundant-param-names-last-token nil
  "Hides inlay hints when parameter name matches the last token of a parameter
node (e.g. foo: bar.foo, foo: &foo)."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-warn-style nil
  "Enables warnings for style guideline mismatches."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-highlight-global-var-declarations nil
  "Whether to highlight global var declarations."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-dangerous-comptime-experiments-do-not-enable nil
  "When true, skips searching for references in std.
Improves lookup speed for functions in user's code.  Renaming and
go-to-definition will continue to work as is."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-skip-std-references nil
  "hen true, skips searching for references in std.
Improves lookup speed for functions in user's code.  Renaming and
 go-to-definition will continue to work as is."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-prefer-ast-check-as-child-process t
  "Favor using `zig ast-check` instead of ZLS's fork."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-record-session nil
  "When true, zls will record all request is receives and write in into
`record_session_path`, so that they can replayed with `zls replay`."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-record-session-path ""
  "Output file path when `record_session` is set.
The recommended file extension *.zlsreplay."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-replay-session-path ""
  "Used when calling `zls replay` for specifying the replay file.
If no extra argument is given `record_session_path` is used as the default path."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-builtin-path ""
  "Path to `builtin'; useful for debugging, automatically set if let null."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zig-lib-path ""
  "Zig library path.
e.g. `/path/to/zig/lib/zig`, used to analyze std library imports."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zig-exe-path ""
  "	Zig executable path.
e.g. /path/to/zig/zig, used to run the custom build runner.  If null, zig is
looked up in PATH.  Will be used to infer the zig standard library path if none
is provided."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-build-runner-path ""
  "Path to the `build_runner.zig` file provided by zls.
null is equivalent to `${executable_directory}/build_runner.zig`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-global-cache-path ""
  "Path to a directory that will be used as zig's cache.
null is equivalent to `${KnownFolders.Cache}/zls`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-build-runner-global-cache-path ""
  "Path to a directory that will be used as the global cache path when executing
a projects build.zig.  null is equivalent to the path shown by `zig env`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-completions-with-replace nil
  "Completions confirm behavior.
If `true', replace the text after the cursor."
  :group 'lsp-zig
  :type 'boolean)

;;
;;; Installation

(defcustom lsp-zig-server-store-path
  (expand-file-name "zig/" lsp-server-install-dir)
  "The path to the file in which zls will be stored."
  :type 'file
  :group 'lsp-zig)

(defconst lsp-zig-download-url-format
  "https://github.com/zigtools/zls/releases/latest/download/zls-%s-%s.%s"
  "Format to the download url link.")

(defun lsp-zig--zls-url ()
  "Return Url points to the zls' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-zig-download-url-format arch "windows" "zip"))
      (darwin
       (format lsp-zig-download-url-format arch "macos" "tar.gz"))
      (gnu/linux
       (format lsp-zig-download-url-format arch "linux" "tar.gz")))))

(defun lsp-zig--stored-zls-executable ()
  "Return the stored zls executable.

This is differ from the variable `lsp-zig-zls-executable'; this is local storage
and not the global storage."
  (f-join lsp-zig-server-store-path
          (pcase system-type ('windows-nt "zls.exe") (_ "zls"))))

(lsp-dependency
 'zls
 '(:system "zls")
 `(:download :url ,(lsp-zig--zls-url)
             :decompress ,(pcase system-type ('windows-nt :zip) (_ :targz))
             :store-path ,(f-join lsp-zig-server-store-path "temp")
             :set-executable? t)
 `(:system ,(lsp-zig--stored-zls-executable)))

;;
;;; Core

(lsp-register-custom-settings
 '(("zls.enable_snippets" lsp-zls-enable-snippets t)
   ("zls.enable_argument_placeholders" lsp-zig-enable-argument-placeholders t)
   ("zls.enable_build_on_save" lsp-zig-enable-build-on-save t)
   ("zls.build_on_save_step" lsp-zig-build-on-save-step)
   ("zls.enable_autofix" lsp-zig-enable-autofix t)
   ("zls.semantic_tokens" lsp-zig-semantic-tokens)
   ("zls.enable_inlay_hints" lsp-zig-enable-inlay-hints t)
   ("zls.inlay_hints_show_variable_type_hints" lsp-zig-inlay-hints-show-variable-type-hints t)
   ("zls.inlay_hints_show_parameter_name" lsp-zig-inlay-hints-show-parameter-name t)
   ("zls.inlay_hints_show_builtin" lsp-zig-inlay-hints-show-builtin t)
   ("zls.inlay_hints_exclude_single_argument" lsp-zig-inlay-hints-exclude-single-argument t)
   ("zls.inlay_hints_hide_redundant_param_names" lsp-zig-inlay-hints-hide-redundant-param-names t)
   ("zls.inlay_hints_hide_redundant_param_names_last_token" lsp-zig-inlay-hints-hide-redundant-param-names-last-token t)
   ("zls.warn_style" lsp-zig-warn-style t)
   ("zls.highlight_global_var_declarations" lsp-zig-highlight-global-var-declarations t)
   ("zls.dangerous_comptime_experiments_do_not_enable" lsp-zig-dangerous-comptime-experiments-do-not-enable t)
   ("zls.skip_std_references" lsp-zig-skip-std-references t)
   ("zls.prefer_ast_check_as_child_process" lsp-zig-prefer-ast-check-as-child-process t)
   ("zls.record_session" lsp-zig-record-session t)
   ("zls.record_session_path" lsp-zig-record-session-path)
   ("zls.replay_session_path" lsp-zig-replay-session-path)
   ("zls.builtin_path" lsp-zig-builtin-path)
   ("zls.zig_lib_path" lsp-zig-zig-lib-path)
   ("zls.zig_exe_path" lsp-zig-zig-exe-path)
   ("zls.build_runner_path" lsp-zig-build-runner-path)
   ("zls.global_cache_path" lsp-zig-global-cache-path)
   ("zls.build_runner_global_cache_path" lsp-zig-build-runner-global-cache-path)
   ("zls.completion_label_details" lsp-zig-completions-with-replace t)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (or (executable-find lsp-zig-zls-executable)
                                  (lsp-zig--stored-zls-executable)))
                   (lambda ()
                     (or (executable-find lsp-zig-zls-executable)
                         (file-executable-p (lsp-zig--stored-zls-executable)))))
  :activation-fn (lsp-activate-on "zig")
  :priority -1
  :server-id 'zls
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'zls callback error-callback))))

(lsp-consistency-check lsp-zig)

(provide 'lsp-zig)
;;; lsp-zig.el ends here
