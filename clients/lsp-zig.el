;;; lsp-zig.el --- lsp-mode Zig integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Riccardo Binetti

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

(defcustom lsp-zig-check-for-update t
  "Whether to automatically check for new updates."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-enable-snippets t
  "Enables snippet completions when the client also supports them."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-enable-argument-placeholders t
  "Whether to enable function argument placeholder completions."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-enable-build-on-save nil
  "Whether to enable build-on-save diagnostics."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-build-on-save-step "install"
  "Select which step should be executed on build-on-save."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-enable-autofix nil
  "Whether to automatically fix errors on save.
Currently supports adding and removing discards."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-semantic-tokens "partial"
  "Traces the communication between Emacs and the language server."
  :group 'lsp-zig
  :type '(choice (const "off")
                 (const "messages")
                 (const "verbose")))

(defcustom lsp-zig-zls-enable-inlay-hints t
  "Enables inlay hint support when the client also supports it."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-show-variable-type-hints t
  "Enable inlay hints for variable type."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-show-parameter-name t
  "Enable inlay hints for parameter names."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-exclude-single-argument t
  "Don't show inlay hints for single argument calls."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-show-builtin t
  "Don't show inlay hints for single argument calls."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-hide-redundant-param-names nil
  "Hides inlay hints when parameter name matches the identifier (e.g. foo: foo)."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-inlay-hints-hide-redundant-param-names-last-token nil
  "Hides inlay hints when parameter name matches the last token of a parameter
node (e.g. foo: bar.foo, foo: &foo)."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-warn-style nil
  "Enables warnings for style guideline mismatches."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-highlight-global-var-declarations nil
  "Whether to highlight global var declarations."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-dangerous-comptime-experiments-do-not-enable nil
  "When true, skips searching for references in std.
Improves lookup speed for functions in user's code.  Renaming and
go-to-definition will continue to work as is."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-skip-std-references nil
  "hen true, skips searching for references in std.
Improves lookup speed for functions in user's code.  Renaming and
 go-to-definition will continue to work as is."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-prefer-ast-check-as-child-process t
  "Favor using `zig ast-check` instead of ZLS's fork."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-record-session nil
  "When true, zls will record all request is receives and write in into
`record_session_path`, so that they can replayed with `zls replay`."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-record-session-path ""
  "Output file path when `record_session` is set.
The recommended file extension *.zlsreplay."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-replay-session-path ""
  "Used when calling `zls replay` for specifying the replay file.
If no extra argument is given `record_session_path` is used as the default path."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-builtin-path ""
  "Path to `builtin'; useful for debugging, automatically set if let null."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-zig-lib-path ""
  "Zig library path.
e.g. `/path/to/zig/lib/zig`, used to analyze std library imports."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-build-runner-path ""
  "Path to the `build_runner.zig` file provided by zls.
null is equivalent to `${executable_directory}/build_runner.zig`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-global-cache-path ""
  "Path to a directory that will be used as zig's cache.
null is equivalent to `${KnownFolders.Cache}/zls`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-build-runner-global-cache-path ""
  "Path to a directory that will be used as the global cache path when executing
a projects build.zig.  null is equivalent to the path shown by `zig env`."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-zls-completions-with-replace nil
  "Completions confirm behavior.
If `true', replace the text after the cursor."
  :group 'lsp-zig
  :type 'boolean)

(defcustom lsp-zig-zls-completion-label-details nil
  "When false, the function signature of completion results is hidden.
Improves readability in some editors."
  :group 'lsp-zig
  :type 'boolean)

;;
;;; Util

(defmacro lsp-zig--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun lsp-zig--execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (lsp-zig--mute-apply
      (= 0 (shell-command (concat cmd " "
                                  (mapconcat #'shell-quote-argument
                                             (cl-remove-if #'null args)
                                             " ")))))))

;;
;;; Installation

(defcustom lsp-zig-server-store-path
  (expand-file-name "zig/" lsp-server-install-dir)
  "The path to the file in which zls will be stored."
  :type 'file
  :group 'lsp-zig)

(defcustom lsp-zig-server-version "0.11.0"
  "The zls version to install."
  :type 'file
  :group 'lsp-zig)

(defconst lsp-zig-download-url-format
  "https://github.com/zigtools/zls/releases/download/%s/zls-%s-%s.%s"
  "Format to the download url link.")

(defun lsp-zig--zls-url ()
  "Return Url points to the zls' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "windows" "zip"))
      (darwin
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "macos" "tar.gz"))
      (gnu/linux
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "linux" "tar.gz")))))

(defvar lsp-zig--server-download-url (lsp-zig--zls-url)
  "The actual url used to download language server.")

(defvar lsp-zig--downloaded-file (f-join lsp-zig-server-store-path "temp.tar")
  "The full file path after downloading the server zipped file.")

(defun lsp-zig--stored-zls-executable ()
  "Return the stored zls executable.

This is differ from the variable `lsp-zig-zls-executable'; this is local storage
and not the global storage."
  (executable-find (f-join lsp-zig-server-store-path "bin/zls")))

(defun lsp-zig--extract-compressed-file ()
  "Install zls."
  (cond ((file-exists-p lsp-zig--downloaded-file)
         ;; Suprisingly, you can just use `tar' to unzip a zip file on Windows.
         ;; Therefore, just use the same command.
         (lsp-zig--execute "tar" "-xvzf" lsp-zig--downloaded-file "-C" lsp-zig-server-store-path)
         ;; Delete the zip file.
         (ignore-errors (delete-file lsp-zig--downloaded-file)))
        (t
         (error "Can't extract the downloaded file: %s" lsp-zig--downloaded-file))))

(lsp-dependency
 'zls
 '(:system "zls")
 `(:download :url ,lsp-zig--server-download-url
             :store-path ,lsp-zig--downloaded-file))

;;
;;; Core

(lsp-register-custom-settings
 '(("zig.zls.trace.server" lsp-zig-trace-server)
   ("zig.zls.checkForUpdate" lsp-zig-check-for-update)
   ("zig.zls.path" lsp-zig-zls-executable)
   ("zig.zls.enableSnippets" lsp-zig-zls-enable-snippets)
   ("zig.zls.enableArgumentPlaceholders" lsp-zig-zls-enable-argument-placeholders)
   ("zig.zls.enableBuildOnSave" lsp-zig-zls-enable-build-on-save)
   ("zig.zls.buildOnSaveStep" lsp-zig-zls-build-on-save-step)
   ("zig.zls.enableAutofix" lsp-zig-zls-enable-autofix)
   ("zig.zls.semanticTokens" lsp-zig-zls-semantic-tokens)
   ("zig.zls.enableInlayHints" lsp-zig-zls-enable-inlay-hints)
   ("zig.zls.inlayHintsShowVariableTypeHints" lsp-zig-zls-inlay-hints-show-variable-type-hints)
   ("zig.zls.inlayHintsShowParameterName" lsp-zig-zls-inlay-hints-show-parameter-name)
   ("zig.zls.inlayHintsShowBuiltin" lsp-zig-zls-inlay-hints-show-builtin)
   ("zig.zls.inlayHintsExcludeSingleArgument" lsp-zig-zls-inlay-hints-exclude-single-argument)
   ("zig.zls.inlayHintsHideRedundantParamNames" lsp-zig-zls-inlay-hints-hide-redundant-param-names)
   ("zig.zls.inlayHintsHideRedundantParamNamesLastToken" lsp-zig-zls-inlay-hints-hide-redundant-param-names-last-token)
   ("zig.zls.warnStyle" lsp-zig-zls-warn-style)
   ("zig.zls.highlightGlobalVarDeclarations" lsp-zig-zls-highlight-global-var-declarations)
   ("zig.zls.dangerousComptimeExperimentsDoNotEnable" lsp-zig-zls-dangerous-comptime-experiments-do-not-enable)
   ("zig.zls.skipStdReferences" lsp-zig-zls-skip-std-references)
   ("zig.zls.preferAstCheckAsChildProcess" lsp-zig-zls-prefer-ast-check-as-child-process)
   ("zig.zls.recordSession" lsp-zig-zls-record-session)
   ("zig.zls.recordSessionPath" lsp-zig-zls-record-session-path)
   ("zig.zls.replaySessionPath" lsp-zig-zls-replay-session-path)
   ("zig.zls.builtinPath" lsp-zig-zls-builtin-path)
   ("zig.zls.zigLibPath" lsp-zig-zls-zig-lib-path)
   ("zig.zls.buildRunnerPath" lsp-zig-zls-build-runner-path)
   ("zig.zls.globalCachePath" lsp-zig-zls-global-cache-path)
   ("zig.zls.buildRunnerGlobalCachePath" lsp-zig-zls-build-runner-global-cache-path)
   ("zig.zls.completionsWithReplace" lsp-zig-zls-completions-with-replace)
   ("zig.zls.completionLabelDetails" lsp-zig-zls-completion-label-details)))

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
  (lambda (_client _callback error-callback _update?)
    (lsp-package-ensure 'zls #'lsp-zig--extract-compressed-file error-callback))))

(lsp-consistency-check lsp-zig)

(provide 'lsp-zig)
;;; lsp-zig.el ends here
