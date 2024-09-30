;;; lsp-pylsp.el --- python-lsp-server support       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: language tools

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

;; pylsp configuration

;;; Code:

(require 'lsp-mode)

(defgroup lsp-pylsp nil
  "LSP support for Python, using python-lsp's Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/python-lsp/python-lsp-server"))

(defcustom lsp-clients-pylsp-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-server-command '("pylsp")
  "Command to start pylsp."
  :risky t
  :group 'lsp-pylsp
  :type '(repeat string))

(defcustom lsp-pylsp-configuration-sources ["flake8"]
  "List of configuration sources to use."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-completion-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-completion-include-params t
  "Auto-completes methods and classes with tabstops for each
parameter."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-definition-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-definition-follow-imports t
  "The goto call will follow imports."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-definition-follow-builtin-imports t
  "If follow_imports is True will decide if it follow builtin
imports."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-hover-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-references-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-signature-help-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-symbols-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-symbols-all-scopes t
  "If True lists the names of all scopes instead of only the
module namespace."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mccabe-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mccabe-threshold 15
  "The minimum threshold that triggers warnings about cyclomatic
complexity."
  :type 'number
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-preload-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-preload-modules nil
  "List of modules to import on startup"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pylint-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pylint-args []
  "Arguments, passed to pylint"
  :risky t
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-exclude nil
  "Exclude files or directories which match these patterns."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-filename nil
  "When parsing directories, only check filenames matching these
patterns."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-select nil
  "Select errors and warnings"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-ignore nil
  "Ignore errors and warnings"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-hang-closing nil
  "Hang closing bracket instead of matching indentation of
opening bracket's line."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pycodestyle-max-line-length nil
  "Set maximum allowed line length."
  :type 'number
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-convention nil
  "Choose the basic list of checked errors by specifying an
existing convention."
  :type '(choice (:tag "pep257" "numpy"))
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-add-ignore nil
  "Ignore errors and warnings in addition to the specified
convention."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-add-select nil
  "Select errors and warnings in addition to the specified
convention."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-ignore nil
  "Ignore errors and warnings"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-select nil
  "Select errors and warnings"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-match "(?!test_).*\\.py"
  "Check only files that exactly match the given regular
expression; default is to match files that don't start with
`test_' but end with `.py'."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pydocstyle-match-dir "[^\\.].*"
  "Search only dirs that exactly match the given regular
expression; default is to match dirs which do not begin with a
dot."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-pyflakes-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-autoimport-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-autoimport-memory nil
  "Make the autoimport database memory only.

Drastically increases startup time."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-autoimport-completions-enabled nil
    "Enable or disable completions from rope-autoimport."
    :type 'boolean
    :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-autoimport-code-actions-enabled nil
    "Enable or disable code actions from rope-autoimport."
    :type 'boolean
    :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-completion-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-rope-completion-eager nil
  "Resolve documentation and detail eagerly."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-autopep8-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-yapf-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-black-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-isort-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-rope-extension-modules nil
  "Builtin and c-extension modules that are allowed to be
imported and inspected by rope."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-rope-rope-folder nil
  "The name of the folder in which rope stores project
configurations and data. Pass `nil` for not using such a folder
at all."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-exclude nil
  "List of glob patterns to exclude from checks."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-filename nil
  "List of glob patterns to include for checks."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-hang-closing nil
  "Toggle whether pycodestyle should enforce matching the indentation of the
opening bracket’s line. When you specify this, it will prefer that you hang the
closing bracket rather than match the indentation."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-ignore nil
  "A list of codes to ignore."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-max-line-length nil
  "Set the maximum length that any line (with some exceptions) may be.
Exceptions include lines that are either strings or comments which are
entirely URLs."
  :type 'integer
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-select nil
  "Specify the list of error codes you wish Flake8 to report. Similarly to
`lsp-pylsp-plugins-flake8-ignore'. You can specify a portion of an error code to
get all that start with that string. For example, you can use E, E4, E43, and
E431"
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-flake8-config nil
  "A path to a config file that will be the only config file read and used.
This will cause Flake8 to ignore all other config files that exist.

NOTE: other parameters as `lsp-pylsp-plugins-flake8-max-line-length' take
precedence over parameters referenced in config."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-use-pyenv-environment nil
  "If enabled, pass the environment got by pyenv to jedi"
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-environment nil
  "Specify the environment that jedi runs on where <environment>/bin/python
should be the python executable. This option will be prioritized over
`lsp-pylsp-plugins-jedi-use-pyenv-environment'."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-completion-fuzzy nil
  "If enabled, uses fuzzy completion in jedi. Requires pylsp >= 0.32.0
Can hit performance, as well as lsp-mode implements its own fuzzy search on
completion items."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-jedi-completion-include-class-objects t
  "If enabled, adds class objects to completion in order to avoid snippet
with init args.

Has no effect if `lsp-pylsp-plugins-jedi-completion-include-params' is disabled.
Requires pylsp >= 0.33.0"
  :type 'boolean
  :group 'lsp-pylsp)

;; See https://github.com/python-lsp/python-lsp-ruff#configuration

(defcustom lsp-pylsp-plugins-ruff-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-executable nil
  "Custom path to ruff."
  :type 'file
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-config nil
  "Custom config for ruff to use."
  :type 'file
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-extend-select nil
  "Rules that are additionally used by ruff."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-extend-ignore nil
  "Rules that are additionally ignored by ruff."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-format nil
  "Rules that should be fixed when running textDocument/formatting.

Note each rule must additionally be marked as fixable by ruff."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-severities nil
  "Optional table of rules where a custom severity is desired."
  :type '(alist :key-type (lsp-string-vector :tag "rules") :value-type (string :tag "severity"))
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-unsafe-fixes nil
  "Whether or not to offer unsafe fixes as code actions.

Note this is ignored with the \"Fix All\" action."
  :type 'boolean
  :group 'lsp-pylsp)

;; Rules that are ignored when a pyproject.toml or ruff.toml is present
(defcustom lsp-pylsp-plugins-ruff-line-length 88
  "Line length to pass to ruff checking and formatting.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml is
present."
  :type 'integer
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-exclude nil
  "Files to be excluded by ruff checking.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-select nil
  "Rules to be enabled by ruff.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-ignore nil
  "Rules to be ignored by ruff.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-per-file-ignores nil
  "Rules that should be ignored for specific files.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type '(alist :key-type (lsp-string-vector :tag "files") :value-type (string :tag "rule"))
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-preview nil
  "Whether to enable the preview style linting and formatting.

Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-ruff-target-version nil
  "The minimum python version to target (applies for both linting and formatting).
    
Note this variable will be ignored when a when a pyproject.toml or ruff.toml
is present."
  :type 'string
  :group 'lsp-pylsp)

;; See https://github.com/python-lsp/pylsp-mypy#configuration

(defcustom lsp-pylsp-plugins-mypy-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-live-mode t
  "If non-nil, type checking is provided as you type.

This writes to a tempfile every time a check is done.  Turning off live_mode
means you must save your changes for mypy diagnostics to update correctly."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-dmypy nil
  "If non-nil, use \"dmypy run\" rather than mypy.

This uses the dmypy daemon and may dramatically improve the responsiveness of
the pylsp server, however this currently does not work in live_mode.  Enabling
this disables live_mode, even for conflicting configs."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-strict nil
  "If non-nil, enable the strict option of mypy.

This option often is too strict to be useful."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-overrides [t]
  "A list of alternate or supplemental command-line options.

This modifies the options passed to mypy or the mypy-specific ones passed to
dmypy run.  When present, the special boolean member True is replaced with
the command-line options that would've been passed had overrides not been
specified.  Later options take precedence, which allows for replacing or
negating individual default options (see mypy.main:process_options and mypy
--help | grep inverse)."
  :type '(vector (choice string boolean))
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-dmypy-status-file ".dmypy.json"
  "The status file dmypy should use.

This modifies the --status-file option passed to dmypy given dmypy is active."
  :type 'string
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-config-sub-paths nil
  "Sub paths under which the mypy configuration file may be found.

For each directory searched for the mypy config file, this also searches the
sub paths specified here."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-report-progress nil
  "If non-nil, report basic progress to the LSP client.

With this option, pylsp-mypy will report when mypy is running, given your editor
supports LSP progress reporting.  For small files this might produce annoying
flashing, especially in with live_mode.  For large projects, enabling this can
be helpful to assure yourself whether mypy is still running."
  :type 'boolean
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-plugins-mypy-exclude nil
  "A list of regular expressions which should be ignored.

The mypy runner wil not be invoked when a document path is matched by one of the
expressions.  Note that this differs from the exclude directive of a mypy config
which is only used for recursively discovering files when mypy is invoked on a
whole directory.  For both windows or unix platforms you should use forward
slashes (/) to indicate paths."
  :type 'lsp-string-vector
  :group 'lsp-pylsp)

(defcustom lsp-pylsp-rename-backend 'jedi
  "Choose renaming backend.

Jedi is preferred but only works for python >= 3.6 and pylsp >= 0.32.0
Beware that Jedi is lazy and doesn't scan the whole project.
So it will rename only references it can find."
  :type '(choice (const :tag "jedi" jedi)
                 (const :tag "rope" rope))
  :group 'lsp-pylsp)

(defun lsp-pylsp-get-pyenv-environment ()
  "Get the pyenv-managed environment for current workspace, where
<ENV>/bin/python is the corresponding Python executable"
  (if lsp-pylsp-plugins-jedi-environment
      lsp-pylsp-plugins-jedi-environment
    (when lsp-pylsp-plugins-jedi-use-pyenv-environment
      (let ((pyenv-version (getenv "PYENV_VERSION"))
            (root (lsp-seq-first (lsp-find-roots-for-workspace lsp--cur-workspace (lsp-session)))))
        (when root
          (setenv "PYENV_VERSION" nil)
          (let* ((pyenv-command-path (executable-find "pyenv"))
                 (python-env (when pyenv-command-path
                               (f-parent
                                (f-parent
                                 (shell-command-to-string
                                  (format "PYENV_DIR='%s' %s which python"
                                          root pyenv-command-path)))))))
            (if python-env (lsp--info "Configure pylsp with environment: %s" python-env)
              (lsp--warn "Can't find the python environment for
              %s even if
              `lsp-pylsp-plugins-jedi-use-pyenv-environment` is
              enabled") root)
            (setenv "PYENV_VERSION" pyenv-version)
            python-env))))))

(lsp-register-custom-settings
 '(("pylsp.rope.ropeFolder" lsp-pylsp-rope-rope-folder)
   ("pylsp.rope.extensionModules" lsp-pylsp-rope-extension-modules)
   ("pylsp.plugins.rope_rename.enabled" (lambda () (eq lsp-pylsp-rename-backend 'rope)) t)
   ("pylsp.plugins.autopep8.enabled" lsp-pylsp-plugins-autopep8-enabled t)
   ("pylsp.plugins.yapf.enabled" lsp-pylsp-plugins-yapf-enabled t)
   ("pylsp.plugins.black.enabled" lsp-pylsp-plugins-black-enabled t)
   ("pylsp.plugins.pyls_isort.enabled" lsp-pylsp-plugins-isort-enabled t)
   ("pylsp.plugins.rope_autoimport.enabled" lsp-pylsp-plugins-rope-autoimport-enabled t)
   ("pylsp.plugins.rope_autoimport.memory" lsp-pylsp-plugins-rope-autoimport-memory t)
   ("pylsp.plugins.rope_autoimport.completions.enabled" lsp-pylsp-plugins-rope-autoimport-completions-enabled t)
   ("pylsp.plugins.rope_autoimport.code_actions.enabled" lsp-pylsp-plugins-rope-autoimport-code-actions-enabled t)
   ("pylsp.plugins.rope_completion.enabled" lsp-pylsp-plugins-rope-completion-enabled t)
   ("pylsp.plugins.rope_completion.eager" lsp-pylsp-plugins-rope-completion-eager t)
   ("pylsp.plugins.pyflakes.enabled" lsp-pylsp-plugins-pyflakes-enabled t)
   ("pylsp.plugins.pydocstyle.matchDir" lsp-pylsp-plugins-pydocstyle-match-dir)
   ("pylsp.plugins.pydocstyle.match" lsp-pylsp-plugins-pydocstyle-match)
   ("pylsp.plugins.pydocstyle.select" lsp-pylsp-plugins-pydocstyle-select)
   ("pylsp.plugins.pydocstyle.ignore" lsp-pylsp-plugins-pydocstyle-ignore)
   ("pylsp.plugins.pydocstyle.addSelect" lsp-pylsp-plugins-pydocstyle-add-select)
   ("pylsp.plugins.pydocstyle.addIgnore" lsp-pylsp-plugins-pydocstyle-add-ignore)
   ("pylsp.plugins.pydocstyle.convention" lsp-pylsp-plugins-pydocstyle-convention)
   ("pylsp.plugins.pydocstyle.enabled" lsp-pylsp-plugins-pydocstyle-enabled t)
   ("pylsp.plugins.pycodestyle.maxLineLength" lsp-pylsp-plugins-pycodestyle-max-line-length)
   ("pylsp.plugins.pycodestyle.hangClosing" lsp-pylsp-plugins-pycodestyle-hang-closing t)
   ("pylsp.plugins.pycodestyle.ignore" lsp-pylsp-plugins-pycodestyle-ignore)
   ("pylsp.plugins.pycodestyle.select" lsp-pylsp-plugins-pycodestyle-select)
   ("pylsp.plugins.pycodestyle.filename" lsp-pylsp-plugins-pycodestyle-filename)
   ("pylsp.plugins.pycodestyle.exclude" lsp-pylsp-plugins-pycodestyle-exclude)
   ("pylsp.plugins.pycodestyle.enabled" lsp-pylsp-plugins-pycodestyle-enabled t)
   ("pylsp.plugins.pylint.enabled" lsp-pylsp-plugins-pylint-enabled t)
   ("pylsp.plugins.pylint.args" lsp-pylsp-plugins-pylint-args)
   ("pylsp.plugins.flake8.enabled" lsp-pylsp-plugins-flake8-enabled)
   ("pylsp.plugins.flake8.exclude" lsp-pylsp-plugins-flake8-exclude)
   ("pylsp.plugins.flake8.filename" lsp-pylsp-plugins-flake8-filename)
   ("pylsp.plugins.flake8.hangClosing" lsp-pylsp-plugins-flake8-hang-closing)
   ("pylsp.plugins.flake8.ignore" lsp-pylsp-plugins-flake8-ignore)
   ("pylsp.plugins.flake8.maxLineLength" lsp-pylsp-plugins-flake8-max-line-length)
   ("pylsp.plugins.flake8.select" lsp-pylsp-plugins-flake8-select)
   ("pylsp.plugins.flake8.config" lsp-pylsp-plugins-flake8-config)
   ("pylsp.plugins.preload.modules" lsp-pylsp-plugins-preload-modules)
   ("pylsp.plugins.preload.enabled" lsp-pylsp-plugins-preload-enabled t)
   ("pylsp.plugins.mccabe.threshold" lsp-pylsp-plugins-mccabe-threshold)
   ("pylsp.plugins.mccabe.enabled" lsp-pylsp-plugins-mccabe-enabled t)
   ("pylsp.plugins.ruff.enabled" lsp-pylsp-plugins-ruff-enabled t)
   ("pylsp.plugins.ruff.executable" lsp-pylsp-plugins-ruff-executable)
   ("pylsp.plugins.ruff.config" lsp-pylsp-plugins-ruff-config)
   ("pylsp.plugins.ruff.extendSelect" lsp-pylsp-plugins-ruff-extend-select)
   ("pylsp.plugins.ruff.extendIgnore" lsp-pylsp-plugins-ruff-extend-ignore)
   ("pylsp.plugins.ruff.format" lsp-pylsp-plugins-ruff-format)
   ("pylsp.plugins.ruff.severities" lsp-pylsp-plugins-ruff-severities)
   ("pylsp.plugins.ruff.unsafeFixes" lsp-pylsp-plugins-ruff-unsafe-fixes t)
   ("pylsp.plugins.ruff.lineLength" lsp-pylsp-plugins-ruff-line-length)
   ("pylsp.plugins.ruff.exclude" lsp-pylsp-plugins-ruff-exclude)
   ("pylsp.plugins.ruff.select" lsp-pylsp-plugins-ruff-select)
   ("pylsp.plugins.ruff.ignore" lsp-pylsp-plugins-ruff-ignore)
   ("pylsp.plugins.ruff.perFileIgnores" lsp-pylsp-plugins-ruff-per-file-ignores)
   ("pylsp.plugins.ruff.preview" lsp-pylsp-plugins-ruff-preview t)
   ("pylsp.plugins.ruff.targetVersion" lsp-pylsp-plugins-ruff-target-version)
   ("pylsp.plugins.pylsp_mypy.enabled" lsp-pylsp-plugins-mypy-enabled t)
   ("pylsp.plugins.pylsp_mypy.live_mode" lsp-pylsp-plugins-mypy-live-mode t)
   ("pylsp.plugins.pylsp_mypy.dmypy" lsp-pylsp-plugins-mypy-dmypy t)
   ("pylsp.plugins.pylsp_mypy.strict" lsp-pylsp-plugins-mypy-strict t)
   ("pylsp.plugins.pylsp_mypy.overrides" lsp-pylsp-plugins-mypy-overrides)
   ("pylsp.plugins.pylsp_mypy.dmypy_status_file" lsp-pylsp-plugins-mypy-dmypy-status-file)
   ("pylsp.plugins.pylsp_mypy.config_sub_paths" lsp-pylsp-plugins-mypy-config-sub-paths)
   ("pylsp.plugins.pylsp_mypy.report_progress" lsp-pylsp-plugins-mypy-report-progress t)
   ("pylsp.plugins.pylsp_mypy.exclude" lsp-pylsp-plugins-mypy-exclude)
   ("pylsp.plugins.jedi_symbols.all_scopes" lsp-pylsp-plugins-jedi-symbols-all-scopes t)
   ("pylsp.plugins.jedi_symbols.enabled" lsp-pylsp-plugins-jedi-symbols-enabled t)
   ("pylsp.plugins.jedi_signature_help.enabled" lsp-pylsp-plugins-jedi-signature-help-enabled t)
   ("pylsp.plugins.jedi_references.enabled" lsp-pylsp-plugins-jedi-references-enabled t)
   ("pylsp.plugins.jedi_hover.enabled" lsp-pylsp-plugins-jedi-hover-enabled t)
   ("pylsp.plugins.jedi_definition.follow_builtin_imports" lsp-pylsp-plugins-jedi-definition-follow-builtin-imports t)
   ("pylsp.plugins.jedi_definition.follow_imports" lsp-pylsp-plugins-jedi-definition-follow-imports t)
   ("pylsp.plugins.jedi_definition.enabled" lsp-pylsp-plugins-jedi-definition-enabled t)
   ("pylsp.plugins.jedi_completion.include_params" lsp-pylsp-plugins-jedi-completion-include-params t)
   ("pylsp.plugins.jedi_completion.enabled" lsp-pylsp-plugins-jedi-completion-enabled t)
   ("pylsp.plugins.jedi_completion.include_class_objects" lsp-pylsp-plugins-jedi-completion-include-class-objects t)
   ("pylsp.plugins.jedi.environment" lsp-pylsp-get-pyenv-environment)
   ("pylsp.plugins.jedi_completion.fuzzy" lsp-pylsp-plugins-jedi-completion-fuzzy t)
   ("pylsp.plugins.jedi_rename.enabled" (lambda () (eq lsp-pylsp-rename-backend 'jedi)) t)
   ("pylsp.configurationSources" lsp-pylsp-configuration-sources)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-pylsp-server-command))
                  :activation-fn (lsp-activate-on "python")
                  :priority -1
                  :server-id 'pylsp
                  :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "pylsp"))))))

(lsp-consistency-check lsp-pylsp)

(provide 'lsp-pylsp)
;;; lsp-pylsp.el ends here
