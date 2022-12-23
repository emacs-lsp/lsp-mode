;;; lsp-pyls.el --- pyls configuration               -*- lexical-binding: t; -*-

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

;; PYLS configuration

;;; Code:

(require 'lsp-mode)

(defgroup lsp-pyls nil
  "LSP support for Python, using Palantir's Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/palantir/python-language-server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-clients-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(define-obsolete-variable-alias
  'lsp-clients-python-command
  'lsp-pyls-server-command
  "6.1")

(defcustom lsp-pyls-disable-warning nil
  "Disable Palantir python-language-server deprecation warning"
  :group 'lsp-pyls
  :type 'boolean
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-pyls-server-command '("pyls")
  "Command to start pyls."
  :risky t
  :group 'lsp-pyls
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-configuration-sources ["pycodestyle"]
  "List of configuration sources to use."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-completion-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-completion-include-params t
  "Auto-completes methods and classes with tabstops for each
parameter."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-definition-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-definition-follow-imports t
  "The goto call will follow imports."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-definition-follow-builtin-imports t
  "If follow_imports is True will decide if it follow builtin
imports."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-hover-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-references-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-signature-help-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-symbols-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-jedi-symbols-all-scopes t
  "If True lists the names of all scopes instead of only the
module namespace."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-mccabe-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-mccabe-threshold 15
  "The minimum threshold that triggers warnings about cyclomatic
complexity."
  :type 'number
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-preload-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-preload-modules nil
  "List of modules to import on startup"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pylint-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pylint-args []
  "Arguments, passed to pylint"
  :risky t
  :type 'lsp-string-vector
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-exclude nil
  "Exclude files or directories which match these patterns."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-filename nil
  "When parsing directories, only check filenames matching these
patterns."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-select nil
  "Select errors and warnings"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-ignore nil
  "Ignore errors and warnings"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-hang-closing nil
  "Hang closing bracket instead of matching indentation of
opening bracket's line."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pycodestyle-max-line-length nil
  "Set maximum allowed line length."
  :type 'number
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-convention nil
  "Choose the basic list of checked errors by specifying an
existing convention."
  :type '(choice (:tag "pep257" "numpy"))
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-add-ignore nil
  "Ignore errors and warnings in addition to the specified
convention."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-add-select nil
  "Select errors and warnings in addition to the specified
convention."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-ignore nil
  "Ignore errors and warnings"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-select nil
  "Select errors and warnings"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-match "(?!test_).*\\.py"
  "Check only files that exactly match the given regular
expression; default is to match files that don't start with
'test_' but end with '.py'."
  :type 'string
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pydocstyle-match-dir "[^\\.].*"
  "Search only dirs that exactly match the given regular
expression; default is to match dirs which do not begin with a
dot."
  :type 'string
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-pyflakes-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-rope-completion-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-autopep8-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-yapf-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-rope-extension-modules nil
  "Builtin and c-extension modules that are allowed to be
imported and inspected by rope."
  :type 'string
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-rope-rope-folder nil
  "The name of the folder in which rope stores project
configurations and data. Pass `nil` for not using such a folder
at all."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-pyls-plugins-flake8-enabled nil
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-exclude nil
  "List of glob patterns to exclude from checks."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-filename nil
  "List of glob patterns to include for checks."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-hang-closing nil
  "Toggle whether pycodestyle should enforce matching the indentation of the
opening bracket’s line. When you specify this, it will prefer that you hang the
closing bracket rather than match the indentation."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-ignore nil
  "A list of codes to ignore."
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-max-line-length nil
  "Set the maximum length that any line (with some exceptions) may be.
Exceptions include lines that are either strings or comments which are
entirely URLs."
  :type 'integer
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-select nil
  "Specify the list of error codes you wish Flake8 to report. Similarly to
`lsp-pyls-plugins-flake8-ignore'. You can specify a portion of an error code to
get all that start with that string. For example, you can use E, E4, E43, and
E431"
  :type '(repeat string)
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pyls-plugins-flake8-config nil
  "A path to a config file that will be the only config file read and used.
This will cause Flake8 to ignore all other config files that exist.

NOTE: other parameters as `lsp-pyls-plugins-flake8-max-line-length' take
precedence over parameters referenced in config."
  :type 'string
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-pyls-plugins-jedi-use-pyenv-environment nil
  "If enabled, pass the environment got by pyenv to jedi"
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-pyls-plugins-jedi-environment nil
  "Specify the environment that jedi runs on where <environment>/bin/python
should be the python executable. This option will be prioritized over
`lsp-pyls-plugins-jedi-use-pyenv-environment'."
  :type 'string
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-pyls-plugins-jedi-completion-fuzzy nil
  "If enabled, uses fuzzy completion in jedi. Requires pyls >= 0.32.0
Can hit performance, as well as lsp-mode implements its own fuzzy search on
completion items."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "7.0"))

(defcustom lsp-pyls-plugins-jedi-completion-include-class-objects t
  "If enabled, adds class objects to completion in order to avoid snippet
with init args.

Has no effect if `lsp-pyls-plugins-jedi-completion-include-params' is disabled.
Requires pyls >= 0.33.0"
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "7.0"))

(defcustom lsp-pyls-rename-backend 'jedi
  "Choose renaming backend.

Jedi is preferred but only works for python >= 3.6 and pyls >= 0.32.0
Beware that Jedi is lazy and doesn't scan the whole project.
So it will rename only references it can find."
  :type '(choice (const :tag "jedi" jedi)
                 (const :tag "rope" rope))
  :group 'lsp-pyls
  :package-version '(lsp-mode . "7.0"))


(defun lsp-pyls-get-pyenv-environment ()
  "Get the pyenv-managed environment for current workspace, where
<ENV>/bin/python is the corresponding Python executable"
  (if lsp-pyls-plugins-jedi-environment
      lsp-pyls-plugins-jedi-environment
    (when lsp-pyls-plugins-jedi-use-pyenv-environment
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
            (if python-env (lsp--info "Configure pyls with environment: %s" python-env)
              (lsp--warn "Can't find the python environment for
              %s even if
              `lsp-pyls-plugins-jedi-use-pyenv-environment` is
              enabled") root)
            (setenv "PYENV_VERSION" pyenv-version)
            python-env))))))

(lsp-register-custom-settings
 '(("pyls.rope.ropeFolder" lsp-pyls-rope-rope-folder)
   ("pyls.rope.extensionModules" lsp-pyls-rope-extension-modules)
   ("pyls.plugins.rope_rename.enabled" (lambda () (eq lsp-pyls-rename-backend 'rope)) t)
   ("pyls.plugins.autopep8.enabled" lsp-pyls-plugins-autopep8-enabled t)
   ("pyls.plugins.yapf.enabled" lsp-pyls-plugins-yapf-enabled t)
   ("pyls.plugins.rope_completion.enabled" lsp-pyls-plugins-rope-completion-enabled t)
   ("pyls.plugins.pyflakes.enabled" lsp-pyls-plugins-pyflakes-enabled t)
   ("pyls.plugins.pydocstyle.matchDir" lsp-pyls-plugins-pydocstyle-match-dir)
   ("pyls.plugins.pydocstyle.match" lsp-pyls-plugins-pydocstyle-match)
   ("pyls.plugins.pydocstyle.select" lsp-pyls-plugins-pydocstyle-select)
   ("pyls.plugins.pydocstyle.ignore" lsp-pyls-plugins-pydocstyle-ignore)
   ("pyls.plugins.pydocstyle.addSelect" lsp-pyls-plugins-pydocstyle-add-select)
   ("pyls.plugins.pydocstyle.addIgnore" lsp-pyls-plugins-pydocstyle-add-ignore)
   ("pyls.plugins.pydocstyle.convention" lsp-pyls-plugins-pydocstyle-convention)
   ("pyls.plugins.pydocstyle.enabled" lsp-pyls-plugins-pydocstyle-enabled t)
   ("pyls.plugins.pycodestyle.maxLineLength" lsp-pyls-plugins-pycodestyle-max-line-length)
   ("pyls.plugins.pycodestyle.hangClosing" lsp-pyls-plugins-pycodestyle-hang-closing t)
   ("pyls.plugins.pycodestyle.ignore" lsp-pyls-plugins-pycodestyle-ignore)
   ("pyls.plugins.pycodestyle.select" lsp-pyls-plugins-pycodestyle-select)
   ("pyls.plugins.pycodestyle.filename" lsp-pyls-plugins-pycodestyle-filename)
   ("pyls.plugins.pycodestyle.exclude" lsp-pyls-plugins-pycodestyle-exclude)
   ("pyls.plugins.pycodestyle.enabled" lsp-pyls-plugins-pycodestyle-enabled t)
   ("pyls.plugins.pylint.enabled" lsp-pyls-plugins-pylint-enabled t)
   ("pyls.plugins.pylint.args" lsp-pyls-plugins-pylint-args)
   ("pyls.plugins.flake8.enabled" lsp-pyls-plugins-flake8-enabled)
   ("pyls.plugins.flake8.exclude" lsp-pyls-plugins-flake8-exclude)
   ("pyls.plugins.flake8.filename" lsp-pyls-plugins-flake8-filename)
   ("pyls.plugins.flake8.hangClosing" lsp-pyls-plugins-flake8-hang-closing)
   ("pyls.plugins.flake8.ignore" lsp-pyls-plugins-flake8-ignore)
   ("pyls.plugins.flake8.maxLineLength" lsp-pyls-plugins-flake8-max-line-length)
   ("pyls.plugins.flake8.select" lsp-pyls-plugins-flake8-select)
   ("pyls.plugins.flake8.config" lsp-pyls-plugins-flake8-config)
   ("pyls.plugins.preload.modules" lsp-pyls-plugins-preload-modules)
   ("pyls.plugins.preload.enabled" lsp-pyls-plugins-preload-enabled t)
   ("pyls.plugins.mccabe.threshold" lsp-pyls-plugins-mccabe-threshold)
   ("pyls.plugins.mccabe.enabled" lsp-pyls-plugins-mccabe-enabled t)
   ("pyls.plugins.jedi_symbols.all_scopes" lsp-pyls-plugins-jedi-symbols-all-scopes t)
   ("pyls.plugins.jedi_symbols.enabled" lsp-pyls-plugins-jedi-symbols-enabled t)
   ("pyls.plugins.jedi_signature_help.enabled" lsp-pyls-plugins-jedi-signature-help-enabled t)
   ("pyls.plugins.jedi_references.enabled" lsp-pyls-plugins-jedi-references-enabled t)
   ("pyls.plugins.jedi_hover.enabled" lsp-pyls-plugins-jedi-hover-enabled t)
   ("pyls.plugins.jedi_definition.follow_builtin_imports" lsp-pyls-plugins-jedi-definition-follow-builtin-imports t)
   ("pyls.plugins.jedi_definition.follow_imports" lsp-pyls-plugins-jedi-definition-follow-imports t)
   ("pyls.plugins.jedi_definition.enabled" lsp-pyls-plugins-jedi-definition-enabled t)
   ("pyls.plugins.jedi_completion.include_params" lsp-pyls-plugins-jedi-completion-include-params t)
   ("pyls.plugins.jedi_completion.enabled" lsp-pyls-plugins-jedi-completion-enabled t)
   ("pyls.plugins.jedi_completion.include_class_objects" lsp-pyls-plugins-jedi-completion-include-class-objects t)
   ("pyls.plugins.jedi.environment" lsp-pyls-get-pyenv-environment)
   ("pyls.plugins.jedi_completion.fuzzy" lsp-pyls-plugins-jedi-completion-fuzzy t)
   ("pyls.plugins.jedi_rename.enabled" (lambda () (eq lsp-pyls-rename-backend 'jedi)) t)
   ("pyls.configurationSources" lsp-pyls-configuration-sources)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clients-python-command))
                  :activation-fn (lsp-activate-on "python")
                  :priority -2
                  :server-id 'pyls
                  :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (unless lsp-pyls-disable-warning
                                      (warn (concat "The palantir python-language-server (pyls) is unmaintained; "
                                                    "a maintained fork is the python-lsp-server (pylsp) project; "
                                                    "you can install it with pip via: pip install python-lsp-server")))
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "pyls"))))))

(lsp-consistency-check lsp-pyls)

(provide 'lsp-pyls)
;;; lsp-pyls.el ends here
