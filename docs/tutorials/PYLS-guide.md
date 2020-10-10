---
disqus: emacs-lsp
---

# Configuring `Emacs` as `Python` IDE with `pyls` as a language server

This guide will show you how to configure `lsp-mode` for Python development.
There are several language servers available for python:
  * [pyls](https://github.com/palantir/python-language-server) by Palantir Technologies (this tutorial focuses on this ls)
  * [python-ms](https://github.com/Microsoft/python-language-server) by Microsoft
  * [jedi](https://github.com/pappasam/jedi-language-server) by pappasam
  * [pyright](https://github.com/microsoft/pyright) by Microsoft

## Features of `pyls` as a language server

* Code completion
* Linting and syntax checking
* Signature help
* Code navigation (references/definitions/etc..)
* Info on hover
* Document Formatting

# Installing the language server

1. Install globally with `pip install --user python-language-server[all]`
2. If your project uses virtualenv in any form (`virtualenv`, `virtualenvwrapper`, `poetry`, etc...) you need to specify a few project specific configurations
    * Go to the root of your project
    * Create a `.dir-locals.el` file. Also, preferably, add it to your `.gitignore`
    * place this code there:
    ```elisp
    ((python-mode .
                  ((lsp-pyls-plugins-jedi-environment . "*ABSOLUTE PATH TO YOUR VIRTUALENV*"))))
    ```
    if you use `virtualenvwrapper`, you can switch it a bit
    ```elisp
    ((python-mode .
                  ((lsp-pyls-plugins-jedi-environment . (concat (file-name-as-directory (getenv "WORKON_HOME")) "*YOUR VENV NAME*")))))
    ```

# lsp-mode configuration

Here is a bare-bones `lsp-mode` configuration template to get you started with your own `lsp-mode` config, or to try out in a separate one-off session. Please note that Emacs configuration frameworks such as Spacemacs or Doom Emacs often ship with `lsp-mode` settings of their own; should you be using such a framework, and find that `lsp-mode` doesn't behave as intended, please make sure to follow this tutorial from a clean starting point.
in your config or you could run in separate session.

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'python-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-headerline-breadcrumb-enable t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
```

# `pyls` configuration

For all the options available for `pyls`, check its [page](https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/) in `languages`

# Feature overview

Now, try to open one of your projects. If installation was successful you will be asked to select a project root.

FIXME: Add a pictures here, these are from CCP guide

_Note:_ the project root is needed by the language server in order to know where to
start the project discovery from. Typically, this is the git repository root but since exceptions to this rule have caused us a lot of trouble in the past (monorepos come to mind), `lsp-mode` by default asks the user to manually confirm the project root the first time a project is opened.
mono-repos it might be not. `lsp-mode` by default will ask the user to confirm
the project root (automatic root selection has caused a lot of trouble for us in
the past).

### Completion

By default, `lsp-mode` uses `company-mode` as its completion frontend. When
present, `company-mode` will be auto-configured and it will just work.

![company-mode completion](images/completion.png "company-mode completion")

### keybindings/`which-key` integration

`lsp-mode` has smart (almost) complete mnemonic keybindings which auto-enable
itself when a certain feature is supported by the server and when the
corresponding `Emacs` package is installed. In addition to that, `lsp-mode`
ships with [which-key](https://github.com/justbur/emacs-which-key "which key url") integration for better discoverability.
By default, `lsp-mode`'s keybindings are available under `s-l` (Super-l), e. g. `s-l h h` will
show documentation at point. You may change the default prefix by setting
`lsp-keymap-prefix`.

![select project root](images/which-key.png "Select project root")

### Mouse support
Yeah, `lsp-mode` supports mouse!

![mouse support](images/mouse.png "Mouse support")

### Refactoring/Code actions

- `lsp-rename` (`s-l r r`) - rename symbol/function at point.
- `lsp-format-buffer` - formats buffer with YAPF. (there is also `lsp-format-region`) TODO: add keybind

### Navigation

`lsp-mode` has integration with `xref` core package and in addition it has

- `xref-find-definitions`(`M-.` and `s-l g g`) - find definition(s) at point
- `xref-find-references`(`s-l g r`) - find references to the symbol at point
![References](images/images/references-helm.png "References helm")
- `helm-imenu` - browse the symbols in current document
- `helm-lsp-workspace-symbol` - find symbol in current project
![workspace symbols](images/workspace-symbols.png "workspace symbols")
- `helm-lsp-global-workspaces-symbol` - find symbol in all projects

_Note:_ if you prefer [ivy](https://github.com/abo-abo/swiper) over `helm` you may check out [lsp-ivy](https://github.com/emacs-lsp/lsp-ivy).

### [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)

`lsp-treemacs` is a package providing integration with
[treemacs](https://github.com/Alexander-Miller/treemacs) and an alternative
tree-view visualization. Refer to the project's readme for further information.

![tree references](images/treemacs-references.png "Treemacs references")

### Help/Documentation

`lsp-mode` automatically enables `eldoc-mode` which will show hover information
in the minibuffer. `lsp-mode` is showing only the one line signature info so if
you want to see the full documentation at point you can use `lsp-describe-thing-at-point`.

![hover](images/hover-info.png)

In addition to that you can also see signature help after pressing `C-M-SPC` or
after pressing trigger char like `(`. If there is more than one applicable signature due to function overloading, you may browse between the available candidates using `M-n/M-p`.
may browse them via `M-n/M-p`

![signature info](images/signature-help.png)

### Diagnostics

For on-the-fly errors `lsp-mode` is using `flycheck` (`flymake` is also
supported). It is configured automatically. In addition to standard `flycheck`
features `lsp-mode` provides a project-wide error list via
`lsp-treemacs-errors-list`.

![errors list](images/errors-list.png)

FIXME: Add block about dap-mode

# Optimizations

If you feel that pyls is running a bit slow there is a few optimization things to check:
1. Make sure that `lsp-mode`'s logging is off `(setq lsp-print-io nil)`
2. Do not turn on fuzzy completion server-side. Lsp-mode is fuzzy on the client side `(setq lsp-pyls-plugins-jedi-completion-fuzzy nil)`
3. Keep the amount of linters in check. Some linters are really heavy like Pylint. If your project doesn't use it, do not enable it `(setq lsp-pyls-plugins-pylint-enabled nil)`

# Some notes to remember

`pyls` uses [jedi](https://github.com/davidhalter/jedi) behind the scenes and inherits its limitations. As a result, using `xref-find-references` will not show you the references across all of the project, as jedi is lazy-loading packages. Until jedi backend changes this or implements caching you should remember that reference finding or renaming are semi-reliable.
