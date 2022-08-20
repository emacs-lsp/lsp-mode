---
author: psibi
template: comment.html
root_file: docs/manual-language-docs/lsp-terraform-ls.md
---

## Server note

This page documents the official language server for Terraform by
Hashicorp.

Note that currently lsp-mode supports two terraform language
servers. This [FAQ entry](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project) shows how to choose a specific one. If
you would want to go with the official Hashicorp's language server,
set this:

``` emacs-lisp
(setq lsp-disabled-clients '(tfls))
```

## terraform-ls

### Commands

#### `lsp-terraform-ls-validate`

Runs terraform validate on project root. All the violations are
published back in the buffer.

![](../examples/lsp-terraform-validate.png)

#### `lsp-terraform-ls-init`

Runs `terraform init` using terraform available from `$PATH`. You have
to make sure that that proper credentials are there.

Note that this is a synchronous action and will timeout after a
certain amount of time.

#### `lsp-terraform-ls-version`

This feature is only available on terraform-ls version >= 0.29.0.

Provides information about the terraform binary version for the
current workspace. This is how it will look like in the minibuffer:

``` shellsession
Required: 1.1.9, Current: 1.0.5
```

Note that if you have changed the version in your `$PATH` meanwhile,
you would have to restart the lsp session using
`lsp-workspace-restart` for it to pick up the newer version.

### Code Lens

This is an experimental feature which can be enabled via the option
`lsp-terraform-ls-enable-show-reference`:

``` emacs-lisp
(setq lsp-terraform-ls-enable-show-reference t)
```

This gif demonstrates how this feature is used:

![](../examples/lsp-terraform-code-lens-refs.gif)

### Semantic token support

Make sure to enable these two variables to ensure that you have
semantic token support for terraform mode:

``` emacs-lisp
(setq lsp-semantic-tokens-enable t)
(setq lsp-semantic-tokens-honor-refresh-requests t)
```

This is how the code looks without semantic tokens support:

![](../examples/lsp-terraform-without-semantic-token.png)

And with semantic token support you get more contextual information
via different faces:

![](../examples/lsp-terraform-with-semantic-token.png)

### Link to Documentation

Link to documentation from module sources for registry modules is
available. Make sure you have this enabled:

``` emacs-lisp
(setq lsp-enable-links t)
```

Note that the default value of `lsp-enable-links` is `t`. So enabling
it isn't strictly required, but you would require this feature for it
to properly work.

This gif demonstrates on how to use it:

![](../examples/lsp-terraform-open-link.gif)

There are two ways to open the link:

- Key binding: Alt + Enter
- Clicking the middle button of your mouse

### Treeview controls

For this feature to work, make sure that you have [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
installed.

#### Providers widget

This widget can be invoked by `lsp-terraform-ls-providers`.

![](../examples/lsp-terraform-providers-treemacs.png)

#### Module calls widget

This widget can be invoked by `lsp-terraform-ls-module-calls`.

If the modules data is empty, you might need to do `terraform init`
for the project.

The widget has a minor mode named
`lsp-terraform-modules-mode`. Following keybinding are available
within the mode:

- `g`: Refresh modules view

![](../examples/lsp-terraform-modules-treemacs.png)
