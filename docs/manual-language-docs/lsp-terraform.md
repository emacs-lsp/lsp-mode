---
author: psibi
disqus: emacs-lsp
root_file: docs/manual-language-docs/lsp-terraform.md
---

## Server note

Currently the mode supports two language servers. This [FAQ entry](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project)
shows how to choose a specific one. If you would want to go with the
official Hashicorp's language server, set this:

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
