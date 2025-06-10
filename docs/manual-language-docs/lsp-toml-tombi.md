---
author: psibi
template: comment.html
root_file: docs/manual-language-docs/lsp-toml-tombi.md
---

## Server note

This page documents the Tombi's language server for Toml.

Note that currently lsp-mode supports two Toml language servers. This
[FAQ entry](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project) shows how to choose a specific one. If you would want
to go with the Tombi's server, set this:

``` emacs-lisp
(setq lsp-disabled-clients '(taplo))
```
