---
root_file: docs/manual-language-docs/lsp-rust-rls.md
---
## RLS

### Customization

This is an incomplete list of the available options

- `lsp-rust-rls-server-command` change command to start RLS

- `lsp-rust-show-hover-context` turn off hover tooltips

### rustfmt

Code formatting with [rustfmt](https://github.com/rust-lang/rustfmt) can be configured with:

`lsp-rust-rustfmt-path` change default path for rustfmt executable

To enable automatic code format on save, add this to your `init.el` (`rust-mode` is assumed to be installed):

```
(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))
```
