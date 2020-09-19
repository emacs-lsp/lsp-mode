LSP Rust
========

NOTE: If you are using `rustic-mode`, you have to change `rustic-lsp-server` instead of `lsp-rust-server`, since it also supports eglot as a lightweight alternative to lsp-mode.

- `lsp-rust-server` Choose LSP server (default is RLS)

- `lsp-rust-switch-server` Switch priorities of lsp servers

## RLS

### Customization

This is an incomplete list of the available options

- `lsp-rust-rls-server-command` change command to start RLS

- `lsp-rust-show-hover-context` turn off hover tooltips

## rustfmt

Code formatting with [rustfmt](https://github.com/rust-lang/rustfmt) can be configured with:

`lsp-rust-rustfmt-path` change default path for rustfmt executable

To enable automatic code format on save, add this to your `init.el` (`rust-mode` is assumed to be installed):

```
(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))
```

## rust-analyzer

### Customization

`lsp-rust-analyzer-server-command` change command to start rust-analyzer

`lsp-rust-analyzer-server-display-inlay-hints` toggle inlay hints

`lsp-rust-analyzer-display-chaining-hints` show inlay type hints for method chains

`lsp-rust-analyzer-display-parameter-hints` show function parameter name inlay hints at the call site

`lsp-rust-analyzer-max-inlay-hint-length` max inlay hint length

`lsp-rust-analyzer-cargo-watch-enable` toggle cargo watch

`lsp-rust-analyzer-cargo-watch-command` cargo watch command, e.g. `check` or `clippy`

`lsp-rust-analyzer-cargo-watch-args` extra args passed to cargo watch command, should be specified as a vector of strings

`lsp-rust-analyzer-use-client-watching` toggle client side watching

`lsp-rust-analyzer-cargo-all-targets` should run cargo watch for all targets or not

`lsp-rust-analyzer-exclude-globs` a list of glob patterns for Cargo package exclusion

`lsp-rust-analyzer-enabled-feature-flags` a list of feature flags that should be enabled

`lsp-rust-analyzer-lru-capacity` Number of syntax trees rust-analyzer keeps in memory

`lsp-rust-analyzer-cargo-override-command` Advanced option, fully override the command rust-analyzer uses for checking. The command should include `--message=format=json` or similar option

`lsp-rust-analyzer-inlay-face` The face to use for the Rust Analyzer inlays

### Commands

#### `lsp-rust-analyzer-syntax-tree`

Display syntax tree for current buffer

![](../examples/lsp-rust-analyzer-syntax-tree.png)

#### `lsp-rust-analyzer-status`

Display status information for rust-analyzer

![](../examples/lsp-rust-analyzer-status.png)

#### `lsp-rust-analyzer-join-lines`

Join selected lines into one, smartly fixing up whitespace and trailing commas

before:

![](../examples/lsp-rust-join-lines-before.png)

after:

![](../examples/lsp-rust-join-lines-after.png)

### inlay-hints

`lsp-rust-analyzer-inlay-hints-mode` enables displaying of inlay hints

NOTE: the inlay hints interact badly with the lsp-ui sideline, because it doesn't seem to consider the overlays in its width calculation, which often leads to lines wrapping around.

![](../examples/lsp-rust-analyzer-inlay-hints.png)

### Macro expansion

`lsp-rust-analyzer-expand-macro` expand macro call at point recursively

Use your own function for displaying macro expansion by customizing `lsp-rust-analyzer-macro-expansion-method`

Formatted and highlighted result with the default function of rustic.

![](../examples/lsp-rust-macro-expansion.png)

### auto-import

Get a list of possible auto import candidates with `lsp-execute-code-action`

![](../examples/lsp-rust-analyzer-auto-import.png)

### Caveats

- Rust Analyzer does not support disabling snippets - https://github.com/rust-analyzer/rust-analyzer/issues/2518

### extract signature

This [unmerged PR](https://github.com/emacs-lsp/lsp-mode/pull/1740) contains an example method that allows
modifying the signature that is displayed by eldoc.

