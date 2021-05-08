## Server note

NOTE: If you are using `rustic-mode`, you have to change `rustic-lsp-server` instead of `lsp-rust-server`, since it also supports eglot as a lightweight alternative to lsp-mode.

- `lsp-rust-server` Choose LSP server (default is rust-analyzer)

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

### Open Cargo.toml

`lsp-rust-analyzer-open-cargo-toml` opens the Cargo.toml closest to the current file. Calling it with a universal argument will open the Cargo.toml in another window.

Corresponds to [the rust-analyzer LSP extension](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#open-cargotoml) 

![](./manual-language-docs/lsp-rust-analyzer-open-cargo-toml.gif)

### Find and execute tests related to current position

`lsp-rust-analyzer-related-tests` find all tests related to the current position, asks for user completion and executes the selected test in a compilation buffer.

Corresponds to [the rust-analyzer LSP extension](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#related-tests) 

In the example below, first you see that
   + On the left, the function `check_infer` is defined, on the right another file is opened with many test functions, some of which call `check_infer`. With the cursor on `check_infer`, call `lsp-rust-analyzer-related-tests` and select `infer_pattern_match_slice` with fuzzy matching. The test is executed on the right with compilation major mode
   + Move the cursor to `fn ellipsize` and attempt to find related tests to no avail. Confirm that the function is indeed untested by using swiper and finding one place in the file, where the function is called

![](./manual-language-docs/lsp-rust-analyzer-find-related-tests.gif)

### Caveats

- Rust Analyzer does not support disabling snippets - https://github.com/rust-analyzer/rust-analyzer/issues/2518

### extract signature

This [unmerged PR](https://github.com/emacs-lsp/lsp-mode/pull/1740) contains an example method that allows
modifying the signature that is displayed by eldoc.

### TRAMP Example

The following is an example configuration for using lsp-mode with a remote rust-analyzer server:

```
(with-eval-after-load "lsp-rust"
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "rust-analyzer")
    :remote? t
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :server-id 'rust-analyzer-remote)))
```
