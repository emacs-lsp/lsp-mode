# Installation

## Install lsp-mode via MELPA

The recommended way to install `lsp-mode` is via `package.el` - the built-in package manager in Emacs. `lsp-mode` is available on the two major `package.el` community maintained repos - [MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `lsp-mode` <kbd>RET</kbd>

### Doom Emacs

[Doom Emacs](https://github.com/hlissner/doom-emacs) has a module to install and configure `lsp-mode` automatically, you just need to add `lsp` below `:tools` in your `init.el`.

To add `lsp-mode` support to some language, you can add the `+lsp` flag to the language you want. Example:

`init.el`
```elisp
...
:lang
(clojure +lsp)
(dart +lsp)
(java +lsp)
...
```

For Doom Emacs module flags and more information, check the [doom-emacs lsp module documentation](https://github.com/hlissner/doom-emacs/tree/develop/modules/tools/lsp).

### Spacemacs

//TODO

## Install a language server

For instructions on how to install a server for your language, check the [available supported servers](./languages.md).
