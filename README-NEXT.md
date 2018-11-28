Emacs Language Server Client
=========

[![Join the chat at https://gitter.im/emacs-lsp/lsp-mode](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://travis-ci.org/emacs-lsp/lsp-mode.svg?branch=master)](https://travis-ci.org/emacs-lsp/lsp-mode)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-mode-badge.svg)](https://stable.melpa.org/#/lsp-mode)
[![MELPA](http://melpa.org/packages/lsp-mode-badge.svg)](http://melpa.org/#/lsp-mode)

Client for [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/) (v3.0). [lsp-mode](https://github.com/emacs-lsp/lsp-mode) aims to provide IDE-like experience by providing optional integration with the most popular Emacs packages like `company`, `flycheck` and `projectile`.

* As you type reporting of parsing and compilation errors (via [flycheck](https://github.com/flycheck/flycheck)/[lsp-ui](https://github.com/emacs-lsp/lsp-ui))
* Code completion - using [company-lsp](https://github.com/tigersoldier/company-lsp) or builtin ```complete-at-point```
* Hovers - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code actions - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code outline - using builtin [imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
* Code navigation - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Code lens (references/implementations) - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Highlights
* Code formatting
* Visual debugger - [dap-mode](https://github.com/yyoncho/dap-mode/)

## Installation

### Install via melpa
The recommended way to install `lsp-mode` is via `package.el` - the built-in package manager in Emacs. `lsp-mode` is available on the two major `package.el` community maintained repos - [MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

### Spacemacs
[lsp-mode](https://github.com/emacs-lsp/lsp-mode) is included in spacemacs. Add `lsp` to `dotspacemacs-configuration-layers` and configure the language that you want to use to be backed by `lsp` backend.

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `lsp-mode` <kbd>[RET]</kbd>

## Configuration
Add the following line in your configuration file where `programming-mode-hook` stands for the language specific hook like `python-mode-hook`.  In addition, `lsp-mode` will automatically detect and configure [lsp-ui](https://github.com/emacs-lsp/lsp-ui) and [company-lsp](https://github.com/tigersoldier/company-lsp). To turn off that behaviour you could set `lsp-auto-configure` to `nil`.
``` emacs-lisp
(require 'lsp)
(add-hook 'programming-mode-hook 'lsp)
```

## Supported languages
Some of the servers are directly supported by `lsp-mode` by requiring
`lsp-clients.el` while other require isntalling additional package which provide
server specific functionality.

| Language           | Extension                                                                    |
|--------------------|------------------------------------------------------------------------------|
| Bash               | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| C (ccls)           | [emacs-ccls](https://github.com/MaskRay/emacs-ccls)                          |
| C (clangd)         | [lsp-clangd](https://github.com/emacs-lsp/lsp-clangd)                        |
| C (cquery)         | [emacs-cquery](https://github.com/cquery-project/emacs-cquery)               |
| CSS                | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| Go                 | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| Groovy             | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| HTML               | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| Haskell            | [lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)                      |
| Java (Eclipse JDT) | [lsp-java](https://github.com/emacs-lsp/lsp-java)                            |
| Javascript         | [lsp-javascript](https://github.com/emacs-lsp/lsp-javascript)                |
| OCaml              | [lsp-ocaml](https://github.com/emacs-lsp/lsp-ocaml)                          |
| PHP                | [lsp-php](https://github.com/emacs-lsp/lsp-php)                              |
| Python             | [Built-in](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-clients.el) |
| Ruby               | [lsp-ruby](https://github.com/emacs-lsp/lsp-ruby)                            |
| Rust               | [lsp-rust](https://github.com/emacs-lsp/lsp-rust)                            |
| Scala              | [lsp-scala](https://github.com/rossabaker/lsp-scala)                         |
| Vue                | [lsp-vue](https://github.com/emacs-lsp/lsp-vue)                              |
## Examples

### Completion
Completion is provided with the native `completion-at-point` (<kbd>C</kbd>-<kbd>M</kbd>-<kbd>i</kbd>),
and should therefore work with any other completion backend. Async completion is provided by
[company-lsp](https://github.com/tigersoldier/company-lsp).

![completion](./examples/completion.png)

### `eldoc` (Help on hover)
Hover support is provided with `eldoc`, which should be enabled automatically.

![eldoc](./examples/eldoc.png)

### Goto definition
Use <kbd>M</kbd> - <kbd>.</kbd> (`xref-find-definition`)
to find the definition for the symbol under point.

![gotodef](./examples/goto-def.gif)

### Symbol references
Use <kbd>M</kbd> - <kbd>?</kbd> (`xref-find-references`)
to find the references to the symbol under point.

![ref](./examples/references.png)

### Symbol Highlighting
![sym_highlight](./examples/sym_highlight.gif)

### Imenu
Add
```emacs-lisp
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
```
to your init file to enable imenu integration.

![imenu-1](./examples/imenu-1.png)
![imenu-2](./examples/imenu-2.png)

#### With [helm-imenu](https://github.com/emacs-helm/helm)
![helm-imenu](./examples/helm-imenu.gif)

### Rename
Use <kbd>M</kbd> - <kbd>x</kbd> `lsp-rename`.

![rename](./examples/rename.gif)

### Hooks

`lsp-mode` provides a handful of hooks that can be used to extend and configure
the behaviour of language servers. A full list of hooks is available in the
[API documentation](./doc/API.org).

## Adding support for languages
See [API docs](./doc/API.org)

Here it is the minimal configuration that is needed for new language server
registation. Refer to the documentation of `lsp--client` for the additional
settings supported on registration time.
``` emacs-lisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                  :major-modes '(python-mode)
                  :server-id 'pyls))
```

### See also
* [`dap-mode`](https://github.com/yyoncho/dap-mode) - Debugger integration for
* [`eglot`](https://github.com/joaotavora/eglot) - An alternative and lighter LSP implementation.
