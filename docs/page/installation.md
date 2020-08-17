# Installation

You need first `lsp-mode`, that is a Emacs client for an LSP server.
Then you need to install the specific LSP server for your language.

## Client

`lsp-mode` has multiple ways to install it.

### Manually via MELPA

The recommended way to install `lsp-mode` is via `package.el` - the built-in package manager in Emacs. `lsp-mode` is available on the two major `package.el` community maintained repos - [MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `lsp-mode` <kbd>RET</kbd>

When updating your packages with `package.el`, we recommend the following procedure:

    1. Delete your LSP-related packages
    2. Restart Emacs
    3. Install the new versions of the packages.

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

[lsp-mode](https://emacs-lsp.github.io/lsp-mode) is included in spacemacs develop branch. Add `lsp` to `dotspacemacs-configuration-layers` and configure the language that you want to use to be backed by `lsp` backend.

### Vanilla Emacs
    
You could go minimal and use `lsp-mode` as it is without external packages with the built-in `flymake` and `completion-at-point` or you could install the following extensions for better experience:

- [lsp-ui](https://emacs-lsp.github.io/lsp-ui/#intro) for fancy sideline, popup documentation, VScode-like peek UI, etc.
- [flycheck](https://github.com/flycheck/flycheck) if you prefer the more popular `flycheck` over renewed `flymake`. `lsp-mode` will automatically pick it up.
- [company-mode](https://github.com/company-mode/company-mode) for completion popups.
- [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) for various tree based UI controls (symbols, errors overview, call hierarchy, etc.)
- [helm-lsp](https://github.com/emacs-lsp/helm-lsp) provides on type completion alternative of `xref-apropos` using `helm`.
- [lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) provides on type completion alternative of `xref-apropos` using `ivy`.
- [dap-mode](https://emacs-lsp.github.io/dap-mode) if your language is supported by the debugger.

```elisp
;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "s-l")

(require 'lsp-mode)
(add-hook 'XXX-mode-hook #'lsp)
```

Where `XXX` could be major mode like `python`, `java`, `c++`. Alternatively, if you want to minimize you configuration you may use `prog-mode-hook`. In case you do that, `lsp` will try to start for each programming mode and echo a message when there is no client registered for the current mode or if the corresponding server is not present. In addition, `lsp-mode` will automatically detect and configure [lsp-ui](https://emacs-lsp.github.io/lsp-ui) and [company-mode](https://github.com/company-mode/company-mode). To turn off that behavior you could set `lsp-auto-configure` to `nil`.

To defer LSP server startup (and DidOpen notifications) until the buffer is visible you can use `lsp-deferred` instead of `lsp`:

```elisp
(add-hook 'XXX-mode-hook #'lsp-deferred)
```

#### use-package
    
Replace `(require 'lsp-mode)` with the following if you use use-package.

```elisp
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (XXX-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

```

To defer LSP server startup (and DidOpen notifications) until the buffer is visible you can use `lsp-deferred` instead of `lsp`:

```elisp
(use-package lsp-mode
    :hook (XXX-mode . lsp-deferred)
    :commands (lsp lsp-deferred))
```

## Install a language server

For instructions on how to install a server for your language, check the [available supported servers](./languages.md).
