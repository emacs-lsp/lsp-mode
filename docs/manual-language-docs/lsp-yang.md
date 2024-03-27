---
author: esmasth
root_file: docs/manual-language-docs/lsp-yang.md
---
# YANG (yang-lsp)

`lsp-mode` provides YANG language support via the [TypeFox/yang-lsp][1] Server.
The server identifier is `yls` as an abbreviation of the released binary called
`yang-language-server`.

## Configuration

Add following configuration in `.emacs` or `init.el` to hook lsp on YANG files,
since [`yang-mode`][2] is the supported major mode.

```lisp
(add-hook 'yang-mode-hook 'lsp)
```

It is also recommended to add following configuration for the `yang.settings`
file, which resides at the project root, to be validated using `lsp-json`.

```lisp
(add-hook 'jsonc-mode-hook 'lsp)
```

## Known Issues

* Files in the project need to be opened in buffer to be known by LSP server
* `yang.settings` is not really associated with its [JSON schema][3]
  [`yang-lsp-settings-schema.json`][4] yet
* yang-lsp settings file `yang.settings` is not respected
* `lsp-format-buffer` does not follow `yang-mode` or `yang.settings`
* Snippets have a bad format with extraneous characters

[1]: https://github.com/TypeFox/yang-lsp
[2]: https://github.com/mbj4668/yang-mode
[3]: https://json-schema.org/
[4]: https://raw.githubusercontent.com/TypeFox/yang-lsp/v0.7.6/schema/yang-lsp-settings-schema.json
