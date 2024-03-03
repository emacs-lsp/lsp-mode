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

It recommended to add following configuration for the `yang.settings` file,
which resides at the user/project/workspace root, to be validated via
[`lsp-json`][5]. This may be automated by `lsp-yang` in later stages.

```lisp
(setq lsp-json-schemas
      `[(:fileMatch ["yang.settings"] :url lsp-yang-yls-settings-schema-url)])
```

To automatically trigger the [`lsp-json`][5] based validation, following
configuration is recommended.

```lisp
(add-hook 'jsonc-mode-hook 'lsp)
```

## Known Issues

* Files in the project need to be opened in buffer to be known by LSP server
* `yang.settings` is not associated with its [JSON schema][3]
  [`yang-lsp-settings-schema.json`][4] yet
* yang-lsp settings file `yang.settings` is not respected
* `lsp-format-buffer` does not follow `yang-mode` or `yang.settings`
* Snippets have a bad format with extraneous characters

[1]: https://github.com/TypeFox/yang-lsp
[2]: https://github.com/mbj4668/yang-mode
[3]: https://json-schema.org/
[4]: https://raw.githubusercontent.com/TypeFox/yang-lsp/v0.7.6/schema/yang-lsp-settings-schema.json
[5]: https://emacs-lsp.github.io/lsp-mode/page/lsp-json/
