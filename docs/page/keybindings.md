# Keybindings

When using `lsp-mode` most of the features depend on server
capabilities. `lsp-mode` provides default bindings which are dynamically
enabled/disabled based on the server functionality. All the commands are
configured `lsp-command-map` which is bound to `lsp-keymap-prefix`
(default `s-l`).

| Keybinding | Description                                                                                                                         |
| ---------- | --------------------------------------------------------------------------------------------------------------------------          |
| `s-l s s`  | Entry point for the server startup.                                                                                                 |
| `s-l s r`  | Restart language server                                                                                                             |
| `s-l s q`  | Shutdown language server                                                                                                            |
| `s-l s d`  | Describes current session                                                                                                           |
| `s-l s D`  | Disconnect the buffer from the language server keeping the server running.                                                          |
| `s-l = =`  | Ask the server to format this document.                                                                                             |
| `s-l = r`  | Ask the server to format the region, or if none is selected, the current line.                                                      |
| `s-l f a`  | Add new project root to the list of workspace folders.                                                                              |
| `s-l f r`  | Remove project root from the list of workspace folders.                                                                             |
| `s-l f b`  | Remove project root from the workspace blacklist.                                                                                   |
| `s-l t l`  | Toggle code-lens overlays.                                                                                                          |
| `s-l t L`  | Toggle client-server protocol logging.                                                                                              |
| `s-l t h`  | Toggle symbol highlighting.                                                                                                         |
| `s-l t S`  | Toggle minor mode for showing information for current line in sideline. (requires `lsp-ui`)                                         |
| `s-l t d`  | Toggle minor mode for showing hover information in child frame. (requires `lsp-ui`)                                                 |
| `s-l t s`  | Toggle signature auto activate.                                                                                                     |
| `s-l t f`  | Toggle on type formatting.                                                                                                          |
| `s-l t T`  | Toggle global minor mode for synchronizing `lsp-mode` workspace folders and `treemacs` projects. (requires `lsp-treemacs`)          |
| `s-l g g`  | Find definitions of the symbol under point.                                                                                         |
| `s-l g r`  | Find references of the symbol under point.                                                                                          |
| `s-l g i`  | Find implementations of the symbol under point.                                                                                     |
| `s-l g t`  | Find type definitions of the symbol under point.                                                                                    |
| `s-l g d`  | Find declarations of the symbol under point.                                                                                        |
| `s-l g h`  | Show the incoming call hierarchy for the symbol at point. (requires `lsp-treemacs`)                                                 |
| `s-l g a`  | Find all meaningful symbols that match pattern.                                                                                     |
| `s-l h h`  | Display the type signature and documentation of the thing at                                                                        |
| `s-l h s`  | Activate signature help.                                                                                                            |
| `s-l h g`  | Trigger display hover information popup and hide it on next typing.                                                                 |
| `s-l r r`  | Rename the symbol (and all references to it).                                                                                       |
| `s-l r o`  | Perform the source.organizeImports code action, if available.                                                                       |
| `s-l a a`  | Execute code action.                                                                                                                |
| `s-l a l`  | Click lsp lens using ‘avy’ package.                                                                                                 |
| `s-l a h`  | Highlight symbol at point.                                                                                                          |
| `s-l p g`  | Peek definitions to the identifier at point. (requires `lsp-ui`)                                                                    |
| `s-l p r`  | Peek references to the identifier at point. (requires `lsp-ui`)                                                                     |
| `s-l p i`  | Peek implementation locations of the symbol at point. (requires `lsp-ui`)                                                           |
| `s-l p s`  | Peek symbols in the workspace. (requires `lsp-ui`)                                                                                  |
| `C-u RET`  | When inserting `C-u` will change the behaviour from insert to replace or vice versa depending on `lsp-completion-default-behaviour` |

## which-key integration

To enable [which-key](https://github.com/justbur/emacs-which-key/)
integration in the active major mode for `lsp-mode-map` you will need to
call the `lsp-enable-which-key-integration` function. This could be done
when `lsp-mode` starts with the following:

```elisp
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
```

You can also enable which-key integration for all major modes by passing
`t` as a parameter. This is useful for buffers that can use multiple
major modes like those using `vue-mode`.

![](../examples/which-key.png)
