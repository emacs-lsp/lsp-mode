---
root_file: docs/manual-language-docs/lsp-gopls.md
---

# Go (gopls)

`lsp-mode` provides Go language support via the
[gopls](https://github.com/golang/tools/tree/master/gopls) language
server. See
[upstream](https://github.com/golang/tools/tree/master/gopls#installation)
for gopls installation instructions.

## Configuration

By default, some
[gopls analyzers](https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md)
are enabled and others are disabled. To override, use:

```
(setq lsp-go-analyses '((shadow . t)
                        (simplifycompositelit . :json-false)))
```

## Troubleshooting

### Working with nested go.mod files

If you encounter an error like one of these:

* `errors loading workspace: You are working in a nested module.
  Please open it as a separate workspace folder`
* `This file is in %s, which is a nested module in the %s module.
  gopls currently requires one module per workspace folder. Please
  open %s as a separate workspace folder`

Then the problem is most likely because `lsp-mode` has not started
gopls with the correct root directory. By default `lsp-mode` will pick
the root of your whole Git repository (or whatever else Projectile
turns up) for all language servers started inside the repo. However,
if you have nested `go.mod` files, you need to have `lsp-mode` start
*separate* instances of gopls for each nested `go.mod`, and use the
appropriate one for each file. You can make this work by manually
invoking `M-x lsp-workspace-folders-add` and adding each nested
`go.mod` directory as a workspace folder. Then reloading LSP with `M-x
lsp` in an affected file (or restarting Emacs) will cause the file to
be re-associated with a gopls process running under the correct
project workspace.

See [#3473](https://github.com/emacs-lsp/lsp-mode/issues/3473) for a
bit more discussion of this particular issue.
