---
root_file: docs/page/integration/iedit.md
---
# iedit

`lsp-mode` can leverage [`iedit`](https://github.com/victorhge/iedit) to edit
semantic matches in parallel:

- `lsp-iedit-highlights`: invoke `iedit` on the symbol highlights at point, all
  of which will be edited in parallel.

  To finish and exit, press `C-g`, with the cursor on `iedit` highlight.

  If an `iedit` session is already active, `lsp-iedit-highlights` will simply
  add the document highlights to it, without restarting it. This way, another
  group of symbol highlights can be added to the current `iedit` session.
