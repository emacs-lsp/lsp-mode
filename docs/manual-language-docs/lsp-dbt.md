---
root_file: docs/manual-language-docs/lsp-dbt.md
---

## Client

The client lives outside lsp-mode, in
[dbt-mode](https://github.com/piotr-yuxuan/dbt-mode), a major mode for the
SQL and Jinja of [dbt](https://www.getdbt.com/) projects, which also
navigates, previews rows and draws lineage without any server.  It runs
one of two servers:

- `dbt lsp`, the language server of dbt 2's proprietary build;
- [dbt-lsp-plus](https://github.com/piotr-yuxuan/dbt-lsp-plus), in front
  of `dbt lsp`, which adds what it lacks: the outline of a model, every
  resource as a workspace symbol, the call hierarchy of models, sources
  and macros, references and renames of sources, definitions from YAML,
  and the last run's failures as diagnostics.  With dbt-oss, which has no
  `dbt lsp`, it runs alone.

## Setup

```elisp
(use-package dbt-mode
  :straight (dbt-mode :type git :host github :repo "piotr-yuxuan/dbt-mode")
  :hook ((dbt-mode . dbt-lsp-deferred)
         ((yaml-mode yaml-ts-mode) . dbt-properties-mode-maybe))
  :custom
  ;; Run the server through dbt-lsp-plus, when it is on exec-path.
  (dbt-lsp-proxy t))
```

`dbt-lsp-deferred` makes the root of the dbt project a folder of the
session before the server starts, so that each dbt project of a
repository gets its own server.

## Options

dbt-mode's options are in the customisation group `dbt`:

- `dbt-lsp-proxy`: whether `dbt lsp` runs through dbt-lsp-plus, when that
  executable exists; default `nil`.
- `dbt-lsp-arguments`: more arguments of `dbt lsp`, such as
  `("--profiles-dir" "/path")`; default `nil`.
- `dbt-lsp-enable-linter`: whether the server lints the SQL with dbt's
  built-in linter; default `nil`.

dbt-lsp-plus's own options are its command line and environment: see
[its configuration](https://github.com/piotr-yuxuan/dbt-lsp-plus/blob/main/docs/reference/configuration.md).
