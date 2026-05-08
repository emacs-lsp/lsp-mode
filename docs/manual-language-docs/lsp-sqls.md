---
root_file: docs/manual-language-docs/lsp-sqls.md
---
# Sample configuration:


``` emacs-lisp
(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(((driver . "mysql") (dataSourceName . "yyoncho:local@tcp(localhost:3306)/foo"))
      ((driver . "mssql") (dataSourceName . "Server=localhost;Database=sammy;User Id=yyoncho;Password=hunter2;"))
      ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=yyoncho password=local dbname=sammy sslmode=disable"))))

```

## Storing Configuration in `<project>/.sqls/config.json`

Alternatively, you can store your configuration in the project root at `<project>/.sqls/config.json`:

```
{
  "sqls": {
    "connections": [
      {
        "driver": "mysql",
        "dataSourceName": "yyoncho:local@tcp(localhost:3306)/foo"
      }, 
      …etc…
    ]
  }
}
```

In this case, you need to set `lsp-sqls-workspace-config-path` to "root":

```emacs-lisp
(setq lsp-sqls-workspace-config-path "root")
```

## Storing Configuration in the Current Directory

If you want to configure it for the current directory, you can create a `.sqls/config.json` file:

```
.sqls/config.json
target.sql
```

For this setup, ensure that `lsp-sqls-workspace-config-path` is set to "workspace":

```emacs-lisp
(setq lsp-sqls-workspace-config-path "workspace")
```

# Switching Connections and Databases

Now, lsp should start in sql-mode buffers. You can choose a server connection using `M-x lsp-execute-code-action` and then selecting "Switch Connections", or directly with `M-x lsp-sql-switch-connection`.

To change the database, use `M-x lsp-execute-code-action` and select "Switch Database" (or `M-x lsp-sql-switch-database`).
