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

Alternatively, you can leave `lsp-sqls-workspace-config-path` to the default "workspace" value, and put a json file in `<project>/.sqls/config.json` containing
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

Now lsp should start in sql-mode buffers, and you can pick a server connection with `M-x lsp-execute-code-action` and "Switch Connections" (or directly with `M-x lsp-sql-switch-connection`). You can change database with `M-x lsp-execute-code-action` and "Switch Database" (or `M-x lsp-sql-switch-database`).
