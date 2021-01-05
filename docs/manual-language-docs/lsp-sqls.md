# Sample configuration:


``` emacs-lisp
(setq lsp-sqls-connections
    '(((driver . "mysql") (dataSourceName . "yyoncho:local@tcp(localhost:3306)/foo"))
      ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=yyoncho password=local dbname=sammy sslmode=disable"))))

```

After you have started the server you have pick server connection and database via `M-x lsp-execute-code-action` (or corresponding `M-x lsp-sqls-*`) command.
