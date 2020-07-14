# Limitations

## File watches

When some of the workspaces that are active in the current project requests file notifications via `workspace/didChangeWatchedFiles`, `lsp-mode` will start monitoring each of the folders in the workspace for changes. In case your project contains a lot of files you might want to disable file monitoring via `lsp-enable-file-watchers` (you may use dir-locals).
