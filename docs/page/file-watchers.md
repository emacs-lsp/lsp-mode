---
root_file: docs/page/file-watchers.md
---
# File watchers

When some of the workspaces that are active in the current project requests file notifications via `workspace/didChangeWatchedFiles`, `lsp-mode` will start monitoring each of the folders in the workspace for changes to notify the server about that. 

In case you have issues with file watchers, first, check what folders are being watched, they are logged on `*lsp-log*` when the server starts, then you may consider check the below:

- If your project has some specific folder that should not be watched, you can exclude it with:

```emacs-lisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.my-folder\\'")
  ;; or
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))
```

- Increase the file watch warning threshold, the default is `1000`, `(setq lsp-file-watch-threshold 2000)` 
- If the folder is some kind of cache folder or something that should always be excluded for everyone, consider opening a pull request or informing the maintainers to add to the [common regex](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el#L306). 
- In last case, disable file watchers with `(setq lsp-enable-file-watchers nil)` (you may use dir-locals).
