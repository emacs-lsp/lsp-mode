---
root_file: docs/page/file-watchers.md
---
# File watchers

When a workspace that is active in the current project requests file notifications via `workspace/didChangeWatchedFiles`, `lsp-mode` will monitor the workspace folders for changes and notify the server about them.

If you have problems with file watchers, first check what folders are being watched (they are logged in the `*lsp-log*` buffer when the server starts) and then check the below:

- If your project has a specific folder that should not be watched, you can exclude it with:

```emacs-lisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.my-folder\\'")
  ;; or
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))
```

- Increase the file watch warning threshold, the default is `1000`: `(setq lsp-file-watch-threshold 2000)`
- If the folder is some kind of cache folder or something that should always be excluded for everyone, consider [opening a pull request](https://github.com/emacs-lsp/lsp-mode/pulls) or [filing a bug](https://github.com/emacs-lsp/lsp-mode/issues) to add to the [common regex](https://github.com/emacs-lsp/lsp-mode/blob/1b13d7c1b39aaad12073095ef7719952568c45db/lsp-mode.el#L340).
- As a last resort, disable file watchers with `(setq lsp-enable-file-watchers nil)` (you may use dir-locals).
