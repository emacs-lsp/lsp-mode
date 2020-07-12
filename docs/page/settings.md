Settings
========

These are `lsp-mode` specific custom settings:

- `lsp-log-io` - If non-nil, print all messages to and from the language server to `*lsp-log*`.
- `lsp-print-performance` - If non-nil, print performance info. to `*lsp-log*`.
- `lsp-report-if-no-buffer` - If non nil the errors will be reported even when the file is not open.
- `lsp-keep-workspace-alive` - If non nil keep workspace alive when the last workspace buffer is closed.
- `lsp-enable-snippet` - Enable/disable snippet completion support.
- `lsp-auto-guess-root` - Automatically guess the project root using projectile/project. Do **not** use this setting unless you are familiar with `lsp-mode` internals and you are sure that all of your projects are following `projectile=/=project.el` conventions.
- `lsp-restart` - Defines how server exited event must be handled.
- `lsp-session-file` - File where session information is stored.
- `lsp-auto-configure` - Auto configure `lsp-mode`. When set to `t`, `lsp-mode` will auto-configure `lsp-ui`, `dap-mode` and other settings that makes sense to enable by default.
- `lsp-document-sync-method` - How to sync the document with the language server.
- `lsp-auto-execute-action` - Auto-execute single action.
- `lsp-eldoc-render-all` - Display all of the info returned by `document/onHover`. If this is nil, `eldoc` will show only the symbol information.
- `lsp-enable-completion-at-point` - Enable `completion-at-point` integration.
- `lsp-enable-xref` - Enable xref integration.
- `lsp-diagnostic-package` - Specifies which package to use for diagnostics. Choose from `:auto`, `:flycheck`, `:flymake` and `:none`. Default is `:auto` which means use `:flycheck` if present.
- `lsp-enable-indentation` - Indent regions using the file formatting functionality provided by the language server.
- `lsp-enable-on-type-formatting` - Enable `textDocument/onTypeFormatting` integration.
- `lsp-before-save-edits` - If non-nil, `lsp-mode` will apply edits suggested by the language server before saving a document.
- `lsp-imenu-show-container-name` - Display the symbol's container name in an imenu entry.
- `lsp-imenu-container-name-separator` - Separator string to use to separate the container name from the symbol while displaying imenu entries.
- `lsp-imenu-sort-methods` - How to sort the imenu items. The value is a list of `kind`, `name` or `position`. Priorities are determined by the index of the element.
- `lsp-response-timeout` - Number of seconds to wait for a response from the language server before timing out.
- `lsp-enable-file-watchers` - If non-nil lsp-mode will watch the files in the workspace if the server has requested that.
- `lsp-server-trace` - Request trace mode on the language server.
- `lsp-semantic-highlighting` - Enable experimental semantic highlighting support
- `lsp-enable-imenu` - If non-nil, automatically enable imenu integration when server provides `textDocument/documentSymbol`.
- `lsp-signature-auto-activate` - Auto activate signature when trigger char is pressed.
- `lsp-signature-render-documentation` - Include signature documentation in signature help.
- `lsp-enable-text-document-color` - Enable `textDocument/documentColor` when server supports it.

## Checking project errors

To see all error statistics in the modeline you can enable `lsp-diagnostics-modeline-mode`. This is especially useful for languages that compilation might be broken due to errors in other files(e.g. Java/Haskell).

![](../examples/modeline-diagnostics.png)

```elisp
(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))
```

_Tip:_ To find out the global errors you might use `lsp-treemacs-errors-list`.

## Code actions on modeline

For a UI feedback of the available code actions, you can enable `lsp-modeline-code-actions-mode` which shows available code actions on modeline:

![](../examples/modeline-code-actions.png)

## Breadcrumb on headerline

For a UI feedback on headerline of the document symbol at point, you can enable `lsp-headerline-breadcrumb-mode` which shows a breadcrumb on top of window.

You can customize the breadcrumb prefix via `lsp-headerline-breadcrumb-prefix`:

`'path-up-to-project` (Default)

![](../examples/headerline-breadcrumb-up-to-project.png)

`'file-name-only`

![](../examples/headerline-breadcrumb-file-name-only.png)

`nil`

![](../examples/headerline-breadcrumb-no-prefix.png)

## Limitations

### File watches

When some of the workspaces that are active in the current project requests file notifications via `workspace/didChangeWatchedFiles`, `lsp-mode` will start monitoring each of the folders in the workspace for changes. In case your project contains a lot of files you might want to disable file monitoring via `lsp-enable-file-watchers` (you may use dir-locals).
