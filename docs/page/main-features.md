# Main features 

## Completion at point

If LSP server supports completion, `lsp-mode` use symbols returned by the server to present the user when completion is triggered via `completion-at-point`.

For better performance and results, use `company-capf` by installing [company-mode](https://company-mode.github.io/).

![](../examples/completion.gif)

You can check above the recommended settings for `company-mode`:

```elisp
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
```

## Code navigation

`lsp-find-definition`

![](../examples/find-definition.gif)

`lsp-find-references`

![](../examples/find-references.gif)

## Code lens

In case the LSP server supports code lens:

![](../examples/code-lens.png)

## Project errors on modeline

To see all error statistics in the modeline you can enable `lsp-modeline-diagnostics-mode` or `(setq lsp-modeline-diagnostics-enable t)`. This is especially useful for languages that compilation might be broken due to errors in other files(e.g. Java/Haskell).

![](../examples/modeline-diagnostics.png)

```elisp
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))
```

_Tip:_ To find out the global errors you might use `lsp-treemacs-errors-list`.

## Code actions on modeline

For a UI feedback of the available code actions, you can enable `lsp-modeline-code-actions-mode` which shows available code actions on modeline:

With `lsp-modeline-code-actions-segments` you can customize what to show on the modeline as you want:

|`lsp-modeline-code-actions-segments`|result|
|:-----:|:------:|
|`'(count icon)` (Default)|![](../examples/modeline-code-actions-count-icon.png)|
|`'(name icon)`|![](../examples/modeline-code-actions-name-icon.png)|
|`'(icon)`|![](../examples/modeline-code-actions-icon.png)|
|`'(count icon name)`|![](../examples/modeline-code-actions-count-icon-name.png)|

## Breadcrumb on headerline

For a UI feedback on headerline of the document symbols at point, current file or project name, you can enable `lsp-headerline-breadcrumb-mode` which shows a breadcrumb on top of window.

You can customize the breadcrumb segments via `lsp-headerline-breadcrumb-segments` variable, some examples:

|`lsp-headerline-breadcrumb-segments`|result|
|:-----:|:------:|
|`'(path-up-to-project file symbols)` (Default)|![](../examples/headerline-breadcrumb-path-up-to-project-file-symbols.png)|
|`'(project file symbols)`|![](../examples/headerline-breadcrumb-project-file-symbols.png)|
|`'(symbols)`|![](../examples/headerline-breadcrumb-symbols.png)|

If `lsp-headerline-breadcrumb-segments` contains `'symbols`, you can optionally label the corresponding entries in the headerline display by setting `lsp-headerline-breadcrumb-enable-symbol-numbers` to `t`.

## Symbol highlights

In case LSP server supports `hover` feature:

![](../examples/symbol-highlights.gif)

## Formatting

![](../examples/formatting.gif)

In general the formatter settings are language server specific(e. g. `JDT LS` uses eclipse formatter file and `lsp-java-format-settings-url` to configure it while clangd uses `clangd-format` and `lsp-dart` uses the built-in `dartfmt` from `Dart SDK`). The only settings that are controlled on `lsp-mode` level are indent size and whether the server should use tabs or spaces.

- Use `c-basic-offset` for `cc-mode` derived moves(e. g. java, C++) to control the tab size.
- Use `tab-width` for any other mode to do the same.
- Use `indent-tabs-mode` for selecting tab/spaces.

## Debugger

`lsp-mode` integrates with [dap-mode](https://emacs-lsp.github.io/dap-mode/) with implements the DAP(Debugger Adapter Protocol), for more information check the [`dap-mode` documentation](https://emacs-lsp.github.io/dap-mode/).

![](../examples/lsp-dart-flutter-debug.gif)

## Integrations

`lsp-mode` supports many integrations for improve the user experience like [treemacs](https://github.com/emacs-lsp/lsp-treemacs), [Helm](https://github.com/emacs-lsp/helm-lsp), [Ivy](https://github.com/emacs-lsp/lsp-ivy) and others. 

For all available integrations, check the `Extensions` section on the left navigation.
