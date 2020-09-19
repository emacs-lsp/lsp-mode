lsp-latex for TeX, LaTeX, etc.
==========

## Server
For information about the LSP server, check the [texlab](https://texlab.netlify.app/).

## Installation(server)
``` bash
cargo install --git https://github.com/latex-lsp/texlab.git
```
### Debugger: Not available

## Installation(client)
You can get `lsp-latex` from melpa. Add melpa to the list `package-archives`,
and run:
<kbd>M-x</kbd> `package-initialize` <kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `lsp-latex`

## Available configurations
These are the available configurations. All configurations `texlab` provides are available.
See also [document of texlab](https://texlab.netlify.app/docs/reference/configuration).

| texlab configuration           | custom variable                         | summary                                                      |
|--------------------------------|-----------------------------------------|--------------------------------------------------------------|
| latex.rootDirectory            | lsp-latex-root-directory                | Directory from which file get compiled                       |
| latex.build.executable         | lsp-latex-build-executable              | Executable used on `lsp-latex-build`                         |
| latex.build.args               | lsp-latex--build-args                   | Args passed to `lsp-latex-texlab-executable`                 |
| latex.build.onSave             | lsp-latex-build-on-save                 | Build after saving file or not                               |
| latex.build.outputDirectory    | lsp-latex-build-output-directory        | Directory which will contain outputs                         |
| latex.build.forwardSearchAfter | lsp-latex-forward-search-after          | Execute forward search after building or not                 |
| latex.forwardSearch.executable | lsp-latex-forward-search-executable     | PDF viewer which will receive forward search signal          |
| latex.forwardSearch.args       | lsp-latex--forward-search-args          | Args passed to `lsp-latex-forward-search-executable`         |
| latex.lint.onChange            | lsp-latex-lint-on-change                | Lint with chktex after changing a file or not                |
| latex.lint.onSave              | lsp-latex-lint-on-save                  | Lint with chktex after saving a file                         |
| bibtex.formatting.lineLength   | lsp-latex-bibtex-formatting-line-length | Max length of characters per line on formatting BibTeX files |
| bibtex.formatting.formatter    | lsp-latex-bibtex-formatting-formatter   | Formatter of BibTeX (texlab or latexindent)                  |

## Other variables
### `lsp-latex-texlab-executable`
Where texlab server located.
### `lsp-latex-texlab-executable-argument-list`
Argument list passed to texlab server.


## Available functions
### `lsp-latex-build`
Build .tex files with texlab.
It use latexmk by default, so add .latexmkrc if you want to customize
latex commands or options. You can change build command and option to other
such as `make`, by changing `lsp-latex-build-executable` and
`lsp-latex-build-args`.

This command builds asynchronously by default, while it build synchronously
with prefix argument(C-u).

### `lsp-latex-forward-search`
Move to current position on pdf viewer.
To use, you should set `lsp-latex-forward-search-executable` and
`lsp-latex-forward-search-args` according to your pdf viewer.
See also [document of texlab](https://texlab.netlify.app/docs/installation/previewing).

## Note
In this package, you can use even texlab v0.4.2 or older, written with Java,
though it is not recommended. If you want to use them, you can write like:

``` emacs-lisp
;; Path to Java executable. If it is added to environmental PATH,
;; you don't have to write this.
(setq lsp-latex-java-executable "/path/to/java")

;; "texlab.jar" must be located at a directory contained in `exec-path'
;; "texlab" must be located at a directory contained in `exec-path'.
(setq lsp-latex-texlab-jar-file 'search-from-exec-path)
;; If you want to put "texlab.jar" somewhere else,
;; you can specify the path to "texlab.jar" as follows:
;; (setq lsp-latex-texlab-jar-file "/path/to/texlab.jar")
```
