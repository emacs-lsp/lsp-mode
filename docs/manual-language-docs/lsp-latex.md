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
