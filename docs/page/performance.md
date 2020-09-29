Performance
===========

## Tuning

Use `M-x lsp-doctor` to validate if your `lsp-mode` is properly configured. In the section below, you could find description for each of the checks:

When configured properly `lsp-mode`'s performance is on par with mainstream LSP clients (e. g. `VScode`, `Theia`, etc). Here are steps to achieve optimal results.

- Use Emacs 27+ with native json support. (Note: this requires that you have [libjansson](http://www.digip.org/jansson/) installed, and that emacs was compiled with \`–with-json\` passed to \`./configure\`.) You can check your installation for native json support by running <kbd>M-:</kbd> `(functionp 'json-serialize)` <kbd>RET</kbd>.
Benchmarks show that Emacs 27 is `~15 times` faster than Emacs when using Elisp json parser implementation.

- Adjust `gc-cons-threshold`. The default setting is too low for `lsp-mode`'s needs due to the fact that client/server communication generates a lot of memory/garbage. You have two options:

    - Set it to big number(100mb) like most of the popular starter kits like Spacemacs/Doom/Prelude, etc do:

    ```elisp
    (setq gc-cons-threshold 100000000)
    ```

    - Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch. If even a 100-fold increase doesn't help, there's some deeper problem with the Lisp code which produces so much garbage, or maybe GC is not the reason for slowdown." Source: <https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/>

- Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.

``` elisp
(setq read-process-output-max (* 1024 1024)) ;; 1mb
```

- Make sure that you are using `company-capf` as the completion provider with:

``` elisp
(setq lsp-completion-provider :capf)
```

*Note:* - to verify which `company` backend implementation you are using do `M-x company-diag` when performing auto-completion.

- Optional: Disable `lsp-ui`. Normally, `lsp-ui` is very fast but in some systems (especially when using `Windows`) `lsp-ui` overlays and popups might slow down emacs.
- Optional: fine-tune `lsp-idle-delay`. This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.

``` elisp
(setq lsp-idle-delay 0.500)
```
### gccemacs

[gccemacs](https://akrl.sdf.org/gccemacs.html) is a bleeding-edge version of `Emacs` that compiles elisp to native code, resulting in more than 200% speedup. For everyone looking for optimal performance, `gccemacs` is the way to go.

## Ignore watch folders/files

If the server supports watch files, by default `lsp-mode` tries to watch all files and folders of the project ignoring the regexp from `lsp-file-watch-ignored`. If you don't want some file or folder
to be watched for performance reasons, you can add a regexp to that variable excluding the file or folder.
Also you can disable the file watch feature with:

```elisp
(setq lsp-enable-file-watchers nil)
```

## Reporting performance problems

If you have tried all of the non-optional steps from the list and `emacs` is still not very responsive please open a PR with the following information:

- Collect **lsp-log** data after setting `lsp-print-performance` to `t`.

``` elisp
(setq lsp-print-performance t)
```

- Include emacs performance report. Use the following step to collect it:
  - `M-x profiler-start` and select `CPU`
  - Reproduce the slow behavior.
  - `M-x profiler-stop`
  - In the profiler report expand all nodes by doing `C-u TAB`.

  *Note:* - `lsp-mode` is just a frontend and the performance depends on server as well. Some servers (e. g. Palantir's Python Language Server) might be slow when performing auto-completion.
