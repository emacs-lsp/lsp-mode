# Installation

## Install via melpa

The recommended way to install `lsp-mode` is via `package.el` - the
built-in package manager in Emacs. `lsp-mode` is available on the two
major `package.el` community maintained repos - [MELPA
Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `lsp-mode` <kbd>RET</kbd>

## Configuration

### Install language server

Check the table below with the list of supported servers and the
corresponding instructions on how to install the server.

### Configure lsp-mode

1.  Vanilla Emacs
    
    You could go minimal and use `lsp-mode` as it is without external
    packages with the built-in `flymake` and `completion-at-point` or
    you could install the following extensions for better experience:
    
      - install [lsp-ui](https://github.com/emacs-lsp/lsp-ui) for fancy
        sideline, popup documentation, VScode-like peek UI, etc.
      - install [flycheck](https://github.com/flycheck/flycheck) if you
        prefer the more popular `flycheck` over renewed `flymake`.
        `lsp-mode` will automatically pick it up.
      - install
        [company-mode](https://github.com/company-mode/company-mode) for
        completion popups.
      - install
        [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) for
        various tree based UI controls (symbols, errors overview, call
        hierarchy, etc.)
      - install [helm-lsp](https://github.com/emacs-lsp/helm-lsp)
        provides on type completion alternative of `xref-apropos` using
        `helm`.
      - install [lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) provides
        on type completion alternative of `xref-apropos` using `ivy`.
      - install [dap-mode](https://github.com/emacs-lsp/dap-mode) if
        your language is supported by the debugger.
    
    <!-- end list -->
    
    ``` elisp
    ;; if you want to change prefix for lsp-mode keybindings.
    (setq lsp-keymap-prefix "s-l")
    
    (require 'lsp-mode)
    (add-hook 'XXX-mode-hook #'lsp)
    ```
    
    where `XXX` could be major mode like `python`, `java`, `c++`.
    Alternatively, if you want to minimize you configuration you may use
    `prog-mode-hook`. In case you do that, `lsp` will try to start for
    each programming mode and echo a message when there is no client
    registered for the current mode or if the corresponding server is
    not present. In addition, `lsp-mode` will automatically detect and
    configure [lsp-ui](https://github.com/emacs-lsp/lsp-ui) and
    [company-lsp](https://github.com/tigersoldier/company-lsp). To turn
    off that behavior you could set `lsp-auto-configure` to `nil`.
    
    To defer LSP server startup (and DidOpen notifications) until the
    buffer is visible you can use `lsp-deferred` instead of `lsp`:
    
    ``` elisp
    (add-hook 'XXX-mode-hook #'lsp-deferred)
    ```

2.  Spacemacs
    
    [lsp-mode](https://github.com/emacs-lsp/lsp-mode) is included in
    spacemacs develop branch. Add `lsp` to
    `dotspacemacs-configuration-layers` and configure the language that
    you want to use to be backed by `lsp` backend.

3.  use-package
    
    Replace `(require 'lsp-mode)` with the following if you use
    use-package.
    
    ``` elisp
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "s-l")
    
    (use-package lsp-mode
      :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
             (XXX-mode . lsp)
             ;; if you want which-key integration
             (lsp-mode . lsp-enable-which-key-integration))
      :commands lsp)
    
    ;; optionally
    (use-package lsp-ui :commands lsp-ui-mode)
    ;; if you are helm user
    (use-package helm-lsp :commands helm-lsp-workspace-symbol)
    ;; if you are ivy user
    (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
    
    ;; optionally if you want to use debugger
    (use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
    
    ;; optional if you want which-key integration
    (use-package which-key
      :config
      (which-key-mode))
    
    ```
    
    To defer LSP server startup (and DidOpen notifications) until the
    buffer is visible you can use `lsp-deferred` instead of `lsp`:
    
    ``` elisp
    (use-package lsp-mode
      :hook (XXX-mode . lsp-deferred)
      :commands (lsp lsp-deferred))
    ```

4.  Recommended settings for lsp-mode related packages
    
    1.  company
        
        ``` elisp
        (setq company-minimum-prefix-length 1
              company-idle-delay 0.0) ;; default is 0.2
        ```

5.  Formatting
    
    In general the formatter settings are language server specific(e. g.
    `JDT
                        LS` uses eclipse formatter file and `lsp-java-format-settings-url`
    to configure it while clangd uses `clangd-format`). The only
    settings that are controlled on `lsp-mode` level are indent size and
    whether the server should use tabs or spaces.
    
      - Use `c-basic-offset` for `cc-mode` derived moves(e. g. java,
        C++) to control the tab size.
      - Use `tab-width` for any other mode to do the same.
      - Use `indent-tabs-mode` for selecting tab/spaces.

6.  Docker
    
    Refer to [lsp-docker](https://github.com/emacs-lsp/lsp-docker/)
    README which provides a guide on how you can run `lsp-mode` in
    `docker` container.

### Performance

Use `M-x lsp-diagnose` to validate if your `lsp-mode` is properly
configured. In the section below, you could find description for each of
the checks:

When configured properly `lsp-mode`'s performance is on par with
mainstream LSP clients (e. g. `VScode`, `Theia`, etc). Here are steps to
achieve optimal results.

  - Use Emacs 27+ with native json support. (Note: this requires that
    you have [libjansson](http://www.digip.org/jansson/) installed, and
    that emacs was compiled with \`–with-json\` passed to
    \`./configure\`.) You can check your installation for native json
    support by running do <kbd>M-:</kbd> `(functionp 'json-serialize)` <kbd>RET</kbd>.
    Benchmarks show that Emacs 27 is `~15 times` faster than Emacs when
    using Elisp json parser implementation.
  - Adjust `gc-cons-threshold`. The default setting is too low for
    `lsp-mode`'s needs due to the fact that client/server communication
    generates a lot of memory/garbage. You have two options:
      - Set it to big number(100mb) like most of the popular starter
        kits like Spacemacs/Doom/Prelude, etc do:
        
        ``` elisp
        (setq gc-cons-threshold 100000000)
        ```
    
      - Follow the method recommended by Gnu Emacs Maintainer Eli
        Zaretskii: "My suggestion is to repeatedly multiply
        gc-cons-threshold by 2 until you stop seeing significant
        improvements in responsiveness, and in any case not to increase
        by a factor larger than 100 or somesuch. If even a 100-fold
        increase doesn't help, there's some deeper problem with the Lisp
        code which produces so much garbage, or maybe GC is not the
        reason for slowdown." Source:
        <https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/>
  - Increase the amount of data which Emacs reads from the process.
    Again the emacs default is too low 4k considering that the some of
    the language server responses are in 800k - 3M range.

<!-- end list -->

``` elisp
(setq read-process-output-max (* 1024 1024)) ;; 1mb
```

  - Optional: use `company-capf` . Although `company-lsp` also supports
    caching `lsp-mode`'s `company-capf` does that by default. To achieve
    that uninstall `company-lsp` or put these lines in your config:

<!-- end list -->

``` elisp
(setq lsp-prefer-capf t)
```

*Note:* - to verify which `company` backend implementation you are using
do `M-x company-diag` when performing auto-completion.

  - Optional: Disable `lsp-ui`. Normally, `lsp-ui` is very fast but in
    some systems (especially when using `Windows`) `lsp-ui` overlays and
    popups might slow down emacs.
  - Optional: fine-tune `lsp-idle-delay`. This variable determines how
    often lsp-mode will refresh the highlights, lenses, links, etc while
    you type.

<!-- end list -->

``` elisp
(setq lsp-idle-delay 0.500)
```

1.  Reporting performance problems
    
    If you have tried all of the non-optional steps from the list and
    `emacs` is still not very responsive please open a PR with the
    following information:
    
      - Collect **lsp-log** data after setting `lsp-print-performance`
        to `t`.
    
    <!-- end list -->
    
    ``` elisp
    (setq lsp-print-performance t)
    ```
    
      - Include emacs performance report. Use the following step to
        collect it:
          - `M-x profiler-start` and select `CPU`
          - Reproduce the slow behavior.
          - `M-x profiler-stop`
          - In the profiler report expand all nodes by doing `C-u TAB`.
        *Note:* - `lsp-mode` is just a frontend and the performance
        depends on server as well. Some servers (e. g. Palantir's Python
        Language Server) might be slow when performing auto-completion.

### How does it work?

`lsp-mode` has predefined list of server configurations (loaded in
`lsp-clients.el`) containing a mapping from `major-mode` to the server
configuration or by using activation function. In addition to the
default server configuration located in `lsp-clients.el` there are few
languages servers which require separate package(check [Supported
languages](https://emacs-lsp.github.io/lsp-mode/page/languages/)). 

When you open a file from a
particular project `lsp-mode` and call `lsp` command `lsp-mode` will
look for server registrations able to handle current file. If there is
such client `lsp-mode` will look for the project root. If you open a
file from the project for the first time you will be prompted to define
the current project root. Once the project root is selected it is saved
in `lsp-session` file and it will be loaded the next time you start
Emacs so you no longer will be asked for a project root when you open a
file from that project. 
Later if you want to change the project root you
may use `lsp-workspace-folder-remove` to remove the project and call
`lsp-workspace-folder-add` to add the root. If you want to force
starting a particular language server in a file you may use <kbd>C-u</kbd> <kbd>M-x</kbd>
`lsp` which will prompt you to select language server to start.
