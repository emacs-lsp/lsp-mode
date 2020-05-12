Additional settings
===================

## Recommended settings for related packages
    
### Company (autocomplete)

```elisp
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
```

## Formatting
 
In general the formatter settings are language server specific(e. g. `JDT LS` uses eclipse formatter file and `lsp-java-format-settings-url` to configure it while clangd uses `clangd-format`). The only settings that are controlled on `lsp-mode` level are indent size and whether the server should use tabs or spaces.

- Use `c-basic-offset` for `cc-mode` derived moves(e. g. java, C++) to control the tab size.
- Use `tab-width` for any other mode to do the same.
- Use `indent-tabs-mode` for selecting tab/spaces.

## Docker

Refer to [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) README which provides a guide on how you can run `lsp-mode` in `docker` container.
