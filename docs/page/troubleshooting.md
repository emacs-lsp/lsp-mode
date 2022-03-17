---
root_file: docs/page/troubleshooting.md
---
Troubleshooting
===============

#### lsp-log

Check `*lsp-log*` buffer and verify that `lsp-mode` is able to find the server. If `lsp-mode` is unable to find the binary but it is on the path it is probably caused by the fact that emacs is running with different path. You may check the current path by executing <kbd>M-:</kbd> `(getenv "PATH")`. If this is the case, you have several options to fix the issue.

Make sure that emacs is using the proper path by running emacs from terminal.
Modify the path using `setpath` function.

#### Server path

Look for `lsp-mode` variable to customize server path. Usually, you may find the variable by doing: <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `lsp-LANGUAGE-SERVER-ID`.

#### Log client<->server json

Set `lsp-log-io` to `t` to inspect communication between client and the server. Use `lsp-workspace-show-log` to switch to the corresponding log buffer.

#### Check project roots

`lsp-describe-session` will show the current projects roots + the started servers and allows inspecting the server capabilities:

![Describe session](../examples/describe.png)

#### Updating package.el packages

If you manage your Emacs packages with the built-in `package.el`, we recommend the following procedure to update your packages:

1. Delete your LSP-related packages
2. Restart Emacs
3. Install the new versions of the packages.

#### Check if any package or wrong config could be causing the issue

Try to repro the issue with our [lsp-start-plain.el](https://github.com/emacs-lsp/lsp-mode/blob/master/scripts/lsp-start-plain.el) file, which has just the necessary config/packages for lsp-mode work using `M-x lsp-start-plain`.
