---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

Before logging the bug, please make sure that:

1. You are using the latest version of `lsp-mode` related packages.
2. You may also try reproduce the issue using clean environment using the following command:
3. Check FAQ and Troubleshooting section (https://github.com/emacs-lsp/lsp-mode#faq and https://github.com/emacs-lsp/lsp-mode#troubleshooting)
``` bash
emacs -q -l lsp-start-plain.el
```
where `lsp-start-plain.el` could be downloaded from `https://github.com/emacs-lsp/lsp-mode/blob/master/scripts/lsp-start-plain.el`.
Alternatively, in will be great if you can reproduce the issue using [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) which provides the minimal configurations for `lsp-mode` and ships with most of the language servers.
3. Consider providing a minimal project to reproduce the issue with. Note that `lsp-mode` supports 30+ languages and the maintainers of the project are not familiar with all of the languages and a sample project + full steps to reproduce will help a lot.


**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior(sample project + file which can be used to reproduce the issue with.)

**Expected behavior**
A clear and concise description of what you expected to happen.

**Which Language Server did you use**
Mention which server/language did you use (e. g. lsp-python, lsp-ccls, lsp-java, etc)

**OS**
Which os do you use?

**Error callstack**
If there is an error please reproduce the issue with `toggle-debug-on-error` and after setting `lsp-print-io` to t, and then include the callstack and attach the content of `*lsp-log*` buffer and the content of the server log(use `lsp-workspace-show-log`)

If `emacs` hangs plase do `M-x` `toggle-debug-on-quit` and then do `C-g` when emacs hangs and include the callstack as part of the issue report.

``` emacs-lisp
(setq lsp-print-io t)
```
