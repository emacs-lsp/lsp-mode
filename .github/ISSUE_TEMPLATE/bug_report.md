---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:

**Expected behavior**
A clear and concise description of what you expected to happen.

**Which Language Server did you use**
Mention which server/language did you use (e. g. lsp-python, lsp-ccls, lsp-java, etc)

**OS**
Which os do you use?

**Error callstack**
If there is an error please reproduce the issue with `toggle-debug-on-error` and after setting `lsp-print-io` to t, and then include the callstack and attach the content of \*Messages\* buffer.

``` emacs-lisp
(setq lsp-print-io t)
```
