---
root_file: docs/manual-language-docs/lsp-json-ls.md
---
# Completions

json-ls (vscode-json-language-server) provides completions for [well known files](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-json.el#L65-L85) and files which manually specify their JSON Schema using the `$schema` property.

In order for json-ls to provide completions, you need to enable snippet support. To do that, make sure you have installed `yasnippet` and that you have enabled the `yasnippet` minor mode.
