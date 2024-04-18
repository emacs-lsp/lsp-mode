---
root_file: docs/page/remote.md
---
# Remote

## TRAMP

`lsp-mode` has support for tramp buffers with the following requirements:

- The language server has to be present on the remote server.
- Having multi folder language server (like [Eclipse JDT LS](https://github.com/eclipse/eclipse.jdt.ls)) cannot have local and remote workspace folders.

### How does it work?

`lsp-mode` detects whether a particular file is located on remote machine and looks for a client which matches current file and it is marked as `:remote?` t. Then `lsp-mode` starts the client through tramp. By default `lsp-mode` will copy the local client and mark it as `remote? t`. In most of the cases it is good enough but certain cases this may not work (e. g. if the server configuration contains references to local paths). In this case the user is supposed to create `.dir-local` configuration to override the references to local paths or open an issue on `lsp-mode` side to make the setting remote agnostic. To turn off automatic remote clients registration you can set `lsp-auto-register-remote-clients` to `nil`.

## Docker

Refer to [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) README which provides a guide on how you can run `lsp-mode` in `docker` container.
