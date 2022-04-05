# Docker integration

### Reason
You can use a docker-wrapped both language server and debug server and connect to it using LSP/DAP.
Imagine that you have a short-time project (or a sideproject) and don't want to bring any software packages to your local machine.
Now it is simple - wrap the language server and debugger you need in `docker` and use `lsp-docker` for LSP and `dap-mode` for DAP.

### Features
Most of the `lsp-mode` and `dap-mode` features, but with the ability to use containerized environments.
Note: some of the features may yet to be tested, so any help finding and fixing them is highly appreciated!

### Components
