# Remote

## TRAMP

LSP mode has support for tramp buffers with the following requirements:

- The language server has to be present on the remote server.
- Having multi folder language server (like [Eclipse JDT LS](https://github.com/eclipse/eclipse.jdt.ls)) cannot have local and remote workspace folders.

### How does it work?

`lsp-mode` detects whether a particular file is located on remote machine and looks for a client which matches current file and it is marked as `:remote?` t. Then `lsp-mode` starts the client through tramp.

### Sample configuration

Here it is example how you can configure python language server to work when using `TRAMP`. Note that if you are trying to convert existing language server configuration you should copy all of it's properties(e. g. `:request-handlers`, `activation-fn`, etc). Also, when you are doing that you should make sure that none of the custom language server settings are not pointing to local path because those settings will be sent to the remote server.

```elisp
(lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "<binary name (e. g. pyls, rls)>")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote))
```

_Note:_ when you do not have root privileges on the remote machine to put the language server on the path you may alter the remote path by changing `tramp-remote-path`.

### Dealing with stderr

With TRAMP, Emacs does not have an easy way to distinguish stdout and stderr, so when the underlying LSP process writes to stderr, it breaks the `lsp-mode` parser. As a workaround, `lsp-mode` is redirecting stderr to `/tmp/<process-name>-<id>~stderr`.


## Docker

Refer to [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) README which provides a guide on how you can run `lsp-mode` in `docker` container.
