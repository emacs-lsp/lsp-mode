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


### Troubleshooting

#### Remote LSP fails to starts/initializes - spinner going for a long time

If you have been waiting for LSP to connect to the remote server for longer than 1-2 minutes (and your connection isn't the problem), it might be an encoding error during the first `initialize` request sent by the client. This can often be observed by an active spinner that never settles to a green tick or whatever symbol you have for successful connection. 

This seems to be caused by incorrect JSON encoding leaving out the last curly brace and thus causing the LSP server to fail to parse it as valid JSON. 

Examples of the same error caused by the client to different servers. 

##### pyls 

```bash
020-06-26 18:09:27,104 UTC - ERROR - pyls_jsonrpc.streams - Failed to parse JSON message b'\n\n{"jsonrpc":"2.0","method":"initialize","params":{"processId":null,"rootPath":"/home/wts/test","clientInfo":{"name":"emacs","version":"GNU Emacs 27.0.91 (build 1, x86_64-apple-darwin19.5.0, NS appkit-1894.50 Version 10.15.5 (Build 19F101))\\n of 2020-06-20"},"rootUri":"file:///home/wts/test","capabilities":{"workspace":{"workspaceEdit":{"documentChanges":true,"resourceOperations":["create","rename","delete"]},"applyEdit":true,"symbol":{"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},"executeCommand":{"dynamicRegistration":false},"didChangeWatchedFiles":{"dynamicRegistration":true},"workspaceFolders":true,"configuration":true},"textDocument":{"declaration":{"linkSupport":true},"definition":{"linkSupport":true},"implementation":{"linkSupport":true},"typeDefinition":{"linkSupport":true},"synchronization":{"willSave":true,"didSave":true,"willSaveWaitUntil":true},"documentSymbol":{"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]},"hierarchicalDocumentSymbolSupport":true},"formatting":{"dynamicRegistration":true},"rangeFormatting":{"dynamicRegistration":true},"rename":{"dynamicRegistration":true,"prepareSupport":true},"codeAction":{"dynamicRegistration":true,"isPreferredSupport":true,"codeActionLiteralSupport":{"codeActionKind":{"valueSet":["","quickfix","refactor","refactor.extract","refactor.inline","refactor.rewrite","source","source.organizeImports"]}}},"completion":{"completionItem":{"snippetSupport":true,"documentationFormat":["markdown"]},"contextSupport":true},"signatureHelp":{"signatureInformation":{"parameterInformation":{"labelOffsetSupport":true}}},"documentLink":{"dynamicRegistration":true,"tooltipSupport":true},"hover":{"contentFormat":["markdown","plaintext"]},"foldingRange":{"dynamicRegistration":true},"callHierarchy":{"dynamicRegistration":false},"publishDiagnostics":{"relatedInformation":true,"tagSupport":{"valueSet":[1,2]},"versionSupport":true}},"window":{"workDoneProgress":true}},"initializationOptions":null,"workDoneToken":"1"},"id":1'
Traceback (most recent call last):
  File "/home/wts/.anaconda3/lib/python3.7/site-packages/pyls_jsonrpc/streams.py", line 40, in listen
    message_consumer(json.loads(request_str.decode('utf-8')))
ValueError: Unexpected character in found when decoding object value
```

##### clangd

```bash
I[08:40:24.923] clangd version 11.0.0 (https://github.com/llvm/llvm-project.git 414f32a9e862b11f51063b75729278f8d81b12e9)
I[08:40:24.923] PID: 2531726
I[08:40:24.923] Working directory: /scratch/work
I[08:40:24.923] argv[0]: /usr/local/bin/clangd
I[08:40:24.923] Starting LSP over stdin/stdout
E[08:40:24.924] JSON parse error: [3:2081, byte=2083]: Expected , or } after object property
```

##### Solution

Upgrade to TRAMP >2.5.0-pre. Check your version by running `M-x tramp-version RET`. Michael Albinus has fixed this in upstream tramp (version 2.5.0 onwards) and lsp-mode now uses a modern start-process method (requires emacs-27.1) with no encoding.

#### Diagnosing other problems

If the LSP server is running on remote, you can either look for a buffer called *\$LSP_SERVER_NAME-stderr* or ssh to the remote machine where the LSP server is running to examine its state. 

```bash
# First find the process ID of your LSP server
$ pgrep pyls # the process name of your LSP server
21229
# now use the process ID to read the stderr of the process 
# works on a unix system where /proc/ 
# this will start streaming and following the contents of your LSP server's 
# stderr. Keep this terminal running. 
# go back to your editor and repeat the actions that you suspect cause the crash or misbehaviour
$ tail -f /proc/21229/fd/2 
2021-01-16 22:35:10,155 UTC - WARNING - pyls_jsonrpc.endpoint - Received cancel notification for unknown message id 4016
2021-01-16 22:35:10,157 UTC - WARNING - pyls_jsonrpc.endpoint - Received cancel notification for unknown message id 4018
```

## Docker

Refer to [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) README which provides a guide on how you can run `lsp-mode` in `docker` container.
