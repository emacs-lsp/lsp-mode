emacs-lsp
=========

A Emacs Lisp library for implementing clients for servers using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck, etc). Clients
will only need to implement functions to send and receive messages, since
the protocol does not mention a transport.
