emacs-lsp
=========

A Emacs Lisp library for implementing clients for servers using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck, etc). Clients
will only need to implement functions to send and receive messages, since
the protocol does not mention a transport.

## Installation

Clone this repository to a suitable path, and add `(add-to-list 'load-path "<path to emacs-lsp>")` to your .emacs/init.el.

## Clients

The library currently defines clients for [Rust Language Server](https://github.com/jonathandturner/rls) and [Sourcegraph's Go Language Server](https://github.com/sourcegraph/go-langserver). You can define clients for language servers which communicate on stdio, check `lsp-mode.el` for examples.
