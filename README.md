emacs-lsp
=========

[![Build Status](https://travis-ci.org/vibhavp/emacs-lsp.svg?branch=master)](https://travis-ci.org/vibhavp/emacs-lsp)

A Emacs Lisp library for implementing clients for servers using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck, etc).
## Installation

Clone this repository to a suitable path, and add
```emacs-lisp
(add-to-list 'load-path "<path to emacs-lsp>")
(require 'lsp-mode)
(global-lsp-mode t)
```
to your .emacs.
## Clients

The library currently defines clients for [Rust Language Server](https://github.com/jonathandturner/rls)
and [Sourcegraph's Go Language Server](https://github.com/sourcegraph/go-langserver). 
For now, the only supported transport is stdio (see `lsp-mode.el` on how to add your 
own clients), support for TCP and sockets is planned.
