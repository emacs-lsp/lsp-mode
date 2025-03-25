---
author: mattiasdrp
template: comment.html
root_file: docs/manual-language-docs/lsp-ocaml.md
---

## ocaml-lsp-server

### Commands

#### `lsp-ocaml-type-enclosing`

Gets the type of ident under the cursor. It will highlight the ident and display its type.

When this function is called it will create a transient keymap `lsp-ocaml-type-enclosing-map` that allows to do the following things:
- Increase/decrease the number of aliases expansions. As an example, suppose we want to type `h` in the following expression:
  ```ocaml
  type t = A | B
  let h : t = A
  ```
  - The lowest verbosity will give `type t`
  - The next verbosity will give `type t = A | B`
- Go up/down the enclosing type (bound to `C-<up>/<down>` by default). As an example:
  ```ocaml
  module A = struct
    let h : t = A
    let f () = ()

    (** Test doc *)
    let g (f: 'a -> 'b) a = f a
  end
  ```
  - Typing on the last `a` will show `'a`
  - Going up will highlight `f a` of type `'b`
  - Going up will highlight `(f: 'a -> 'b) a = f a` of type `('a -> 'b) -> 'a -> 'b`
  - Going up will highlight the whole module and display its entire type
- Copy the current type (bound to `C-w` by default)

#### `lsp-ocaml-find-alternate-file`

Find the interface corresponding to an implementation or the implementation corresponding to an interface.
