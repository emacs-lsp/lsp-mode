---
disqus: emacs-lsp
---

# Configuring `Emacs` as `Python` IDE with `pyls` as a language server

This guide will show you how to configure `lsp-mode` for Python development.
There are several language servers available for python:
  * [pyls](https://github.com/palantir/python-language-server) by Palantir Technologies (this tutorial focuses on this ls)
  * [python-ms](https://github.com/Microsoft/python-language-server) by Microsoft
  * [jedi](https://github.com/pappasam/jedi-language-server) by pappasam
  * [pyright](https://github.com/microsoft/pyright) by Microsoft

## Features of `pyls` as a language server

* Code completion
* Linting and syntax checking
* Signature help
* Code navigation (references/definitions/etc..)
* Info on hover
* Document Formatting

# Installing the language server

There are two ways to install `pyls`:
1. Make a global install
2. Installation per-project

Per project installation is a tad bit faster but almost unnoticeable, so if you find global installation
a bit too slow and you have exhausted your [optimization](#optimizations) options, try a per-project approach.

## Global install

1. Install globally with `pip install --user python-language-server[all]`
2. If your project uses virtualenv in any form (`virtualenv`, `virtualenvwrapper`, `poetry`, etc...) you need to specify a few project specific configurations
    * Go to the root of your project
    * Create a `.dir-locals.el`  file. Also, preferably, add it to your `.gitignore`
    * place this code there:
    ```elisp
    ((python-mode .
                  ((lsp-pyls-plugins-jedi-environment . "*ABSOLUTE PATH TO YOUR VIRTUALENV*"))))
    ```
    if you use virtualenvwrapper, you can switch it a bit
    ```elisp
    ((python-mode .
                  ((lsp-pyls-plugins-jedi-environment . (concat (file-name-as-directory (getenv "WORKON_HOME")) "*venv name*")))))
    ```

## Installation per-project

Per-project installation allows you to have different versions of `pyls` for your different projects.
It requires your project to have some sort of virtualenv and a few more steps.

1. Install `pyls` inside your virtualenv
```bash
source $PATH_TO_YOUR_VENV/bin/activate
pip install python-language-server[all]
```
2. You need to make emacs know about virtualenv, and you need to do it **before** a call to `(lsp)`
    * If you use Spacemacs \
    You are in luck. Just create a file `.venv` with a path to your virtualenv in the root of your project.
    * If you use Doom Emacs \
    TBA (try the vanilla emacs approach, as well as check this [issue](https://github.com/hlissner/doom-emacs/issues/1666) in doom repo)
    * If you use vanilla Emacs \
    Check how spacemacs [does it](https://github.com/syl20bnr/spacemacs/blob/a9fc0d5fcc46aaef95aef99de3097fd79f0e3d26/layers/%2Blang/python/funcs.el#L225) with the [pyvenv](https://github.com/jorgenschaefer/pyvenv) emacs package \
    It should look something like this (ignoring other options except `:hook`):
    ```elisp
    (use-package lsp-mode
        :hook
        (python-mode . (lambda ()
                         (progn
                           (pyvenv-activate "*PATH TO YOUR VIRTUALENV*")
                           (lsp)))))
    ```
    of if you use `virtualenvwrapper`:
    ```elisp
    (use-package lsp-mode
        :hook
        (python-mode . (lambda ()
                         (progn
                           (pyvenv-workon "*venv name*")
                           (lsp)))))
    ```


# Optimizations
