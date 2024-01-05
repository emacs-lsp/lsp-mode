---
root_file: docs/page/faq.md
---
# FAQ

### :grey_question: How do I troubleshoot `"Server FOO-LS:pid exited with status signal. Do you want to restart it? (y or n)"`?

This message indicates that the language server has crashed for some reason. You may check the server stderr which is `*FOO-LS::stderr*`. If you get this try to run the exact command that `lsp-mode` is running in the terminal. You may find it in `*lsp-log*` buffer.

---
### :grey_question: How to configure a server with local variables?

Add `lsp` server call to `hack-local-variables-hook` which runs right after the local variables are loaded.
      ```elisp
      (add-hook 'hack-local-variables-hook
                (lambda () (when (derived-mode-p 'XXX-mode) (lsp))))
      ```

---
### :grey_question: I have multiple language servers registered for language FOO. Which one will be used when opening a project?

The highest number is highest priority. Note this is the opposite of [Unix priority (niceness)](https://en.wikipedia.org/wiki/Nice_(Unix)). Servers defined in `lsp-mode` tend to have lower priority than the external packages (priority 0 if unspecified). If a server is registered with `:add-on?` flag set to `t` it will be started in parallel to the other servers that are registered for the current mode. If the server that you want to use is not with the highest priority you may use `lsp-disabled-clients` to disable the server with higher `priority` or use `lsp-enabled-clients` to enable only the servers you want to use. In order to find the server ids you may check `*lsp-log*` buffer.

---
### :grey_question: I have multiple language servers for language `FOO` and I want to select the server per project, what can I do?

You may create `dir-local` for each of the projects and specify list of `lsp-enabled-clients`. This will narrow the list of the clients that are going to be tested for the project.

---
### :grey_question: The completion does not work fine and inserts arguments and placeholders, what I am doing wrong?

make sure you have installed `yasnippet` and you have `yasnippet` minor mode enabled.

---
### :grey_question: I am getting "Package ‘spinner-1.7.3’ is unavailable" when trying to install `lsp-mode`.

This is caused by GPG keys used by the ELPA package manager not being up to date. You may fix by installing: [gnu-elpa-keyring-update](https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html)

---
### :grey_question: The flycheck does not work in `typescript`, `html` and `javascript` blocks in `vue-mode`. How to fix that?

This is caused by the fact that `vue-mode` uses multiple major modes in single file and the `lsp-ui` checker may not associated with the major mode at point. You could fix that by adding the following lines to your config.
      ```elisp
      (with-eval-after-load 'lsp-mode
        (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode)))
      ```

---
### :grey_question: I have disabled snippets and `Rust Analyzer` server inserts redundant `$0` when performing completion?

`Rust Analyzer` does not support disabling snippets - see <https://github.com/rust-analyzer/rust-analyzer/issues/2518>

---
### :grey_question: How do I force `lsp-mode` to forget the workspace folders for multi root servers so the workspace folders are added on demand?

Use the following snippet:
  ``` elisp
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  ```


---
### :grey_question: When using `clojure` with `clojure-lsp` `lsp-mode` is interfering with typing (e. g. deleting whitespace while typing), how to fix that?

The issue is caused by `clojure-lsp` server being more aggressive with formatting(deleting whitespaces) and `aggressive-indent-mode` being on and calling it while typing. The solution is either to disable `aggressive-indent-mode` or to disable `lsp-mode` formatting via `lsp-enable-indentation`.


---
### :grey_question: How do I disable automatic installation?

    ``` elisp
    (setq lsp-enable-suggest-server-download nil)
    ```
