# FAQ

- How do I troubleshoot `"Server FOO-LS:pid exited with status signal. Do you want to restart it? (y or n)"`?
    - This message indicates that the language server has crashed for some reason. You may check the server stderr which is `*FOO-LS::stderr*`. If you get this try to run the exact command that `lsp-mode` is running in the terminal. You may find it in `*lsp-log*` buffer.
- How to configure a server with local variables?
    - Add `lsp` server call to `hack-local-variables-hook` which runs right after the local variables are loaded.
      ```elisp
      (add-hook 'hack-local-variables-hook
                (lambda () (when (derived-mode-p 'XXX-mode) (lsp))))
      ```
- I have multiple language servers registered for language FOO. Which one will be used when opening a project?
    - The one with highest priority wins. `lsp-clients.el` predefined servers have priority -1, lower than external packages (priority 0 if unspecified). If a server is registered with `:add-on?` flag set to `t` it will be started in parallel to the other servers that are registered for the current mode.
- I have multiple language servers for language `FOO` and I want to select the server per project, what can I do?
    - You may create `dir-local` for each of the projects and specify list of `lsp-enabled-clients`. This will narrow the list of the clients that are going to be tested for the project.
- The completion does not work fine and inserts arguments and placeholders, what I am doing wrong?
    - make sure you have installed `yasnippet` and you have `yasnippet` minor mode enabled.
- I am getting "Package ‘spinner-1.7.3’ is unavailable" when trying to install `lsp-mode`.
    - This is caused by GPG keys used by the ELPA package manager not being up to date. You may fix by installing: [gnu-elpa-keyring-update](https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html)
- The flycheck does not work in `typescript`, `html` and `javascript` blocks in `vue-mode`. How to fix that?
    - This is caused by the fact that `vue-mode` uses multiple major modes in single file and the `lsp-ui` checker may not associated with the major mode at point. You could fix that by adding the following lines to your config.
      ```elisp
      (with-eval-after-load 'lsp-mode
        (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode)))
      ```
- I have disabled snippets and `Rust Analyzer` server inserts redundant `$0` when performing completion?
    - `Rust Analyzer` does not support disabling snippets - see <https://github.com/rust-analyzer/rust-analyzer/issues/2518>
- How do I force `lsp-mode` to forget the workspace folders for multi root
  servers so the workspace folders are added on demand?
    - Use the following snippet:
  ``` elisp
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  ```
