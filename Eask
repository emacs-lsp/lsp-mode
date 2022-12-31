(package "lsp-mode"
         "8.0.1"
         "LSP mode")

(website-url "https://github.com/emacs-lsp/lsp-mode")
(keywords "languages")

(package-file "lsp-mode.el")

(load-paths "./" "./clients/" "./test/")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(files
 "lsp-protocol.el"
 "lsp-mode.el"
 "lsp.el"
 "lsp-completion.el"
 "lsp-diagnostics.el"
 "lsp-headerline.el"
 "lsp-iedit.el"
 "lsp-ido.el"
 "lsp-lens.el"
 "lsp-modeline.el"
 "lsp-semantic-tokens.el"
 "clients/*.el")

(depends-on "emacs" "26.3")
(depends-on "dash")
(depends-on "f")
(depends-on "ht")
(depends-on "spinner")
(depends-on "markdown-mode")
(depends-on "lv")
(depends-on "eldoc")

(development
 (depends-on "flycheck")
 (depends-on "ert-runner")
 (depends-on "espuds")
 (depends-on "ecukes")
 (depends-on "undercover")
 (depends-on "deferred")
 (depends-on "el-mock")
 (depends-on "org"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
