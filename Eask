;; -*- mode: emacs-lisp -*-

;; FIXME: prefer Emacs 28's `lisp-data-mode' over `emacs-lisp-mode'

(package "lsp-mode"
         "8.0.1"
         "LSP mode")

(package-file "lsp-mode.el")

(load-paths "./test/")

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

(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "f")
(depends-on "ht")
(depends-on "spinner")
(depends-on "markdown-mode")
(depends-on "lv")

(development
 (depends-on "flycheck")
 (depends-on "ert-runner")
 (depends-on "espuds")
 (depends-on "ecukes")
 (depends-on "undercover")
 (depends-on "deferred")
 (depends-on "el-mock"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432