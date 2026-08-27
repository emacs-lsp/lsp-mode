;; -*- mode: eask; lexical-binding: t -*-

(package "lsp-mode"
         "10.0.1"
         "LSP mode")

(website-url "https://github.com/emacs-lsp/lsp-mode")
(keywords "languages")

(package-file "lsp-mode.el")

(load-paths "./" "./clients/" "./test/")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'jcs-elpa)
(source 'melpa)

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

(depends-on "emacs" "29.1")
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
 (depends-on "org")
 (depends-on "elenv"))

;; Emacs 32 changed the default of `current-time-list' to nil, so `current-time'
;; returns a (TICKS . HZ) cons cell.  Eask's `eask-current-time' still expects
;; the older list form and fails with "Wrong type argument: listp, 1000000000",
;; which breaks `eask package'.  See emacs-eask/cli#431; remove this once a
;; corrected Eask is released.
(setq current-time-list t)

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)
             (setq byte-compile-docstring-max-column 1000)))
