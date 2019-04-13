(require 'f)

(when (require 'undercover nil t)
  (undercover "*.el"))

(defvar lsp-mode-support-path
  (f-dirname load-file-name))

(defvar lsp-mode-features-path
  (f-parent lsp-mode-support-path))

(defvar lsp-mode-root-path
  (f-parent lsp-mode-features-path))

(add-to-list 'load-path lsp-mode-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'lsp-mode)
  (require 'espuds)
  (require 'ert))

(Setup
 (setq lsp-prefer-flymake :none)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
