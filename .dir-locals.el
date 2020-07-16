((nil
  (require-final-newline . t))
 (emacs-lisp-mode
  (eval . (let ((dirloc-lsp-defun-regexp
                 (concat
                  (concat "^\\s-*("
                          "lsp-defun"
                          "\\s-+\\(")
                  (or (bound-and-true-p lisp-mode-symbol-regexp)
                      "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                  "\\)")))
            (add-to-list 'imenu-generic-expression
                         (list "Functions" dirloc-lsp-defun-regexp 1))))
  (flycheck-disabled-checkers . '(emacs-lisp-checkdoc))
  (indent-tabs-mode . nil)))
