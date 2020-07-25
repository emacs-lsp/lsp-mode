((nil
  (require-final-newline . t))
 (emacs-lisp-mode
  (eval . (progn
            (let ((dirloc-lsp-defun-regexp
                   (concat
                    (concat "^\\s-*("
                            "lsp-defun"
                            "\\s-+\\(")
                    (or (bound-and-true-p lisp-mode-symbol-regexp)
                        "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                    "\\)")))
              (add-to-list 'imenu-generic-expression
                           (list "Functions" dirloc-lsp-defun-regexp 1)))

            (defvar lsp--override-calculate-lisp-indent?
              nil
              "Whether to override the default
              `calculate-lisp-indent'.")

            (setq-local lsp--override-calculate-lisp-indent? t)

            ;; FIXME: This doesn't appear to correct the behavior of
            ;; `evil-indent' or `indent-region'. But it does seem to
            ;; work with `newline-and-indent'.

            ;; FIXME: This also doesn't work particularly well with
            ;; comma-evaluated forms inside backticks if any arguments
            ;; are on the same line as the function. But there is a workaround.

            ;; For example, the incorrectly-indented

            ;; `(:range ,(lsp--range (lsp--point-to-position start)
            ;;            (plist-get lsp--before-change-vals :end-pos)))

            ;; can be changed to

            ;; `(:range ,(lsp--range
            ;;            (lsp--point-to-position start)
            ;;            (plist-get lsp--before-change-vals :end-pos)))

            ;; by executing `newline-and-indent'.

            ;; https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
            (defun wrap~calculate-lisp-indent (fn &optional parse-start)
              "Add better indentation for quoted and backquoted lists."
              (if (not lsp--override-calculate-lisp-indent?)
                  (funcall fn parse-start)
                ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar`
                ;; with it's value omitted, marking it special and only defining it locally. So
                ;; if you don't have this, you'll get a void variable error.
                (defvar calculate-lisp-indent-last-sexp)
                (save-excursion
                  (beginning-of-line)
                  (let ((indent-point (point))
                        state
                        ;; setting this to a number inhibits calling hook
                        (desired-indent nil)
                        (retry t)
                        calculate-lisp-indent-last-sexp containing-sexp)
                    (cond ((or (markerp parse-start) (integerp parse-start))
                           (goto-char parse-start))
                          ((null parse-start) (beginning-of-defun))
                          (t (setq state parse-start)))
                    (unless state
                      ;; Find outermost containing sexp
                      (while (< (point) indent-point)
                        (setq state (parse-partial-sexp (point) indent-point 0))))
                    ;; Find innermost containing sexp
                    (while (and retry
                                state
                                (> (elt state 0) 0))
                      (setq retry nil)
                      (setq calculate-lisp-indent-last-sexp (elt state 2))
                      (setq containing-sexp (elt state 1))
                      ;; Position following last unclosed open.
                      (goto-char (1+ containing-sexp))
                      ;; Is there a complete sexp since then?
                      (if (and calculate-lisp-indent-last-sexp
                               (> calculate-lisp-indent-last-sexp (point)))
                          ;; Yes, but is there a containing sexp after that?
                          (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                                          indent-point 0)))
                            (if (setq retry (car (cdr peek))) (setq state peek)))))
                    (if retry
                        nil
                      ;; Innermost containing sexp found
                      (goto-char (1+ containing-sexp))
                      (if (not calculate-lisp-indent-last-sexp)
                          ;; indent-point immediately follows open paren.
                          ;; Don't call hook.
                          (setq desired-indent (current-column))
                        ;; Find the start of first element of containing sexp.
                        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                        (cond ((looking-at "\\s(")
                               ;; First element of containing sexp is a list.
                               ;; Indent under that list.
                               )
                              ((> (save-excursion (forward-line 1) (point))
                                  calculate-lisp-indent-last-sexp)
                               ;; This is the first line to start within the containing sexp.
                               ;; It's almost certainly a function call.
                               (if (or
                                    ;; Containing sexp has nothing before this line
                                    ;; except the first element. Indent under that element.
                                    (= (point) calculate-lisp-indent-last-sexp)

                                    ;; First sexp after `containing-sexp' is a keyword. This
                                    ;; condition is more debatable. It's so that I can have
                                    ;; unquoted plists in macros. It assumes that you won't
                                    ;; make a function whose name is a keyword.
                                    ;; (when-let (char-after (char-after (1+ containing-sexp)))
                                    ;;   (char-equal char-after ?:))

                                    ;; Check for quotes or backquotes around.
                                    (let* ((positions (elt state 9))
                                           (last (car (last positions)))
                                           (rest (reverse (butlast positions)))
                                           (any-quoted-p nil)
                                           (point nil))
                                      (or
                                       (when-let (char (char-before last))
                                         (or (char-equal char ?')
                                             (char-equal char ?`)))
                                       (progn
                                         (while (and rest (not any-quoted-p))
                                           (setq point (pop rest))
                                           (setq any-quoted-p
                                                 (or
                                                  (when-let (char (char-before point))
                                                    (or (char-equal char ?')
                                                        (char-equal char ?`)))
                                                  (save-excursion
                                                    (goto-char (1+ point))
                                                    (looking-at-p
                                                     "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                                         any-quoted-p))))
                                   ;; Containing sexp has nothing before this line
                                   ;; except the first element.  Indent under that element.
                                   nil
                                 ;; Skip the first element, find start of second (the first
                                 ;; argument of the function call) and indent under.
                                 (progn (forward-sexp 1)
                                        (parse-partial-sexp (point)
                                                            calculate-lisp-indent-last-sexp
                                                            0 t)))
                               (backward-prefix-chars))
                              (t
                               ;; Indent beneath first sexp on same line as
                               ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                               ;; almost certainly a function call.
                               (goto-char calculate-lisp-indent-last-sexp)
                               (beginning-of-line)
                               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                                   0 t)
                               (backward-prefix-chars)))))
                    ;; Point is at the point to indent under unless we are inside a string.
                    ;; Call indentation hook except when overridden by lisp-indent-offset
                    ;; or if the desired indentation has already been computed.
                    (let ((normal-indent (current-column)))
                      (cond ((elt state 3)
                             ;; Inside a string, don't change indentation.
                             nil)
                            ((and (integerp lisp-indent-offset) containing-sexp)
                             ;; Indent by constant offset
                             (goto-char containing-sexp)
                             (+ (current-column) lisp-indent-offset))
                            ;; in this case calculate-lisp-indent-last-sexp is not nil
                            (calculate-lisp-indent-last-sexp
                             (or
                              ;; try to align the parameters of a known function
                              (and lisp-indent-function
                                   (not retry)
                                   (funcall lisp-indent-function indent-point state))
                              ;; If the function has no special alignment
                              ;; or it does not apply to this argument,
                              ;; try to align a constant-symbol under the last
                              ;; preceding constant symbol, if there is such one of
                              ;; the last 2 preceding symbols, in the previous
                              ;; uncommented line.
                              (and (save-excursion
                                     (goto-char indent-point)
                                     (skip-chars-forward " \t")
                                     (looking-at ":"))
                                   ;; The last sexp may not be at the indentation
                                   ;; where it begins, so find that one, instead.
                                   (save-excursion
                                     (goto-char calculate-lisp-indent-last-sexp)
                                     ;; Handle prefix characters and whitespace
                                     ;; following an open paren.  (Bug#1012)
                                     (backward-prefix-chars)
                                     (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                                   (line-beginning-position))
                                                     (and containing-sexp
                                                          (>= (1+ containing-sexp) (point)))))
                                       (forward-sexp -1)
                                       (backward-prefix-chars))
                                     (setq calculate-lisp-indent-last-sexp (point)))
                                   (> calculate-lisp-indent-last-sexp
                                      (save-excursion
                                        (goto-char (1+ containing-sexp))
                                        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                                        (point)))
                                   (let ((parse-sexp-ignore-comments t)
                                         indent)
                                     (goto-char calculate-lisp-indent-last-sexp)
                                     (or (and (looking-at ":")
                                              (setq indent (current-column)))
                                         (and (< (line-beginning-position)
                                                 (prog2 (backward-sexp) (point)))
                                              (looking-at ":")
                                              (setq indent (current-column))))
                                     indent))
                              ;; another symbols or constants not preceded by a constant
                              ;; as defined above.
                              normal-indent))
                            ;; in this case calculate-lisp-indent-last-sexp is nil
                            (desired-indent)
                            (t
                             normal-indent)))))))

            (advice-add #'calculate-lisp-indent :around #'wrap~calculate-lisp-indent)))
  (flycheck-disabled-checkers . '(emacs-lisp-checkdoc))
  (indent-tabs-mode . nil)))
