(require 's)
(require 'f)
(require 'seq)
(require 'lsp-clients)

(defconst lsp-doc--client-groups
  (sort
   '(lsp-clojure lsp-css lsp-html lsp-intelephense lsp-pyls lsp-rust lsp-solargraph lsp-vetur lsp-xml lsp-groovy lsp-typescript-javascript lsp-typescript lsp-flow lsp-php lsp-ocaml lsp-clangd lsp-dart lsp-elixir lsp-fortran lsp-kotlin lsp-hack lsp-metals lsp-fsharp lsp-erlang lsp-yaml)
   (lambda (s1 s2)
     (string< (symbol-name s1) (symbol-name s2)))))

(defun lsp-doc-get-public-symbols ()
  (seq-filter
   (lambda (sym)
     (and (symbolp sym)
          (s-match "^lsp\\(?:\\-[a-zA-Z0-9]+\\)*$" (symbol-name sym))))
   obarray))

(defun lsp-doc--group-get-variables (group)
  (let ((custom-group (get group 'custom-group)))
    (seq-map
     (apply-partially #'car)
     (seq-filter (lambda (p)
                   (and (consp p)
                        (eq (cadr p) 'custom-variable)))
                 custom-group))))

(defun lsp-doc--group-gen-asciidoc (group)
  (let ((vars (lsp-doc--group-get-variables group)))
    (concat
     (format "[id=\"%s-vars\"]
==== \`%s\` settings\n\n" group group)
     (mapconcat
      (lambda (sym)
        (let* ((dflt-val (default-value sym))
               (type (get sym 'custom-type)))
          (format "[id=\"%s\"]
- \`%s\`
____
Default value: \`pass:[%s]\`

%s%s
____" sym sym

(pp-to-string (if (and (memq type '(file directory)) (stringp dflt-val))
                  (f-short dflt-val)
                dflt-val))
(documentation-property sym 'variable-documentation)
(if-let (version (get sym 'custom-package-version))
    (format "

NOTE: Introduced in \`%s\` %s
" (car version) (cdr version))
  ""))))
      vars "\n"))))

(defun lsp-doc--write-asciidoc (group file)
  (let ((doc (lsp-doc--group-gen-asciidoc group)))
    (write-region (concat doc "\n") nil file)))

(seq-doseq (group lsp-doc--client-groups)
  (let ((doc-dir (f-expand (format "./%s" group))))
    (message "Writing docs for %s" group)
    (unless (file-directory-p doc-dir)
      (make-directory doc-dir))
    (lsp-doc--write-asciidoc group (f-join doc-dir (format "%s-vars.adoc" group)))))

(lsp-doc--write-asciidoc 'lsp-mode "lsp-mode-vars.adoc")

;; (write-region
;;  (mapconcat
;;   (lambda (group)
;;     (format "include::%s/index.adoc[]" group))
;;   lsp-doc--client-groups "\n\n")
;;  nil
;;  "lsp-langs.adoc")
