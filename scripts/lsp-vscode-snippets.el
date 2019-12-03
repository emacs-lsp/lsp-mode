
(defun lsp-load-and-save-snippets (file-name mode dir)
  (ht-each
   (-lambda (name (&hash "description" "body" "prefix"))
     (with-temp-buffer (insert (format "# -*- mode: snippet -*-
# name: %s
# key: %s
# --
%s
"
                                       name
                                       prefix
                                       (lsp-fix-snippet (s-join "\n" body))))
                       (yas-load-snippet-buffer mode)
                       (f-write-text (buffer-string) 'utf-8 (f-join dir prefix))
                       ))
   (json-parse-string (f-read-text file-name))))

(defun lsp-fix-snippet (string)
  (-let* ((regex "\\(\\${[[:digit:]]*:[[:alnum:]]*}\\).*")
          (part (cl-second (s-match regex string))))
    (if part
        (let* ((index (s-index-of part string))
               (s1 (substring string 0 (+ index (length part))))
               (s2 (substring string (+ index (length part)))))
          (concat s1 (lsp-fix-snippet (s-replace part (concat "$" (cl-second (s-match "{\\([[:digit:]]*\\).*}" part))) s2))))
      string)))

;; (lsp-load-and-save-snippets "/home/kyoncho/Sources/vscode-mssql/snippets/mssql.json" 'sql-mode "/home/kyoncho/Sources/lsp/lsp-mssql/snippets/")
