;;; lsp-generate-bindings.el --- Generates lsp-mode bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; script for generating elisp bindings from json schema.

;;; Code:

(with-temp-buffer
  (insert-file-contents-literally "generated.protocol.schema.json")

  (-let* ((json-object-type 'plist)
          ((&plist :$defs defs :properties) (json-read-from-string (buffer-string))))
    `(progn
       ,@(->> (append defs properties)
          (-partition 2)
          (-filter (-lambda ((_ (&plist :type :enum)))
                     (and (not (string= type "object"))
                      enum)))
          (-keep (-lambda ((type (&plist :enum)))
                   (cons
                    (list
                     'defvar (intern (format "lsp/%s-lookup"
                                      (s-dashed-words (substring (symbol-name type) 1))))
                     (if (eq type :TextDocumentSyncKind)
                         (apply 'vector (-map #'intern enum))
                       (apply 'vector nil (-map #'intern enum))))
                    (seq-map-indexed (-lambda (value index)
                                       (list 'defconst (intern (format "lsp/%s-%s"
                                                                (s-dashed-words (substring (symbol-name type) 1))
                                                                (s-dashed-words  value)))
                                        (if (eq type :TextDocumentSyncKind)
                                            index
                                          (1+ index))))
                     enum))))
          (apply #'nconc))

       ,(->> (append defs properties)
         (-partition 2)
         (-filter (-lambda ((_ (&plist :type)))
                    (string= type "object")))
         (-map (-lambda ((name type-data))
                 (let* ((key (intern (substring (symbol-name name) 1)))
                        (required (-map (lambda (field-name)
                                          (intern (concat ":" field-name))) (plist-get type-data :required)))
                        (params (->> (plist-get type-data :properties)
                                 (-partition 2)
                                 (-map #'cl-first)
                                 (-filter (-not (-partial #'-contains? required))))))
                  (list key required params))))

         (cl-list* 'lsp-interface)))))


(provide 'lsp-generate-bindings)
;;; lsp-generate-bindings.el ends here
