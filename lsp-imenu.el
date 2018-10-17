;; Copyright (C) 2016-2018  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Imenu integration with lsp-mode.  Enable with:
;; (require 'lsp-imenu)
;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;;; Code:

(require 'imenu)
(require 'lsp-methods)
(require 'seq)

(defgroup lsp-imenu nil
  "Customization group for `lsp-imenu'."
  :group 'lsp-mode)

(defcustom lsp-imenu-show-container-name t
  "Display the symbol's container name in an imenu entry."
  :type 'boolean
  :group 'lsp-imenu)

(defcustom lsp-imenu-container-name-separator "/"
  "Separator string to use to separate the container name from the symbol while displaying imenu entries."
  :type 'string
  :group 'lsp-imenu)

(defcustom lsp-imenu-sort-methods '(kind name)
  "How to sort the imenu items.

The value is a list of `kind' `name' or `position'. Priorities
are determined by the index of the element."
  :type '(repeat (choice (const name)
                         (const position)
                         (const kind))))

(defconst lsp--imenu-compare-function-alist
  (list (cons 'name #'lsp--imenu-compare-name)
        (cons 'kind #'lsp--imenu-compare-kind)
        (cons 'position #'lsp--imenu-compare-position))
  "An alist of (METHOD . FUNCTION).

METHOD is one of the symbols accepted by
`lsp-imenu-sort-methods'.

FUNCTION takes two hash tables representing DocumentSymbol. It
returns a negative number, 0, or a positive number indicating
whether the first parameter is less than, equal to, or greater
than the second parameter.")

(define-inline lsp--point-to-marker (p)
  (inline-quote (save-excursion (goto-char ,p) (point-marker))))

(defun lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym)))
    (cons (if (and lsp-imenu-show-container-name container)
              (concat container lsp-imenu-container-name-separator name)
            name)
          start-point)))

(defun lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym)))
    (if (gethash "children" sym)
        (cons name
              (cons (cons (format "(%s)" (lsp--get-symbol-type sym)) start-point)
                    (lsp--imenu-create-hierarchical-index (gethash "children" sym))))
      (cons (format "%s (%s)" name (lsp--get-symbol-type sym)) start-point))))

(defun lsp--symbol-get-start-point (sym)
  "Get the start point of the name of SYM.

SYM can be either DocumentSymbol or SymbolInformation."
  (let* ((location (gethash "location" sym))
         (name-range (or (and location (gethash "range" location))
                         (gethash "selectionRange" sym)))
         (start-point (lsp--position-to-point
                       (gethash "start" name-range))))
    (if imenu-use-markers (lsp--point-to-marker start-point) start-point)))

(defun lsp--symbol-filter (sym)
  "Determine if SYM is for the current document."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the current
  ;; buffer file.
  (when-let (location (gethash "location" sym))
    (not
     (eq
      (find-buffer-visiting (lsp--uri-to-path (gethash "uri" (gethash "location" sym))))
      (current-buffer)))))

(defun lsp--get-symbol-type (sym)
  "The string name of the kind of SYM."
  (or (cdr (assoc (gethash "kind" sym) lsp--symbol-kind)) "Other"))

(defun lsp--imenu-create-index ()
  "Create imenu index from document symbols."
  (let ((symbols (lsp--get-document-symbols)))
    (if (lsp--imenu-hierarchical-p symbols)
        (lsp--imenu-create-hierarchical-index symbols)
      (mapcar (lambda (nested-alist)
                (cons (car nested-alist)
                      (mapcar #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
              (seq-group-by #'lsp--get-symbol-type (lsp--imenu-filter-symbols symbols))))))

(defun lsp--imenu-filter-symbols (symbols)
  "Filter out unsupported symbols from SYMBOLS."
  (seq-remove #'lsp--symbol-filter symbols))

(defun lsp--imenu-hierarchical-p (symbols)
  "Determine whether any element in SYMBOLS has children."
  (seq-some (lambda (sym)
              (gethash "children" sym))
            symbols))

(defun lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (mapcar (lambda (sym)
              (lsp--symbol-to-hierarchical-imenu-elem sym))
            (sort (lsp--imenu-filter-symbols symbols)
                  (lambda (sym1 sym2)
                    (lsp--imenu-symbol-lessp sym1 sym2))))))

(defun lsp--imenu-symbol-lessp (sym1 sym2)
  (let* ((compare-results (mapcar (lambda (method)
                                    (funcall (alist-get method lsp--imenu-compare-function-alist)
                                             sym1 sym2))
                                  lsp-imenu-sort-methods))
         (result (seq-find (lambda (result)
                             (not (= result 0)))
                           compare-results
                           0)))
    (and (numberp result) (< result 0))))

(defun lsp--imenu-compare-kind (sym1 sym2)
  (let ((kind1 (gethash "kind" sym1))
        (kind2 (gethash "kind" sym2)))
    (- kind1 kind2)))

(defun lsp--imenu-compare-position (sym1 sym2)
  (let ((position1 (lsp--symbol-get-start-point sym1))
        (position2 (lsp--symbol-get-start-point sym2)))
    (- position1 position2)))

(defun lsp--imenu-compare-name (sym1 sym2)
  (let* ((name1 (gethash "name" sym1))
         (name2 (gethash "name" sym2))
         (result (compare-strings name1 0 (length name1) name2 0 (length name2))))
    (if (numberp result)
        result
      0)))

(defun lsp-enable-imenu ()
  "Use lsp-imenu for the current buffer."
  (setq-local imenu-create-index-function #'lsp--imenu-create-index))

(provide 'lsp-imenu)
;;; lsp-imenu.el ends here
