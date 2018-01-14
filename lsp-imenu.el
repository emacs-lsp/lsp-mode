;; Copyright (C) 2016-2017  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

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

(define-inline lsp--point-to-marker (p)
  (inline-quote (save-excursion (goto-char ,p) (point-marker))))

(defun lsp--symbol-to-imenu-elem (sym)
  (let ((pt (lsp--position-to-point (gethash "start"
                                      (gethash "range" (gethash "location" sym))))))
    (cons (gethash "name" sym) (if imenu-use-markers (lsp--point-to-marker pt) pt))))

(defun lsp--symbol-filter (sym)
  (not
    (lsp--equal-files
      (lsp--uri-to-path (gethash "uri" (gethash "location" sym)))
      (buffer-file-name))))

(defun lsp--get-symbol-type (sym)
  (or (cdr (assoc (gethash "kind" sym) lsp--symbol-kind)) "Other"))

(defun lsp--imenu-create-index ()
  (let ((symbols (seq-remove #'lsp--symbol-filter (lsp--get-document-symbols))))
    (mapcar (lambda (nested-alist)
              (cons (car nested-alist)
                (mapcar #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
      (seq-group-by #'lsp--get-symbol-type symbols))))

(defun lsp-enable-imenu ()
  (setq-local imenu-create-index-function #'lsp--imenu-create-index))

(provide 'lsp-imenu)
;;; lsp-imenu.el ends here
