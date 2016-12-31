;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>

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

(defconst lsp--message-type-face
  '((1 . 'compilation-error)
    (2 . 'compilation-warning)
    (3 . 'compilation-message)
    (4 . 'compilation-info-face)))

(defsubst lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (save-excursion
    (goto-char (point-min))
    (forward-line (gethash "line" params))
    (move-to-column (gethash "character" params))
    (point)))

(defsubst lsp--assert-type (obj pred)
  (if (funcall pred obj)
      obj
    (signal 'wrong-type-argument `(,pred ,obj))))

(provide 'lsp-common)
