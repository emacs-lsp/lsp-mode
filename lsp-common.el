(defconst lsp--message-type-face
  '((1 . 'compilation-error)
    (2 . 'compilation-warning)
    (3 . 'compilation-message)
    (4 . 'compilation-info-face)))

(defsubst lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (pos-at-line-col (gethash "line" params) (gethash "character" params)))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun pos-at-line-col (l c)
  (save-excursion
    (goto-char (point-min))
    (forward-line l)
    (move-to-column c)
    (point)))

(provide 'lsp-common)
