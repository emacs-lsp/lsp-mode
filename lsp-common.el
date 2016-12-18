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

(provide 'lsp-common)
