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
  (save-excursion
      (goto-char (point-min))
      (forward-line (1- (gethash "line" params)))
      (+ (point) (gethash "character" params))))

(provide 'lsp-common)
