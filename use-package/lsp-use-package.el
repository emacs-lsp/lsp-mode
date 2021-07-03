;;; lsp-use-package.el --- Integration with `use-package' -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: convenience

;; Package-Requires: ((use-package "2.4.1") (emacs "26.1") (lsp-mode "7.0"))
;; Version: 0.0.1

;; URL: https://github.com/emacs-lsp/lsp-mode

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

;; Provides `:ensure-servers' keyword for `use-package'
;; Example:
;; (use-package lsp-mode
;;   :ensure-servers (jdtls ts-ls))

;;; Code:

(require 'use-package)

(defun use-package-handler/:ensure-servers (name _keyword arg rest state)
  "The `:ensure-servers' handler.
NAME of the section.
ARG - the value in that section
STATE - current state of the `use-package' expansion.
REST - the remaining content."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar (lambda (var)
               `(lsp-ensure-server (quote ,var)))
             arg)
     body)))

;;;###autoload
(defun use-package-normalize-ensure-servers (_name label arg &optional _recursed)
  "Normalize the arguments to diminish down to a list of symbols.
LABEL - the label for the section.
ARG - keyword value to normalize."
  (cond
   ((use-package-non-nil-symbolp arg)
    (list arg))
   ((-all? #'symbolp arg)
    arg)
   (t
    (use-package-error
     (concat label " wants a symbol, or list of symbols")))))

;;;###autoload
(defun use-package-normalize/:ensure-servers (name keyword args)
  "Normalize ARGS under KEYWORD section.
NAME is the name of the section."
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-ensure-servers name) t))

;;;###autoload (with-eval-after-load 'use-package (add-to-list 'use-package-keywords :ensure-servers t))

(provide 'lsp-use-package)
;;; lsp-use-package.el ends here
