;;; test-helper.el --- Helpers for lsp-mode-test.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Google LLC <phst@google.com>

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

;; Initializes test support for ‘lsp-mode’.

;;; Code:


(setq safe-local-variable-values
      '((flycheck-disabled-checkers emacs-lisp-checkdoc)))


;; (when (require 'undercover nil t)
;;   (undercover "*.el" (:report-type :codecov)))

(add-to-list 'load-path
             (file-name-as-directory (f-parent (f-parent (f-this-file)))))

;;; test-helper.el ends here
