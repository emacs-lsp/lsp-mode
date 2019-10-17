;;; lsp-generate-settings.el --- Functions for generating settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;; Functions for generating settings from package.json file

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 's)

(defun lsp--convert-type (type enum)
  "Convert vscode manifest TYPE to the elisp equivalent.
ENUM is the value of enum key in vscode manifest."
  (cond
   ((and enum (not (or (equal "boolean" type)
                       (equal '("boolean") type)))) `(choice (:tag ,@(append enum nil))))
   (t (pcase type
        ("boolean" 'boolean)
        ('("boolean") 'boolean)
        ("string" 'string)
        ("number" 'number)
        ("integer" 'number)
        ("array" 'lsp-string-vector)
        (`(,type . ,_rest) `(repeat ,(lsp--convert-type type nil)))))))

(defun lsp-generate-settings (file-name)
  "Generate settings for SERVER.
FILE-NAME is path to package.json vscode manifest."
  (let* ((json-array-type 'list)
         (parsed (json-read-file file-name))
         (properties (->> parsed
                          (assoc 'contributes)
                          cl-rest
                          (assoc 'configuration)
                          cl-rest
                          (assoc 'properties)
                          cl-rest))
         props-to-register)
    (append
     (-map (-lambda ((prop-name . (&alist 'type 'default 'enum 'description)))
             (let ((type (lsp--convert-type type enum))
                   (prop-symbol (intern (format "lsp-%s" (s-dashed-words (symbol-name prop-name)))) ))
               (push (append (list (symbol-name prop-name) prop-symbol) (when (equal type 'boolean) (list t)))
                     props-to-register)
               `(defcustom ,prop-symbol
                  ,(cond
                    ((equal default :json-false) nil)
                    ((and default (listp default)) (apply 'vector default))
                    (t default))
                  ,description
                  :type ',type))
             )
           properties)
     `((lsp-register-custom-settings ',props-to-register)))))

(provide 'lsp-generate-settings)

;;; lsp-generate-settings.el ends here
