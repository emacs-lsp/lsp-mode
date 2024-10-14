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

;; Usage
;; (lsp-generate-settings "/home/kyoncho/Sources/vscode-java/package.json" 'lsp-java)

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
                       (equal '("boolean") type))))
    `(choice ,@(mapcar (lambda (e) `(const ,(format "\"%s\"" e))) enum)))
   (t (pcase type
        ("boolean" 'boolean)
        ('("boolean") 'boolean)
        ("string" 'string)
        ("number" 'number)
        ("integer" 'number)
        ("array" 'lsp-string-vector)
        (`(,type . ,_rest) `(repeat ,(lsp--convert-type type nil)))))))

(defun lsp-generate-settings (file-name group)
  "Generate settings for SERVER.
FILE-NAME is path to package.json vscode manifest."
  (let ((json-array-type 'list))
    (->> (json-read-file file-name)
         (assoc 'contributes)
         cl-rest
         (assoc 'configuration)
         cl-rest
         (assoc 'properties)
         cl-rest
         (-keep
          (-lambda ((prop-name . (&alist 'type 'default 'enum 'description 'markdownDescription)))
            (let ((type (lsp--convert-type type enum))
                  (prop-symbol (intern (format "lsp-%s" (s-dashed-words (symbol-name prop-name)))) ))
              (unless (boundp prop-symbol)
                (format "(lsp-defcustom %s %s
  \"%s\"
  :type '%s
  :group '%s
  :package-version '(lsp-mode . \"9.0.0\")
  :lsp-path \"%s\")"
                        prop-symbol
                        (prin1-to-string
                         (cond
                          ((equal default :json-false) nil)
                          ((and default (listp default)) (apply 'vector default))
                          (t default)))
                        (or description markdownDescription)
                        type
                        group
                        (symbol-name prop-name))))))
         (s-join "\n\n"))))

(provide 'lsp-generate-settings)

;;; lsp-generate-settings.el ends here
