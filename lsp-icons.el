;;; lsp-icons.el --- LSP icons management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacs-lsp maintainers
;;
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
;;
;;; Commentary:
;;
;;  LSP icons management
;;
;;; Code:

(declare-function all-the-icons-material "ext:all-the-icons" t t)
(declare-function lsp-treemacs-symbol-icon "ext:lsp-treemacs" (kind))
(declare-function lsp-treemacs-get-icon "ext:lsp-treemacs" (icon-name))

(defun lsp-icons--fix-image-background (image)
  "Fix IMAGE background if it is a file otherwise return as an icon."
  (if image
      (let ((display-image (get-text-property 0 'display image)))
        (if (and (listp display-image)
                 (plist-member (cl-copy-list (cl-rest display-image)) :type))
            (propertize " " 'display
                        (cl-list* 'image
                                  (plist-put
                                   (cl-copy-list
                                    (cl-rest display-image))
                                   :background (face-attribute 'header-line :background nil t))))
          (if (stringp display-image)
              (replace-regexp-in-string "\s\\|\t" "" display-image)
            (replace-regexp-in-string "\s\\|\t" "" image))))
    ""))

(defun lsp-icons-get-by-file-ext (file-ext)
  "Get an icon by file FILE-EXT."
  (when (and file-ext
             (require 'lsp-treemacs nil t))
    (lsp-icons--fix-image-background
     (lsp-treemacs-get-icon file-ext))))

(defun lsp-icons-get-by-symbol-kind (kind)
  "Get an icon by symbol KIND."
  (when (and kind
             (require 'lsp-treemacs nil t))
    (lsp-icons--fix-image-background
     (lsp-treemacs-symbol-icon kind))))

(defun lsp-icons-all-the-icons-material-icon (icon-name face fallback)
  "Get a material icon from all-the-icons by ICON-NAME using FACE.
Fallback to FALLBACK string if not found or not available."
  (if (require 'all-the-icons nil t)
      (all-the-icons-material icon-name
                              :face face)
    (propertize fallback 'face face)))

(provide 'lsp-icons)
;;; lsp-icons.el ends here
