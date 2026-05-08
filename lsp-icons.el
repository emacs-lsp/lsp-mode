;;; lsp-icons.el --- LSP icons management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2026 emacs-lsp maintainers
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
(require 'lsp-mode)

(defgroup lsp-icons nil
  "LSP icons"
  :group 'lsp-mode
  :tag "LSP Icons")

(defcustom lsp-headerline-breadcrumb-icons-enable t
  "If non-nil, icons support is enabled for headerline-breadcrumb."
  :type 'boolean
  :group 'lsp-icons)

(defcustom lsp-modeline-code-action-icons-enable t
  "If non-nil, icons support is enabled for modeline-code-action"
  :type 'boolean
  :group 'lsp-icons)

(declare-function lsp-treemacs-symbol-icon "ext:lsp-treemacs" (kind))
(declare-function lsp-treemacs-get-icon "ext:lsp-treemacs" (icon-name))

(defun lsp-icons--enabled-for-feature (feature)
  "Check if icons support is enabled for FEATURE."
  (cond
   ((eq feature 'headerline-breadcrumb) lsp-headerline-breadcrumb-icons-enable)
   ((eq feature 'modeline-code-action) lsp-modeline-code-action-icons-enable)
   (t t)))

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

(defun lsp-icons-get-by-file-ext (file-ext &optional feature)
  "Get an icon by file FILE-EXT.
FEATURE is the feature that will use the icon which we should check
if its enabled."
  (when (and file-ext
             (lsp-icons--enabled-for-feature feature)
             (functionp 'lsp-treemacs-get-icon))
    (lsp-icons--fix-image-background
     (lsp-treemacs-get-icon file-ext))))

(defun lsp-icons-get-by-symbol-kind (kind &optional feature)
  "Get an icon by symbol KIND.
FEATURE is the feature that will use the icon which we should check
if its enabled."
  (when (and kind
             (lsp-icons--enabled-for-feature feature)
             (functionp 'lsp-treemacs-symbol-icon))
    (lsp-icons--fix-image-background
     (lsp-treemacs-symbol-icon kind))))

(defun lsp-icons-all-the-icons-icon (icon-set icon-name face fallback &optional feature &rest args)
  "Get icon ICON-NAME from `all-the-icons' ICON-SET using FACE.
If ARGS is provided, it's a plist passed directly to the `all-the-icons' function.
Fallback to FALLBACK string if not found or not available.
FEATURE is the feature that will use the icon which we should check
if its enabled."
  (let ((icon-set-fn (intern-soft (concat "all-the-icons-" (symbol-name icon-set)))))
    (if (and (fboundp icon-set-fn)
             (lsp-icons--enabled-for-feature feature))
        (apply icon-set-fn icon-name :face face args)
      (propertize fallback 'face face))))

(lsp-consistency-check lsp-icons)

(provide 'lsp-icons)
;;; lsp-icons.el ends here
