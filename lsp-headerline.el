;;; lsp-headerline.el --- LSP headerline features -*- lexical-binding: t; -*-
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
;;  LSP headerline features
;;
;;; Code:

(require 'lsp-mode)

(defcustom lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
  "Face used on breadcrumb text on modeline."
  :type '(repeat
          (choice (const :tag "Include the project name." project)
                  (const :tag "Include the open file name." file)
                  (const :tag "Include the directories up to project." path-up-to-project)
                  (const :tag "Include document symbols if server supports it." symbols)))
  :group 'lsp-mode)

(defcustom lsp-headerline-breadcrumb-enable-symbol-numbers nil
  "Whether to label symbols with numbers on the breadcrumb."
  :type 'boolean
  :group 'lsp-mode)

(defface lsp-headerline-breadcrumb-separator-face '((t :inherit shadow :height 0.8))
  "Face used for breadcrumb separator on headerline."
  :group 'lsp-faces)

(defface lsp-headerline-breadcrumb-prefix-face '((t :inherit font-lock-string-face))
  "Face used for breadcrumb prefix on headerline."
  :group 'lsp-faces)

(defface lsp-headerline-breadcrumb-project-prefix-face '((t :inherit font-lock-string-face :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lsp-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lsp-faces)

(defface lsp-headerline-breadcrumb-unknown-project-prefix-face '((t :inherit shadow :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lsp-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lsp-faces)

(defface lsp-headerline-breadcrumb-symbols-face '((t :inherit font-lock-doc-face :weight bold))
  "Face used for breadcrumb symbols text on headerline."
  :group 'lsp-faces)

(defface lsp-headerline-breadcrumb-deprecated-face '((t :inherit font-lock-doc-face
                                                        :strike-through t))
  "Face used on breadcrumb deprecated text on modeline."
  :group 'lsp-faces)

(defvar-local lsp-headerline--string nil
  "Holds the current breadcrumb string on headerline.")
(defvar-local lsp-headerline--path-up-to-project-string nil
  "Holds the current breadcrumb path-up-to-project segments for
caching purposes.")

(declare-function all-the-icons-material "ext:all-the-icons" t t)
(declare-function lsp-treemacs-symbol-icon "ext:lsp-treemacs" (kind))
(declare-function lsp-treemacs-get-icon "ext:lsp-treemacs" (icon-name))

(defun lsp-headerline--fix-image-background (image)
  "Fix IMAGE background if it is a file otherwise return as an icon."
  (if (get-text-property 0 'display image)
      (propertize " " 'display
                  (cl-list* 'image
                            (plist-put
                             (cl-copy-list
                              (cl-rest (get-text-property
                                        0 'display
                                        image)))
                             :background (face-attribute 'header-line :background))))
    (replace-regexp-in-string "\s\\|\t" "" image)))

(defun lsp-headerline--filename-with-icon (file-path)
  "Return the filename from FILE-PATH with the extension related icon."
  (let ((filename (f-filename file-path)))
    (-if-let* ((file-ext (f-ext file-path))
               (icon (and file-ext
                          (require 'lsp-treemacs nil t)
                          (lsp-treemacs-get-icon file-ext))))
        (format "%s %s"
                (lsp-headerline--fix-image-background icon)
                filename)
      filename)))

(defun lsp-headerline--arrow-icon ()
  "Build the arrow icon for headerline breadcrumb."
  (if (require 'all-the-icons nil t)
      (all-the-icons-material "chevron_right"
                              :face 'lsp-headerline-breadcrumb-separator-face)
    (propertize "›" 'face 'lsp-headerline-breadcrumb-separator-face)))

(lsp-defun lsp-headerline--symbol-icon ((&DocumentSymbol :kind))
  "Build the SYMBOL icon for headerline breadcrumb."
  (when (require 'lsp-treemacs nil t)
    (concat (lsp-headerline--fix-image-background (lsp-treemacs-symbol-icon kind))
            " ")))

(lsp-defun lsp-headerline--go-to-symbol ((&DocumentSymbol
                                          :selection-range (&RangeToPoint :start selection-start)
                                          :range (&RangeToPoint :start narrowing-start
                                                                :end narrowing-end)))
  "Go to breadcrumb symbol.
If the buffer is narrowed and the target symbol lies before the
minimum reachable point in the narrowed buffer, then widen and
narrow to the outer symbol."
  (when (buffer-narrowed-p)
    (narrow-to-region
     (min (point-min) narrowing-start)
     (max (point-max) narrowing-end)))
  (goto-char selection-start))

(lsp-defun lsp-headerline--narrow-to-symbol ((&DocumentSymbol :range (&RangeToPoint :start :end)))
  "Narrow to breadcrumb symbol range."
  (narrow-to-region start end))

(defun lsp-headerline--with-action (local-map help-echo-string display-string)
  "Assign LOCAL-MAP and HELP-ECHO-STRING to the region around the
DISPLAY-STRING."
  (propertize display-string
              'mouse-face 'header-line-highlight
              'help-echo help-echo-string
              'local-map local-map))

(defun lsp-headerline--directory-with-action (full-path directory-display-string)
  "Build action for FULL-PATH and DIRECTORY-DISPLAY-STRING."
  (lsp-headerline--with-action (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1]
                                   (lambda ()
                                     (interactive)
                                     (dired full-path)))
                                 (define-key map [header-line mouse-2]
                                   (lambda ()
                                     (interactive)
                                     (dired-other-window full-path)))
                                 map)
                               (format "mouse-1: browse '%s' with Dired\nmouse-2: browse '%s' with Dired in other window"
                                       directory-display-string
                                       directory-display-string)
                               directory-display-string))

(lsp-defun lsp-headerline--symbol-with-action ((symbol &as &DocumentSymbol :name) symbol-display-string)
  "Build action for SYMBOL and SYMBOL-STRING."
  (lsp-headerline--with-action (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1]
                                   (lambda ()
                                     (interactive)
                                     (lsp-headerline--go-to-symbol symbol)))
                                 (define-key map [header-line mouse-2]
                                   (lambda ()
                                     (interactive)
                                     (lsp-headerline--narrow-to-symbol symbol)))
                                 map)
                               (format "mouse-1: go to '%s' symbol\nmouse-2: narrow to '%s' range" name name)
                               symbol-display-string))

(defun lsp-headerline--path-up-to-project-root (root-path path)
  "Find recursively the folders until the project ROOT-PATH.
PATH is the current folder to be checked."
  (let ((current-path path)
        headerline-path-components)
    (while (not (lsp-f-same? root-path current-path))
      (push (lsp-headerline--directory-with-action current-path
                                                   (f-filename current-path))
            headerline-path-components)
      (setq current-path (lsp-f-parent current-path)))
    headerline-path-components))

(defun lsp-headerline--build-project-string ()
  "Build the project-segment string for the breadcrumb."
  (-if-let (root (lsp-workspace-root))
      (propertize (lsp-headerline--directory-with-action
                   root
                   (f-filename root))
                  'font-lock-face
                  'lsp-headerline-breadcrumb-project-prefix-face)
    (propertize "<unknown>"
                'font-lock-face
                'lsp-headerline-breadcrumb-unknown-project-prefix-face)))

(defun lsp-headerline--build-file-string ()
  "Build the file-segment string for the breadcrumb."
  (propertize (lsp-headerline--filename-with-icon (buffer-file-name))
              'font-lock-face 'lsp-headerline-breadcrumb-prefix-face))

(defun lsp-headerline--build-path-up-to-project-string ()
  "Build the path-up-to-project segment for the breadcrumb."
  (if-let (root (lsp-workspace-root))
      (mapconcat (lambda (next-dir)
                   (propertize next-dir
                               'font-lock-face
                               'lsp-headerline-breadcrumb-prefix-face))
                 (lsp-headerline--path-up-to-project-root
                  root
                  (lsp-f-parent (buffer-file-name)))
                 (format " %s " (lsp-headerline--arrow-icon)))
    ""))

(defun lsp-headerline--build-symbol-string ()
  "Build the symbol segment for the breadcrumb."
  (if (lsp-feature? "textDocument/documentSymbol")
      (-if-let* ((lsp--document-symbols-request-async t)
                 (symbols (lsp--get-document-symbols))
                 (symbols-hierarchy (lsp--symbols->symbols-hierarchy symbols))
                 (enumerated-symbols-hierarchy
                  (-map-indexed (lambda (index elt)
                                  (cons elt (1+ index)))
                                symbols-hierarchy)))
          (mapconcat (-lambda (((symbol-to-append &as &DocumentSymbol :deprecated? :name)
                                . index))
                       (let* ((symbol2-name
                               (propertize name
                                           'font-lock-face
                                           (if deprecated?
                                               'lsp-headerline-breadcrumb-deprecated-face
                                             'lsp-headerline-breadcrumb-symbols-face)))
                              (symbol2-icon
                               (lsp-headerline--symbol-icon symbol-to-append))
                              (full-symbol-2
                               (concat
                                (if lsp-headerline-breadcrumb-enable-symbol-numbers
                                    (concat
                                     (propertize (number-to-string index)
                                                 'face
                                                 'lsp-headerline-breadcrumb-symbols-face)
                                     " ")
                                  "")
                                (if symbol2-icon
                                    (concat symbol2-icon symbol2-name)
                                  symbol2-name))))
                         (lsp-headerline--symbol-with-action symbol-to-append full-symbol-2)))
                     enumerated-symbols-hierarchy
                     (format " %s " (lsp-headerline--arrow-icon)))
        "")
    ""))

(defun lsp-headerline--build-string ()
  "Build the header-line string."
  (string-trim-right
   (mapconcat
    (lambda (segment)
      (let ((segment-string
             (pcase segment
               ('project (lsp-headerline--build-project-string))
               ('file (lsp-headerline--build-file-string))
               ('path-up-to-project
                (or lsp-headerline--path-up-to-project-string
                    (lsp-headerline--build-path-up-to-project-string)))
               ('symbols (lsp-headerline--build-symbol-string))
               (_ (progn
                    (lsp-log "'%s' is not a valid entry for `lsp-headerline-breadcrumb-segments'"
                             (symbol-name segment))
                    "")))))
        (if (eq segment-string "")
            ""
          (format "%s %s "
                  (lsp-headerline--arrow-icon)
                  segment-string))))
    lsp-headerline-breadcrumb-segments
    "")))

(defun lsp-headerline--check-breadcrumb (&rest _)
  "Request for document symbols to build the breadcrumb."
  (setq lsp-headerline--string (lsp-headerline--build-string))
  (force-mode-line-update))

(defun lsp-headerline--breadcrumb-cache-path-up-to-project ()
  "Cache the path-up-to-project breadcrumb segment if enabled."
  (when (and lsp-headerline-breadcrumb-enable
             (member 'path-up-to-project lsp-headerline-breadcrumb-segments))
    (setq lsp-headerline--path-up-to-project-string (lsp-headerline--build-path-up-to-project-string))))

(defun lsp-headerline--enable-breadcrumb ()
  "Enable headerline breadcrumb mode."
  (when (and lsp-headerline-breadcrumb-enable
             (lsp-feature? "textDocument/documentSymbol"))
    (lsp-headerline-breadcrumb-mode 1)))

(defun lsp-headerline--disable-breadcrumb ()
  "Disable headerline breadcrumb mode."
  (lsp-headerline-breadcrumb-mode -1))

;;;###autoload
(define-minor-mode lsp-headerline-breadcrumb-mode
  "Toggle breadcrumb on headerline."
  :group 'lsp-mode
  :global nil
  (cond
   (lsp-headerline-breadcrumb-mode
    (lsp-headerline--breadcrumb-cache-path-up-to-project)
    (add-to-list 'header-line-format '(t (:eval lsp-headerline--string)))

    (add-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb nil t)
    (add-hook 'lsp-configure-hook #'lsp-headerline--enable-breadcrumb nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-headerline--disable-breadcrumb nil t))
   (t
    (remove-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb t)
    (remove-hook 'lsp-configure-hook #'lsp-headerline--enable-breadcrumb t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-headerline--disable-breadcrumb t)

    (setq lsp-headerline--path-up-to-project-string nil)
    (setq header-line-format (remove '(t (:eval lsp-headerline--string)) header-line-format)))))

;;;###autoload
(defun lsp-breadcrumb-go-to-symbol (symbol-position)
  "Go to the symbol on breadcrumb at SYMBOL-POSITION."
  (interactive "P")
  (if (numberp symbol-position)
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp--symbols->symbols-hierarchy symbols)))
              (lsp-headerline--go-to-symbol (nth (1- symbol-position) symbols-hierarchy))
            (lsp--info "Symbol not found for position %s" symbol-position))
        (lsp--info "Server does not support breadcrumb."))
    (lsp--info "Call this function with a number representing the symbol position on breadcrumb")))

;;;###autoload
(defun lsp-breadcrumb-narrow-to-symbol (symbol-position)
  "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION."
  (interactive "P")
  (if (numberp symbol-position)
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp--symbols->symbols-hierarchy symbols)))
              (lsp-headerline--narrow-to-symbol (nth (1- symbol-position) symbols-hierarchy))
            (lsp--info "Symbol not found for position %s" symbol-position))
        (lsp--info "Server does not support breadcrumb."))
    (lsp--info "Call this function with a number representing the symbol position on breadcrumb")))

(provide 'lsp-headerline)
;;; lsp-headerline.el ends here
