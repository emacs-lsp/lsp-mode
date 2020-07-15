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

(defvar-local lsp--headerline-breadcrumb-string nil
  "Holds the current breadcrumb string on headerline.")
(defvar-local lsp--headerline-breadcrumb-path-up-to-project-string nil
  "Holds the current breadcrumb path-up-to-project segment string
for caching purposes.")

(declare-function all-the-icons-material "ext:all-the-icons" t t)
(declare-function lsp-treemacs-symbol-icon "ext:lsp-treemacs" (kind))
(declare-function treemacs-current-theme "ext:treemacs-themes")
(declare-function treemacs-get-icon-value "ext:treemacs-icons" (ext &optional tui theme) t)
(declare-function treemacs-theme->name "ext:treemacs-themes" t t)

(defun lsp--fix-image-background (image)
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

(defun lsp--filename-with-icon (file-path)
  "Return the filename from FILE-PATH with the extension related icon."
  (let ((filename (f-filename file-path))
        (file-ext (f-ext file-path)))
    (if file-ext
        (when (require 'treemacs nil t)
          (if-let (icon (treemacs-get-icon-value file-ext nil (treemacs-theme->name (treemacs-current-theme))))
              (format "%s %s"
                      (lsp--fix-image-background icon)
                      filename)
            filename))
      filename)))

(defun lsp--headerline-breadcrumb-arrow-icon ()
  "Build the arrow icon for headerline breadcrumb."
  (if (require 'all-the-icons nil t)
      (all-the-icons-material "chevron_right"
                              :face 'lsp-headerline-breadcrumb-separator-face)
    (propertize "›" 'face 'lsp-headerline-breadcrumb-separator-face)))

(lsp-defun lsp--headerline-breadcrumb-symbol-icon ((&DocumentSymbol :kind))
  "Build the SYMBOL icon for headerline breadcrumb."
  (when (require 'lsp-treemacs nil t)
    (concat (lsp--fix-image-background (lsp-treemacs-symbol-icon kind))
            " ")))

(lsp-defun lsp--headerline-breadcrumb-go-to-symbol
  ((&DocumentSymbol :selection-range (&RangeToPoint :start selection-range-start)
                    :range (&RangeToPoint :start narrowing-range-start
                                          :end narrowing-range-end)))
  "Go to breadcrumb symbol.
If the buffer is narrowed and the target symbol lies before the
minimum reachable point in the narrowed buffer, then widen and
narrow to the outer symbol."
  (when (< selection-range-start (point-min))
      (narrow-to-region
       narrowing-range-start
       narrowing-range-end))
  (goto-char selection-range-start))

(lsp-defun lsp--headerline-breadcrumb-narrow-to-symbol ((&DocumentSymbol :range (&RangeToPoint :start :end)))
  "Narrow to breadcrumb symbol range."
  (narrow-to-region start end))

(lsp-defun lsp--headerline-with-action ((symbol &as &DocumentSymbol :name) symbol-string)
  "Build action for SYMBOL and SYMBOL-STRING."
  (propertize symbol-string
              'mouse-face 'header-line-highlight
              'help-echo (format "mouse-1: go to '%s' symbol\nmouse-2: narrow to '%s' range" name name)
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [header-line mouse-1]
                             (lambda ()
                               (interactive)
                               (lsp--headerline-breadcrumb-go-to-symbol symbol)))
                           (define-key map [header-line mouse-2]
                             (lambda ()
                               (interactive)
                               (lsp--headerline-breadcrumb-narrow-to-symbol symbol)))
                           map)))

(defun lsp--headerline-path-up-to-project-root (root-path path)
  "Find recursively the folders until the project ROOT-PATH.
PATH is the current folder to be checked."
  (let ((current-path path)
        headerline-path-components)
    (while (not (lsp-f-same? root-path current-path))
      (push (lsp--filename-with-icon current-path) headerline-path-components)
      (setq current-path (lsp-f-parent current-path)))
    headerline-path-components))

(defun lsp--headerline-breadcrumb-build-project-string ()
  "Build the project segment string for the breadcrumb."
  (when (member 'project lsp-headerline-breadcrumb-segments)
    (format "%s%s %s"
            (if (eq (cl-first lsp-headerline-breadcrumb-segments) 'project)
                ""
              " ")
            (lsp--headerline-breadcrumb-arrow-icon)
            (-if-let (root (lsp-workspace-root))
                (propertize (f-filename root)
                            'font-lock-face 'lsp-headerline-breadcrumb-project-prefix-face)
              (propertize "<unknown>"
                          'font-lock-face 'lsp-headerline-breadcrumb-unknown-project-prefix-face)))))

(defun lsp--headerline-breadcrumb-build-file-string ()
  "Build the file segment string for the breadcrumb."
  (when (member 'file lsp-headerline-breadcrumb-segments)
    (format "%s%s %s"
            (if (eq (cl-first lsp-headerline-breadcrumb-segments) 'file)
                ""
              " ")
            (lsp--headerline-breadcrumb-arrow-icon)
            (propertize (lsp--filename-with-icon (buffer-file-name))
                        'font-lock-face 'lsp-headerline-breadcrumb-prefix-face))))

(defun lsp--headerline-breadcrumb-build-path-up-to-project-string ()
  "Build the path-up-to-project segment string for the breadcrumb."
  (when (member 'path-up-to-project lsp-headerline-breadcrumb-segments)
    (when-let (root (lsp-workspace-root))
      (seq-reduce (lambda (last-dirs next-dir)
                    (format "%s%s %s"
                            (if last-dirs
                                (concat last-dirs " ")
                              (if (eq (cl-first lsp-headerline-breadcrumb-segments) 'path-up-to-project)
                                  ""
                                " "))
                            (lsp--headerline-breadcrumb-arrow-icon)
                            (propertize next-dir 'font-lock-face 'lsp-headerline-breadcrumb-prefix-face)))
                  (lsp--headerline-path-up-to-project-root root (lsp-f-parent (buffer-file-name))) nil))))

(defun lsp--headerline-breadcrumb-build-symbols-string ()
  "Build the symbols segment string for the breadcrumb."
  (when (and (member 'symbols lsp-headerline-breadcrumb-segments)
             (lsp-feature? "textDocument/documentSymbol"))
    (-when-let* ((lsp--document-symbols-request-async t)
                 (symbols (lsp--get-document-symbols))
                 (symbols-hierarchy (lsp-symbols->symbols-hierarchy symbols))
                 (enumerated-symbols-hierarchy
                  (-map-indexed (lambda (index elt)
                                  (cons elt (1+ index)))
                                symbols-hierarchy)))
      (concat
       (if (eq (cl-first lsp-headerline-breadcrumb-segments) 'symbol)
           ""
         " ")
       (mapconcat
        (-lambda (((symbol-to-append &as &DocumentSymbol :deprecated? :name)
                   . index))
          (let* ((symbol2-name
                  (propertize name
                              'font-lock-face
                              (if deprecated?
                                  'lsp-headerline-breadcrumb-deprecated-face
                                'lsp-headerline-breadcrumb-symbols-face)))
                 (symbol2-icon
                  (lsp--headerline-breadcrumb-symbol-icon symbol-to-append))
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
            (format "%s %s"
                    (lsp--headerline-breadcrumb-arrow-icon)
                    (lsp--headerline-with-action symbol-to-append full-symbol-2))))
        enumerated-symbols-hierarchy
        " ")))))

(defun lsp--headerline-build-string ()
  "Build the header-line string."
  (seq-reduce (lambda (last-segment next-segment)
                (concat last-segment
                        (or (pcase next-segment
                              ('project (lsp--headerline-breadcrumb-build-project-string))
                              ('file (lsp--headerline-breadcrumb-build-file-string))
                              ('path-up-to-project (or lsp--headerline-breadcrumb-path-up-to-project-string
                                                       (lsp--headerline-breadcrumb-build-path-up-to-project-string)))
                              ('symbols (lsp--headerline-breadcrumb-build-symbols-string)))
                            "")))
              lsp-headerline-breadcrumb-segments ""))

(defun lsp--document-symbols->symbols-hierarchy (document-symbols)
  "Convert DOCUMENT-SYMBOLS to symbols hierarchy."
  (-let (((symbol &as &DocumentSymbol? :children?)
          (seq-some (-lambda ((symbol &as &DocumentSymbol :range (&RangeToPoint :start :end)))
                      (when (<= start (point) end)
                        symbol))
                    document-symbols)))
    (if children?
        (cons symbol (lsp--document-symbols->symbols-hierarchy children?))
      (when symbol
        (list symbol)))))

(defun lsp--symbols-informations->symbols-hierarchy (symbols-informations)
  "Convert SYMBOLS-INFORMATIONS to symbols hierarchy."
  (seq-filter (-lambda ((symbol &as &SymbolInformation :location (&Location :range (&RangeToPoint :start :end))))
                (when (<= start (point) end)
                  symbol))
              symbols-informations))

(defun lsp-symbols->symbols-hierarchy (symbols)
  "Convert SYMBOLS to symbols-hierarchy."
  (when-let (first-symbol (lsp-seq-first symbols))
    (if (lsp-symbol-information? first-symbol)
        (lsp--symbols-informations->symbols-hierarchy symbols)
      (lsp--document-symbols->symbols-hierarchy symbols))))

(defun lsp-headerline--check-breadcrumb (&rest _)
  "Request for document symbols to build the breadcrumb."
  (setq lsp--headerline-breadcrumb-string (lsp--headerline-build-string))
  (force-mode-line-update))

(defun lsp-headerline--breadcrumb-cache-path-up-to-project ()
  "Cache the path-up-to-project breadcrumb segment if enabled."
  (when (and lsp-headerline-breadcrumb-enable
             (member 'path-up-to-project lsp-headerline-breadcrumb-segments))
    (setq lsp--headerline-breadcrumb-path-up-to-project-string (lsp--headerline-breadcrumb-build-path-up-to-project-string))))

(defun lsp--headerline-enable-breadcrumb ()
  "Enable headerline breadcrumb mode."
  (when (and lsp-headerline-breadcrumb-enable
             (lsp-feature? "textDocument/documentSymbol"))
    (lsp-headerline-breadcrumb-mode 1)))

(defun lsp--headerline-disable-breadcrumb ()
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
    (add-to-list 'header-line-format '(t (:eval lsp--headerline-breadcrumb-string)))

    (add-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb nil t)
    (add-hook 'lsp-configure-hook #'lsp--headerline-enable-breadcrumb nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp--headerline-disable-breadcrumb nil t))
   (t
    (remove-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb t)
    (remove-hook 'lsp-configure-hook #'lsp--headerline-enable-breadcrumb t)
    (remove-hook 'lsp-unconfigure-hook #'lsp--headerline-disable-breadcrumb t)

    (setq lsp--headerline-breadcrumb-path-up-to-project-string nil)
    (setq header-line-format (remove '(t (:eval lsp--headerline-breadcrumb-string)) header-line-format)))))

;;;###autoload
(defun lsp-breadcrumb-go-to-symbol (symbol-position)
  "Go to the symbol on breadcrumb at SYMBOL-POSITION."
  (interactive "P")
  (if (numberp symbol-position)
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp-symbols->symbols-hierarchy symbols)))
              (lsp--headerline-breadcrumb-go-to-symbol (nth (1- symbol-position) symbols-hierarchy))
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
                     (symbols-hierarchy (lsp-symbols->symbols-hierarchy symbols)))
              (lsp--headerline-breadcrumb-narrow-to-symbol (nth (1- symbol-position) symbols-hierarchy))
            (lsp--info "Symbol not found for position %s" symbol-position))
        (lsp--info "Server does not support breadcrumb."))
    (lsp--info "Call this function with a number representing the symbol position on breadcrumb")))

(provide 'lsp-headerline)
;;; lsp-headerline.el ends here
