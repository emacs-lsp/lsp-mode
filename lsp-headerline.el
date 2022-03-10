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

(require 'lsp-icons)
(require 'lsp-mode)

(defgroup lsp-headerline nil
  "LSP support for headerline"
  :prefix "lsp-headerline-"
  :group 'lsp-mode
  :tag "LSP Headerline")

(defcustom lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
  "Segments used in breadcrumb text on headerline."
  :type '(repeat
          (choice (const :tag "Include the project name." project)
                  (const :tag "Include the open file name." file)
                  (const :tag "Include the directories up to project." path-up-to-project)
                  (const :tag "Include document symbols if server supports it." symbols)))
  :group 'lsp-headerline)

(defcustom lsp-headerline-breadcrumb-enable-symbol-numbers nil
  "Whether to label symbols with numbers on the breadcrumb."
  :type 'boolean
  :group 'lsp-headerline)

(defcustom lsp-headerline-breadcrumb-enable-diagnostics t
  "If non-nil, apply different face on the breadcrumb based on the errors."
  :type 'boolean
  :group 'lsp-headerline
  :package-version '(lsp-mode . "8.0.0"))

(defface lsp-headerline-breadcrumb-separator-face '((t :inherit shadow :height 0.8))
  "Face used for breadcrumb separator on headerline."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-path-face '((t :inherit font-lock-string-face))
  "Face used for breadcrumb paths on headerline."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-path-error-face
  '((t :underline (:style wave :color "Red1")
       :inherit lsp-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an error under that path"
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-path-warning-face
  '((t :underline (:style wave :color "Yellow")
       :inherit lsp-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an warning under that path"
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-path-info-face
  '((t :underline (:style wave :color "Green")
       :inherit lsp-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an info under that path"
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-path-hint-face
  '((t :underline (:style wave :color "Green")
       :inherit lsp-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an hint under that path"
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-project-prefix-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lsp-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-unknown-project-prefix-face
  '((t :inherit shadow :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lsp-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-symbols-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face used for breadcrumb symbols text on headerline."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-symbols-error-face
  '((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Red1")))
  "Face used for breadcrumb symbols text on headerline when there
is an error in symbols range."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-symbols-warning-face
  '((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Yellow")))
  "Face used for breadcrumb symbols text on headerline when there
is an warning in symbols range."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-symbols-info-face
  '((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Green")))
  "Face used for breadcrumb symbols text on headerline when there
is an info in symbols range."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-symbols-hint-face
  '((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Green")))
  "Face used for breadcrumb symbols text on headerline when there
is an hints in symbols range."
  :group 'lsp-headerline)

(defface lsp-headerline-breadcrumb-deprecated-face
  '((t :inherit lsp-headerline-breadcrumb-symbols-face
       :strike-through t))
  "Face used on breadcrumb deprecated text on modeline."
  :group 'lsp-headerline)

(defvar-local lsp-headerline--string nil
  "Holds the current breadcrumb string on headerline.")

(defvar lsp-headerline-arrow nil
  "Holds the current breadcrumb string on headerline.")

(defvar-local lsp-headerline--path-up-to-project-segments nil
  "Holds the current breadcrumb path-up-to-project segments for
caching purposes.")

(defun lsp-headerline--arrow-icon ()
  "Build the arrow icon for headerline breadcrumb."
  (or
   lsp-headerline-arrow
   (setq lsp-headerline-arrow (lsp-icons-all-the-icons-material-icon
                               "chevron_right"
                               'lsp-headerline-breadcrumb-separator-face
                               ">"
                               'headerline-breadcrumb))))

(lsp-defun lsp-headerline--symbol-icon ((&DocumentSymbol :kind))
  "Build the SYMBOL icon for headerline breadcrumb."
  (concat (lsp-icons-get-by-symbol-kind kind 'headerline-breadcrumb)
          " "))

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
  "Assign LOCAL-MAP and HELP-ECHO-STRING to the region around the DISPLAY-STRING."
  (propertize display-string
              'mouse-face 'header-line-highlight
              'help-echo help-echo-string
              'local-map local-map))

(defmacro lsp-headerline--make-mouse-handler (&rest body)
  "Making mouse event handler.
Switch to current mouse interacting window before doing BODY."
  (declare (debug t) (indent 0))
  `(lambda (event)
     (interactive "e")
     (select-window (posn-window (elt event 1)))
     ,@body))

(defun lsp-headerline--directory-with-action (full-path directory-display-string)
  "Build action for FULL-PATH and DIRECTORY-DISPLAY-STRING."
  (lsp-headerline--with-action (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1]
                                   (lsp-headerline--make-mouse-handler
                                     (dired full-path)))
                                 (define-key map [header-line mouse-2]
                                   (lsp-headerline--make-mouse-handler
                                     (dired-other-window full-path)))
                                 map)
                               (format "mouse-1: browse '%s' with Dired\nmouse-2: browse '%s' with Dired in other window"
                                       directory-display-string
                                       directory-display-string)
                               (propertize directory-display-string
                                           'lsp-full-path full-path)))

(declare-function evil-set-jump "ext:evil-jumps")

(lsp-defun lsp-headerline--symbol-with-action ((symbol &as &DocumentSymbol :name) symbol-display-string)
  "Build action for SYMBOL and SYMBOL-STRING."
  (lsp-headerline--with-action (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1]
                                   (lsp-headerline--make-mouse-handler
                                     (when (bound-and-true-p evil-mode)
                                       (evil-set-jump))
                                     (lsp-headerline--go-to-symbol symbol)))
                                 (define-key map [header-line mouse-2]
                                   (lsp-headerline--make-mouse-handler
                                     (-let (((&DocumentSymbol :range (&RangeToPoint :start :end)) symbol))
                                       (if (and (eq (point-min) start) (eq (point-max) end))
                                           (widen)
                                         (lsp-headerline--narrow-to-symbol symbol)))))
                                 map)
                               (format "mouse-1: go to '%s' symbol\nmouse-2: %s"
                                       name
                                       (-let (((&DocumentSymbol :range (&RangeToPoint :start :end)) symbol))
                                         (if (and (eq (point-min) start) (eq (point-max) end))
                                             "widen"
                                           (format "narrow to '%s' range" name))))
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
  (let* ((file-path (or (buffer-file-name) ""))
         (filename (f-filename file-path)))
    (if-let ((file-ext (f-ext file-path)))
        (concat (lsp-icons-get-by-file-ext file-ext 'headerline-breadcrumb)
                " "
                (propertize filename
                            'font-lock-face
                            (lsp-headerline--face-for-path file-path)))
      filename)))


(defun lsp-headerline--face-for-path (dir)
  "Calculate the face for DIR."
  (if-let ((diags (lsp-diagnostics-stats-for (directory-file-name dir))))
      (cl-labels ((check-severity
                   (severity)
                   (not (zerop (aref diags severity)))))
        (cond
         ((not lsp-headerline-breadcrumb-enable-diagnostics)
          'lsp-headerline-breadcrumb-path-face)
         ((check-severity lsp/diagnostic-severity-error)
          'lsp-headerline-breadcrumb-path-error-face)
         ((check-severity lsp/diagnostic-severity-warning)
          'lsp-headerline-breadcrumb-path-warning-face)
         ((check-severity lsp/diagnostic-severity-information)
          'lsp-headerline-breadcrumb-path-info-face)
         ((check-severity lsp/diagnostic-severity-hint)
          'lsp-headerline-breadcrumb-path-hint-face)
         (t 'lsp-headerline-breadcrumb-path-face)))
    'lsp-headerline-breadcrumb-path-face))

(defun lsp-headerline--severity-level-for-range (range)
  "Get the severiy level for RANGE."
  (let ((range-severity 10))
    (mapc (-lambda ((&Diagnostic :range (&Range :start) :severity?))
            (when (lsp-point-in-range? start range)
              (setq range-severity (min range-severity severity?))))
          (lsp--get-buffer-diagnostics))
    range-severity))

(defun lsp-headerline--build-path-up-to-project-string ()
  "Build the path-up-to-project segment for the breadcrumb."
  (if-let ((root (lsp-workspace-root)))
      (let ((segments (or
                       lsp-headerline--path-up-to-project-segments
                       (setq lsp-headerline--path-up-to-project-segments
                             (lsp-headerline--path-up-to-project-root
                              root
                              (lsp-f-parent (buffer-file-name)))))))
        (mapconcat (lambda (next-dir)
                     (propertize next-dir
                                 'font-lock-face
                                 (lsp-headerline--face-for-path
                                  (get-text-property
                                   0 'lsp-full-path next-dir))))
                   segments
                   (concat " " (lsp-headerline--arrow-icon) " ")))
    ""))

(lsp-defun lsp-headerline--face-for-symbol ((&DocumentSymbol :deprecated?
                                                             :range))
  "Get the face for SYMBOL."
  (let ((range-severity (lsp-headerline--severity-level-for-range range)))
    (cond
     (deprecated? 'lsp-headerline-breadcrumb-deprecated-face)
     ((not lsp-headerline-breadcrumb-enable-diagnostics)
      'lsp-headerline-breadcrumb-symbols-face)
     ((= range-severity lsp/diagnostic-severity-error)
      'lsp-headerline-breadcrumb-symbols-error-face)
     ((= range-severity lsp/diagnostic-severity-warning)
      'lsp-headerline-breadcrumb-symbols-warning-face)
     ((= range-severity lsp/diagnostic-severity-information)
      'lsp-headerline-breadcrumb-symbols-info-face)
     ((= range-severity lsp/diagnostic-severity-hint)
      'lsp-headerline-breadcrumb-symbols-hint-face)
     (t 'lsp-headerline-breadcrumb-symbols-face))))

(defun lsp-headerline--build-symbol-string ()
  "Build the symbol segment for the breadcrumb."
  (if (lsp-feature? "textDocument/documentSymbol")
      (-if-let* ((lsp--document-symbols-request-async t)
                 (symbols (lsp--get-document-symbols))
                 (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols))
                 (enumerated-symbols-hierarchy
                  (-map-indexed (lambda (index elt)
                                  (cons elt (1+ index)))
                                symbols-hierarchy)))
          (mapconcat
           (-lambda (((symbol &as &DocumentSymbol :name)
                      . index))
             (let* ((symbol2-name
                     (propertize name
                                 'font-lock-face
                                 (lsp-headerline--face-for-symbol symbol)))
                    (symbol2-icon (lsp-headerline--symbol-icon symbol))
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
               (lsp-headerline--symbol-with-action symbol full-symbol-2)))
           enumerated-symbols-hierarchy
           (concat " " (lsp-headerline--arrow-icon) " "))
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
               ('path-up-to-project (lsp-headerline--build-path-up-to-project-string))
               ('symbols (lsp-headerline--build-symbol-string))
               (_ (lsp-log "'%s' is not a valid entry for `lsp-headerline-breadcrumb-segments'"
                           (symbol-name segment))
                  ""))))
        (if (eq segment-string "")
            ""
          (concat (lsp-headerline--arrow-icon)
                  " "
                  segment-string
                  " "))))
    lsp-headerline-breadcrumb-segments
    "")))

(defun lsp-headerline--check-breadcrumb (&rest _)
  "Request for document symbols to build the breadcrumb."
  (setq lsp-headerline--string (lsp-headerline--build-string))
  (force-mode-line-update))

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
  :group 'lsp-headerline
  :global nil
  (cond
   (lsp-headerline-breadcrumb-mode
    ;; make sure header-line-format, if non-nil, is a list.  as
    ;; mode-line-format says: "The value may be nil, a string, a
    ;; symbol or a list."
    (unless (listp header-line-format)
      (setq header-line-format (list header-line-format)))
    (add-to-list 'header-line-format '(t (:eval lsp-headerline--string)))

    (add-hook 'xref-after-jump-hook #'lsp-headerline--check-breadcrumb nil t)

    (add-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb nil t)
    (add-hook 'lsp-configure-hook #'lsp-headerline--enable-breadcrumb nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-headerline--disable-breadcrumb nil t))
   (t
    (remove-hook 'lsp-on-idle-hook #'lsp-headerline--check-breadcrumb t)
    (remove-hook 'lsp-configure-hook #'lsp-headerline--enable-breadcrumb t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-headerline--disable-breadcrumb t)

    (remove-hook 'xref-after-jump-hook #'lsp-headerline--check-breadcrumb t)

    (setq lsp-headerline--path-up-to-project-segments nil)
    (setq header-line-format (remove '(t (:eval lsp-headerline--string)) header-line-format)))))

;;;###autoload
(defun lsp-breadcrumb-go-to-symbol (symbol-position)
  "Go to the symbol on breadcrumb at SYMBOL-POSITION."
  (interactive "P")
  (if (numberp symbol-position)
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols)))
              (lsp-headerline--go-to-symbol (nth (1- symbol-position) symbols-hierarchy))
            (lsp--info "Symbol not found for position %s" symbol-position))
        (lsp--info "Server does not support breadcrumb."))
    (lsp--info "Call this function with a number representing the symbol position on breadcrumb")))

(declare-function evil-set-command-property "ext:evil-common")

(with-eval-after-load 'evil
  (evil-set-command-property 'lsp-breadcrumb-go-to-symbol :jump t))

;;;###autoload
(defun lsp-breadcrumb-narrow-to-symbol (symbol-position)
  "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION."
  (interactive "P")
  (if (numberp symbol-position)
      (if (lsp-feature? "textDocument/documentSymbol")
          (-if-let* ((lsp--document-symbols-request-async t)
                     (symbols (lsp--get-document-symbols))
                     (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols)))
              (lsp-headerline--narrow-to-symbol (nth (1- symbol-position) symbols-hierarchy))
            (lsp--info "Symbol not found for position %s" symbol-position))
        (lsp--info "Server does not support breadcrumb."))
    (lsp--info "Call this function with a number representing the symbol position on breadcrumb")))

(lsp-consistency-check lsp-headerline)

(provide 'lsp-headerline)
;;; lsp-headerline.el ends here
