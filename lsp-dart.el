;;; lsp-dart.el --- LSP mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Takeshi Tsukamoto

;; Author: Takeshi Tsukamoto <itometeam@gmail.com>
;; Keywords: languages

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

;; dart analysis server client

;;; Code:

(require 'lsp-mode)
(require 'ht)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server")
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk/"
  "Install directory for dart-sdk."
  :group 'lsp-dart
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-server-command nil
  "The analysis_server executable to use"
  :type '(repeat string)
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-dart-only-analyze-projects-with-open-files t
  "When set to non-nil, analysis will only be performed for projects that have open files
rather than the root workspace folder. Defaults to t"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-dart-suggest-from-unimported-libraries t
  "When set to nil, completion will not include synbols that are not already
imported into the current file. Defaults to true"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-dart-closing-labels t
  "When set to non-nil, dart/textDocument/publishClosingLabel notifications will
be sent with information to render editor closing labels. Defaults to nil"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-closing-labels-prefix " "
  "The prefix string to be concatened with the closing label.
Defaults to a single space"
  :type 'string
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-closing-labels-size 0.9
  "The font size factor to be multiplied by the closing labels font size.
Defaults to 0.9"
  :type 'float
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-outline nil
  "When set to non-nil, dart/textDocument/publishOutline notifications will
be sent with outline information for open files. Defaults to t"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-flutter-outline nil
  "When set to non-nil, dart/textDocument/publishFlutterOutline notifications will
be sent with Flutter outline information for open files. Defaults to t"
  :type 'boolean
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

;; lsp-treemacs

(defvar treemacs-position 'left)
(defvar treemacs-width 35)

(declare-function lsp-treemacs--open-file-in-mru "ext:lsp-treemacs" (file))
(declare-function lsp-treemacs-render "ext:lsp-treemacs" (tree title expand? &optional buffer-name right-click-actions))

(defcustom lsp-dart-outline-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The outline tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-dart-flutter-outline-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The Flutter outline tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart
  :package-version '(lsp-mode . "6.3"))

(defun lsp-dart--outline-kind->icon (kind)
  "Maps an outline KIND to a treemacs icon symbol.
Kinds from https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/spec/generated/java/types/ElementKind.java"
  (pcase kind
    ("CLASS" 'class)
    ("CLASS_TYPE_ALIAS" 'class)
    ("COMPILATION_UNIT" 'document)
    ("FIELD" 'field)
    ("METHOD" 'method)
    ("CONSTRUCTOR" 'namespace)
    ("CONSTRUCTOR_INVOCATION" 'namespace)
    ("GETTER" 'property)
    ("SETTER" 'property)
    ("TOP_LEVEL_VARIABLE" 'constant)
    ("FUNCTION" 'method)
    ("FUNCTION_INVOCATION" 'method)
    ("FUNCTION_TYPE_ALIAS" 'method)
    ("LABEL" 'number)
    ("LIBRARY" 'template)
    ("EXTENSION" 'interface)
    ("LOCAL_VARIABLE" 'field)
    ("MIXIN" 'interface)
    ("PARAMETER" 'string)
    ("TYPE_PARAMETER" 'string)
    ("UNIT_TEST_GROUP" 'structure)
    ("UNIT_TEST_TEST" 'method)
    ("ENUM" 'enumerator)
    ("ENUM_CONSTANT" 'enumitem)))

(defun lsp-dart--outline-tree-ret-action (uri range)
  "Build the ret action for and item in the outline tree view.
URI is the source of the item.
RANGE is the range of positions to where this item should point."
  (interactive)
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point (gethash "start" range)))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-dart--outline->tree (uri items)
  "Builds a outline tree.
URI is the source of the outline.
ITEMS is the outline items data."
  (seq-map (-lambda ((&hash "children"
                            "element" (&hash "kind" "name" "parameters" "range")))
             (let ((label (concat name (when parameters
                                         (propertize (concat " " parameters)
                                                     'face 'lsp-lens-face)))))
               (list :key label
                     :label label
                     :icon (lsp-dart--outline-kind->icon kind)
                     :children (lambda (&rest _)
                                 (unless (seq-empty-p children)
                                   (lsp-dart--outline->tree uri children)))
                     :ret-action (lambda (&rest _) (lsp-dart--outline-tree-ret-action uri range)))))
           items))

(defun lsp-dart--flutter-outline->tree (uri items)
  "Builds a Flutter outline tree.
URI is the source of the outline.
ITEMS is the outline items data."
  (seq-map (lambda (item)
             (-let* (((&hash "children" "kind"
                             "dartElement" element "className" class-name "label" extra-label "codeRange" range) item)
                     (widget? (not (string= kind "DART_ELEMENT")))
                     (label (if widget?
                                (concat class-name " " extra-label)
                              (concat (gethash "name" element)
                                      (when (gethash "parameters" element)
                                        (propertize (concat " " (gethash "parameters" element))
                                                    'face 'lsp-lens-face)))))
                     (icon (if widget?
                               'flutter
                             (lsp-dart--outline-kind->icon (gethash "kind" element)))))
               (list :key label
                     :label label
                     :icon icon
                     :children (lambda (&rest _)
                                 (unless (seq-empty-p children)
                                   (lsp-dart--flutter-outline->tree uri children)))
                     :ret-action (lambda (&rest _) (lsp-dart--outline-tree-ret-action uri range)))))
           items))

(defun lsp-dart--build-outline-tree (uri outline)
  "Render outline view to a URI with OUTLINE data."
  (lsp-treemacs-render
   (lsp-dart--outline->tree uri outline)
   "Outline"
   t
   "*Dart Outline*"))

(defun lsp-dart--build-flutter-outline-tree (uri outline)
  "Render Flutter outline view to a URI with OUTLINE data."
  (lsp-treemacs-render
   (lsp-dart--flutter-outline->tree uri outline)
   "Flutter Outline"
   t
   "*Flutter Outline*"))

(defun lsp-dart--show-outline (ignore-focus?)
  "Shows an outline tree.
Focus on it if IGNORE-FOCUS? is null."
  (-let* (((&hash "uri" "outline" (&hash "children")) (lsp-workspace-get-metadata "current-outline"
                                                                                  (lsp-find-workspace 'lsp-find-workspace)))
          (buffer (lsp-dart--build-outline-tree uri children))
          (window (display-buffer-in-side-window buffer lsp-dart-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--show-flutter-outline (ignore-focus?)
  "Shows an Flutter outline tree.
Focus on it if IGNORE-FOCUS? is null."
  (-let* (((&hash "uri" "outline" (&hash "children")) (lsp-workspace-get-metadata "current-flutter-outline"
                                                                                  (lsp-find-workspace 'lsp-find-workspace)))
          (buffer (lsp-dart--build-flutter-outline-tree uri children))
          (window (display-buffer-in-side-window buffer lsp-dart-flutter-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--handle-outline (workspace params)
  "Outline notification handling.
PARAMS outline notification data sent from WORKSPACE.
It updates the outline view if it already exists."
  (lsp-workspace-set-metadata "current-outline" params workspace)
  (when (get-buffer-window "*Dart Outline*")
    (lsp-dart--show-outline t)))

(defun lsp-dart--handle-flutter-outline (workspace params)
  "Flutter outline notification handling.
PARAMS Flutter outline notification data sent from WORKSPACE.
It updates the Flutter outline view if it already exists."
  (lsp-workspace-set-metadata "current-flutter-outline" params workspace)
  (when (get-buffer-window "*Flutter Outline*")
    (lsp-dart--show-flutter-outline t)))

(defun lsp-dart-show-outline (ignore-focus?)
  "Shows an outline tree and focus on it if IGNORE-FOCUS? is null."
  (interactive "P")
  (lsp-dart--show-outline ignore-focus?))

(defun lsp-dart-show-flutter-outline (ignore-focus?)
  "Shows a Flutter outline tree and focus on it if IGNORE-FOCUS? is null."
  (interactive "P")
  (lsp-dart--show-flutter-outline ignore-focus?))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (or
   lsp-dart-server-command
   `(,(expand-file-name (f-join lsp-dart-sdk-dir "bin/dart"))
     ,(expand-file-name (f-join lsp-dart-sdk-dir "bin/snapshots/analysis_server.dart.snapshot"))
     "--lsp")))

(defun lsp-dart--handle-closing-labels (_workspace params)
  "Closing labels notification handling.
PARAMS closing labels notification data sent from WORKSPACE."
  (-let* (((&hash "uri" "labels") params)
          (buffer (lsp--buffer-for-file (lsp--uri-to-path uri))))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-closing-labels t)
        (seq-doseq (label-ht labels)
          (save-excursion
            (-let* ((label (gethash "label" label-ht))
                    (range (gethash "range" label-ht))
                    ((beg . end) (lsp--range-to-region range))
                    (end-line (progn
                                (goto-char end)
                                (line-end-position)))
                    (overlay (make-overlay beg end-line buffer)))
              (overlay-put overlay 'lsp-dart-closing-labels t)
              (overlay-put overlay 'after-string (propertize (concat lsp-dart-closing-labels-prefix " " label)
                                                             'display `((height ,lsp-dart-closing-labels-size))
                                                             'cursor t
                                                             'font-lock-face 'font-lock-comment-face)))))))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   'lsp-dart--server-command)
                  :major-modes '(dart-mode)
                  :priority -1
                  :initialization-options
                  `((onlyAnalyzeProjectsWithOpenFiles . ,lsp-dart-only-analyze-projects-with-open-files)
                    (suggestFromUnimportedLibraries . ,lsp-dart-suggest-from-unimported-libraries)
                    (closingLabels . ,lsp-dart-closing-labels)
                    (outline . ,lsp-dart-outline)
                    (flutterOutline . ,lsp-dart-flutter-outline))
                  :notification-handlers (ht ("dart/textDocument/publishClosingLabels" 'lsp-dart--handle-closing-labels)
                                             ("dart/textDocument/publishOutline" 'lsp-dart--handle-outline)
                                             ("dart/textDocument/publishFlutterOutline" 'lsp-dart--handle-flutter-outline))
                  :server-id 'dart_analysis_server))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
