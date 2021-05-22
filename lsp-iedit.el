;;; lsp-iedit.el --- `iedit' integration -*- lexical-binding: t -*-
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

;;; Commentary:

;; This module provides features that allow starting `iedit' on various
;; different lsp-based, semantic units (like documentHighlights, and
;; linkedEditingRanges in the future).

;;; Code:

(require 'lsp-mode)
(require 'dash)

(declare-function iedit-make-occurrence-overlay "iedit-lib" (begin end))
(declare-function iedit-start-buffering "iedit-lib" ())
(declare-function iedit-lib-start "iedit-lib" (mode-exit-func))
(declare-function iedit-done "iedit" ())
(declare-function evil-multiedit-state "evil-multiedit" ())

(defvar iedit-mode)
(defvar iedit-auto-buffering)
(defvar iedit-occurrences-overlays)
(defvar iedit-occurrence-keymap)
(defvar iedit-mode-occurrence-keymap)
(defvar evil-multiedit--dont-recall)

(defun lsp-iedit--on-ranges (ranges)
  "Start an `iedit' operation using RANGES.
RANGES shall be a list of lsp-`&Range's. They can be acquired
from various lsp protocol requests, e.g.
`textDocument/documentHighlight', ...."
  (require 'iedit)
  (unless (seq-empty-p ranges)
    (mapc (-lambda ((&RangeToPoint :start :end))
            (push (iedit-make-occurrence-overlay start end)
                  iedit-occurrences-overlays))
          ranges)
    ;; See `iedit-start'; TODO: upstream this
    (setq iedit-occurrence-keymap iedit-mode-occurrence-keymap)
    (setq iedit-mode t)
    (when iedit-auto-buffering
      (iedit-start-buffering))
    (iedit-lib-start 'iedit-done)
    (run-hooks 'iedit-mode-hook)
    (add-hook 'before-revert-hook 'iedit-done nil t)
    (add-hook 'kbd-macro-termination-hook 'iedit-done nil t)
    (add-hook 'change-major-mode-hook 'iedit-done nil t)
    (add-hook 'iedit-aborting-hook 'iedit-done nil t)
    (message "%d occurrences of \"%s\""
             (seq-length ranges)
             (lsp--range-text (lsp-seq-first ranges)))))

;;;###autoload
(defun lsp-iedit-highlights ()
  "Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'."
  (interactive)
  (let ((highlights (lsp-request "textDocument/documentHighlight"
                                 (lsp--text-document-position-params)))
        (-compare-fn (-lambda ((&Location :range (&Range :start l-start :end l-end))
                                          (&Location :range (&Range :start r-start :end r-end)))
                               (and (lsp--position-equal l-start r-start)
                                    (lsp--position-equal l-end   r-end)))))
    (lsp-iedit--on-ranges (mapcar #'lsp:document-highlight-range (-distinct highlights)))))

;;;###autoload
(defun lsp-evil-multiedit-highlights ()
  "Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'."
  (interactive)
  (require 'evil-multiedit)
  (when (fboundp 'ahs-clear) (ahs-clear))
  (setq evil-multiedit--dont-recall t)
  (lsp-iedit-highlights)
  (evil-multiedit-state))

(lsp-consistency-check lsp-iedit)

(provide 'lsp-iedit)
;;; lsp-iedit.el ends here
