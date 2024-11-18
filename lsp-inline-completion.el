;;; lsp-inline-completion.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 emacs-lsp maintainers

;; Author: Rodrigo Kassick
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (ht "2.3") (spinner "1.7.3"))
;; Version: 9.0.1

;; URL: https://github.com/emacs-lsp/lsp-mode
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

;; Inline Completions support
;; Specification here https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_inlineCompletion

;;; Code:

(require 'lsp-mode)
(require 'dash)

(defun lsp-inline-completion--trigger-kind (implicit)
  (plist-put (lsp--text-document-position-params)
             :context (ht ("triggerKind"
                           (if implicit
                               lsp/inline-completion-trigger-automatic
                             lsp/inline-completion-trigger-invoked)))))

;;;###autoload
(defun lsp-inline-completion-get-items (&optional implicit)
  "Calls textDocument_inlineCompletion and returns a list of InlineCompletionItem's"

  (lsp--spinner-start)
  (unwind-protect
      (-some-->
          (lsp-request-while-no-input "textDocument/inlineCompletion"
                                      (lsp-inline-completion--trigger-kind implicit))
        ;; Kludge to workaround multiple backends responding -- it may
        ;; come as a list (multiple servers) or as a single ht (single
        ;; server). Promote it to list and move on
        (if (ht-p it) (list it) it)

        ;; Response may or may not come inside an :items. Parse each
        ;; response to ensure compatibility.
        (map 'list (lambda (elt)
                     (if (lsp-inline-completion-list? elt)
                         (lsp:inline-completion-list-items elt)
                       elt))
             it)

        ;; Join everything into a single list
        (apply 'seq-concatenate `(list ,@it)))

    ;; Clean up
    (lsp--spinner-stop)))

(provide 'lsp-inline-completion)
