;;; lsp-cpp.el --- Specific functionality for C++ language servers -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daniel Martín
;; URL: https://github.com/emacs-lsp/lsp-mode
;; Keywords: languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains specific configuration settings for C++ language
;; servers.
;;
;; ** Clang-tidy Flycheck integration (Clangd) **
;;
;; If you invoke `flycheck-display-error-explanation' on a
;; `clang-tidy' error (if Clangd is configured to show `clang-tidy'
;; diagnostics), Emacs will open a detailed explanation about the
;; message by querying the LLVM website. As an embedded web browser is
;; used to show the documentation, this feature requires that Emacs is
;; compiled with libxml2 support.

;;; Code:

(require 'dom)
(eval-when-compile (require 'subr-x))

(defvar flycheck-explain-error-buffer)
(declare-function flycheck-error-id "ext:flycheck" (err))

(defun lsp-cpp-flycheck-clang-tidy--skip-http-headers ()
  "Position point just after HTTP headers."
  (re-search-forward "^$"))

(defun lsp-cpp-flycheck-clang-tidy--narrow-to-http-body ()
  "Narrow the current buffer to contain the body of an HTTP response."
  (lsp-cpp-flycheck-clang-tidy--skip-http-headers)
  (narrow-to-region (point) (point-max)))

(defun lsp-cpp-flycheck-clang-tidy--decode-region-as-utf8 (start end)
  "Decode a region from START to END in UTF-8."
  (condition-case nil
      (decode-coding-region start end 'utf-8)
    (coding-system-error nil)))

(defun lsp-cpp-flycheck-clang-tidy--remove-crlf ()
  "Remove carriage return and line feeds from the current buffer."
  (save-excursion
    (while (re-search-forward "\r$" nil t)
      (replace-match "" t t))))

(defun lsp-cpp-flycheck-clang-tidy--extract-relevant-doc-section ()
  "Extract the parts of the LLVM clang-tidy documentation that are relevant.

This function assumes that the current buffer contains the result
of browsing 'clang.llvm.org', as returned by `url-retrieve'.
More concretely, this function returns the main <div> element
with class 'section', and also removes 'headerlinks'."
  (goto-char (point-min))
  (lsp-cpp-flycheck-clang-tidy--narrow-to-http-body)
  (lsp-cpp-flycheck-clang-tidy--decode-region-as-utf8 (point-min) (point-max))
  (lsp-cpp-flycheck-clang-tidy--remove-crlf)
  (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
         (section (dom-by-class dom "section")))
    ;; `dom-remove-node' was added in 2016. Some Emacs version may not
    ;; have it.
    (when (fboundp 'dom-remove-node)
      (dolist (headerlink (dom-by-class section "headerlink"))
        (dom-remove-node section headerlink)))
    section))

(defun lsp-cpp-flycheck-clang-tidy--explain-error (explanation &rest args)
  "Explain an error in the Flycheck error explanation buffer using EXPLANATION.

EXPLANATION is a function with optional ARGS that, when
evaluated, inserts the content in the appropriate Flycheck
buffer."
  (with-current-buffer flycheck-explain-error-buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (apply explanation args)
      (goto-char (point-min)))))

(defun lsp-cpp-flycheck-clang-tidy--show-documentation (error-id)
  "Show clang-tidy documentation about ERROR-ID.

Information comes from the clang.llvm.org website."
  (url-retrieve (format
                 "https://clang.llvm.org/extra/clang-tidy/checks/%s.html" error-id)
                (lambda (status)
                  (if-let ((error-status (plist-get status :error)))
                      (lsp-cpp-flycheck-clang-tidy--explain-error
                       #'insert
                       (format
                        "Error accessing clang-tidy documentation: %s"
                        (error-message-string error-status)))
                    (let ((doc-contents
                           (lsp-cpp-flycheck-clang-tidy--extract-relevant-doc-section)))
                      (lsp-cpp-flycheck-clang-tidy--explain-error
                       #'shr-insert-document doc-contents)))))
  "Loading documentation...")

;;;###autoload
(defun lsp-cpp-flycheck-clang-tidy-error-explainer (error)
  "Explain a clang-tidy ERROR by scraping documentation from llvm.org."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (if-let (clang-tidy-error-id (flycheck-error-id error))
      (condition-case err
          (lsp-cpp-flycheck-clang-tidy--show-documentation clang-tidy-error-id)
        (error
         (format
          "Error accessing clang-tidy documentation: %s"
          (error-message-string err))))
    (error "The clang-tidy error message does not contain an [error-id]")))

(provide 'lsp-cpp)
;;; lsp-cpp.el ends here
