;;; lsp-clojure.el --- Clojure Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Benedek Fazekas

;; Author: Benedek Fazekas <benedek.fazekas@gmail.com>
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

;; lsp-clojure client

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)

(defgroup lsp-clojure nil
  "LSP support for Clojure."
  :link '(url-link "https://github.com/snoe/clojure-lsp")
  :group 'lsp-mode
  :tag "Language Server")

(defcustom lsp-clojure-server-command  '("bash" "-c" "clojure-lsp")
  "The clojure-lisp server command."
  :group 'lsp-clojure
  :risky t
  :type 'list)

;; Refactorings

(defun lsp-clojure--refactoring-call (refactor-name &rest additional-args)
  "Send an executeCommand request for REFACTOR-NAME with ADDITIONAL-ARGS if there are more arguments expected after the line and column numbers."
  (lsp--cur-workspace-check)
  (lsp--send-execute-command refactor-name
                             (apply #'vector
                                    (cl-list* (lsp--buffer-uri)
                                              (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                              (current-column)
                                              additional-args))))

(defun lsp-clojure-add-missing-libspec ()
  "Apply add-missing-libspec refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "add-missing-libspec"))

(defun lsp-clojure-clean-ns ()
  "Apply clean-ns refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "clean-ns"))

(defun lsp-clojure-cycle-coll ()
  "Apply cycle-coll refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-coll"))

(defun lsp-clojure-cycle-privacy ()
  "Apply cycle-privacy refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-privacy"))

(defun lsp-clojure-expand-let ()
  "Apply expand-let refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "expand-let"))

(defun lsp-clojure-extract-function (function-name)
  "Move form at point into a new function named FUNCTION-NAME."
  (interactive "MFunction name: ") ;; Name of the function
  (lsp-clojure--refactoring-call "extract-function" function-name))

(defun lsp-clojure-introduce-let (binding-name)
  "Move form at point into a new let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "introduce-let" binding-name))

(defun lsp-clojure-move-to-let (binding-name)
  "Move form at point into nearest existing let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "move-to-let" binding-name))

(defun lsp-clojure-thread-first ()
  "Apply thread-first refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first"))

(defun lsp-clojure-thread-first-all ()
  "Apply thread-first-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first-all"))

(defun lsp-clojure-thread-last ()
  "Apply thread-last refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last"))

(defun lsp-clojure-thread-last-all ()
  "Apply thread-last-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last-all"))

(defun lsp-clojure-unwind-all ()
  "Apply unwind-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-all"))

(defun lsp-clojure-unwind-thread ()
  "Apply unwind-thread refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-thread"))

(defun lsp-clj--file-in-jar (uri)
  (string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" uri)
  (when-let* ((entry (match-string 3 uri))
              (path (lsp--uri-to-path (match-string 2 uri)))
              (name (format "%s:%s" path entry))
              (content (lsp-send-request (lsp-make-request "clojure/dependencyContents" (list :uri uri)))))
    (if (find-buffer-visiting name)
        (message "buffer %s exists" name)
      (with-current-buffer (generate-new-buffer name)
        (insert content)
        (set-visited-file-name name)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (set-auto-mode)
        (current-buffer)))
    name))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clojure-server-command))
                  :major-modes '(clojure-mode clojurec-mode clojurescript-mode)
                  :uri-handlers (lsp-ht ("jar" #'lsp-clj--file-in-jar))
                  :initialization-options '(:dependency-scheme "jar")
                  :server-id 'clojure-lsp))

(provide 'lsp-clojure)
