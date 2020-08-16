;;; lsp-doc.el --- LSP doc converter -*- lexical-binding: t; -*-

;; Keywords: languages, tool
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (spinner "1.7.3") (markdown-mode "2.3") (lv "0"))
;; Version: 6.4

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

;; Tool for convert elisp files into documentation.

;;; Code:

(require 'f)
(require 'dash)
(require 'seq)
(require 'ht)
(require 'lsp-mode)

(defun lsp-doc--load-all-lsps ()
  "Load all supported LSPs from lsp-mode."
  (seq-do (lambda (package)
            (require package nil t))
          lsp-client-packages))

(defun lsp-doc--clients ()
  "Return a list of hash-map of all clients."
(let ((json-array-type 'vector)
      (json-object-type 'hash-table)
      (json-false nil))
       (json-read-file "lsp-clients.json")))

(defun lsp-doc--client->variables (client)
  "Return all available custom variables from a CLIENT."
  (let ((custom-group (get client 'custom-group)))
    (seq-map
     (apply-partially #'car)
     (seq-filter (lambda (p)
                   (and (consp p)
                        (eq (cadr p) 'custom-variable)))
                 custom-group))))

(defun lsp-doc--decorate-value (key value)
  "For a given KEY return a decorated VALUE."
  (pcase key
    ("installation" (format "`%s`" value))
    ("installation-url" (format "\n\nFor more instructions on how to install, check [here](%s)." value))
    (_ value)))

(defun lsp-doc--replace-placeholders (client)
  "Replace found placeholders for a CLIENT."
  (while (re-search-forward "{{\\([][:word:]\\[.-]+\\)}}" nil t)
    (let* ((key (match-string 1))
           (value (gethash key client)))
      (if value
          (replace-match (lsp-doc--decorate-value key value))
        (replace-match "")))))

(defun lsp-doc--variables (client-name)
  "Return all custom variables for a CLIENT-NAME."
  (let* ((group (intern (concat "lsp-" client-name)))
         (custom-group (get group 'custom-group)))
    (seq-map
     (apply-partially #'car)
     (seq-filter (lambda (p)
                   (and (consp p)
                        (eq (cadr p) 'custom-variable)))
                 custom-group))))

(defun lsp-doc--pretty-default-value (variable)
  "Return default value for a VARIABLE formatted."
  (let ((default (default-value variable))
        (type (get variable 'custom-type)))
    (if (and (memq type '(file directory))
             (stringp default))
        (format "%s" (f-short default))
      (format "%s" default))))

(defun lsp-doc--variable->value (variable key client)
  "Return a decorated value for a VARIABLE, KEY and a CLIENT."
  (pcase key
    ("name" (symbol-name variable))
    ("default" (lsp-doc--pretty-default-value variable))
    ("documentation" (or (documentation-property variable 'variable-documentation)
                         ""))
    (_ "")))

(defun lsp-doc--add-variables (client file)
  "Add CLIENT variables to FILE."
  (-let* (((&hash "name" client-name) client))
    (--each (lsp-doc--variables client-name)
      (with-temp-buffer
        (insert-file-contents "../template/lsp-client-var.md")
        (while (re-search-forward "{{\\([][:word:]\\[.-]+\\)}}" nil t)
          (let* ((key (match-string 1))
                 (value (lsp-doc--variable->value it key client)))
            (replace-match value t t)))
        (append-to-file (point-min) (point-max) file)))))

(defun lsp-doc--generate-for (client)
  "Generate documentation for CLIENT."
  (-let* (((&hash "name") client)
         (file (file-truename (concat "page/lsp-" name ".md"))))
    (unless (file-exists-p file)
      (copy-file "template/lsp-client.md" file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (lsp-doc--replace-placeholders client)
        (save-buffer 0)
        (lsp-doc--add-variables client file)))))

(defun lsp-doc-generate ()
  "Generate documentation for all supported LSPs."
  (interactive)
  (lsp-doc--load-all-lsps)
  (seq-doseq (client (lsp-doc--clients))
    (lsp-doc--generate-for client)))

(provide 'lsp-doc)
;;; lsp-doc.el ends here
