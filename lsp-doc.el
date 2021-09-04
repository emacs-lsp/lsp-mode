;;; lsp-doc.el --- LSP doc generator -*- lexical-binding: t; -*-

;; Keywords: languages, tool
;; Package-Requires: ((emacs "26.1") (lsp-mode "7.0.1") (emacs "26.1") (dash "2.18.0") (f "0.20.0") (ht "2.3") (spinner "1.7.3") (markdown-mode "2.3") (lv "0"))
;; Version: 8.0.0

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
(require 's)
(require 'dash)
(require 'seq)
(require 'ht)
(require 'lsp-mode)

;; Clients

(defun lsp-doc--load-all-lsps ()
  "Load all supported LSPs from lsp-mode."
  (seq-do (lambda (package)
            (require package nil t))
          lsp-client-packages))

(defun lsp-doc--load-all-features ()
  "Load all features from lsp-mode."
  (->> (f-files "../")
    (-map #'f-filename)
    (--filter (and (s-suffix? ".el" it)
                   (not (s-prefix? "." it))))
    (-map (lambda (package)
            (require (intern (f-no-ext package)) nil t)))))

(defun lsp-doc--clients ()
  "Return a list of hash-map of all clients."
(let ((json-array-type 'vector)
      (json-object-type 'hash-table)
      (json-false nil))
       (json-read-file "lsp-clients.json")))

(defun lsp-doc--build-manual-doc (client-name)
  "Build manual documentation for CLIENT-NAME."
  (let ((manual-doc-file (file-truename (concat "../manual-language-docs/lsp-" client-name ".md"))))
    (when (file-exists-p manual-doc-file)
        (with-temp-buffer
          (insert-file-contents manual-doc-file)
          (buffer-string)))))

(defun lsp-doc--decorate-value (key value client-name)
  "For a given KEY return a decorated VALUE for CLIENT-NAME."
  (or (pcase key
        ("installation"
         (when value
           (format "`%s`" value)))
        ("lsp-install-server"
         (when value
           (format "This Server supports automatic install.\nInstall this language server with <kbd>M-x</kbd>`lsp-install-server`<kbd>RET</kbd>`%s`<kbd>RET</kbd>." value)))
        ("installation-url"
         (when value
           (format "\n\nFor more instructions on how to install manually, check [here](%s)." value)))
        ("manual-documentation"
         (lsp-doc--build-manual-doc client-name))
        (_ value))
      ""))

(defun lsp-doc--replace-client-placeholders (client)
  "Replace found placeholders for a CLIENT."
  (while (re-search-forward "{{\\([][:word:]\\[.-]+\\)}}" nil t)
    (let* ((key (match-string 1))
           (value (gethash key client))
           (client-name (gethash "name" client)))
      (replace-match (lsp-doc--decorate-value key value client-name)))))

(defun lsp-doc--variables (name)
  "Return all custom variables for a NAME."
  (let* ((group (intern (concat "lsp-" name)))
         (custom-group (get group 'custom-group)))
    (->> custom-group
      (seq-filter (lambda (p)
                    (and (consp p)
                         (or (eq (cadr p) 'custom-variable)
                             (eq (cadr p) 'custom-face)))))
      (seq-map (apply-partially #'car))
      (seq-sort #'string<))))

(defun lsp-doc--pretty-default-value (variable)
  "Return default value for a VARIABLE formatted."
  (let ((default (if (facep variable)
                     (face-default-spec variable)
                   (default-value variable)))
        (type (get variable 'custom-type)))
    (if (and (memq type '(file directory))
             (stringp default))
        (format "%s" (f-short default))
      (format "%s" default))))

(defun lsp-doc--variable->value (variable key)
  "Return a decorated value for a VARIABLE, KEY and a CLIENT."
  (pcase key
    ("name" (symbol-name variable))
    ("type" (if (facep variable)
                "face"
              (format "%s" (get variable 'custom-type))))
    ("default" (lsp-doc--pretty-default-value variable))
    ("documentation" (->> (or (documentation-property variable 'variable-documentation)
                              (documentation-property variable 'face-documentation)
                              "")
                       (replace-regexp-in-string "‘\\|’" "`")))
    (_ "")))

(defun lsp-doc--add-client-variables (client file)
  "Add CLIENT variables to FILE."
  (-let* (((&hash "name" client-name "common-group-name" common-group-name) client)
          (client-variables (append (when common-group-name
                                      (lsp-doc--variables common-group-name))
                                    (lsp-doc--variables client-name))))
    (--each client-variables
      (with-temp-buffer
        (insert-file-contents "../template/lsp-var.md")
        (while (re-search-forward "{{\\([][:word:]\\[.-]+\\)}}" nil t)
          (let* ((key (match-string 1))
                 (value (lsp-doc--variable->value it key)))
            (replace-match value t t)))
        (append-to-file (point-min) (point-max) file)))))

(defun lsp-doc--generate-for-client (client)
  "Generate documentation for CLIENT."
  (-let* (((&hash "name") client)
         (file (file-truename (concat "page/lsp-" name ".md"))))
    (unless (file-exists-p file)
      (copy-file "template/lsp-client.md" file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (lsp-doc--replace-client-placeholders client)
        (save-buffer 0)
        (lsp-doc--add-client-variables client file)))))


;; Features

(defvar lsp-doc--core-features
  '(("Core" . "mode")
    ("Completion" . "completion")
    ("Diagnostics" . "diagnostics")
    ("Headerline" . "headerline")
    ("Modeline" . "modeline")
    ("Lens" . "lens")
    ("Icons" . "icons")
    ("Semantic tokens" . "semantic-tokens"))
  "A list of hash-map of all core features.
Make sure to make mkdocs.yml updated as well.")

(defvar lsp-doc--extension-features
  '(("Dired" . "dired")
    ("Iedit" . "iedit")
    ("Ido" . "ido"))
  "A list of hash-map of all extension features.
Make sure to make mkdocs.yml updated as well.")

(defun lsp-doc--add-feature-variables (group dest-file)
  "Add FEATURE variables from GROUP to DEST-FILE."
  (if-let ((variables (lsp-doc--variables group)))
      (--each variables
        (with-temp-buffer
          (insert-file-contents "../../template/lsp-var.md")
          (while (re-search-forward "{{\\([][:word:]\\[.-]+\\)}}" nil t)
            (let* ((key (match-string 1))
                   (value (lsp-doc--variable->value it key)))
              (replace-match value t t)))
          (append-to-file (point-min) (point-max) dest-file)))
    (with-temp-buffer
      (insert "No custom variables available.\n\n")
      (append-to-file (point-min) (point-max) dest-file))))

(defun lsp-doc--add-feature-core-settings ()
  "Add the core features settings."
  (-each lsp-doc--core-features
    (-lambda ((feature . group))
      (let* ((filename (concat "page/settings/" group ".md"))
             (_ (write-region "" nil filename))
             (dest-file (file-truename filename)))
        (with-current-buffer (find-file-noselect dest-file)
          (with-temp-buffer
            (insert "# " feature "\n\n")
            (append-to-file (point-min) (point-max) dest-file))
          (lsp-doc--add-feature-variables group dest-file)
          (save-buffer 0))))))

(defun lsp-doc--add-feature-extension-settings ()
  "Add the extension feature settings."
  (-each lsp-doc--extension-features
    (-lambda ((feature . group))
      (let* ((filename (concat "page/settings/" group ".md"))
             (_ (write-region "" nil filename))
             (dest-file (file-truename filename)))
        (with-current-buffer (find-file-noselect dest-file)
          (with-temp-buffer
            (insert "# " feature "\n\n")
            (append-to-file (point-min) (point-max) dest-file))
          (lsp-doc--add-feature-variables group dest-file)
          (save-buffer 0))))))

(defun lsp-doc--generate-feature-settings ()
  "Generate core documentation for features."
  (unless (file-exists-p "page/settings")
    (make-directory "page/settings" t))
  (lsp-doc--add-feature-core-settings)
  (lsp-doc--add-feature-extension-settings))


;; Public

(defun lsp-doc-generate ()
  "Generate documentation for all supported LSPs."
  (interactive)
  (lsp-doc--load-all-lsps)
  (lsp-doc--load-all-features)
  (seq-doseq (client (lsp-doc--clients))
    (lsp-doc--generate-for-client client))
  (lsp-doc--generate-feature-settings))

(provide 'lsp-doc)
;;; lsp-doc.el ends here
