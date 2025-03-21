;;; lsp-ocaml.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, ocaml

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

;; LSP Clients for the Ocaml Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'find-file)

(defgroup lsp-ocaml nil
  "LSP support for OCaml, using ocaml-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/freebroccolo/ocaml-language-server"))

(define-obsolete-variable-alias
  'lsp-ocaml-ocaml-lang-server-command
  'lsp-ocaml-lang-server-command
  "lsp-mode 6.1")

(defcustom lsp-ocaml-lang-server-command
  '("ocaml-language-server" "--stdio")
  "Command to start ocaml-language-server."
  :group 'lsp-ocaml
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-ocaml-lang-server-command))
                  :major-modes '(reason-mode caml-mode tuareg-mode)
                  :priority -1
                  :server-id 'ocaml-ls))

(defgroup lsp-ocaml-lsp-server nil
  "LSP support for OCaml, using ocaml-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ocaml/ocaml-lsp"))

(define-obsolete-variable-alias 'lsp-merlin 'lsp-ocaml-lsp-server "lsp-mode 6.1")
(define-obsolete-variable-alias 'lsp-merlin-command 'lsp-ocaml-lsp-server-command "lsp-mode 6.1")

(defcustom lsp-ocaml-lsp-server-command
  '("ocamllsp")
  "Command to start ocaml-lsp-server."
  :group 'lsp-ocaml-lsp-server
  :type '(choice
          (string :tag "Single string value")
          (repeat :tag "List of string values"
                  string)))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (lambda () lsp-ocaml-lsp-server-command))
  :major-modes '(reason-mode caml-mode tuareg-mode)
  :priority 0
  :server-id 'ocaml-lsp-server))

(defcustom lsp-cut-signature 'space
  "If non-nil, signatures returned on hover will not be split on newline."
  :group 'lsp-ocaml-lsp-server
  :type '(choice (symbol :tag "Default behaviour" 'cut)
                 (symbol :tag "Display all the lines with spaces" 'space)))

(defcustom lsp-ocaml-markupkind 'markdown
  "Preferred markup format"
  :group 'lsp-ocaml-lsp-server
  :type '(choice (symbol :tag "Markdown" 'markdown)
                 (symbol :tag "Plain text" 'plaintext)))

(defcustom lsp-ocaml-enclosing-type-verbosity 1
  "Number of expansions of aliases in answers."
  :group 'lsp-ocaml-lsp-server
  :type 'int)

;;; -------------------
;;; OCaml-lsp extensions interface
;;; -------------------

;;; The following functions are used to create an interface between custom OCaml-lsp requests and lsp-mode

(defun lsp-ocaml--switch-impl-intf ()
  "Switch to the file(s) that the current file can switch to.

OCaml-lsp LSP protocol documented here
https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/docs/ocamllsp/switchImplIntf-spec.md"
  (-if-let* ((params (lsp-make-ocaml-lsp-switch-impl-intf-params
                      :uri (lsp--buffer-uri)))
             (uris (lsp-request "ocamllsp/switchImplIntf" params)))
      uris
    (lsp--warn "Your version of ocaml-lsp doesn't support the switchImplIntf extension")))

(defun lsp-ocaml--type-enclosing (verbosity index)
  "Get the type of the identifier at point.

OCaml-lsp protocol documented here
https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/docs/ocamllsp/typeEnclosing-spec.md"
  (-if-let* ((params (lsp-make-ocaml-lsp-type-enclosing-params
                      :uri (lsp--buffer-uri)
                      :at (lsp--cur-position)
                      :index index
                      :verbosity verbosity))
             (result (lsp-request "ocamllsp/typeEnclosing" params)))
      result
    (lsp--warn "Your version of ocaml-lsp doesn't support the typeEnclosing extension")))

(defun lsp-ocaml--get-documentation (identifier content-format)
  "Get the documentation of IDENTIFIER or the identifier at point if IDENTIFIER.

OCaml-lsp protocol documented here
https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/docs/ocamllsp/getDocumentation-spec.md"
  (-if-let* ((position (if identifier nil (lsp--cur-position)))
             ((&TextDocumentPositionParams :text-document :position) (lsp--text-document-position-params identifier position))
             (params (lsp-make-ocaml-lsp-get-documentation-params
                      :textDocument text-document
                      :position position
                      :contentFormat content-format)))
      ;; Don't exit if the request returns nil, an identifier can have no documentation
      (lsp-request "ocamllsp/getDocumentation" params)
    (lsp--warn "Your version of ocaml-lsp doesn't support the getDocumentation extension")))

;;; -------------------
;;; OCaml-lsp general utilities
;;; -------------------

(defun lsp-ocaml--has-one-element-p (lst)
  "Returns t if LST contains only one element."
  (and lst (= (length lst) 1)))

;;; -------------------
;;; OCaml-lsp URI utilities
;;; -------------------

(defun lsp-ocaml--load-uri (uri &optional other-window)
  "Check if URI exists and opens its buffer or create a new one.

If OTHER-WINDOW is not nil, opens the buffer in an other window."
  (let ((path (lsp--uri-to-path uri)))
    (cond

     ;; A buffer already exists with PATH
     ((bufferp (get-file-buffer path))
      (ff-switch-to-buffer (get-file-buffer path) other-window)
      path)

     ;; PATH is an existing file
     ((file-exists-p path)
      (ff-find-file path other-window nil)
      path)

     ;; PATH is not an existing file
     (t
      nil))))

(defun lsp-ocaml--find-alternate-uri ()
  "Returns the URI corresponding to the alternate file if there's only one or prompt for a choice."
  (let ((uris (lsp-ocaml--switch-impl-intf)))
    (if (lsp-ocaml--has-one-element-p uris)
        (car uris)
      (let* ((filenames (mapcar #'f-filename uris))
             (selected-file (completing-read "Choose an alternate file " filenames)))
        (nth (cl-position selected-file filenames :test #'string=) uris)))))

;;; -------------------
;;; OCaml-lsp type enclosing utilities
;;; ------------------

(defvar-local lsp-ocaml--type-enclosing-verbosity)
(defvar-local lsp-ocaml--type-enclosing-index)
(defvar-local lsp-ocaml--type-enclosing-saved-type)

(defun lsp-ocaml--init-type-enclosing-config ()
  "Create a new config for the type enclosing requests."
  (setq lsp-ocaml--type-enclosing-verbosity lsp-ocaml-enclosing-type-verbosity)
  (setq lsp-ocaml--type-enclosing-index 0)
  (setq lsp-ocaml--type-enclosing-saved-type nil))


;;; -------------------
;;; OCaml-lsp type enclosing transient map
;;; -------------------

(defvar lsp-ocaml-type-enclosing-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<up>") #'lsp-ocaml--type-enclosing-go-up)
    (define-key keymap (kbd "C-<down>") #'lsp-ocaml--type-enclosing-go-down)
    (define-key keymap (kbd "C-w") #'lsp-ocaml--type-enclosing-copy)
    (define-key keymap (kbd "C-t") #'lsp-ocaml--type-enclosing-increase-verbosity)
    (define-key keymap (kbd "C-<right>") #'lsp-ocaml--type-enclosing-increase-verbosity)
    (define-key keymap (kbd "C-<left>") #'lsp-ocaml--type-enclosing-decrease-verbosity)
    keymap)
  "Keymap for OCaml-lsp type enclosing transient mode.")

(defun lsp-ocaml--type-enclosing-decrease-verbosity ()
  "Decreases the number of expansions of aliases in answer."
  (interactive)
  (let ((verbosity (max 0 (1- lsp-ocaml--type-enclosing-verbosity))))
    (setq lsp-ocaml--type-enclosing-verbosity verbosity))
  (lsp-ocaml--get-and-display-type-enclosing))

(defun lsp-ocaml--type-enclosing-increase-verbosity ()
  "Increases the number of expansions of aliases in answer."
  (interactive)
  (let ((verbosity (1+ lsp-ocaml--type-enclosing-verbosity)))
    (setq lsp-ocaml--type-enclosing-verbosity verbosity))
  (lsp-ocaml--get-and-display-type-enclosing t))

(defun lsp-ocaml--type-enclosing-copy ()
  "Copy the type of the saved enclosing type to the kill-ring."
  (interactive)
  (when lsp-ocaml--type-enclosing-saved-type
    (message "Copied `%s' to kill-ring"
             lsp-ocaml--type-enclosing-saved-type)
    (kill-new lsp-ocaml--type-enclosing-saved-type)))

(defun lsp-ocaml--get-and-display-type-enclosing (&optional increased-verbosity)
  "Compute the type enclosing request.

If INCREASED-VERBOSITY is t, if the computed type is the same as the previous one, decrease the verbosity.
This allows to make sure that we don't increase infinitely the verbosity."
  (-let* ((verbosity lsp-ocaml--type-enclosing-verbosity)
          (index lsp-ocaml--type-enclosing-index)
          (saved-type lsp-ocaml--type-enclosing-saved-type)
          (type_result (lsp-ocaml--type-enclosing verbosity index))
          ((&ocaml-lsp:TypeEnclosingResult :type :enclosings) type_result)
          ;; Get documentation informations
          (markupkind (symbol-name lsp-ocaml-markupkind))
          (doc_result (lsp-ocaml--get-documentation nil markupkind))
          (doc (cl-getf (cl-getf doc_result :doc) :value))
          ;; Regroup the type and documentation at point
          (contents `(:kind ,markupkind
                            :value ,(mapconcat #'identity `("```ocaml" ,type "```" "***" ,doc) "\n"))))
    (when (and increased-verbosity
               (string= type lsp-ocaml--type-enclosing-saved-type))
      (setq lsp-ocaml--type-enclosing-verbosity (1- verbosity)))
    (setq lsp-ocaml--type-enclosing-saved-type type)
    (lsp--display-contents contents)
    contents))

;;; -------------------
;;; OCaml-lsp extensions
;;; -------------------

;;; The following functions are interactive implementations of the OCaml-lsp requests

(defun lsp-ocaml-find-alternate-file ()
  "Returns the URI corresponding to the alternate file if there's only one or prompt for a choice."
  (interactive)
  (let ((uri (lsp-ocaml--find-alternate-uri)))
    (unless (lsp-ocaml--load-uri uri nil)
      (message "No alternate file %s could be found for %s" (f-filename uri) (buffer-name)))))

(defun lsp-ocaml-type-enclosing ()
  "Returns the type of the indent at point."
  (interactive)
  (lsp-ocaml--init-type-enclosing-config)
  (when-let* ((contents (lsp-ocaml--get-and-display-type-enclosing)))
    (set-transient-map lsp-ocaml-type-enclosing-map t)))

(lsp-consistency-check lsp-ocaml)

(provide 'lsp-ocaml)
;;; lsp-ocaml.el ends here
