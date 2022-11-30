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
  "Command to start ocaml-language-server."
  :group 'lsp-ocaml
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
  :group 'lsp-ocaml
  :type '(choice (symbol :tag "Default behaviour" 'cut)
                 (symbol :tag "Display all the lines with spaces" 'space)))

(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql ocaml-lsp-server)) &optional storable)
  "Extract a representative line from OCaml's CONTENTS, to show in the echo area.
This function splits the content between the signature
and the documentation to display the signature
and truncate it if it's too wide.
The STORABLE argument is used if you want to use this
function to get the type and, for example, kill and yank it.

An example of function using STORABLE is:

  (defun mdrp/lsp-get-type-and-kill ()
    (interactive)
    (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request \"textDocument/hover\")
                    (lsp--send-request)
                    (lsp:hover-contents))))
      (let ((contents (and contents
                    (lsp--render-on-hover-content
                     contents
                     t))))
        (let ((contents
               (pcase (lsp-workspaces)
                 (`(,workspace)
                  (lsp-clients-extract-signature-on-hover
                   contents
                   (lsp--workspace-server-id workspace)
                   t))
                 (lsp-clients-extract-signature-on-hover
                   contents
                   nil)
                 )))
          (message \"Copied %s to kill-ring\" contents)
          (kill-new contents)))))"
  (let ((type (s-trim (lsp--render-element (lsp-make-marked-string
                                            :language "ocaml"
                                            :value (car (s-split "---" (lsp--render-element contents))))))))
    (if (equal nil storable)
        (if (eq lsp-cut-signature 'cut)
            (car (s-lines type))
          ;; else lsp-cut-signature is 'space
          (let ((ntype (s-replace "\n" " " type)))
            (if (>= (length ntype) (frame-width))
                (concat (substring ntype 0 (- (frame-width) 4)) "...")
              ntype)))
      type)))

(lsp-consistency-check lsp-ocaml)

(provide 'lsp-ocaml)
;;; lsp-ocaml.el ends here
