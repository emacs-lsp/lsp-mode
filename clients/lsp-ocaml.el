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

;;; -------------------
;;; OCaml-lsp custom variables
;;; -------------------

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

;;; -------------------
;;; OCaml-lsp extensions interface
;;; -------------------

;;; The following functions are used to create an interface between custom OCaml-lsp requests and lsp-mode

(defun lsp-ocaml--switch-impl-intf ()
  "Switch to the file(s) that the current file can switch to.

OCaml-lsp custom protocol documented here
https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/docs/ocamllsp/switchImplIntf-spec.md"
  (-if-let* ((params (lsp-make-ocaml-lsp-switch-impl-intf-params
                      :uri (lsp--buffer-uri)))
             (uris (lsp-request "ocamllsp/switchImplIntf" params)))
      uris
    (lsp--warn "Your version of ocaml-lsp doesn't support the switchImplIntf extension")))

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
  "Check if URI exists and open its buffer or create a new one.

If OTHER-WINDOW is not nil, open the buffer in an other window."
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
  "Return the URI corresponding to the alternate file if there's only one or prompt for a choice."
  (let ((uris (lsp-ocaml--switch-impl-intf)))
    (if (lsp-ocaml--has-one-element-p uris)
        (car uris)
      (let* ((filenames (mapcar #'f-filename uris))
             (selected-file (completing-read "Choose an alternate file " filenames)))
        (nth (cl-position selected-file filenames :test #'string=) uris)))))

;;; -------------------
;;; OCaml-lsp extensions
;;; -------------------

;;; The following functions are interactive implementations of the OCaml-lsp requests

(defun lsp-ocaml-find-alternate-file ()
  "Return the URI corresponding to the alternate file if there's only one or prompt for a choice."
  (interactive)
  (let ((uri (lsp-ocaml--find-alternate-uri)))
    (unless (lsp-ocaml--load-uri uri nil)
      (message "No alternate file %s could be found for %s" (f-filename uri) (buffer-name)))))

(lsp-consistency-check lsp-ocaml)

(provide 'lsp-ocaml)
;;; lsp-ocaml.el ends here
