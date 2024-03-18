;;; lsp-ido.el --- `ido' integration -*- lexical-binding: t -*-
;;
;; Copyright (C) 2021 emacs-lsp maintainers
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

;; This module provides an interactive ido interface to the workspace symbol
;; functionality offered by lsp-mode.

;;; Code:

(require 'ido)
(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-ido nil
  "LSP support for ido-based symbol completion"
  :group 'lsp-mode
  :tag "LSP ido")

(defcustom lsp-ido-symbol-kind-to-string
  ["    "          ; Unknown - 0
   "File"          ; File - 1
   "Modu"          ; Module - 2
   "Nmsp"          ; Namespace - 3
   "Pack"          ; Package - 4
   "Clss"          ; Class - 5
   "Meth"          ; Method - 6
   "Prop"          ; Property - 7
   "Fld "          ; Field - 8
   "Cons"          ; Constructor - 9
   "Enum"          ; Enum - 10
   "Intf"          ; Interface - 11
   "Func"          ; Function - 12
   "Var "          ; Variable - 13
   "Cnst"          ; Constant - 14
   "Str "          ; String - 15
   "Num "          ; Number - 16
   "Bool "         ; Boolean - 17
   "Arr "          ; Array - 18
   "Obj "          ; Object - 19
   "Key "          ; Key - 20
   "Null"          ; Null - 21
   "EmMm"          ; EnumMember - 22
   "Srct"          ; Struct - 23
   "Evnt"          ; Event - 24
   "Op  "          ; Operator - 25
   "TPar"]          ; TypeParameter - 26
  "A vector of 26 items representing the SymbolKind."
  :group 'lsp-ido
  :type '(vector string))

(defcustom lsp-ido-show-symbol-filename
  t
  "Whether to show the project-relative path to a symbol's point of definition."
  :group 'lsp-ido
  :type 'boolean)

(defcustom lsp-ido-show-symbol-kind
  t
  "Whether to show the symbol's kind when showing lsp symbols."
  :group 'lsp-ido
  :type 'boolean)

(eval-when-compile
  (lsp-interface
   (lsp-ido:FormattedSymbolInformation
    (:kind :name :location :textualRepresentation)
    (:containerName :deprecated))))

(lsp-defun lsp-ido--transform-candidate
  ((symbol-information &as &SymbolInformation :kind :location (&Location :uri))
   lsp-ido--results project-root)
  (let* ((sanitized-kind (if (< kind (length lsp-ido-symbol-kind-to-string)) kind 0))
         (type (elt lsp-ido-symbol-kind-to-string sanitized-kind))
         (typestr (if lsp-ido-show-symbol-kind
                      (format "[%s] " type)
                    ""))
         (pathstr (if lsp-ido-show-symbol-filename
                      (propertize (format " . %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                  'face 'font-lock-comment-face)
                    ""))
         (textual-representation
          (lsp-render-symbol-information symbol-information "."))
         (entry (concat typestr textual-representation pathstr)))
    (puthash entry symbol-information lsp-ido--results)))

(lsp-defun lsp-ido--jump-selected-candidate
  ((&SymbolInformation
    :location (&Location :uri :range (&Range :start (&Position :line :character)))))
  "Jump to selected candidate."
  (find-file (lsp--uri-to-path uri))
  (goto-char (point-min))
  (forward-line line)
  (forward-char character))

(defun lsp-ido--workspace-symbol (workspaces query)
  "Search against WORKSPACES based on QUERY."
  (let* ((lsp-ido--results (make-hash-table :test 'equal))
         (workspace-root (lsp-workspace-root))
         (raw-choices
          (with-lsp-workspaces workspaces
            (lsp-request
             "workspace/symbol"
             (lsp-make-workspace-symbol-params :query query)))))
    (mapc (lambda (it)
            (lsp-ido--transform-candidate it lsp-ido--results workspace-root))
          raw-choices)
    lsp-ido--results))

;;;###autoload
(defun lsp-ido-workspace-symbol (arg)
  "`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (let* ((query (if arg "" (read-string "Workspace symbol: ")))
         (hash-table-candidates (lsp-ido--workspace-symbol (lsp-workspaces) query))
         (choice (ido-completing-read
                  "Workspace symbol: "
                  (hash-table-keys hash-table-candidates)
                  nil
                  nil
                  (when arg (thing-at-point 'symbol)))))
    (lsp-ido--jump-selected-candidate (gethash choice hash-table-candidates))))

(lsp-consistency-check lsp-ido)

(provide 'lsp-ido)
;;; lsp-ido.el ends here
