;;; lsp-tex.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, tex

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

;; LSP Clients for the Tex Typesetting Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-tex nil
  "LSP support for TeX and friends, using Digestif and texlab."
  :group 'lsp-mode
  :link '(url-link "https://github.com/astoff/digestif/")
  :link '(url-link "https://github.com/latex-lsp/texlab"))

(defcustom lsp-tex-server 'texlab
  "Choose LSP tex server."
  :type '(choice (const :tag "texlab" texlab)
                 (const :tag "digestif" digestif))
  :group 'lsp-tex)

(defcustom lsp-clients-digestif-executable "digestif"
  "Command to start the Digestif language server."
  :group 'lsp-tex
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-digestif-executable)
                  :major-modes '(plain-tex-mode latex-mode context-mode texinfo-mode LaTex-mode)
                  :priority (if (eq lsp-tex-server 'digestif) 1 -1)
                  :server-id 'digestif))

(defcustom lsp-clients-texlab-executable "texlab"
  "Command to start the texlab language server."
  :group 'lsp-tex
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-texlab-executable)
                  :major-modes '(plain-tex-mode latex-mode LaTeX-mode)
                  :priority (if (eq lsp-tex-server 'texlab) 1 -1)
                  :server-id 'texlab))

(lsp-consistency-check lsp-tex)

(provide 'lsp-tex)
;;; lsp-tex.el ends here
