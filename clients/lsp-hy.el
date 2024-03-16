;;; lsp-rpm-spec.el --- lsp-mode integration for Hy -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, hy, hylang

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

;; LSP Client for Hylang

;;; Code:

(require 'lsp-mode)

(defgroup lsp-hy nil
  "LSP support for Hy."
  :group 'lsp-mode
  :link '(url-link ""))

(defcustom lsp-clients-hy-server-executable
  '("hyuga")
  "LSP support for the Hy Programming Language, using the hyuga."
  :group 'lsp-hy
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-hy-server-executable))
                  :activation-fn (lsp-activate-on "hy")
                  :priority 0
                  :completion-in-comments? t
                  :major-modes '(hy-mode)
                  :server-id 'hyuga))

(lsp-consistency-check lsp-hy)

(provide 'lsp-hy)
