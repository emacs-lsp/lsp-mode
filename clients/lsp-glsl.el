;;; lsp-glsl.el --- GLSL client -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: languages lsp glsl

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
;;
;; LSP client for the GLSL.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-glsl nil
  "LSP support for GLSL."
  :group 'lsp-mode
  :link '(url-link "https://github.com/svenstaro/glsl-language-server"))

(defcustom lsp-glsl-executable '("glslls" "--stdin")
  "Command to run the GLSL language server."
  :group 'lsp-glsl
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     lsp-glsl-executable))
  :activation-fn (lsp-activate-on "glsl")
  :priority -1
  :server-id 'glslls))

(provide 'lsp-glsl)
;;; lsp-glsl.el ends here
