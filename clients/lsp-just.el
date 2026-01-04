;;; lsp-just.el --- lsp-mode Just integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 lsp-mode maintainers

;; Author: lsp-mode maintainers
;; Keywords: languages

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

;;  Client for Just (https://github.com/terror/just-lsp)

;;; Code:

(require 'lsp-mode)

(defgroup lsp-just nil
  "Settings for the Just Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/terror/just-lsp")
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-just-executable '("just-lsp")
  "Command to run the Just language server."
  :group 'lsp-just
  :risky t
  :type '(repeat string))

(lsp-dependency 'just-language-server
                 '(:system "just-lsp"))

(lsp-register-client
 (make-lsp-client :server-id 'just-lsp
                  :new-connection (lsp-stdio-connection (lambda () lsp-just-executable))
                  :major-modes '(just-mode
                                 just-ts-mode)))

(lsp-consistency-check lsp-just)

(provide 'lsp-just)
;;; lsp-just.el ends here
