;;; lsp-ron.el --- lsp-mode RON integration -*- lexical-binding: t; -*-

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

;;  Client for RON, Rusty Object notation (https://github.com/ron-rs/ron)

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ron nil
  "Settings for the RON Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/jasonjmcghee/ron-lsp/")
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-ron-executable '("ron-lsp")
  "Command to run the RON language server."
  :group 'lsp-ron
  :risky t
  :type '(repeat string))

(lsp-dependency 'ron-language-server
                '(:system "ron-lsp"))

(lsp-register-client
 (make-lsp-client :server-id 'ron-lsp
                  :new-connection (lsp-stdio-connection (lambda () lsp-ron-executable))
                  :major-modes '(ron-mode)))

(lsp-consistency-check lsp-ron)

(provide 'lsp-ron)
;;; lsp-ron.el ends here
