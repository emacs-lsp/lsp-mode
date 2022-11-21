;;; lsp-astro.el --- lsp-mode astro integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Paweł Kobojek

;; Author: Paweł Kobojek
;; Keywords: languages,astro

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

;;  client for astro-ls

;;; Code:
(require 'lsp-mode)

(defgroup lsp-astro nil
  "LSP support for Astro.build, using astro-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/withastro/language-tools"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
                  :activation-fn (lsp-activate-on "astro")
                  :server-id 'astro-ls))



(lsp-consistency-check lsp-astro)
(provide 'lsp-astro)
;;; lsp-astro.el ends here
