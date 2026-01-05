;;; lsp-astro.el --- lsp-mode astro integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Paweł Kobojek, Luca Fanselau
;; Copyright (C) 2022-2026 emacs-lsp maintainers

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

;;  LSP client for astro-ls

;;; Code:

(require 'lsp-mode)

(defun lsp-astro--get-initialization-options ()
  "Try to get the typescript server path, to supply to the astro language server."
  (let ((library (f-join (lsp-workspace-root) "node_modules/typescript/lib")))
    (if (file-exists-p library)
        `(:typescript (:tsdk ,library))
      (lsp-warn "Unable to find typescript server path for astro-ls. Guessed: %s" library))))

(defgroup lsp-astro nil
  "LSP support for Astro.build, using astro-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/withastro/language-tools"))

(lsp-dependency 'astro-language-server
                '(:system "astroserver")
                '(:npm :package "@astrojs/language-server"
                       :path "astroserver"))


(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
                  :activation-fn (lsp-activate-on "astro")
                  :initialization-options #'lsp-astro--get-initialization-options
                  :server-id 'astro-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'astro-language-server callback error-callback))))

(lsp-consistency-check lsp-astro)

(provide 'lsp-astro)
;;; lsp-astro.el ends here
