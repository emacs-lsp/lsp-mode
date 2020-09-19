;;; lsp-svelte.el --- LSP Svelte integration -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Stepan Lusnikov

;; Author: Stepan Lusnikov <endenwer@gmail.com>
;; Keywords: lsp svelte

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

;; LSP client for Svelte

;;; Code:

(require 'lsp-mode)

(defgroup lsp-svelte nil
  "LSP support for Svelte."
  :group 'lsp-mode
  :link '(url-link
          "https://github.com/sveltejs/language-tools"))

(lsp-dependency 'svelte-language-server
                '(:system "svelteserver")
                '(:npm :package "svelte-language-server"
                       :path "svelteserver"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'svelte-language-server)
                       "--stdio")))
  :activation-fn (lambda (file-name _mode)
                   (string= (f-ext file-name) "svelte"))
  :server-id 'svelte-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'svelte-language-server callback error-callback))))

(provide 'lsp-svelte)
;;; lsp-svelte.el ends here
