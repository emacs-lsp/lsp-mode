;;; lsp-biome.el --- LSP client for Biome -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pradyuman Vig
;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: Pradyuman Vig <me@pmn.co>
;; Keywords: languages, tools

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

;; LSP client for Biome, using the `biome lsp-proxy' command.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-biome nil
  "LSP support for Biome."
  :group 'lsp-mode
  :link '(url-link "https://biomejs.dev"))

(lsp-defcustom lsp-biome-require-configuration nil
  "Whether Biome requires a configuration file before providing features."
  :type 'boolean
  :group 'lsp-biome
  :lsp-path "biome.requireConfiguration")

(lsp-defcustom lsp-biome-configuration-path nil
  "Path to a preferred Biome configuration file or its directory."
  :type '(choice (const :tag "Automatic" nil)
          string)
  :group 'lsp-biome
  :lsp-path "biome.configurationPath")

(lsp-defcustom lsp-biome-inline-config nil
  "Biome configuration to merge over configuration read from disk."
  :type '(choice (const :tag "None" nil)
          sexp)
  :group 'lsp-biome
  :lsp-path "biome.inlineConfig")

(lsp-defcustom lsp-biome-go-to-definition nil
  "Whether Biome provides go-to-definition support."
  :type 'boolean
  :group 'lsp-biome
  :lsp-path "biome.goToDefinition")

(lsp-dependency 'biome
                '(:system "biome")
                '(:npm :package "@biomejs/biome"
                  :path "biome"))

(defun lsp-biome--server-command ()
  "Return the command used to start the Biome language server."
  (let* ((local-path (concat "node_modules/.bin/biome"
                             (when (eq system-type 'windows-nt) ".cmd")))
         (root (locate-dominating-file default-directory local-path)))
    (list (if root
              (expand-file-name local-path root)
            (lsp-package-path 'biome))
          "lsp-proxy")))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection #'lsp-biome--server-command)
  :activation-fn
  (lsp-activate-on
   "javascript"
   "javascriptreact"
   "typescript"
   "typescriptreact"
   "json"
   "jsonc"
   "css"
   "graphql"
   "html"
   "vue"
   "svelte"
   "astro")
  :server-id 'biome
  :priority -1
  :add-on? t
  :multi-root t
  :initialized-fn
  (lambda (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration
       (lsp-configuration-section "biome"))))
  :synchronize-sections '("biome")
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'biome callback error-callback))))

(lsp-consistency-check lsp-biome)

(provide 'lsp-biome)
;;; lsp-biome.el ends here
