;;; lsp-oxfmt.el --- LSP client for Oxfmt -*- lexical-binding: t; -*-

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

;; LSP client for Oxfmt, using the `oxfmt --lsp' command.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-oxfmt nil
  "LSP support for Oxfmt."
  :group 'lsp-mode
  :link '(url-link "https://oxc.rs/docs/guide/usage/formatter/editors"))

(lsp-defcustom lsp-oxfmt-config-path nil
  "Path to an Oxfmt configuration file.
When set, Oxfmt disables automatic configuration discovery."
  :type '(choice (const :tag "Automatic" nil)
                 string)
  :group 'lsp-oxfmt
  :lsp-path "oxc.fmt.configPath")

(lsp-defcustom lsp-oxfmt-disable-nested-config nil
  "Whether to disable nested Oxfmt configuration discovery.
When nil, use the language server default."
  :type '(choice (const :tag "Server default" nil)
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" :json-false))
  :group 'lsp-oxfmt
  :lsp-path "oxc.fmt.disableNestedConfig")

(lsp-dependency 'oxfmt
                '(:system "oxfmt")
                '(:npm :package "oxfmt"
                       :path "oxfmt"))

(defun lsp-oxfmt--server-command ()
  "Return the command used to start the Oxfmt language server."
  (let* ((local-path (concat "node_modules/.bin/oxfmt"
                             (when (eq system-type 'windows-nt) ".cmd")))
         (root (locate-dominating-file default-directory local-path)))
    (list (if root
              (expand-file-name local-path root)
            (lsp-package-path 'oxfmt))
          "--lsp")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-oxfmt--server-command)
  :activation-fn
  (lsp-activate-on
   "javascript"
   "javascriptreact"
   "typescript"
   "typescriptreact"
   "json"
   "jsonc"
   "css"
   "scss"
   "less"
   "graphql"
   "toml"
   "html"
   "vue"
   "svelte"
   "markdown"
   "mdx"
   "yaml")
  :server-id 'oxfmt
  :priority -1
  :add-on? t
  :multi-root t
  :initialized-fn
  (lambda (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration
       (lsp-configuration-section "oxc"))))
  :synchronize-sections '("oxc")
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'oxfmt callback error-callback))))

(lsp-consistency-check lsp-oxfmt)

(provide 'lsp-oxfmt)
;;; lsp-oxfmt.el ends here
