;;; lsp-solargraph.el --- Solargraph server configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;; Code:

(require 'lsp-mode)

(defgroup lsp-solargraph nil
  "LSP support for Ruby, using the Solargraph language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/castwide/solargraph")
  :package-version '(lsp-mode . "6.1"))

;; (defcustom lsp-solargraph-check-gem-version t
;;   "Automatically check if a new version of the Solargraph gem is available."
;;   :type 'boolean)

(defcustom lsp-solargraph-completion t
  "Enable completion"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-hover t
  "Enable hover"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-diagnostics t
  "Enable diagnostics"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-autoformat nil
  "Enable automatic formatting while typing (WARNING: experimental)"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-formatting t
  "Enable document formatting"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-symbols t
  "Enable symbols"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-definitions t
  "Enable definitions (go to, etc.)"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-rename t
  "Enable symbol renaming"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-references t
  "Enable finding references"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-folding t
  "Enable folding ranges"
  :type 'boolean
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-log-level "warn"
  "Level of debug info to log. `warn` is least and `debug` is most."
  :type '(choice (const :tag "warn" "info" "debug"))
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

;; https://github.com/castwide/solargraph#solargraph-and-bundler
(defcustom lsp-solargraph-use-bundler nil
  "Run solargraph under bundler"
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-solargraph-multi-root t
  "If non nil, `solargraph' will be started in multi-root mode."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-solargraph-library-directories
  '("~/.rbenv/" "/usr/lib/ruby/" "~/.rvm/" "~/.gem/")
  "List of directories which will be considered to be libraries."
  :type '(repeat string)
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-solargraph-server-command '("solargraph" "stdio")
  "Command to start yaml-languageserver."
  :type '(repeat string)
  :group 'lsp-solargraph
  :package-version '(lsp-mode . "8.0.1"))

(defun lsp-solargraph--build-command ()
  "Build solargraph command"
  (if lsp-solargraph-use-bundler
      (append '("bundle" "exec") lsp-solargraph-server-command)
    lsp-solargraph-server-command))

(lsp-register-custom-settings
 '(("solargraph.logLevel" lsp-solargraph-log-level)
   ("solargraph.folding" lsp-solargraph-folding t)
   ("solargraph.references" lsp-solargraph-references t)
   ("solargraph.rename" lsp-solargraph-rename t)
   ("solargraph.definitions" lsp-solargraph-definitions t)
   ("solargraph.symbols" lsp-solargraph-symbols t)
   ("solargraph.formatting" lsp-solargraph-formatting t)
   ("solargraph.autoformat" lsp-solargraph-autoformat t)
   ("solargraph.diagnostics" lsp-solargraph-diagnostics t)
   ("solargraph.hover" lsp-solargraph-hover t)
   ("solargraph.completion" lsp-solargraph-completion t)
   ("solargraph.useBundler" lsp-solargraph-use-bundler t)))

;; Ruby
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-solargraph--build-command)
  :major-modes '(ruby-mode enh-ruby-mode)
  :priority -1
  :multi-root lsp-solargraph-multi-root
  :library-folders-fn (lambda (_workspace) lsp-solargraph-library-directories)
  :server-id 'ruby-ls
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "solargraph"))))))

(lsp-consistency-check lsp-solargraph)

(provide 'lsp-solargraph)
;;; lsp-solargraph.el ends here
