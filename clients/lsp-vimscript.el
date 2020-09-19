;;; lsp-vimscript.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, vim, vimscript

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

;; LSP Clients for the VimScript Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-vim nil
  "LSP support for viml using vim-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/iamcco/vim-language-server"))

(defcustom lsp-clients-vim-executable '("vim-language-server" "--stdio")
  "Command to start the vim language server."
  :group 'lsp-vim
  :risky t
  :type 'file)

(defcustom lsp-clients-vim-initialization-options '((iskeyword . "vim iskeyword option")
                                                    (vimruntime . "/usr/bin/vim")
                                                    (runtimepath . "/usr/bin/vim")
                                                    (diagnostic . ((enable . t)))
                                                    (indexes . ((runtimepath . t)
                                                                (gap . 100)
                                                                (count . 3)))
                                                    (suggest . ((fromVimruntime . t)
                                                                (fromRuntimepath . :json-false))))
  "Initialization options for vim language server."
  :group 'lsp-vim
  :type 'alist)

(lsp-dependency 'vim-language-server
                '(:system "vim-language-server")
                '(:npm :package "vim-language-server"
                       :path "vim-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find (cl-first lsp-clients-vim-executable))
                                            (lsp-package-path 'vim-language-server))
                                       ,@(cl-rest lsp-clients-vim-executable))))
                  :major-modes '(vimrc-mode)
                  :priority -1
                  :server-id 'vimls
                  :initialization-options (lambda () lsp-clients-vim-initialization-options)
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'vim-language-server
                                                            callback error-callback))))

(provide 'lsp-vimscript)
;;; lsp-vimscript.el ends here
