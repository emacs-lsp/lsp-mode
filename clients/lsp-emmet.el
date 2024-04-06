;;; lsp-emmet.el --- lsp-mode Emmet integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 emacs-lsp maintainers

;; Author: lsp-mode maintainers
;; Keywords: lsp, emmet

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

;; LSP Client for Emmet

;;; Code:

(require 'lsp-mode)

;;; emmet-ls
(defgroup lsp-emmet-ls nil
  "Settings for emmet-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/aca/emmet-ls")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-emmet-ls-command '("emmet-ls" "--stdio")
  "The command that starts emmet-ls."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-emmet-ls
  :package-version '(lsp-mode . "9.0.0"))

(lsp-dependency 'emmet-ls
                '(:system "emmet-ls")
                '(:npm :package "emmet-ls"
                       :path "emmet-ls"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find (cl-first lsp-emmet-ls-command))
                            (lsp-package-path 'emmet-ls))
                       ,@(cl-rest lsp-emmet-ls-command))))
  :activation-fn (lsp-activate-on "html" "css" "scss" "less" "javascriptreact" "typescriptreact")
  :priority -1
  :add-on? t
  :multi-root t
  :server-id 'emmet-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'emmet-ls callback error-callback))))

(lsp-consistency-check lsp-emmet)

(provide 'lsp-emmet)
;;; lsp-emmet.el ends here
