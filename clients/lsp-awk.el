;;; lsp-awk.el --- AWK client -*- lexical-binding: t; -*-

;; Copyright (C) 2023 emacs-lsp maintainers

;; Author: Konstantin Kharlamov <Hi-Angel@yandex.ru>
;; Keywords: languages lsp awk

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
;; LSP client for AWK language.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-awk nil
  "LSP support for AWK."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Beaglefoot/awk-language-server"))

(defcustom lsp-awk-executable '("awk-language-server")
  "Command to run the AWK language server."
  :group 'lsp-awk
  :risky t
  :type 'list)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-awk-executable)
  :activation-fn (lsp-activate-on "awk")
  :priority -1
  :server-id 'awkls))

(provide 'lsp-awk)
;;; lsp-awk.el ends here
