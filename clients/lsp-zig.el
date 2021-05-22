;;; lsp-zig.el --- lsp-mode Zig integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Riccardo Binetti

;; Author: Riccardo Binetti <rbino@gmx.com>
;; Keywords: languages,tools

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

;;  client for zls, the Zig language server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-zig nil
  "LSP support for Zig via zls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/zigtools/zls"))

(defcustom lsp-zig-zls-executable "zls"
  "The zls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-zig
  :type 'string)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-zig-zls-executable))
  :activation-fn (lsp-activate-on "zig")
  :server-id 'zls))

(lsp-consistency-check lsp-zig)

(provide 'lsp-zig)
;;; lsp-zig.el ends here
