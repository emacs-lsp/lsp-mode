;;; lsp-cmake.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, cmake

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

;; LSP Clients for the CMake build tool.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-cmake nil
  "LSP support for CMake, using cmake-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/regen100/cmake-language-server"))

(defcustom lsp-cmake-server-command "cmake-language-server"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'lsp-cmake
  :package-version '(lsp-mode . "8.0.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     lsp-cmake-server-command))
                  :activation-fn (lsp-activate-on "cmake")
                  :priority -1
                  :server-id 'cmakels))

(lsp-consistency-check lsp-cmake)

(provide 'lsp-cmake)
;;; lsp-cmake.el ends here
