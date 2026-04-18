;;; lsp-fish.el --- lsp-mode fish-shell integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 lsp-mode maintainers

;; Author: lsp-mode maintainers
;; Keywords: languages

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

;;  Client for fish, the user-friendly command line shell (https://github.com/fish-shell/fish-shell/)

;;; Code:

(require 'lsp-mode)

(defgroup lsp-fish nil
  "Settings for the fish-shell Language Server."
  :group 'lsp-mode
  :link '(url-link "https://fish-lsp.dev/")
  :package-version '(lsp-mode . "10.0.0"))

(defcustom lsp-fish-executable '("fish-lsp" "start")
  "Command to run the fish-shell language server."
  :group 'lsp-fish
  :risky t
  :type '(repeat string))

(lsp-dependency 'fish-language-server
                '(:system "fish-lsp"))

(lsp-register-client
 (make-lsp-client :server-id 'fish-lsp
                  :new-connection (lsp-stdio-connection (lambda () lsp-fish-executable))
                  :activation-fn (lsp-activate-on "fish")
                  :major-modes '(fish-mode)))

(lsp-consistency-check lsp-fish)

(provide 'lsp-fish)
;;; lsp-fish.el ends here
