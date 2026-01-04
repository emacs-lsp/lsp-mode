;;; lsp-move.el --- MOVE client settings -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dmitri Makarov
;; Copyright (C) 2023-2026 emacs-lsp maintainers

;; Author: Dmitri Makarov
;; Keywords: lsp, move

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

;;; To enable lsp-move include the following lisp code in init.el after
;;; loading lsp-mode
;;;
;;;    (with-eval-after-load 'lsp-mode
;;;      (require 'move-mode)
;;;      (require 'lsp-move)
;;;      (add-hook 'move-mode-hook #'lsp)
;;;
;;; See `lsp-clients-move-analyzer-executable' to customize the path to move-analyzer.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-move nil
  "LSP support for Move."
  :group 'lsp-mode
  :link '(url-link "https://github.com/move-language/move"))

(defcustom lsp-clients-move-analyzer-executable "move-analyzer"
  "The move-analyzer executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-move
  :risky t
  :type 'file)

(defcustom lsp-clients-move-analyzer-args '()
  "Extra arguments for the move-analyzer executable."
  :group 'lsp-move
  :risky t
  :type '(repeat string))

(defun lsp-clients--move-analyzer-command ()
  "Generate the language server startup command."
  `(,lsp-clients-move-analyzer-executable ,@lsp-clients-move-analyzer-args))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   'lsp-clients--move-analyzer-command)
  :activation-fn (lsp-activate-on "move")
  :major-modes '(move-mode)
  :priority -1
  :server-id 'move-analyzer))

(provide 'lsp-move)

;;; lsp-move.el ends here
