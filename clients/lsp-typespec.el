;;; lsp-typespec.el --- Typespec Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024  jeremy.ymeng@gmail.com

;; Author: Jeremy Meng  <jeremy.ymeng@gmail.com>
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

;; lsp-typespec client

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)

(defgroup lsp-typespec nil
  "LSP support for Typespec."
  :link '(url-link "https://github.com/microsoft/typespec/blob/9c95ccda8c84c7c6afa24b2f4b21cf1ecbe680dd/packages/compiler/cmd/tsp-server.js")
  :group 'lsp-mode
  :tag "Lsp Typespec")

(defcustom lsp-typespec-custom-server-command nil
  "The typespec-lisp server command."
  :group 'lsp-typespec
  :risky t
  :type '(repeat string))

(lsp-dependency
 'typespec-lsp
 '(:npm
   :package "@typespec/compiler"
   :path "tsp-server")
 '(:system "tsp-server"))

(defun lsp-typespec--server-executable-path ()
  "Return the typespec-lsp server command."
  (or
   (when-let* ((workspace-folder (lsp-find-session-folder (lsp-session) default-directory)))
     (let ((tsp-server-local-path (f-join workspace-folder "node_modules" ".bin"
                                          (if (eq system-type 'windows-nt) "tsp-server.cmd" "tsp-server"))))
       (when (f-exists? tsp-server-local-path)
         tsp-server-local-path)))
   (executable-find "tsp-server")
   (lsp-package-path 'tsp-server)
   "tsp-server"))

(lsp-register-client
 (make-lsp-client
  :semantic-tokens-faces-overrides '(:types (("docCommentTag" . font-lock-keyword-face)
                                             ("event" . default)))
  :new-connection (lsp-stdio-connection `(,(lsp-typespec--server-executable-path) "--stdio"))
  :activation-fn (lsp-activate-on "typespec")
  :major-modes '(typespec-mode typespec-ts-mode)
  :server-id 'typespec-lsp))

(lsp-consistency-check lsp-typespec)

(defun lsp-typespec-semantic-tokens-refresh (&rest _)
  "Force refresh semantic tokens."
  (when-let* ((workspace (and lsp-semantic-tokens-enable
                              (lsp-find-workspace 'typespec-lsp (buffer-file-name)))))
    (--each (lsp--workspace-buffers workspace)
      (when (lsp-buffer-live-p it)
        (lsp-with-current-buffer it
          (lsp-semantic-tokens--enable))))))

(with-eval-after-load 'typespec
  (when lsp-semantic-tokens-enable
    ;; refresh tokens
    (dolist (hook '(typespec-mode-hook typespec-ts-mode-hook))
      (add-hook hook #'lsp-typespec-semantic-tokens-refresh))))

(provide 'lsp-typespec)
;;; lsp-typespec.el ends here

