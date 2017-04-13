;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com> -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Rust Language Server specific code

(defun lsp--rust-rls-command ()
  (let ((rls-root (getenv "RLS_ROOT")))
    (if rls-root
      `("cargo" "+nightly" "run" "--quiet" ,(concat
                                              "--manifest-path="
                                              (concat
                                                (file-name-as-directory
                                                  (expand-file-name rls-root))
                                                "Cargo.toml"))
         "--release")
      "rls")))

(lsp-define-client 'rust-mode "rust" 'stdio
  #'(lambda ()
      (read-directory-name "Workspace root (should contain Cargo.toml): " default-directory))
  :command (lsp--rust-rls-command)
  :name "Rust Language Server")

(lsp-client-on-notification 'rust-mode "rustDocument/diagnosticsBegin"
  #'(lambda (_w _p)))
(lsp-client-on-notification 'rust-mode "rustDocument/diagnosticsEnd"
  #'(lambda (_w _p) (message "")))

(provide 'lsp-rust)
