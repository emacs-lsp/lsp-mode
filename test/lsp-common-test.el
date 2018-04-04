;;; lsp-common-test.el --- unit tests for lsp-io.el -*- lexical-binding: t -*-

;; Copyright (C) 2017  Lukas Fuermetz <fuermetz@mailbox.org>.

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

;;; Code:

(require 'ert)
(require 'lsp-common)

(ert-deftest lsp--path-to-uri ()
  (let ((lsp--uri-file-prefix "file:///"))
    (should (equal (lsp--path-to-uri "c:/Users/?/") "file:///c:/Users/%3F/")))
  (let ((lsp--uri-file-prefix "file://"))
    (should (equal (lsp--path-to-uri "/root/file/hallo welt") "file:///root/file/hallo%20welt"))))

(ert-deftest lsp--path-to-uri ()
  (let ((lsp--uri-file-prefix "file:///")
        (system-type 'windows-nt))
    (should (equal (lsp--uri-to-path "file:///c:/Users/%7B%7D/") "c:/Users/{}/")))
  (let ((lsp--uri-file-prefix "file://"))
    (should (equal (lsp--uri-to-path "/root/%5E/%60") "/root/^/`"))))

(ert-deftest lsp-common-test--path-to-uri-custom-schemes ()
  (let* ((client (make-lsp--client))
         (lsp--cur-workspace (make-lsp--workspace :client client)))
    (lsp-client-register-uri-handler  client "custom" (lambda (_) "file-path"))
    (should (equal (lsp--uri-to-path "custom://file-path") "file-path"))))

(ert-deftest lsp-common-test--unexpected-scheme ()
  (condition-case err
    (lsp--uri-to-path "will-fail://file-path")
    (error (should (string= (error-message-string err)
                     "Unsupported file scheme: will-fail://file-path")))))

;;; lsp-common-test.el ends here
