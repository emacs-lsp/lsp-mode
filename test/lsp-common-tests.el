;;; lsp-common-tests.el --- unit tests for lsp-io.el -*- lexical-binding: t -*-

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
  (let ((lsp--uri-file-prefix "file:///"))
    (should (equal (lsp--uri-to-path "file:///c:/Users/%7B%7D/") "c:/Users/{}/")))
  (let ((lsp--uri-file-prefix "file://"))
    (should (equal (lsp--uri-to-path "/root/%5E/%60") "/root/^/`"))))

(provide 'lsp-common-tests)
;;; lsp-common-tests.el ends here
