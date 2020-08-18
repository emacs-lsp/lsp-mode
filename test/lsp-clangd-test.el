;;; lsp-clangd-test.el --- unit tests for clangd -*- lexical-binding: t -*-

;; Copyright (C) 2019 Daniel Martín <mardani29@yahoo.es>.

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

;;; Commentary:

;; Unit tests for Clangd servers.

;;; Code:

(require 'ert)
(require 'lsp-clangd)

(ert-deftest lsp-clangd-extract-signature-on-hover ()
  (should (string= (lsp-clients-extract-signature-on-hover
                    (lsp-make-markup-content :kind lsp/markup-kind-markdown
                                             :value "Sample\n ```cpp\n// In Function.hpp\nvoid function(int n);\n```")
                    'clangd)
                   "void function(int n);"))
  (should (string= (lsp-clients-extract-signature-on-hover
                    (lsp-make-markup-content :kind lsp/markup-kind-markdown
                                             :value "Sample\n ```cpp\nvoid function(int n);\n```")
                    'clangd)
                   "void function(int n);"))
  (should (string= (lsp-clients-extract-signature-on-hover
                    (lsp-make-markup-content :kind lsp/markup-kind-markdown
                                             :value "Sample\n ```cpp\n   void function(int n);\n```")
                    'clangd)
                   "void function(int n);"))
  (should-error (lsp-clients-extract-signature-on-hover
                 (lsp-make-markup-content :value "Wrong")
                 'clangd)))

(ert-deftest lsp-clients-join-region ()
  (with-temp-buffer
    (insert "void function(int n);")
    (should (string= (lsp-clangd-join-region (point-min) (point-max)) "void function(int n);"))
    (erase-buffer)
    (insert "    void function(int n);")
    (should (string= (lsp-clangd-join-region (point-min) (point-max)) "void function(int n);"))
    (erase-buffer)
    (insert "void foo(int n,
                      int p,
                      int k);")
    (should (string= (lsp-clangd-join-region (point-min) (point-max)) "void foo(int n, int p, int k);"))
    (erase-buffer)
    (insert "void foo(int n,
                  int p,
                  int k);")
    (should (string= (lsp-clangd-join-region (point-min) (point-max)) "void foo(int n, int p, int k);"))))

;;; lsp-clangd-test.el ends here
