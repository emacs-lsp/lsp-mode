;;; lsp-clients-test.el --- unit tests for lsp-clients.el -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'lsp-clients)
(require 'js) ;; Standard mode in Emacs for JS.

(defconst test-location (file-name-directory (or load-file-name buffer-file-name)))

;; Some of the different flavors of the @flow tag we may
;; encounter, and things we don't want to match.
(defconst lsp-flow-regular-tag "// @flow")
(defconst lsp-flow-c-style-tag "/* @flow */")
(defconst lsp-flow-simple-multiline-tag
  "/**
    * @flow
    */")
(defconst lsp-flow-multiline-with-comments-before-tag
  "/**
    * This is a wonderful Flow file.
    *
    * Author: Awesome programmer.
    *
    * @flow
    */")
(defconst lsp-flow-wrong-tag
  "/**
    * @notflow
    */")
(defconst lsp-flow-but-not-in-comment-tag "@flow")

(ert-deftest lsp-flow-regular-tag-detection ()
  (should (with-temp-buffer
            (insert lsp-flow-regular-tag)
            (lsp-clients-flow-tag-string-present-p))))

(ert-deftest lsp-flow-c-style-tag-detection ()
  (should (with-temp-buffer
            (insert lsp-flow-c-style-tag)
            (lsp-clients-flow-tag-string-present-p))))

(ert-deftest lsp-flow-simple-multiline-tag-detection ()
  (should (with-temp-buffer
            (insert lsp-flow-simple-multiline-tag)
            (lsp-clients-flow-tag-string-present-p))))

(ert-deftest lsp-flow-simple-multiline-with-comments-before-tag-detection ()
  (should (with-temp-buffer
            (insert lsp-flow-multiline-with-comments-before-tag)
            (lsp-clients-flow-tag-string-present-p))))

(ert-deftest lsp-flow-wrong-tag-detection ()
  (should (not (with-temp-buffer
                 (insert lsp-flow-wrong-tag)
                 (lsp-clients-flow-tag-string-present-p)))))

(ert-deftest lsp-flow-but-not-in-comment-tag-detection ()
  (should (not (with-temp-buffer
                 (insert lsp-flow-but-not-in-comment-tag)
                 (lsp-clients-flow-tag-string-present-p)))))

(ert-deftest lsp-flow-should-activate-on-flow-project ()
  ;; Set `js-mode' ON and check that a Flow project activates the Flow
  ;; LSP client.
  (let ((major-mode 'js-mode))
    (should (lsp-clients-flow-activate-p (concat test-location "fixtures/SampleFlowProject/src/sample.js") nil))))

(ert-deftest lsp-flow-should-activate-on-flow-project-without-flow-file-comment ()
  (let ((major-mode 'js-mode))
    (should (lsp-clients-flow-activate-p (concat test-location "fixtures/SampleFlowProject/src/sample-without-flow-comment.js") nil))))

(ert-deftest lsp-flow-should-not-activate-if-not-flow-project-or-no-tag ()
  (let ((major-mode 'js-mode))
    (should (not (lsp-clients-flow-activate-p (concat test-location "fixtures/SampleJsProject/src/sample.js") nil)))))

(ert-deftest lsp-flow-should-not-activate-on-typescript-project ()
  ;; Set `js-mode' ON and check that a TypeScript project does not
  ;; activate the Flow LSP client.
  (let ((major-mode 'js-mode))
    (should (not (lsp-clients-flow-activate-p (concat test-location "fixtures/SampleTypeScriptProject/src/sample.ts") nil)))))

(ert-deftest lsp-typescript-javascript-activates-based-on-file-extension ()
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "abc.js"))
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "abc.jsx"))
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "abc.ts"))
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "abc.tsx"))
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "a1.ts"))
  (should (lsp-typescript-javascript-tsx-jsx-activate-p "a1.d.ts"))
  (should (not (lsp-typescript-javascript-tsx-jsx-activate-p "abc.tsxx")))
  (should (not (lsp-typescript-javascript-tsx-jsx-activate-p "abc.jss"))))

(ert-deftest lsp-clients-extract-signature-from-clangd-on-hover ()
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
    (should (string= (lsp-join-region (point-min) (point-max)) "void function(int n);"))
    (erase-buffer)
    (insert "    void function(int n);")
    (should (string= (lsp-join-region (point-min) (point-max)) "void function(int n);"))
    (erase-buffer)
    (insert "void foo(int n,
                      int p,
                      int k);")
    (should (string= (lsp-join-region (point-min) (point-max)) "void foo(int n, int p, int k);"))
    (erase-buffer)
    (insert "void foo(int n,
                  int p,
                  int k);")
    (should (string= (lsp-join-region (point-min) (point-max)) "void foo(int n, int p, int k);"))))

;;; lsp-clients-test.el ends here
