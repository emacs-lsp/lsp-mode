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
(require 'lsp-test-utils)

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


;;;
;;; Integration tests with the sample project
;;;

(defmacro lsp-in-sample-cpp-project (&rest body)
  "Creates a macro to wrap test BODY with.

Add the fixtures/SampleCppProject to lsp-workspaces, opens the main.cpp file
and starts lsp. After the test BODY runs - tidy up."
  `(progn
     ;; lsp setup
     ;; save to temp
     (setq actual-clangd-args lsp-clients-clangd-args)
     ;; HACK until https://github.com/emacs-lsp/lsp-mode/issues/2278 lands
     ;; or we might keep it forever
     (setq lsp-clients-clangd-args '("--compile-commands-dir=build/" "--log=verbose"))

     ;; snippet complains in logs
     (setq lsp-enable-snippet nil)
     (lsp-workspace-folders-add (f-join lsp-test-location "fixtures/SampleCppProject/"))
     (find-file (f-join lsp-test-location "fixtures/SampleCppProject/src/main.cpp"))
     ;; initialise the workspace
     (setq lsp-diagnostic-package :none)
     (setq lsp-log-io t)

     (lsp)
     (sleep-for 2)

     (message "The test body will start in %s" (buffer-file-name))
     ;; run our test body
     ,@body

     ;; lsp tidy up
     (find-file (f-join lsp-test-location "fixtures/SampleCppProject/src/main.cpp"))
     (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures/SampleCppProject/"))
     (setq lsp-clients-clangd-args actual-clangd-args)))

(ert-deftest lsp-clangd-initialised-workspace ()
  (skip-unless (memq system-type '(gnu/linux)))
  (lsp-in-sample-cpp-project
   (->
    ;; now check that the workspace has started
    (lsp-test-wait (eq 'initialized
                       (lsp--workspace-status (cl-first (lsp-workspaces)))))
   (deferred:sync!))
   ))

(ert-deftest lsp-clangd-switch-to-other-from-cpp ()
  (skip-unless (memq system-type '(gnu/linux)))
  (lsp-in-sample-cpp-project
   (->
    (lsp-test-wait (eq 'initialized
                       (lsp--workspace-status (cl-first (lsp-workspaces)))))

    (deferred::nextc
      (should (string= (buffer-name) "main.cpp"))
      (lsp-clangd-find-other-file)
      (should (string= (buffer-name) "main.h"))
      (lsp)
      ;; give enough time for clangd to respond for main.h
      ;; otherwise will get errors like
      ;; The connected server(s) does not support
      ;; method textDocument/switchSourceHeader.
      (sleep-for 1)
      (lsp-clangd-find-other-file)

      (should (string= (buffer-name) "main.cpp")))

    (deferred:sync!))))

(ert-deftest lsp-clangd-switch-to-nonexistent-other ()
  (skip-unless (memq system-type '(gnu/linux)))
  (lsp-in-sample-cpp-project
   (->
    (lsp-test-wait (eq 'initialized
                       (lsp--workspace-status (cl-first (lsp-workspaces)))))

    (deferred::nextc
      (find-file (f-join lsp-test-location "fixtures/SampleCppProject/src/individual_file.cpp"))
      (should (string= (buffer-name) "individual_file.cpp"))
      ;; after opening a new buffer - need to make sure we are in the same lsp session
      (lsp)
      (sleep-for 1)
      (should-error (lsp-clangd-find-other-file) :type 'user-error)

      (should (string= (buffer-name) "individual_file.cpp")))

    (deferred:sync!))))

;;; lsp-clangd-test.el ends here
