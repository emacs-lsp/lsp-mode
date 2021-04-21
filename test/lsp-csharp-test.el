;;; lsp-csharp-test.el --- tests for C# -*- lexical-binding: t -*-

;; Copyright (C) 2021 Saulius Menkevičius <sauliusmenkevicius@fastmail.com>.

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

;; Tests for C# integration.

;;; Code:

(require 'ert)
(require 'lsp-csharp)
(require 'lsp-integration-test)

;;;
;;; Integration tests with the sample project
;;;

(defmacro lsp-in-sample-csharp-project (&rest body)
  "Create a macro to wrap test BODY with.

Add the fixtures/SampleCSharpProject to lsp-workspaces, opens the
Program.cs file and starts lsp. After the test BODY runs - tidy up."
  `(progn
     ;; lsp setup

     ;; snippet complains in logs
     (setq lsp-enable-snippet nil)
     (lsp-workspace-folders-add (f-join lsp-test-location "fixtures/SampleCSharpProject/"))
     (find-file (f-join lsp-test-location "fixtures/SampleCSharpProject/Program.cs"))
     ;; initialise the workspace
     (setq lsp-diagnostic-package :none)
     (setq lsp-log-io t)

     (lsp)
     (sleep-for 2)

     (message "The test body will start in %s" (buffer-file-name))
     ;; run our test body
     ,@body

     ;; lsp tidy up
     (find-file (f-join lsp-test-location "fixtures/SampleCSharpProject/Program.cs"))
     (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures/SampleCSharpProject/"))))

(ert-deftest lsp-clangd-initialised-workspace ()
  (skip-unless (memq system-type '(gnu/linux darwin windows-nt)))
  (lsp-in-sample-csharp-project
   (->
    ;; now check that the workspace has started
    (lsp-test-wait (eq 'initialized
                       (lsp--workspace-status (cl-first (lsp-workspaces)))))
   (deferred:sync!))))

;;; lsp-csharp-test.el ends here
