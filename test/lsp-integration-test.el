;;; lsp-integration-test.el --- lsp integration tests   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'lsp-mode)
(require 'f)
(require 'deferred)
(require 'dash)

(defconst lsp-test-location (file-name-directory (or load-file-name buffer-file-name)))

(defun lsp-test--wait-for (form &optional d)
  (--doto (or d (deferred:new #'identity))
    (run-with-timer
     0.001 nil
     (lambda ()
       (if-let (result (eval form))
           (deferred:callback-post it result)
         (lsp-test--wait-for form it))))))

(defmacro lsp-test-wait (form)
  `(lsp-test--wait-for '(progn ,form)))

(defun lsp-def-request-async (method params)
  (--doto (deferred:new #'identity)
    (lsp-request-async method params (-partial #'deferred:callback-post it)
                       :error-handler (-partial #'deferred:errorback-post it))))

(ert-deftest lsp-text-document-hover-request ()
  (lsp-workspace-folders-add (f-join lsp-test-location "fixtures"))
  (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
  (lsp)
  (-> (lsp-test-wait
       (eq 'initialized (lsp--workspace-status
                         (cl-first (lsp-workspaces)))))
      (deferred:nextc (lambda (_)
                        (goto-char (point-min))
                        (search-forward "fn1")
                        (lsp-def-request-async "textDocument/hover"
                                               (lsp--text-document-position-params)) ))
      (deferred:nextc (lambda (contents)
                        (should (hash-table-p contents))))
      (deferred:sync!))
  (kill-buffer)
  (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures")))

(provide 'lsp-integration-test)
;;; lsp-integration-test.el ends here
