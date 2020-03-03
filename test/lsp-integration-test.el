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

(defun lsp-def-request-async (method params &rest args)
  (--doto (deferred:new #'identity)
    (apply #'lsp-request-async method params (-partial #'deferred:callback-post it)
           :error-handler (-partial #'deferred:errorback-post it)
           args)))

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

(defmacro deferred::nextc (d &rest body)
  `(deferred:nextc ,d (lambda (result)
                        ,@body)))

(defmacro lsp-with-pyls (&rest body)
  `(progn
     (lsp-workspace-folders-add (f-join lsp-test-location "fixtures"))
     (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
     (lsp)
     ,@body

     (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
     (save-buffer)
     (kill-buffer)
     (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures"))))

(ert-deftest lsp-text-document-hover-request-tick ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'tick)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (insert "x")
           (delete-char -1)))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(ert-deftest lsp-test-current-buffer-mode ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'current)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (switch-to-buffer "*scratch*")
           (let ((post-command-hook (->> (f-join lsp-test-location "fixtures/pyls/test.py")
                                         find-buffer-visiting
                                         (buffer-local-value 'post-command-hook))))
             (run-hooks 'post-command-hook))))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(ert-deftest lsp-test-current-buffer-mode ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'alive)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (kill-buffer (current-buffer))))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(ert-deftest lsp-test-current-buffer-mode ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'alive)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (-> (f-join lsp-test-location "fixtures/pyls/test.py")
               (find-buffer-visiting)
               (kill-buffer))))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(provide 'lsp-integration-test)
;;; lsp-integration-test.el ends here
