;;; lsp-clojure-test.el --- Clojure client tests -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
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

;; Optional tree support must not load tree UI packages during registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lsp-mode)
(require 'lsp-semantic-tokens)

(defconst lsp-clojure-test--client-file (locate-library "lsp-clojure"))

(defun lsp-clojure-test--register (available loaded)
  "Register Clojure with tree library AVAILABLE and feature LOADED.
Assert that registration does not require tree UI packages."
  (let ((lsp-clients (copy-hash-table lsp-clients))
        (tree-features (mapcar #'featurep '(lsp-treemacs treemacs all-the-icons)))
        (real-featurep (symbol-function 'featurep))
        (real-locate (symbol-function 'locate-library))
        (real-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (eq feature 'lsp-treemacs) loaded
                   (funcall real-featurep feature subfeature))))
              ((symbol-function 'locate-library)
               (lambda (library &rest args)
                 (if (equal library "lsp-treemacs")
                     (progn
                       ;; Already loaded support should not need a file.
                       (should-not loaded)
                       (and available "/optional/lsp-treemacs.el"))
                   (apply real-locate library args))))
              ((symbol-function 'require)
               (lambda (feature &rest args)
                 (should-not (memq feature '(lsp-treemacs treemacs all-the-icons)))
                 (apply real-require feature args))))
      (load lsp-clojure-test--client-file nil t))
    (should (equal tree-features
                   (mapcar #'featurep '(lsp-treemacs treemacs all-the-icons))))
    (alist-get 'testTree
               (alist-get 'experimental
                          (lsp--client-custom-capabilities
                           (gethash 'clojure-lsp lsp-clients))))))

(ert-deftest lsp-clojure-register-with-available-tree-library ()
  (should (eq t (lsp-clojure-test--register t nil))))

(ert-deftest lsp-clojure-register-without-tree-library ()
  (should-not (lsp-clojure-test--register nil nil)))

(ert-deftest lsp-clojure-register-with-already-loaded-tree-library ()
  (should (eq t (lsp-clojure-test--register nil t))))

(ert-deftest lsp-clojure-tree-commands-require-support-lazily ()
  (lsp-clojure-test--register t nil)
  (dolist (commands '((lsp-clojure-show-test-tree . lsp-clojure--show-test-tree)
                      (lsp-clojure-show-project-tree . lsp-clojure--show-project-tree)))
    (dolist (available '(nil t))
      (let (required shown)
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (should (eq feature 'lsp-treemacs))
                     (should-not filename)
                     (should noerror)
                     (setq required t)
                     available))
                  ((symbol-function (cdr commands))
                   (lambda (ignore-focus)
                     (should required)
                     (should (eq ignore-focus 'test-focus))
                     (setq shown t))))
          (if available
              (funcall (car commands) 'test-focus)
            (should-error (funcall (car commands) 'test-focus) :type 'error))
          (should required)
          (should (eq shown available)))))))

(ert-deftest lsp-clojure-tree-notification-requires-support-lazily ()
  (lsp-clojure-test--register t nil)
  (dolist (available '(nil t))
    (with-temp-buffer
      (let ((notification (lsp-make-clojure-lsp-test-tree-params :uri "file:///test.clj" :tree nil))
            (buffer (current-buffer))
            required)
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (should (eq feature 'lsp-treemacs))
                     (should-not filename)
                     (should noerror)
                     (setq required t)
                     available))
                  ((symbol-function 'find-buffer-visiting)
                   (lambda (_file) (should required) buffer))
                  ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
          (lsp-clojure--handle-test-tree nil notification)
          (should required)
          (if available
              (should (eq lsp-clojure--test-tree-data notification))
            (should-not lsp-clojure--test-tree-data)))))))

(ert-deftest lsp-clojure-broken-tree-library-errors-only-on-use ()
  ;; Availability is not proof that an optional package can load successfully.
  ;; A broken installation should not break registration of unrelated clients.
  (skip-unless (not (featurep 'lsp-treemacs)))
  (let* ((directory (make-temp-file "lsp-clojure-test-" t))
         (load-path (cons directory load-path))
         (lsp-clients (copy-hash-table lsp-clients)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "lsp-treemacs.el" directory)
            (insert "(error \"Broken optional tree library\")\n"))
          (load lsp-clojure-test--client-file nil t)
          (should (eq t (alist-get 'testTree
                                  (alist-get 'experimental
                                             (lsp--client-custom-capabilities
                                              (gethash 'clojure-lsp lsp-clients))))))
          (should-not (featurep 'lsp-treemacs))
          (should (equal (should-error (lsp-clojure-show-test-tree nil))
                         '(error "Broken optional tree library"))))
      (delete-directory directory t))))

(provide 'lsp-clojure-test)
;;; lsp-clojure-test.el ends here
