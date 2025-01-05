;;; lsp-test-utils.el --- unit test utilities -*- lexical-binding: t -*-

;; Copyright (C) emacs-lsp maintainers

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

;; This file contains definitions required by other test units.  Reusable
;; utilities like functions, macros, and customizables should prefer to be
;; defined here to avoid having to load a unit test directly, because Emacs
;; checks for reused ERT test names since 29.1, which may also be caused by
;; loading the same test unit multiple times.  See also discussion in
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66782

;;; Code:

(defun lsp-test--wait-for (form &optional d)
  (--doto (or d (deferred:new #'identity))
    (run-with-timer
     0.001 nil
     (lambda ()
       (if-let* ((result (eval form)))
           (deferred:callback-post it result)
         (lsp-test--wait-for form it))))))

(defmacro lsp-test-wait (form)
  `(lsp-test--wait-for '(progn ,form)))

(provide 'lsp-test-utils)
;;; lsp-test-utils.el ends here
