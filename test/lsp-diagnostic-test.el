;;; lsp-diagnostic-test.el --- unit tests for diagnostic-related routines in lsp-mode -*- lexical-binding: t -*-

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
(require 'lsp-mode)

(defvar lsp--test-diagnostics
  (list
   (make-lsp-diagnostic
   :line 1
   :column 4
   :severity 1)

   (make-lsp-diagnostic
    :line 53
    :column 4
    :severity 2)

   (make-lsp-diagnostic
    :line 46
    :column 7
    :severity 3)

   (make-lsp-diagnostic
    :line 100
    :column 23
    :severity 2)

   (make-lsp-diagnostic
    :line 98
    :column 12
    :severity 3))
  "Some sample `lsp-diagnostic' structures for testing.")

(ert-deftest lsp-diagnostics-by-severity ()
  ;; Check diagnostics of severity 1.
  (should (equal (list
                  (make-lsp-diagnostic :line 1 :column 4 :severity 1))
                 (cdr (assq '1 (lsp-diagnostics-by-severity lsp--test-diagnostics)))))
  ;; Check diagnostics of severity 2.
  (should (equal (list
                   (make-lsp-diagnostic :line 53 :column 4 :severity 2)
                   (make-lsp-diagnostic :line 100 :column 23 :severity 2))
                 (cdr (assq '2 (lsp-diagnostics-by-severity lsp--test-diagnostics)))))
  ;; Check diagnostics of severity 3,
  (should (equal (list
                  (make-lsp-diagnostic :line 46 :column 7 :severity 3)
                  (make-lsp-diagnostic :line 98 :column 12 :severity 3))
                 (cdr (assq '3 (lsp-diagnostics-by-severity lsp--test-diagnostics))))))

(ert-deftest lsp-calculate-diagnostic-statistics ()
  (should (equal "1/2/2" (lsp-calculate-diagnostic-statistics lsp--test-diagnostics)))
  (let ((modified-diagnostics (remove (make-lsp-diagnostic :line 1 :column 4 :severity 1) lsp--test-diagnostics)))
    (should (equal "2/2" (lsp-calculate-diagnostic-statistics modified-diagnostics))))
  (should (equal "" (lsp-calculate-diagnostic-statistics '()))))
