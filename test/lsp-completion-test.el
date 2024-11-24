;;; lsp-completion-test.el --- lsp-completion tests  -*- lexical-binding: t; -*-

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

(require 'lsp-completion)
(require 'ert)
(require 'el-mock)

(ert-deftest lsp-completion-test-candidate-kind ()
  (should (eq (lsp-completion--candidate-kind
               (propertize " " 'lsp-completion-item
                           (lsp-make-completion-item :kind 3)))
              'function)))

(ert-deftest lsp-completion-test-fuz-score ()
  (cl-labels ((do-test (query cands expected)
                (let ((completion-ignore-case t))
                  (should (equal
                           (sort cands
                                 (lambda (l r) (> (or (lsp-completion--fuz-score query l) 0)
                                                  (or (lsp-completion--fuz-score query r) 0))))
                           expected)))))
    (do-test "as"
             '("hashCode() : int"
               "asSubclass(Class<U> clazz) : Class<? extends U>")
             '("asSubclass(Class<U> clazz) : Class<? extends U>"
               "hashCode() : int"))
    (do-test "as"
             '("hash-map"
               "as-definition"
               "as-def"
               "As-selection"
               "To-as-expected"
               "amused"
               "subclass-1"
               "superand-sort")
             '("as-definition"    ; Prefix match
               "as-def"           ; Also prefix match (stable)
               "As-selection"     ; case mismatch, rank lower to near next rank
               "hash-map"         ; middle match
               "amused"           ; partial match with prefix match
               "To-as-expected"   ; more in middle match
               "subclass-1"       ; more in middle match
               "superand-sort"    ; partial match without prefix match
               ))
    (do-test "f"
             '("f" "foo" "Foo" "aFoo" "afoo")
             '("f" "foo" "Foo" "afoo" "aFoo"))
    (do-test "foo"
             '("foo" "afoo" "aafoo" "aaafoo" "Foo" "aFoo" "aaFoo" "aaaFoo")
             '("foo" "Foo" "afoo" "aFoo" "aafoo" "aaFoo" "aaafoo" "aaaFoo"))
    (do-test "F"
             '("F" "foo" "Foo" "aFoo" "afoo")
             '("F" "Foo" "foo" "aFoo" "afoo"))
    (do-test "Fo"
             '("Fo" "daFo" "safo")
             '("Fo" "daFo" "safo"))
    (do-test "F"
             '("F" "daFo" "safo")
             '("F" "daFo" "safo"))))

(ert-deftest lsp-completion-test-get-context-trigger-characters-no-cache ()
  (setq lsp-completion--cache nil)
  (cl-labels ((do-get-context (arg)
                              (let ((non-essential arg))
                                (lsp-completion--get-context '("_") nil)))
              (do-test-trigger-kind (arg)
                                    (lsp:completion-context-trigger-kind
                                     (do-get-context arg)))
              (do-test-trigger-character (arg)
                                         (lsp:completion-context-trigger-character?
                                          (do-get-context arg))))
    (mocklet ((lsp-completion--looking-back-trigger-characterp))
      (should (equal (do-test-trigger-kind nil) 1))
      (should (equal (do-test-trigger-character nil) nil))
      (should (equal (do-test-trigger-kind t) 1))
      (should (equal (do-test-trigger-character t) nil)))
    (mocklet ((lsp-completion--looking-back-trigger-characterp => "_"))
      (should (equal (do-test-trigger-kind nil) 1))
      (should (equal (do-test-trigger-character nil) nil))
      (should (equal (do-test-trigger-kind t) 2))
      (should (equal (do-test-trigger-character t) "_")))))

(ert-deftest lsp-completion-test-get-context-incomplete-completion ()
  (setq lsp-completion--cache (list "not-used" :incomplete))
  (cl-labels ((do-get-context (arg same-session?)
                              (let ((non-essential arg))
                                (lsp-completion--get-context '("_") same-session?)))
              (do-test-trigger-kind (arg same-session?)
                                    (lsp:completion-context-trigger-kind
                                     (do-get-context arg same-session?))))
    (mocklet ((lsp-completion--looking-back-trigger-characterp))
      ;; When the user manually invokes completion,
      (should (equal (do-test-trigger-kind nil nil) 1)) ;; and the session is different: expect a new completion
      (should (equal (do-test-trigger-kind nil t) 3)) ;; and the session is the same: expect a continued completion

      ;; When the user is typing a non-trigger character into the buffer,
      (should (equal (do-test-trigger-kind t nil) 1)) ;; and the session is different: expect a new completion
      (should (equal (do-test-trigger-kind t t) 3))) ;; and the session is the same: expect a continued completion

    (mocklet ((lsp-completion--looking-back-trigger-characterp => "_"))
      ;; When the user manually invokes completion,
      (should (equal (do-test-trigger-kind nil nil) 1)) ;; and the session is different: expect a new completion
      (should (equal (do-test-trigger-kind nil t) 3)) ;; and the session is the same: expect a continued completion

      ;; When the user is typing a trigger-character into the buffer,
      (should (equal (do-test-trigger-kind t nil) 2)) ;; and the session is different: expect a new trigger-character completion
      (should (equal (do-test-trigger-kind t t) 2))) ;; and the session is the same: expect a new trigger-character completion
    ))

;;; lsp-completion-test.el ends here
