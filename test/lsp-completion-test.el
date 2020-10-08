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

(ert-deftest lsp-completion-test-candidate-kind ()
  (should (eq (lsp-completion--candidate-kind
               (propertize " " 'lsp-completion-item
                           (lsp-make-completion-item :kind 3)))
              'function)))

(ert-deftest lsp-completion-test-fuz-score ()
  (cl-labels ((do-test (query cands expected)
                       (should (equal
                                (sort cands
                                      (lambda (l r) (> (lsp-completion--fuz-score query l)
                                                       (lsp-completion--fuz-score query r))))
                                expected))))
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

(provide 'lsp-completion-test)
;;; lsp-completion-test.el ends here
