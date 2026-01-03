;;; lsp-protocol-test.el --- lsp-protocol tests      -*- lexical-binding: t; -*-

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

;; Tests for lsp-protocol.

;;; Code:


(require 'lsp-protocol)
(require 'ert)
(require 'seq)

(eval-and-compile
  (lsp-interface (MyPosition (:line :character :camelCase) (:optional)))
  (lsp-interface (MyRange (:start :end) nil))
  (lsp-interface (MyExtendedRange (:start :end :specialProperty) nil)))

(ert-deftest lsp-test-lsp-interface ()
  (let ((position (lsp-make-my-position :character 1 :line 2)))
    (should (eq 2 (lsp:my-position-line position)))

    (-let (((&MyPosition :line) position))
      (should (eq 2 line)))

    (lsp-put position :_external "external")
    (should (string= "external" (lsp-get position :_external)))
    (-let (((&MyPosition :_external ext) position))
      (should (string= "external" ext)))

    (lsp:set-my-position-optional? position "opt")
    (-let (((&MyPosition :optional?) position))
      (should (string= "opt" optional?)))

    (-let (((&MyPosition? :optional?) nil))
      (should (null optional?)))

    (should-not (lsp-my-position? position))
    (lsp:set-my-position-camel-case position "camel-case")
    (should-not (lsp-my-position? "xx"))
    (should (lsp-my-position? position))

    (-let (((&MyPosition? :line) nil))
      (should (null line)))

    (should (= (-> (lsp-make-my-position :character 1 :line 2)
                   (lsp:set-my-position-line 100)
                   (lsp:my-position-line))
               100))
    (should (lsp-my-position? (lsp-make-my-position :character nil :line 2 :camel-case nil)))

    (should (lsp-my-position? (lsp-make-my-position :character nil :line 2 :camelCase nil)))))

(ert-deftest lsp-test-pcase-patterns ()
  (let ((particular-range (lsp-make-my-range :start
                                             (lsp-make-my-position :line 10 :character 20 :camelCase nil)
                                             :end
                                             (lsp-make-my-position :line 30 :character 40 :camelCase nil)))
        (particular-extended-range
         (lsp-make-my-extended-range :start
                                     (lsp-make-my-position :line 10 :character 20 :camelCase nil)
                                     :end
                                     (lsp-make-my-position :line 30 :character 40 :camelCase nil)
                                     :specialProperty 42)))
    (should (pcase particular-range
              ((lsp-interface MyRange
                              :start (lsp-interface MyPosition
                                                    :line start-line :character start-char :camel-case start-camelcase)
                              :end (lsp-interface MyPosition
                                                  :line end-line :character end-char :camel-case end-camelCase))
               t)
              (_ nil)))

    (should (pcase particular-extended-range
              ((lsp-interface MyExtendedRange)
               t)
              (_ nil)))

    ;; a subclass can be matched by a pattern for a parent class
    (should (pcase particular-extended-range
              ((lsp-interface MyRange
                              :start (lsp-interface MyPosition
                                                    :line start-line :character start-char :camel-case start-camelcase)
                              :end  (lsp-interface MyPosition
                                                   :line end-line :character end-char :camel-case end-camelCase))
               t)
              (_ nil)))

    ;; the new patterns should be able to be used with existing ones
    (should (pcase (list particular-range
                         particular-extended-range)
              ((seq (lsp-interface MyRange)
                    (lsp-interface MyExtendedRange))
               t)
              (_ nil)))

    ;; the existing seq pattern should detect that the ranges are
    ;; not in the order specified by the inner patterns
    (should-not (pcase (list particular-range
                             particular-extended-range)
                  ((seq (lsp-interface MyExtendedRange)
                        (lsp-interface MyRange))
                   t)
                  (_ nil)))

    ;; when a binding appears more than once, then the first
    ;; occurrence binds if it can match, and the subsequent
    ;; occurrences turn into equality checks.  Since :character
    ;; appears twice as a key name, the first instance binds it to 20,
    ;; and the second instance is an equality check against the other
    ;; :character value, which is different.
    (should-not (pcase particular-range
                  ((lsp-interface MyRange
                                  :start (lsp-interface MyPosition
                                                        :line start-line :character :camel-case start-camelcase)
                                  :end  (lsp-interface MyPosition
                                                       :line end-line :character :camel-case end-camelCase))
                   t)
                  (_ nil)))

    ;; if an optional property is requested when it does not exist, we
    ;; should still match if the required stuff matches. Missing
    ;; optional properties are bound to nil.
    (should (pcase particular-range
              ((lsp-interface MyRange :start (lsp-interface MyPosition :optional?))
               (null optional?))
              (_ nil)))

    ;; we cannot request a key (whether or not it is optional) not in
    ;; the interface, even if the expr-val has all the types specified
    ;; by the interface. This is a programmer error.
    (should-error (pcase particular-range
                    ((lsp-interface MyRange :something-unrelated)
                     t)
                    (_ nil)))

    ;; we do not use camelCase at this stage. This is a programmer error.
    (should-error (pcase particular-range
                    ((lsp-interface MyRange :start (lsp-interface MyPosition :camelCase))
                     t)
                    (_ nil)))
    (should (pcase particular-range
              ((lsp-interface MyRange :start (lsp-interface MyPosition :camel-case))
               t)
              (_ nil)))

    ;; :end is missing, so we should fail to match the interface.
    (should-not (pcase (lsp-make-my-range :start (lsp-make-my-position :line 10 :character 20 :camelCase nil))
                  ((lsp-interface MyRange)
                   t)
                  (_ nil)))))

(ert-deftest lsp-test-member? ()
  (let ((input (if lsp-use-plists
                   (list :import_for_trait_assoc_item nil)
                 (ht ("import_for_trait_assoc_item" nil)))))
    (should (lsp-member? input :import_for_trait_assoc_item))
    (lsp-put input :import_for_trait_assoc_item :json-false)
    (should (eq (lsp-get input :import_for_trait_assoc_item) :json-false))))

(ert-deftest lsp-test-inline-completion-pcase-patterns ()
  "Regression test for issue #4723: InlineCompletion pcase patterns.
Verify that InlineCompletion interfaces can be used in pcase patterns
without `Eager macro-expansion failure' errors."
  ;; Test InlineCompletionItem with required property
  (let ((item (lsp-make-inline-completion-item :insert-text "test completion")))
    (should (pcase item
              ((lsp-interface InlineCompletionItem :insert-text text)
               (string= text "test completion"))
              (_ nil))))

  ;; Test InlineCompletionItem with optional properties (nil values)
  (let ((item-with-opts (lsp-make-inline-completion-item
                         :insert-text "completion"
                         :filter-text? "filter"
                         :command? nil)))
    (should (pcase item-with-opts
              ((lsp-interface InlineCompletionItem :insert-text text :filter-text? filter)
               (and (string= text "completion")
                    (string= filter "filter")))
              (_ nil))))

  ;; Test InlineCompletionList
  (let ((list (lsp-make-inline-completion-list
               :items (vector (lsp-make-inline-completion-item
                               :insert-text "completion 1")))))
    (should (pcase list
              ((lsp-interface InlineCompletionList :items items)
               (and (vectorp items) (= (length items) 1)))
              (_ nil))))

  ;; Test InlineCompletionContext with invoked trigger
  (let ((context-invoked (lsp-make-inline-completion-context
                          :trigger-kind lsp/inline-completion-trigger-invoked)))
    (should (pcase context-invoked
              ((lsp-interface InlineCompletionContext :trigger-kind kind)
               (= kind lsp/inline-completion-trigger-invoked))
              (_ nil))))

  ;; Test InlineCompletionContext with automatic trigger
  (let ((context-auto (lsp-make-inline-completion-context
                       :trigger-kind lsp/inline-completion-trigger-automatic)))
    (should (pcase context-auto
              ((lsp-interface InlineCompletionContext :trigger-kind kind)
               (= kind lsp/inline-completion-trigger-automatic))
              (_ nil))))

  ;; Test InlineCompletionParams with nested context
  (let ((params (lsp-make-inline-completion-params
                 :text-document (lsp-make-text-document-identifier :uri "file:///test.rs")
                 :position (lsp-make-position :line 10 :character 5)
                 :context (lsp-make-inline-completion-context
                           :trigger-kind lsp/inline-completion-trigger-invoked))))
    (should (pcase params
              ((lsp-interface InlineCompletionParams :text-document doc :position pos :context ctx)
               (and (lsp-text-document-identifier? doc)
                    (lsp-position? pos)
                    (lsp-inline-completion-context? ctx)))
              (_ nil)))))

;;; lsp-protocol-test.el ends here
