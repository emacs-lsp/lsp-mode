;;; lsp-mode-test.el --- unit tests for lsp-mode.el  -*- lexical-binding: t; -*-

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

(require 'lsp-mode)
(require 'lsp-completion)
(require 'ert)

(ert-deftest lsp-mode-test-merging-completion-results ()
  (let ((merged-completions (lsp--merge-results
                             `([,(lsp-make-completion-item :label "$any"
                                                           :kind? 2
                                                           :detail? "method"
                                                           :sort-text? "$any")]
                               ,(lsp:set-completion-list-is-incomplete (lsp-make-completion-list  :items [])
                                                                       nil))
                             "textDocument/completion")))
    (should (lsp-completion-list? merged-completions))
    (should (lsp-completion--sort-completions (lsp:completion-list-items merged-completions)))))

(defun lsp--json-string-equal? (str1 str2)
  "Roughly compare json string STR1 and STR2."
  (equal (sort (split-string str1 "[:\f\t\n\r\v{},]+") #'string-lessp)
         (sort (split-string str2 "[:\f\t\n\r\v{},]+") #'string-lessp)))

(ert-deftest lsp--merge-results ()
  (should (lsp--json-string-equal?
           (json-encode (lsp--merge-results
                         `(,(lsp-make-completion-item
                             :label "1"
                             :documentation? "bcd"
                             :detail? "b"
                             :additional-text-edits?
                             (lsp-make-text-edit :new-text "a"
                                                 :range (lsp-make-range :start 10 :end 11)))
                           ,(lsp-make-completion-item
                             :label "1"
                             :documentation? (lsp-make-markup-content :kind lsp/markup-kind-markdown
                                                                      :value "xyz")
                             :detail? "cd"
                             :additional-text-edits?
                             (lsp-make-text-edit :new-text "b"
                                                 :range (lsp-make-range :start 13 :end 15)))
                           ,(lsp-make-completion-item
                             :label "1"))
                         "completionItem/resolve"))
           "{\"additionalTextEdits\":[{\"newText\":\"a\",\"range\":{\"start\":10,\"end\":11}},{\"newText\":\"b\",\"range\":{\"start\":13,\"end\":15}}],\"detail\":\"b cd\",\"documentation\":{\"kind\":\"markdown\",\"value\":\"bcd\\nxyz\"},\"label\":\"1\"}"))

  (should (lsp--json-string-equal?
           (lsp--json-serialize (lsp--merge-results
                                 `(,(lsp-make-completion-item :label "1")
                                   ,(lsp-make-completion-item :label "1"))
                                 "completionItem/resolve"))
           "{\"label\":\"1\"}")))

(ert-deftest lsp-generate-unique-names ()
  (let ((fn (lsp--create-unique-string-fn)))
    (should (equal (-map fn '("a" "b"))
                   '("a" "b"))))

  (let ((fn (lsp--create-unique-string-fn)))
    (should (equal (-map fn '("a" "a"))
                   '("a" "a (1)"))))

  (let ((fn (lsp--create-unique-string-fn)))
    (should (equal (-map fn '("a" "a" "a"))
                   '("a" "a (1)" "a (2)")))))

(ert-deftest lsp-formatting-params-test ()
  (let ((standard-indent 10))
    (should (equal 10 (symbol-value (lsp--get-indent-width 'python-mode)))))

  (let ((standard-indent 100))
    (should (= 100 (symbol-value (lsp--get-indent-width 'python-mode))))))

(defvar lsp-test-my-var "bar")

(ert-deftest lsp-resolve-value-test ()
  (should (string= "foo" (lsp-resolve-value "foo")))
  (should (string= "bar" (lsp-resolve-value 'lsp-test-my-var )))
  (should (string= "fn-result" (lsp-resolve-value (-const "fn-result" )))))

(provide 'lsp-mode-test)
;;; lsp-mode-test.el ends here
