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

;;

;;; Code:


(require 'lsp-protocol)
(require 'ert)

(defun lsp-test-interface ()
  (lsp-interface (MyPosition (:line :character :camelCase) (:optional)))

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

    (should (not (lsp-my-position? position)))
    (lsp:set-my-position-camel-case position "camel-case")
    (should (not (lsp-my-position? "xx")))
    (should (lsp-my-position? position))

    (-let (((&MyPosition? :line) nil))
      (should (null line)))))

(ert-deftest lsp-test-lsp-interface ()
  (let (lsp-use-plists)
    (lsp-test-interface)))

(ert-deftest lsp-test-lsp-interface-plist ()
  (let ((lsp-use-plists t))
    (lsp-test-interface)))



(provide 'lsp-protocol-test)
;;; lsp-protocol-test.el ends here
