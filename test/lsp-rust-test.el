;;; lsp-rust-test.el --- unit tests for lsp-rust -*- lexical-binding: t; -*-

;; Copyright (C) 2026 lsp-mode maintainers

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

;; Tests for the lsp-rust client.

;;; Code:

(require 'ert)
(require 'lsp-mode)
(require 'lsp-rust)

(ert-deftest lsp-rust-analyzer-merge-settings-adds-new-keys ()
  (should (equal (lsp-rust-analyzer--merge-settings
                  '(:a 1) '(:b 2))
                 '(:a 1 :b 2))))

(ert-deftest lsp-rust-analyzer-merge-settings-override-wins ()
  (should (equal (lsp-rust-analyzer--merge-settings
                  '(:a 1 :b nil) '(:a 2 :b 3))
                 '(:a 2 :b 3))))

(ert-deftest lsp-rust-analyzer-merge-settings-merges-nested-plists ()
  (should (equal (lsp-rust-analyzer--merge-settings
                  '(:hover (:memoryLayout (:enable t :size "both") :links (:enable t)))
                  '(:hover (:memoryLayout (:enable :json-false))))
                 '(:hover (:memoryLayout (:enable :json-false :size "both")
                           :links (:enable t))))))

(ert-deftest lsp-rust-analyzer-merge-settings-replaces-non-plists ()
  "Vectors and scalars are replaced rather than merged."
  (should (equal (lsp-rust-analyzer--merge-settings
                  '(:cargo (:features ["a" "b"]) :x (:y 1))
                  '(:cargo (:features ["c"]) :x 5))
                 '(:cargo (:features ["c"]) :x 5)))
  (should (equal (lsp-rust-analyzer--merge-settings
                  '(:x nil) '(:x (:y 1)))
                 '(:x (:y 1)))))

(ert-deftest lsp-rust-analyzer-merge-settings-does-not-modify-arguments ()
  (let ((base (list :a (list :b 1) :c 2))
        (override (list :a (list :b 2 :d 3))))
    (lsp-rust-analyzer--merge-settings base override)
    (should (equal base '(:a (:b 1) :c 2)))
    (should (equal override '(:a (:b 2 :d 3))))))

(ert-deftest lsp-rust-analyzer-merge-settings-with-empty-override ()
  (should (equal (lsp-rust-analyzer--merge-settings '(:a (:b 1)) nil)
                 '(:a (:b 1)))))

(ert-deftest lsp-rust-analyzer-init-options-default ()
  "Without extra settings the init options are the derived ones."
  (let ((lsp-rust-analyzer-extra-settings nil))
    (should (equal (lsp-rust-analyzer--init-options)
                   (lsp-rust-analyzer--make-init-options)))))

(ert-deftest lsp-rust-analyzer-init-options-include-extra-settings ()
  (let ((lsp-rust-analyzer-extra-settings
         '(:procMacro (:ignored (:async-trait ["async_trait"]))
           :hover (:memoryLayout (:enable :json-false))
           :completion (:addCallParenthesis :json-false))))
    (let ((opts (lsp-rust-analyzer--init-options)))
      ;; New keys are added ...
      (should (equal (plist-get (plist-get opts :procMacro) :ignored)
                     '(:async-trait ["async_trait"])))
      (should (eq (plist-get (plist-get (plist-get opts :hover) :memoryLayout) :enable)
                  :json-false))
      ;; ... keys derived from the regular variables are kept when merging
      ;; into an existing section ...
      (should (plist-member (plist-get opts :procMacro) :enable))
      (should (plist-member (plist-get opts :completion) :addCallArgumentSnippets))
      ;; ... and extra settings take precedence over them.
      (should (eq (plist-get (plist-get opts :completion) :addCallParenthesis)
                  :json-false)))))

(ert-deftest lsp-rust-analyzer-client-uses-merged-init-options ()
  (let ((lsp-rust-analyzer-extra-settings '(:numThreads 4)))
    (should (equal (plist-get (lsp--create-initialization-options
                               nil (gethash 'rust-analyzer lsp-clients))
                              :numThreads)
                   4))))

(provide 'lsp-rust-test)
;;; lsp-rust-test.el ends here
