;;; lsp-mode-test.el --- unit tests for lsp-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski
;; Copyright (C) 2020-2026 lsp-mode maintainers

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

(ert-deftest lsp-diagnostics-stats-test ()
  (let ((workspace (make-lsp--workspace))
        (lsp-diagnostic-stats (ht)))
    (clrhash lsp-diagnostic-stats)
    (lsp--on-diagnostics workspace
                         (lsp-make-publish-diagnostics-params
                          :uri (lsp--path-to-uri "/foo/bar/baz/txt.txt")
                          :diagnostics (vector
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-error)
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-warning))))
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/foo"))
                   [0 1 1 0 0]))
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/foo/bar"))
                   [0 1 1 0 0]))
    (lsp--on-diagnostics workspace
                         (lsp-make-publish-diagnostics-params
                          :uri (lsp--path-to-uri "/foo/bar/baz/txt.txt")
                          :diagnostics (vector
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-error))))
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/foo"))
                   [0 1 0 0 0]))
    (lsp--on-diagnostics workspace
                         (lsp-make-publish-diagnostics-params
                          :uri (lsp--path-to-uri "/foo/bar/baz/txt.txt")
                          :diagnostics []))
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/"))
                   [0 0 0 0 0]))))

(ert-deftest lsp-diagnostics-stats-workspace-shutdown-test ()
  (let ((workspace (make-lsp--workspace))
        (lsp-diagnostic-stats (ht)))
    (lsp--on-diagnostics workspace
                         (lsp-make-publish-diagnostics-params
                          :uri (lsp--path-to-uri "/foo/bar/baz/txt.txt")
                          :diagnostics (vector
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-error)
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-warning))))
    (lsp--on-diagnostics workspace
                         (lsp-make-publish-diagnostics-params
                          :uri (lsp--path-to-uri "/foo/bar/baz/txt.txt1")
                          :diagnostics (vector
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-error)
                                        (lsp-make-diagnostic :severity? lsp/diagnostic-severity-warning))))
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/foo"))
                   [0 2 2 0 0]))
    (lsp-diagnostics--workspace-cleanup workspace)
    (should (equal (lsp-diagnostics-stats-for (expand-file-name "/foo"))
                   [0 0 0 0 0]))))

(ert-deftest lsp-point-in-range?-test ()
  (let ((range (lsp-make-range :start (lsp-make-position :character 1 :line 1)
                               :end (lsp-make-position :character 3 :line 3))))
    (should (lsp-point-in-range? (lsp-make-position :character 2 :line 2)
                                 range))
    (should-not (lsp-point-in-range? (lsp-make-position :character 0 :line 0)
                                     range))
    (should-not (lsp-point-in-range? (lsp-make-position :character 10 :line 10)
                                     range))
    (should (lsp-point-in-range? (lsp-make-position :character 1 :line 1)
                                 range))
    (should (lsp-point-in-range? (lsp-make-position :character 3 :line 3)
                                 range))))

(ert-deftest lsp-test--merge-hover ()
  (should (= (length (lsp:hover-contents
                      (lsp--merge-results
                       (list (lsp-make-hover :contents (vector "string"
                                                               (lsp-make-marked-string :language "java"
                                                                                       :value "XXX")))
                             (lsp-make-hover :contents []))
                       "textDocument/hover")))
             2)))

(ert-deftest lsp-test--register-custom-settings-override ()
  "Test that custom settings can be overridden."
  (clrhash lsp-client-settings)
  (lsp-register-custom-settings '(("foo" "original value")))
  (lsp-register-custom-settings '(("foo" "new value")))
  (should (equal (gethash "foo" lsp-client-settings) '("new value"))))

(ert-deftest lsp-resolve-final-command-with-nil-values ()
  "Test that lsp-resolve-final-command handles nil values in command list (issue #4099).
This test reproduces the bug where command lists with nil values cause
'Invalid command list' assertion errors."
  ;; Directly pass a list with nil values - this simulates what happens when
  ;; lsp-resolve-value returns a list containing nil elements
  (let ((command (list "node" nil "server.js" nil "--stdio")))
    (should (equal (lsp-resolve-final-command command t)
                   '("node" "server.js" "--stdio")))))

(ert-deftest lsp--capability-test ()
  "Test lsp--capability returns correct values for various capability states.
This tests the fix for empty capability objects like DefinitionOptions {}."
  ;; Create capabilities structure that works in both plist and hash-table modes
  (let ((capabilities (if lsp-use-plists
                          ;; plist mode: empty objects are parsed as nil
                          (list :definitionProvider nil
                                :hoverProvider t
                                :completionProvider (list :triggerCharacters ["." ":"]))
                        ;; hash-table mode
                        (let ((ht (make-hash-table :test 'equal)))
                          (puthash "definitionProvider" nil ht)
                          (puthash "hoverProvider" t ht)
                          (let ((completion-ht (make-hash-table :test 'equal)))
                            (puthash "triggerCharacters" ["." ":"] completion-ht)
                            (puthash "completionProvider" completion-ht ht))
                          ht))))
    ;; Test 1: Capability exists with nil value (empty object like DefinitionOptions {})
    ;; Should return truthy value, not nil
    (should (lsp--capability :definitionProvider capabilities))

    ;; Test 2: Capability exists with truthy value
    ;; Should return the actual value
    (should (eq t (lsp--capability :hoverProvider capabilities)))

    ;; Test 3: Capability exists with a structured value
    ;; Should return the actual value
    (let ((completion-cap (lsp--capability :completionProvider capabilities)))
      (should completion-cap)
      (should-not (eq t completion-cap)))

    ;; Test 4: Capability does not exist
    ;; Should return nil
    (should-not (lsp--capability :nonExistentProvider capabilities))))

(ert-deftest lsp--capability-string-key-test ()
  "Test lsp--capability accepts string keys and converts them properly."
  (let ((capabilities (if lsp-use-plists
                          (list :definitionProvider nil)
                        (let ((ht (make-hash-table :test 'equal)))
                          (puthash "definitionProvider" nil ht)
                          ht))))
    ;; String key should be converted to keyword and work correctly
    (should (lsp--capability "definitionProvider" capabilities))))

(ert-deftest lsp-null?-test ()
  "Test lsp-null? correctly identifies JSON null values."
  (if lsp-use-plists
      ;; plist mode: null is represented as :json-null
      (progn
        (should (lsp-null? :json-null))
        (should-not (lsp-null? nil))
        (should-not (lsp-null? t))
        (should-not (lsp-null? '(:foo "bar"))))
    ;; hash-table mode: null is represented as nil
    (progn
      (should (lsp-null? nil))
      (should-not (lsp-null? t))
      (should-not (lsp-null? (make-hash-table))))))

(ert-deftest lsp--capability-explicit-null-test ()
  "Test lsp--capability returns nil when capability is explicitly null.
This tests the distinction between empty objects ({}) and explicit null."
  (let ((capabilities (if lsp-use-plists
                          ;; plist mode: null is :json-null, empty object is nil
                          (list :definitionProvider nil      ; empty object {}
                                :referencesProvider :json-null) ; explicit null
                        ;; hash-table mode: null is nil
                        (let ((ht (make-hash-table :test 'equal)))
                          (puthash "definitionProvider" (make-hash-table :test 'equal) ht)
                          (puthash "referencesProvider" nil ht)
                          ht))))
    ;; Empty object should be detected as capability supported
    (should (lsp--capability :definitionProvider capabilities))
    ;; Explicit null should be detected as capability not supported
    (should-not (lsp--capability :referencesProvider capabilities))))

;;; Hook Management Tests (Issue #4815)

(ert-deftest lsp-update-on-type-formatting-hook-enabled ()
  "Test that handler is created and hook is added when feature is enabled."
  (with-temp-buffer
    (let ((lsp-enable-on-type-formatting t)
          (handler-created nil)
          (hook-added nil))
      (cl-letf (((symbol-function 'lsp--on-type-formatting-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-handler))
                ((symbol-function 'add-hook)
                 (lambda (hook fn &optional _depth _local)
                   (when (and (eq hook 'post-self-insert-hook)
                              (eq fn 'mock-handler))
                     (setq hook-added t)))))
        (lsp--update-on-type-formatting-hook)
        (should handler-created)
        (should hook-added)))))

(ert-deftest lsp-update-on-type-formatting-hook-disabled-optimization ()
  "Test that handler creation is skipped when feature is disabled (optimization).
This verifies issue #4815 is fixed - handler should NOT be created when
lsp-enable-on-type-formatting is nil and not in cleanup mode."
  (with-temp-buffer
    (let ((lsp-enable-on-type-formatting nil)
          (handler-created nil))
      (cl-letf (((symbol-function 'lsp--on-type-formatting-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-handler)))
        (lsp--update-on-type-formatting-hook)
        (should-not handler-created)))))

(ert-deftest lsp-update-on-type-formatting-hook-cleanup ()
  "Test that cleanup mode creates handler for proper hook removal."
  (with-temp-buffer
    (let ((lsp-enable-on-type-formatting nil)
          (handler-created nil)
          (hook-removed nil))
      (cl-letf (((symbol-function 'lsp--on-type-formatting-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-handler))
                ((symbol-function 'remove-hook)
                 (lambda (hook fn &optional _local)
                   (when (and (eq hook 'post-self-insert-hook)
                              (eq fn 'mock-handler))
                     (setq hook-removed t)))))
        (lsp--update-on-type-formatting-hook :cleanup)
        (should handler-created)
        (should hook-removed)))))

(ert-deftest lsp-update-signature-help-hook-enabled ()
  "Test that handler is created when signature help is enabled."
  (with-temp-buffer
    (let ((lsp-signature-auto-activate t)
          (handler-created nil)
          (hook-added nil))
      (cl-letf (((symbol-function 'lsp--signature-help-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-sig-handler))
                ((symbol-function 'add-hook)
                 (lambda (hook fn &optional _depth _local)
                   (when (and (eq hook 'post-self-insert-hook)
                              (eq fn 'mock-sig-handler))
                     (setq hook-added t)))))
        (lsp--update-signature-help-hook)
        (should handler-created)
        (should hook-added)))))

(ert-deftest lsp-update-signature-help-hook-disabled-optimization ()
  "Test that handler creation is skipped when signature help is disabled.
This verifies the optimization - handler should NOT be created when
lsp-signature-auto-activate is nil and not in cleanup mode."
  (with-temp-buffer
    (let ((lsp-signature-auto-activate nil)
          (handler-created nil))
      (cl-letf (((symbol-function 'lsp--signature-help-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-sig-handler)))
        (lsp--update-signature-help-hook)
        (should-not handler-created)))))

(ert-deftest lsp-update-signature-help-hook-on-trigger-char ()
  "Test that handler is created with :on-trigger-char activation mode."
  (with-temp-buffer
    (let ((lsp-signature-auto-activate '(:on-trigger-char))
          (handler-created nil))
      (cl-letf (((symbol-function 'lsp--signature-help-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-sig-handler))
                ((symbol-function 'add-hook) #'ignore))
        (lsp--update-signature-help-hook)
        (should handler-created)))))

(ert-deftest lsp-update-signature-help-hook-after-completion-only ()
  "Test that handler is NOT created when only :after-completion is set.
This verifies the optimization - :after-completion does not require
post-self-insert-hook, so handler creation should be skipped."
  (with-temp-buffer
    (let ((lsp-signature-auto-activate '(:after-completion))
          (handler-created nil))
      (cl-letf (((symbol-function 'lsp--signature-help-handler-create)
                 (lambda ()
                   (setq handler-created t)
                   'mock-sig-handler)))
        (lsp--update-signature-help-hook)
        (should-not handler-created)))))

;;; lsp-mode-test.el ends here
