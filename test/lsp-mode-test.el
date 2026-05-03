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
    (should (lsp-completion--sort-completions (lsp:completion-list-items merged-completions))))

  (let ((merged-completions (lsp--merge-results
                             `([])
                             "textDocument/completion")))
    (should (lsp-completion-list? merged-completions))
    (should (not (lsp:completion-list-is-incomplete merged-completions)))
    (should (seq-empty-p (lsp:completion-list-items merged-completions)))))

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

;;; Position encoding tests

(ert-deftest lsp--utf-16-column-ascii ()
  "UTF-16 column equals codepoint column for ASCII text."
  (with-temp-buffer
    (insert "hello")
    (goto-char 4)                         ; after "hel"
    (should (= (lsp--utf-16-column) 3))))

(ert-deftest lsp--utf-16-column-supplementary ()
  "Supplementary plane characters count as 2 UTF-16 code units."
  (with-temp-buffer
    (insert "a\U0001F600b")               ; a + emoji + b
    (goto-char (point-max))               ; after "a😀b"
    (should (= (lsp--utf-16-column) 4)))) ; 1 + 2 + 1

(ert-deftest lsp--move-to-utf-16-column-supplementary ()
  "Moving by UTF-16 offset accounts for surrogate pairs."
  (with-temp-buffer
    (insert "a\U0001F600b")
    (lsp--move-to-utf-16-column 3)       ; past 'a' (1) + emoji (2)
    (should (= (char-after) ?b))))

(ert-deftest lsp--utf-32-column-supplementary ()
  "UTF-32 column counts each character as 1 regardless of plane."
  (with-temp-buffer
    (insert "a\U0001F600b")
    (goto-char (point-max))
    (should (= (lsp--utf-32-column) 3))))

(ert-deftest lsp--utf-8-column-multibyte ()
  "UTF-8 column counts bytes, not characters."
  (with-temp-buffer
    (insert "aé")                          ; 'a' = 1 byte, 'é' = 2 bytes
    (goto-char (point-max))
    (should (= (lsp--utf-8-column) 3))))

(ert-deftest lsp--move-to-utf-8-column-multibyte ()
  "Moving by UTF-8 byte offset lands on the correct character."
  (with-temp-buffer
    (insert "aéb")                         ; a(1) + é(2) + b(1)
    (lsp--move-to-utf-8-column 3)         ; 1 + 2 = byte offset of 'b'
    (should (= (char-after) ?b))))

(ert-deftest lsp--set-position-encoding-utf-16 ()
  "Setting encoding to utf-16 installs the correct functions."
  (with-temp-buffer
    (lsp--set-position-encoding "utf-16")
    (should (eq lsp--position-column-function #'lsp--utf-16-column))
    (should (eq lsp--move-to-column-function #'lsp--move-to-utf-16-column))))

(ert-deftest lsp--set-position-encoding-utf-32 ()
  "Setting encoding to utf-32 installs the correct functions."
  (with-temp-buffer
    (lsp--set-position-encoding "utf-32")
    (should (eq lsp--position-column-function #'lsp--utf-32-column))
    (should (eq lsp--move-to-column-function #'lsp--move-to-utf-32-column))))

(ert-deftest lsp--line-character-to-point-utf-16 ()
  "line-character-to-point respects UTF-16 encoding."
  (with-temp-buffer
    (insert "a\U0001F600b\n")
    (lsp--set-position-encoding "utf-16")
    ;; line 0, character 3 (UTF-16 units: a=1, emoji=2) → point at 'b'
    (let ((pt (lsp--line-character-to-point 0 3)))
      (should (= (char-after pt) ?b)))))

(ert-deftest lsp--move-to-column-clamps-to-eol ()
  "Column beyond line length clamps to end of line."
  (with-temp-buffer
    (insert "ab\n")
    (goto-char (point-min))
    (let ((eol (line-end-position)))
      (should (= (lsp--move-to-utf-16-column 100) eol))
      (goto-char (point-min))
      (should (= (lsp--move-to-utf-32-column 100) eol))
      (goto-char (point-min))
      (should (= (lsp--move-to-utf-8-column 100) eol)))))

;;; lsp-download-install tests
;;
;; These tests verify that lsp-download-install uses make-process (curl) rather
;; than url-copy-file in a background thread.  On macOS the NS backend asserts
;; that url-retrieve-synchronously runs on the main thread; violating that
;; causes SIGABRT.  The tests mock make-process so no network access is needed.

(defmacro lsp-test--with-download-mocks (process-exit-event &rest body)
  "Run BODY with make-process, file predicates, and make-thread mocked.
The mock for make-process captures the sentinel and, if PROCESS-EXIT-EVENT is
non-nil, immediately calls it with that event string.  make-thread runs its
lambda synchronously so decompression callbacks are observable within the test."
  (declare (indent 1))
  `(let (lsp-test--sentinel lsp-test--make-process-args)
     (cl-letf* (((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'f-exists?) (lambda (_) nil))
                ((symbol-function 'f-delete) #'ignore)
                ((symbol-function 'mkdir) #'ignore)
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq lsp-test--make-process-args args
                         lsp-test--sentinel (plist-get args :sentinel))
                   (when ,process-exit-event
                     (funcall lsp-test--sentinel nil ,process-exit-event))
                   :mock-process))
                ((symbol-function 'make-thread)
                 (lambda (fn &optional _name) (funcall fn))))
       ,@body)))

(ert-deftest lsp-download-install--no-curl ()
  "error-callback fires immediately when curl is not on the PATH."
  (let (error-result)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (lsp-download-install
       (lambda () (error "success callback must not be called"))
       (lambda (err) (setq error-result err))
       :url "https://example.com/server.gz"
       :store-path "/tmp/lsp-test-server"))
    (should error-result)
    (should (string-match-p "curl" (error-message-string error-result)))))

(ert-deftest lsp-download-install--uses-make-process ()
  "make-process is called with curl and the correct URL and output path."
  (lsp-test--with-download-mocks nil
    (lsp-download-install
     #'ignore #'ignore
     :url "https://example.com/server.gz"
     :store-path "/tmp/lsp-test-server"
     :decompress :gzip)
    (let ((cmd (plist-get lsp-test--make-process-args :command)))
      (should (equal (car cmd) "curl"))
      (should (member "--location" cmd))
      (should (member "https://example.com/server.gz" cmd))
      (should (member "/tmp/lsp-test-server.gz" cmd)))))

(ert-deftest lsp-download-install--success-no-decompress ()
  "callback is called after sentinel signals successful curl exit."
  (let (callback-called)
    (lsp-test--with-download-mocks "finished\n"
      (lsp-download-install
       (lambda () (setq callback-called t))
       (lambda (err) (error "unexpected error: %S" err))
       :url "https://example.com/server"
       :store-path "/tmp/lsp-test-server"))
    (should callback-called)))

(ert-deftest lsp-download-install--curl-failure ()
  "error-callback is called when the curl process exits abnormally."
  (let (error-result)
    (lsp-test--with-download-mocks "exited abnormally with code 6\n"
      (lsp-download-install
       (lambda () (error "success callback must not be called"))
       (lambda (err) (setq error-result err))
       :url "https://example.com/server"
       :store-path "/tmp/lsp-test-server"))
    (should error-result)
    (should (string-match-p "curl failed" (error-message-string error-result)))))

(ert-deftest lsp-download-install--decompresses-after-download ()
  "The correct decompression function is called after a successful download."
  (let (gunzip-called unzip-called targz-called)
    (cl-letf (((symbol-function 'lsp-gunzip) (lambda (_) (setq gunzip-called t)))
              ((symbol-function 'lsp-unzip) (lambda (_ _d) (setq unzip-called t)))
              ((symbol-function 'lsp-tar-gz-decompress) (lambda (_ _d) (setq targz-called t))))
      (lsp-test--with-download-mocks "finished\n"
        (lsp-download-install #'ignore #'ignore
                              :url "https://example.com/s.gz"
                              :store-path "/tmp/lsp-s" :decompress :gzip))
      (should gunzip-called)
      (lsp-test--with-download-mocks "finished\n"
        (lsp-download-install #'ignore #'ignore
                              :url "https://example.com/s.zip"
                              :store-path "/tmp/lsp-s" :decompress :zip))
      (should unzip-called)
      (lsp-test--with-download-mocks "finished\n"
        (lsp-download-install #'ignore #'ignore
                              :url "https://example.com/s.tar.gz"
                              :store-path "/tmp/lsp-s" :decompress :targz))
      (should targz-called))))

;;; lsp-mode-test.el ends here
