;;; lsp-common-test.el --- unit tests for lsp-io.el -*- lexical-binding: t -*-

;; Copyright (C) 2017  Lukas Fuermetz <fuermetz@mailbox.org>.

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
(require 'elenv)

(defgroup lsp-test nil
  ""
  :group 'lsp-mode)

(ert-deftest lsp--path-to-uri-1 ()
  (elenv-with-windows
   (let ((lsp--uri-file-prefix "file:///"))
     (should (equal (lsp--path-to-uri "c:/Users/?/") "file:///c:/Users/%3F/"))))
  (elenv-with-os '(darwin gnu/linux)
                 (let ((lsp--uri-file-prefix "file://"))
                   (should (equal (lsp--path-to-uri "/root/file/hallo welt") "file:///root/file/hallo%20welt")))
                 (should (equal (lsp--uri-to-path "file:///home/nim-%23devel")
                                "/home/nim-#devel"))
                 (should (equal (lsp--uri-to-path "file:///home/yyoncho/Sources/DemoProjects/lua/lua.lua#1#9")
                                "/home/yyoncho/Sources/DemoProjects/lua/lua.lua"))))

(ert-deftest lsp--path-to-uri-2 ()
  (let ((lsp--uri-file-prefix "file:///")
        (system-type 'windows-nt))
    (should (equal (lsp--uri-to-path "file:///c:/Users/%7B%7D/") "c:/Users/{}/")))
  (let ((lsp--uri-file-prefix "file://")
        (system-type 'gnu/linux))
    (should (equal (lsp--uri-to-path "/root/%5E/%60") "/root/^/`"))))

(ert-deftest lsp-common-test--path-to-uri-custom-schemes ()
  (let* ((client (make-lsp--client :uri-handlers (ht ("custom" (lambda (_) "file-path")))))
         (lsp--cur-workspace (make-lsp--workspace :client client)))
    (should (equal (lsp--uri-to-path "custom://file-path") "file-path"))))

(ert-deftest lsp-common-test--unexpected-scheme ()
  (should (equal (lsp--uri-to-path "will-fail://file-path")
                 "will-fail://file-path")))

(ert-deftest lsp--uri-to-path--handle-utf8 ()
  ;; In Emacs 26.1, the default coding system on Windows is `cp1252`
  ;; Set the variable `locale-coding-system' to utf-8 temporarily can resolved
  ;; this issue.
  ;;
  ;; See PR #2533
  (let ((locale-coding-system 'utf-8))
    (let ((lsp--uri-file-prefix "file:///")
          (system-type 'windows-nt))
      (should (equal (lsp--uri-to-path "file:///c:/Users/%E4%BD%A0%E5%A5%BD/") "c:/Users/你好/")))
    (let ((lsp--uri-file-prefix "file://")
          (system-type 'gnu/linux))
      (should (equal (lsp--uri-to-path "/root/%E4%BD%A0%E5%A5%BD/%E8%B0%A2%E8%B0%A2") "/root/你好/谢谢")))))

(ert-deftest lsp-byte-compilation-test ()
  (seq-doseq (library (-filter
                       (lambda (file)
                         (and (f-ext? file "el")
                              (not (s-contains? ".dir-locals" file))
                              (not (s-contains? "test" file))))
                       (append (when (or load-file-name buffer-file-name)
                                 (f-files (f-parent (f-dirname (or load-file-name buffer-file-name)))))
                               (f-files default-directory))))
    ;; TODO: turn this back on!
    (let ((byte-compile-error-on-warn nil))
      (message "Testing file %s" library)
      (should (byte-compile-file (save-excursion
                                   (find-library library)
                                   (buffer-file-name)))))))

(ert-deftest lsp--find-session-folder ()
  (let* ((project (make-temp-file "foo"))
         (nested (f-join project "nested-project")))

    (f-delete project)
    (make-directory nested t)

    (cl-assert (string= project
                        (lsp-find-session-folder
                         (make-lsp-session :folders (list project))
                         (f-join project "file")))
               t
               "failed to find the proper root")
    (cl-assert (string= nested
                        (lsp-find-session-folder
                         (make-lsp-session :folders (list project nested))
                         (f-join nested "foo")))
               t
               "failed to find nested project")
    (cl-assert (null (lsp-find-session-folder
                      (make-lsp-session :folders (list project nested))
                      "/foo"))
               t
               "Should not find any root.")))

(defun lsp-ht->alist (table)
  (ht-amap (cons key (if (ht? value)
                         (lsp-ht->alist value)
                       value))
           table))

(defcustom lsp-prop1 "10"
  "docs"
  :group 'lsp-python
  :type 'string
  :risky t
  :type 'list)

(lsp-register-custom-settings '(("section1.prop1" "banana")))
(lsp-register-custom-settings '(("section1.prop1" lsp-prop1)))

(ert-deftest lsp--custom-settings-test-1 ()
  (cl-assert (equal (lsp-ht->alist  (lsp-configuration-section "section1"))
                    '(("section1" ("prop1" . "10")))))
  (let ((lsp-prop1 1))
    (cl-assert (equal (lsp-ht->alist (lsp-configuration-section "section1"))
                      '(("section1" ("prop1" . 1))))))
  (let ((lsp-prop1 (-const 10)))
    (cl-assert (lsp-ht->alist (lsp-configuration-section "section1")))))

(ert-deftest lsp--build-workspace-configuration-response-test-1 ()
  (let ((request
          (lsp-make-configuration-params
           :items (list (lsp-make-configuration-item :section "section1")))))

    (cl-assert (equal (lsp-ht->alist (aref (lsp--build-workspace-configuration-response request) 0))
                      '(("prop1" . "10"))))

    (let ((lsp-prop1 1))
      (cl-assert (equal (lsp-ht->alist (aref (lsp--build-workspace-configuration-response request) 0))
                        '(("prop1" . 1))))))

  (let ((request (lsp-make-configuration-params
                  :items (list (lsp-make-configuration-item :section "section1.prop1")))))
    (cl-assert (equal (aref (lsp--build-workspace-configuration-response request) 0) "10"))
    (let ((lsp-prop1 1))
      (cl-assert (equal (aref (lsp--build-workspace-configuration-response request) 0) 1)))))

(defcustom lsp-nested-prop1 "10"
  "docs"
  :risky t
  :type 'list)

(defcustom lsp-nested-prop2 "20"
  "docs"
  :risky t
  :type 'string)

(lsp-register-custom-settings '(("section2.nested.prop1" lsp-nested-prop1)))
(lsp-register-custom-settings '(("section2.nested.prop2" lsp-nested-prop2)))

(ert-deftest lsp--custom-settings-test-2 ()
  (let ((actual (lsp-ht->alist (lsp-configuration-section "section2"))))
    (cl-assert (or (equal actual
                          '(("section2" ("nested" ("prop1" . "10") ("prop2" . "20")))))
                   (equal actual
                          '(("section2" ("nested" ("prop2" . "20") ("prop1" . "10")))))))))

(ert-deftest lsp--build-workspace-configuration-response-test-2 ()
  (-let* ((request (lsp-make-configuration-params
                    :items (list (lsp-make-configuration-item :section "section2.nested"))))
          (result (aref (lsp--build-workspace-configuration-response request) 0)))
    (cl-assert (equal (ht-get result "prop2") "20"))
    (cl-assert (equal (ht-get result "prop1") "10"))))

(defcustom lsp-prop3 nil
  "docs"
  :group 'lsp-python
  :risky t
  :type 'string)

(lsp-register-custom-settings '(("section3.prop1" lsp-prop3 t)))

(ert-deftest lsp--boolean-property ()
  (cl-assert (equal (lsp-ht->alist  (lsp-configuration-section  "section3"))
                    '(("section3" ("prop1" . :json-false))))))

(ert-deftest lsp--build-workspace-configuration-response-test-3 ()
  (let ((request (ht ("items" (list (ht ("section" "section3.prop1")))))))
    (cl-assert (equal (aref (lsp--build-workspace-configuration-response request) 0)
                      :json-false))))

(lsp-register-custom-settings '(("section4.prop1" "value")))

(ert-deftest lsp--non-boolean-property ()
  (cl-assert (equal (lsp-ht->alist  (lsp-configuration-section "section4"))
                    '(("section4" ("prop1" . "value"))))))

(ert-deftest lsp--build-workspace-configuration-response-test-4 ()
  (let ((request (ht ("items" (list (ht ("section" "section4.prop1")))))))
    (cl-assert (equal (aref (lsp--build-workspace-configuration-response request) 0) "value"))))

(ert-deftest lsp--f-ancestor-of? ()
  (should (lsp-f-ancestor-of? "~/tmp" "~/tmp/test"))
  (should (lsp-f-ancestor-of? "~/tmp/" "~/tmp/test"))
  (should (lsp-f-ancestor-of? "~/tmp/" "~/tmp/test/"))
  (should-not (lsp-f-ancestor-of? "~/tmp/t" "~/tmp/test"))
  (should-not (lsp-f-ancestor-of? "~/tm" "~/tmp/test"))
  (should-not (lsp-f-ancestor-of? "~/tm/" "~/tmp/test"))
  (should-not (lsp-f-ancestor-of? "~/tm/" "~/tmp/test/"))
  ;; Windows path
  (when (equal system-type 'windows-nt)
    (should (lsp-f-ancestor-of? "test\\tmp" "test/tmp/a"))
    (should-not (lsp-f-ancestor-of? "test\\tmp" "test\\tmp-a"))))

;;; lsp-common-test.el ends here
