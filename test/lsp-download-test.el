;;; lsp-download-test.el --- Tests for lsp-download functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 emacs-lsp maintainers

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

;;; Commentary:

;; Tests for lsp-download-install and related download functionality

;;; Code:

(require 'ert)
(require 'lsp-mode)
(require 'url)
(require 'cl-lib)

(defmacro lsp-download-test--with-mocked-url-retrieve (response-data &rest body)
  "Mock url-retrieve to return RESPONSE-DATA and execute BODY."
  (declare (indent 1))
  `(cl-letf* ((url-retrieve-calls 0)
              ((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (cl-incf url-retrieve-calls)
                 (run-at-time 0.01 nil
                              (lambda ()
                                (with-temp-buffer
                                  (insert ,response-data)
                                  (goto-char (point-min))
                                  (funcall callback nil cbargs)))))))
     ,@body))

(ert-deftest lsp-download-install-callback-success ()
  "Test that lsp-download-install calls success callback on successful download."
  (let* ((temp-file (make-temp-file "lsp-test-download"))
         (callback-called nil)
         (error-called nil)
         (test-content "test file content"))
    (unwind-protect
        (lsp-download-test--with-mocked-url-retrieve
            (concat "HTTP/1.1 200 OK\r\n\r\n" test-content)
          (lsp-download-install
           (lambda () (setq callback-called t))
           (lambda (_err) (setq error-called t))
           :url "http://example.com/test.jar"
           :store-path temp-file)
          
          ;; Wait for async operation
          (sleep-for 0.1)
          
          (should callback-called)
          (should-not error-called)
          (should (f-exists? temp-file))
          (should (string= test-content (f-read temp-file))))
      (when (f-exists? temp-file)
        (f-delete temp-file)))))

(ert-deftest lsp-download-install-callback-error ()
  "Test that lsp-download-install calls error callback on failed download."
  (let* ((temp-file (make-temp-file "lsp-test-download"))
         (callback-called nil)
         (error-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url callback &optional cbargs silent inhibit-cookies)
                     (run-at-time 0.01 nil
                                  (lambda ()
                                    (funcall callback '(:error (error "Network error")) cbargs))))))
          (lsp-download-install
           (lambda () (setq callback-called t))
           (lambda (_err) (setq error-called t))
           :url "http://example.com/test.jar"
           :store-path temp-file)
          
          ;; Wait for async operation
          (sleep-for 0.1)
          
          (should-not callback-called)
          (should error-called))
      (when (f-exists? temp-file)
        (f-delete temp-file)))))

(ert-deftest lsp-download-install-large-file-async ()
  "Test that lsp-download-install doesn't block UI with large files."
  (let* ((temp-file (make-temp-file "lsp-test-download"))
         (download-started nil)
         (download-completed nil)
         ;; Simulate a large file with 10MB of data
         (large-content (make-string (* 10 1024 1024) ?x)))
    (unwind-protect
        (lsp-download-test--with-mocked-url-retrieve
            (concat "HTTP/1.1 200 OK\r\n\r\n" large-content)
          (lsp-download-install
           (lambda () (setq download-completed t))
           (lambda (_err) (error "Download failed"))
           :url "http://example.com/large.jar"
           :store-path temp-file)
          
          (setq download-started t)
          
          ;; UI should not be blocked - download-started should be set
          ;; but download-completed should still be nil
          (should download-started)
          (should-not download-completed)
          
          ;; Wait for async completion
          (sleep-for 0.2)
          
          (should download-completed)
          (should (f-exists? temp-file))
          ;; Verify file size
          (should (= (f-size temp-file) (* 10 1024 1024))))
      (when (f-exists? temp-file)
        (f-delete temp-file)))))

(ert-deftest lsp-download-install-with-decompress ()
  "Test that lsp-download-install handles decompression options."
  (let* ((temp-dir (make-temp-file "lsp-test-dir" t))
         (store-path (f-join temp-dir "test.jar"))
         (download-path (concat store-path ".zip"))
         (callback-called nil))
    (unwind-protect
        (cl-letf* (((symbol-function 'lsp-unzip)
                    (lambda (file dir)
                      ;; Mock unzip - just create the target file
                      (f-write "unzipped content" 'utf-8 store-path)))
                   ((symbol-function 'url-retrieve)
                    (lambda (url callback &rest args)
                      (run-at-time 0.01 nil
                                   (lambda ()
                                     (with-temp-buffer
                                       (insert "HTTP/1.1 200 OK\r\n\r\nZIP_CONTENT")
                                       (goto-char (point-min))
                                       (funcall callback nil args)))))))
          
          (lsp-download-install
           (lambda () (setq callback-called t))
           (lambda (_err) (error "Download failed"))
           :url "http://example.com/test.zip"
           :store-path store-path
           :decompress :zip)
          
          ;; Wait for async operation
          (sleep-for 0.1)
          
          (should callback-called)
          (should (f-exists? store-path))
          (should (string= "unzipped content" (f-read store-path))))
      (when (f-exists? temp-dir)
        (f-delete temp-dir t)))))

(ert-deftest lsp-download-install-creates-parent-dirs ()
  "Test that lsp-download-install creates parent directories if needed."
  (let* ((temp-base (make-temp-file "lsp-test-base" t))
         (nested-path (f-join temp-base "a" "b" "c" "test.jar"))
         (callback-called nil))
    (unwind-protect
        (lsp-download-test--with-mocked-url-retrieve
            "HTTP/1.1 200 OK\r\n\r\ntest content"
          (should-not (f-exists? (f-parent nested-path)))
          
          (lsp-download-install
           (lambda () (setq callback-called t))
           (lambda (_err) (error "Download failed"))
           :url "http://example.com/test.jar"
           :store-path nested-path)
          
          ;; Wait for async operation
          (sleep-for 0.1)
          
          (should callback-called)
          (should (f-exists? nested-path))
          (should (f-exists? (f-parent nested-path))))
      (when (f-exists? temp-base)
        (f-delete temp-base t)))))

(ert-deftest lsp-package-ensure-with-download-provider ()
  "Test that lsp-package-ensure works with download provider."
  (let* ((temp-file (make-temp-file "lsp-test-download"))
         (callback-called nil)
         (test-dependency 'test-server))
    (unwind-protect
        (progn
          ;; Register a test dependency
          (puthash test-dependency
                   `(:download :url "http://example.com/test.jar"
                               :store-path ,temp-file)
                   lsp--dependencies)
          
          (lsp-download-test--with-mocked-url-retrieve
              "HTTP/1.1 200 OK\r\n\r\nserver content"
            (lsp-package-ensure
             test-dependency
             (lambda () (setq callback-called t))
             (lambda (_err) (error "Install failed")))
            
            ;; Wait for async operation
            (sleep-for 0.1)
            
            (should callback-called)
            (should (f-exists? temp-file))))
      ;; Cleanup
      (when (f-exists? temp-file)
        (f-delete temp-file))
      (remhash test-dependency lsp--dependencies))))

(provide 'lsp-download-test)
;;; lsp-download-test.el ends here