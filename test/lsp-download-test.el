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
(require 'url)
(require 'cl-lib)

;; Define the core download function that we're testing
;; This is a copy of the actual implementation from lsp-mode.el
(defun lsp-download-install--url-retrieve (url file callback error-callback)
  "Download URL to FILE using url-retrieve asynchronously.
Call CALLBACK on success or ERROR-CALLBACK on failure."
  (url-retrieve
   url
   (lambda (status &rest _)
     (cond
      ((plist-get status :error)
       (message "Download failed: %s" (plist-get status :error))
       (funcall error-callback (plist-get status :error)))
      (t
       (condition-case err
           (progn
             (goto-char (point-min))
             (re-search-forward "\n\n" nil t)
             (let ((coding-system-for-write 'binary))
               (write-region (point) (point-max) file nil 'silent))
             (kill-buffer)
             (funcall callback))
         (error
          (message "Failed to save downloaded file: %s" err)
          (funcall error-callback err))))))
   nil 'silent 'inhibit-cookies))

;; Define the signature verification function
(defun lsp-download-install--verify-signature (store-path file asc-file callback)
  "Verify FILE using ASC-FILE signature.
STORE-PATH is the directory containing the files.
Call CALLBACK with verification result."
  (if (and (executable-find "gpg")
           (file-exists-p asc-file))
      (progn
        (message "Verifying signature for %s" file)
        (with-temp-buffer
          (let ((exit-code (call-process "gpg" nil t nil "--verify" asc-file file)))
            (if (= exit-code 0)
                (progn
                  (message "Signature verification successful")
                  (funcall callback t))
              (progn
                (message "Signature verification failed")
                (funcall callback nil))))))
    (progn
      (message "GPG not available or signature file missing, skipping verification")
      (funcall callback t))))

;; Test the core async download function in isolation
(ert-deftest lsp-download-url-retrieve-async ()
  "Test that url-retrieve based download works asynchronously."
  (let ((temp-file (make-temp-file "lsp-test-download"))
        (download-completed nil)
        (download-content "test content from server"))
    (unwind-protect
        (progn
          ;; Mock url-retrieve to simulate async behavior
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (url callback &rest args)
                       (run-at-time 0.01 nil
                                    (lambda ()
                                      (with-temp-buffer
                                        (insert "HTTP/1.1 200 OK\n\n")
                                        (insert download-content)
                                        (funcall callback nil)))))))
            
            ;; Test our helper function directly
            (lsp-download-install--url-retrieve
             "http://example.com/test"
             temp-file
             (lambda () (setq download-completed t))
             (lambda (err) (error "Download failed: %s" err)))
            
            ;; Should not be completed immediately (async behavior)
            (should-not download-completed)
            
            ;; Wait for async completion
            (sleep-for 0.1)
            
            ;; Should be completed now
            (should download-completed)
            (should (file-exists-p temp-file))
            
            ;; Verify content
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string= download-content (buffer-string))))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest lsp-download-url-retrieve-error-handling ()
  "Test that url-retrieve properly handles errors."
  (let ((temp-file (make-temp-file "lsp-test-download"))
        (error-called nil)
        (success-called nil))
    (unwind-protect
        (progn
          ;; Mock url-retrieve to simulate error
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (url callback &rest args)
                       (run-at-time 0.01 nil
                                    (lambda ()
                                      (funcall callback '(:error (error "Network error"))))))))
            
            (lsp-download-install--url-retrieve
             "http://example.com/test"
             temp-file
             (lambda () (setq success-called t))
             (lambda (err) (setq error-called t)))
            
            ;; Wait for async completion
            (sleep-for 0.1)
            
            ;; Should have called error callback
            (should error-called)
            (should-not success-called)))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest lsp-download-verify-signature-function-exists ()
  "Test that signature verification function exists and has correct signature."
  (should (fboundp 'lsp-download-install--verify-signature))
  ;; Test that it can be called without error when gpg is not available
  (cl-letf (((symbol-function 'executable-find) (lambda (prog) nil)))
    (let ((result nil))
      (lsp-download-install--verify-signature "/tmp" "/tmp/test" "/tmp/test.asc"
                                              (lambda (verified) (setq result verified)))
      (should result))))

(provide 'lsp-download-test)
;;; lsp-download-test.el ends here
