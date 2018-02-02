;;; lsp-mode-test.el --- unit tests for lsp-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Google Inc.

;; Author: Philipp Stephani <phst@google.com>

;; This program is free software; you can redistribute it and/or modify
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

;; Unit tests for lsp-mode.el.

;;; Code:

(require 'lsp-mode)

(require 'ert)
(require 'f)

(defconst lsp-mode-tests--directory
  (file-name-directory (f-this-file))
  "Directory where this file resides in.")

(defconst lsp-mode-tests--logfile
  (expand-file-name "test_server.log" lsp-mode-tests--directory)
  "File where the test server should write its logs.")

(lsp-define-stdio-client
 test-client "go" (lambda () lsp-mode-tests--directory)
 (list (expand-file-name "test_server" lsp-mode-tests--directory)
       "-logfile" lsp-mode-tests--logfile))

(ert-deftest lsp-define-stdio-client ()
  (should (fboundp 'test-client-enable)))

(ert-deftest lsp-mode/test-server ()
  (let ((lsp-print-io t)
        ;; Enable ‘debug-on-error’ so that errors that are normally suppressed
        ;; using ‘with-demoted-errors’ become visible.
        (debug-on-error t))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents
           (expand-file-name "test_input.go" lsp-mode-tests--directory) :visit)
          (test-client-enable)
          ;; Give test server a bit of time to process asynchronous
          ;; notifications.
          (sleep-for 0.1))
      ;; Print stderr of test server for debugging purposes.
      (with-temp-buffer
        (condition-case err
            (insert-file-contents lsp-mode-tests--logfile)
          ;; Don’t fail hard if the server didn’t create the logfile.
          ;; Signaling an error here would mask out actual test failures.
          (file-error (message "Error inserting logfile: %S" err)))
        (terpri)
        (princ (buffer-string))
        (terpri)))))

;;; lsp-mode-test.el ends here
