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

(ert-deftest lsp-define-stdio-client ()
  (lsp-define-stdio-client test-stdio-client "test language"
                           (lambda () default-directory)
                           '("/bin/false"))
  (should (fboundp 'test-stdio-client-enable))
  (with-temp-buffer
    (test-stdio-client-enable)))

(ert-deftest lsp-define-tcp-client ()
  (lsp-define-tcp-client test-tcp-client "test language"
                           (lambda () default-directory)
                           '("/bin/false") "localhost" 12345)
  (should (fboundp 'test-tcp-client-enable))
  (with-temp-buffer
    (test-tcp-client-enable)))

(ert-deftest lsp-define-whitelist ()
  (lsp-define-stdio-client test-stdio-client "test language"
                           (lambda () "/tmp/baz/")
                           '("/bin/false"))
  (should (fboundp 'test-stdio-client-whitelist-add))
  (should (fboundp 'test-stdio-client-whitelist-remove))
  (should (not lsp-project-whitelist))
  (with-temp-buffer
    (test-stdio-client-whitelist-add)
    (should (equal lsp-project-whitelist '("^/tmp/baz/$") ))
    ;; Should not add a duplicate
    (test-stdio-client-whitelist-add)
    (should (equal lsp-project-whitelist '("^/tmp/baz/$") ))
    ;; testing remove
    (customize-save-variable 'lsp-project-whitelist
           (add-to-list 'lsp-project-whitelist (lsp--as-regex "/tmp/foo")))
    (should (equal lsp-project-whitelist '("^/tmp/foo$" "^/tmp/baz/$") ))
    (test-stdio-client-whitelist-remove)
    (should (equal lsp-project-whitelist '("^/tmp/foo$") ))
    ;; Should be idempotent
    (test-stdio-client-whitelist-remove)
    (should (equal lsp-project-whitelist '("^/tmp/foo$") ))
    ))

;;; lsp-mode-test.el ends here
