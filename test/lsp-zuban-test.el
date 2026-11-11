;;; lsp-zuban-test.el --- unit tests for lsp-zuban -*- lexical-binding: t; -*-

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

;; Tests for the lsp-zuban client.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lsp-mode)
(require 'lsp-zuban)

(defun lsp-zuban-test--init-options ()
  "Return the initialization options built by the Zuban client."
  (lsp--create-initialization-options
   nil (gethash 'zuban lsp-clients)))

(ert-deftest lsp-zuban-client-is-registered ()
  "The Zuban client must be registered for Python with a low priority."
  (let ((client (gethash 'zuban lsp-clients)))
    (should client)
    (should (= (lsp--client-priority client) -1))
    (let ((activation-fn (lsp--client-activation-fn client)))
      (cl-letf (((symbol-function 'lsp-buffer-language) (lambda () "python")))
        (should (funcall activation-fn "foo.py" 'python-mode)))
      (cl-letf (((symbol-function 'lsp-buffer-language) (lambda () "rust")))
        (should-not (funcall activation-fn "foo.rs" 'rust-mode))))))

(ert-deftest lsp-zuban-default-server-command ()
  "Zuban is started with `zuban server' by default."
  (should (equal (default-value 'lsp-zuban-server-command)
                 '("zuban" "server"))))

(ert-deftest lsp-zuban-init-options-omitted-by-default ()
  "With all options at their defaults nothing is sent to the server."
  (let ((lsp-zuban-type-checking-mode nil)
        (lsp-zuban-disable-language-services nil)
        (lsp-zuban-diagnostic-mode nil)
        (lsp-zuban-python-executable nil)
        (lsp-zuban-inlay-hint-mode nil))
    (should-not (lsp-zuban-test--init-options))))

(ert-deftest lsp-zuban-init-options-included-when-set ()
  "Configured options are sent under Zuban's option names."
  (let ((lsp-zuban-type-checking-mode "mypy")
        (lsp-zuban-disable-language-services t)
        (lsp-zuban-diagnostic-mode "workspace")
        (lsp-zuban-python-executable "/usr/bin/python3")
        (lsp-zuban-inlay-hint-mode "off"))
    (let ((opts (lsp-zuban-test--init-options)))
      (should (equal (plist-get opts :typeCheckingMode) "mypy"))
      (should (eq (plist-get opts :disableLanguageServices) t))
      (should (equal (plist-get opts :diagnosticMode) "workspace"))
      (should (equal (plist-get opts :pythonExecutable) "/usr/bin/python3"))
      (should (equal (plist-get opts :inlayHintMode) "off")))))

(provide 'lsp-zuban-test)
;;; lsp-zuban-test.el ends here
