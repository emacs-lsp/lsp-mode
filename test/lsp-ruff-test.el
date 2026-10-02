;;; lsp-ruff-test.el --- unit and integration tests for lsp-ruff -*- lexical-binding: t; -*-

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

;; Tests for the lsp-ruff client.

;;; Code:

(require 'ert)
(require 'f)
(require 'dash)
(require 'lsp-mode)
(require 'lsp-ruff)

(defconst lsp-ruff-test-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defun lsp-ruff-test--lint-options ()
  "Return the `:lint' settings built by the ruff client."
  (let* ((client (gethash 'ruff lsp-clients))
         (opts (funcall (lsp--client-initialization-options client))))
    (plist-get (plist-get opts :settings) :lint)))

(ert-deftest lsp-ruff-lint-rule-defaults-are-nil ()
  "The lint rule lists must default to nil.
ruff treats an empty JSON array as \"select no rules\", which
silences all diagnostics out of the box (issue #5119), while
nil/omitted falls back to ruff's own defaults and configuration
files."
  (should-not (default-value 'lsp-ruff-lint-select))
  (should-not (default-value 'lsp-ruff-lint-extend-select))
  (should-not (default-value 'lsp-ruff-lint-ignore)))

(ert-deftest lsp-ruff-lint-options-omitted-when-nil ()
  "Nil rule lists must not be sent to the server at all.
Sending an empty array selects zero rules; the keys must be
omitted entirely so ruff uses its own defaults (issue #5119)."
  (let ((lsp-ruff-lint-select nil)
        (lsp-ruff-lint-extend-select nil)
        (lsp-ruff-lint-ignore nil))
    (let ((lint (lsp-ruff-test--lint-options)))
      (should (plist-member lint :enable))
      (should-not (plist-member lint :select))
      (should-not (plist-member lint :extendSelect))
      (should-not (plist-member lint :ignore)))))

(ert-deftest lsp-ruff-lint-options-included-when-set ()
  "Explicitly configured rule lists must still be sent."
  (let ((lsp-ruff-lint-select ["E" "F"])
        (lsp-ruff-lint-extend-select ["B"])
        (lsp-ruff-lint-ignore ["E501"]))
    (let ((lint (lsp-ruff-test--lint-options)))
      (should (equal (plist-get lint :select) ["E" "F"]))
      (should (equal (plist-get lint :extendSelect) ["B"]))
      (should (equal (plist-get lint :ignore) ["E501"])))))

(defun lsp-ruff-test--wait-until (pred &optional timeout)
  "Poll PRED until non-nil, failing after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 60))))
    (while (not (funcall pred))
      (when (> (float-time) deadline)
        (error "Timeout waiting for condition"))
      (sleep-for 0.05))))

(ert-deftest lsp-ruff-reports-diagnostics ()
  "Ruff must report diagnostics with default settings (issue #5119)."
  (skip-unless (executable-find "ruff"))
  (let ((lsp-restart 'ignore)
        (lsp-warn-no-matched-clients nil)
        (lsp-enable-snippet nil))
    (let* ((workspace (f-join lsp-ruff-test-location "fixtures"))
           (fixture (f-join lsp-ruff-test-location "fixtures/ruff/test.py")))
      (unwind-protect
          (progn
            (lsp-workspace-folders-add workspace)
            (find-file fixture)
            (lsp)
            ;; wait for the ruff workspace to be up
            (lsp-ruff-test--wait-until
             (lambda ()
               (cl-some (lambda (w)
                          (eq 'ruff (lsp--client-server-id
                                     (lsp--workspace-client w))))
                        (lsp-workspaces))))
            (let ((ruff-workspace
                   (cl-find-if (lambda (w)
                                 (eq 'ruff (lsp--client-server-id
                                            (lsp--workspace-client w))))
                               (lsp-workspaces))))
              ;; wait for diagnostics from the ruff workspace
              (lsp-ruff-test--wait-until
               (lambda ()
                 (gethash (lsp--fix-path-casing (buffer-file-name))
                          (lsp--workspace-diagnostics ruff-workspace))))
              (let ((diags (gethash (lsp--fix-path-casing (buffer-file-name))
                                    (lsp--workspace-diagnostics ruff-workspace))))
                (should (cl-some (lambda (diag)
                                   (equal "E722" (lsp:diagnostic-code? diag)))
                                 diags))))
            (should (equal (buffer-string)
                           "def mvce() -> float:
    try:
        return 1 / 0
    except:
        return \"bad\"
")))
        (let ((buf (find-buffer-visiting fixture)))
          (when buf
            (with-current-buffer buf
              (set-buffer-modified-p nil)
              (kill-buffer buf))))
        (lsp-workspace-folders-remove workspace)))))

(provide 'lsp-ruff-test)
;;; lsp-ruff-test.el ends here
