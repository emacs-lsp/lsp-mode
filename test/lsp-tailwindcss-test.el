;;; lsp-tailwindcss-test.el --- unit tests for lsp-tailwindcss -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 lsp-mode maintainers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'lsp-tailwindcss)

(ert-deftest lsp-tailwindcss--package-version-test ()
  "Test `lsp-tailwindcss--package-version` function."
  (let ((temp-dir (make-temp-file "test-lsp-tailwindcss" t)))
    (unwind-protect
        (progn
          ;; Test case 1: tailwindcss in dependencies
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"tailwindcss\": \"^3.0.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "^3.0.0")))

          ;; Test case 2: tailwindcss in devDependencies
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"devDependencies\": {\"tailwindcss\": \"~2.1.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "~2.1.0")))

          ;; Test case 3: tailwindcss in both, dependencies is preferred
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"tailwindcss\": \"1.0.0\"}, \"devDependencies\": {\"tailwindcss\": \"2.0.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "1.0.0")))

          ;; Test case 4: tailwindcss not present
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"another-package\": \"1.0.0\"}}"))
            (should (null (lsp-tailwindcss--package-version))))

          ;; Test case 5: no package.json
          (let ((buffer-file-name (expand-file-name "index.js" (expand-file-name "foo" temp-dir))))
            (delete-file (expand-file-name "package.json" temp-dir))
            (should (null (lsp-tailwindcss--package-version)))))
      (delete-directory temp-dir t))))

(ert-deftest lsp-tailwindcss--version-v4-p-test ()
  "Test `lsp-tailwindcss--version-v4-p` function."
  ;; Versions that should be considered v4 or greater
  (should (lsp-tailwindcss--version-v4-p "^4"))
  (should (lsp-tailwindcss--version-v4-p "^4.1"))
  (should (lsp-tailwindcss--version-v4-p "~4"))
  (should (lsp-tailwindcss--version-v4-p "~4.1"))
  (should (lsp-tailwindcss--version-v4-p "4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "4.1.2"))
  (should (lsp-tailwindcss--version-v4-p "^4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "~4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "5.0.0"))
  (should (lsp-tailwindcss--version-v4-p "10.0.0"))
  (should (lsp-tailwindcss--version-v4-p "^11.2.3"))

  ;; Versions that should not be considered v4
  (should-not (lsp-tailwindcss--version-v4-p "^3"))
  (should-not (lsp-tailwindcss--version-v4-p "^3.1"))
  (should-not (lsp-tailwindcss--version-v4-p "~3"))
  (should-not (lsp-tailwindcss--version-v4-p "~3.1"))
  (should-not (lsp-tailwindcss--version-v4-p "3.0.0"))
  (should-not (lsp-tailwindcss--version-v4-p "3.9.9"))
  (should-not (lsp-tailwindcss--version-v4-p "^3.1.0"))
  (should-not (lsp-tailwindcss--version-v4-p "~3.1.0"))
  (should-not (lsp-tailwindcss--version-v4-p "0.4.0"))
  (should-not (lsp-tailwindcss--version-v4-p "1.2.3"))
  (should-not (lsp-tailwindcss--version-v4-p "latest"))
  (should-not (lsp-tailwindcss--version-v4-p "next")))

;;; lsp-tailwindcss-test.el ends here
