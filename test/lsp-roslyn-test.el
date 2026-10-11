;;; lsp-roslyn-test.el --- unit tests for lsp-roslyn -*- lexical-binding: t; -*-

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

;; Unit tests for the lsp-roslyn client.

;;; Code:

(require 'ert)
(require 'f)
(require 'lsp-mode)
(require 'lsp-roslyn)

(defun lsp-roslyn-test--find-solution (file-in-project)
  "Simulate `lsp-roslyn--find-solution-file' for FILE-IN-PROJECT."
  (cl-letf (((symbol-function 'lsp-roslyn--pick-solution-file-interactively)
             (lambda (solutions) solutions)))
    (let ((buffer-file-name file-in-project))
      (lsp-roslyn--find-solution-file))))

(ert-deftest lsp-roslyn-find-slnx-solution-file ()
  "Detect .slnx solution files, not only .sln.
Regression test for issue #5106."
  (let* ((root (file-truename (make-temp-file "roslyn-slnx-test" t)))
         (solution (expand-file-name "MySolution.slnx" root))
         (project-dir (expand-file-name "src/project/" root))
         (source (expand-file-name "code.cs" project-dir)))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (write-region "" nil solution)
          (write-region "" nil source)
          (should (equal (lsp-roslyn-test--find-solution source) solution)))
      (delete-directory root t))))

(ert-deftest lsp-roslyn-find-sln-solution-file ()
  "Classic .sln solution files are still detected."
  (let* ((root (file-truename (make-temp-file "roslyn-sln-test" t)))
         (solution (expand-file-name "MySolution.sln" root))
         (project-dir (expand-file-name "src/project/" root))
         (source (expand-file-name "code.cs" project-dir)))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (write-region "" nil solution)
          (write-region "" nil source)
          (should (equal (lsp-roslyn-test--find-solution source) solution)))
      (delete-directory root t))))

(ert-deftest lsp-roslyn-finds-both-sln-and-slnx-solutions ()
  "When .sln and .slnx coexist, both are offered as candidates."
  (let* ((root (file-truename (make-temp-file "roslyn-both-test" t)))
         (sln (expand-file-name "Classic.sln" root))
         (slnx (expand-file-name "Modern.slnx" root))
         (project-dir (expand-file-name "src/project/" root))
         (source (expand-file-name "code.cs" project-dir)))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (write-region "" nil sln)
          (write-region "" nil slnx)
          (write-region "" nil source)
          (let ((result (lsp-roslyn-test--find-solution source)))
            (should (equal (sort result #'string-lessp)
                           (sort (list sln slnx) #'string-lessp)))))
      (delete-directory root t))))

(provide 'lsp-roslyn-test)
;;; lsp-roslyn-test.el ends here
