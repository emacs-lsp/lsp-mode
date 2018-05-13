;;; lsp-file-watch.el --- File notifications tests   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan <kyoncho@myoncho>
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
;; Tests for file notifications functions.

;;; Code:
(require 'lsp-mode)
(require 'filenotify)
(require 'lsp-common)
(require 'em-glob)

(ert-deftest lsp-file-watch--notifications ()
  :tags '(no-win)
  (let* ((temp-directory (file-name-as-directory
                           (concat temporary-file-directory "common-test-dir")))
          (matching-file (concat temp-directory "file.ext"))
          (nested-dir (file-name-as-directory
                        (concat temp-directory "nested")))
          (nested-matching-file (concat nested-dir "file.ext"))
          (create-lockfiles nil)
         events watch expected-events)

    (when (file-exists-p temp-directory)
      (delete-directory temp-directory t))
    (mkdir temp-directory)
    (mkdir nested-dir)

    (setq watch (lsp-create-watch
                  temp-directory
                  (list (eshell-glob-regexp "*.ext"))
                  (lambda (event)
                    (message "received: %s" event)
                    (add-to-list 'events (cdr event)))))

    (write-region "bla" nil matching-file)

    (sit-for 0.2)
    ;; created/changed events
    (setq expected-events `((changed ,matching-file)
                             (created ,matching-file)))
    (should (equal events expected-events))

    ;; create file that doesn't match the regexp
    (write-region "bla" nil (concat temp-directory "file.ex"))
    (should (equal expected-events events))

    ;; not interested in directories
    (mkdir (concat temp-directory "dirname.ext"))
    (sit-for 0.1)

    ;; not changed
    (should (equal expected-events events))

    (write-region "bla" nil nested-matching-file)
    (sit-for 0.1)

    (add-to-list 'expected-events (list 'created nested-matching-file))
    (add-to-list 'expected-events (list 'changed nested-matching-file))

    (should (equal expected-events events))
    (let* ((nested-dir2 (file-name-as-directory
                          (concat temp-directory "nested2")))
           (nested-matching-file2 (concat nested-dir2 "newNestedFile.ext")))

      (mkdir nested-dir2)
      (write-region "bla" nil nested-matching-file2)
      (write-region "bla1" nil (concat nested-matching-file2 "not-matching"))

      (sit-for 0.1)

      (add-to-list 'expected-events (list 'created nested-matching-file2))

      (should (equal expected-events events))
      (write-region "bla2" nil nested-matching-file2)

      (add-to-list 'expected-events (list 'changed nested-matching-file2))

      (sit-for 0.1)
      (should (equal expected-events events)))

    (should (equal expected-events events))

    ;; delete directory
    (delete-directory nested-dir t)

    (sit-for 0.1)
    (add-to-list 'expected-events (list 'deleted nested-matching-file))
    (should (equal expected-events events))

    (lsp-kill-watch watch)

    ;; create directory and then create a file
    ;; no updates after change
    (write-region "bla1" nil matching-file)
    (sit-for 0.1)

    (should (equal expected-events events))))

(ert-deftest lsp-file-watch--relative-path ()
  :tags '(no-win)
  (let* ((temp-directory (file-name-as-directory
                           (concat temporary-file-directory "common-test-dir")))
          (matching-file (concat temp-directory "file.ext"))
          (nested-dir (file-name-as-directory
                        (concat temp-directory "nested")))
          (nested-matching-file (concat nested-dir "file.ext"))
         events watch expected-events)

    (when (file-exists-p temp-directory)
      (delete-directory temp-directory t))
    (mkdir temp-directory)
    (mkdir nested-dir)

    (setq watch (lsp-create-watch
                  temp-directory
                  (list (eshell-glob-regexp "file.ext"))
                  (lambda (event)
                    (message "received: %s" event)
                    (add-to-list 'events (cdr event)))))

    (write-region "bla" nil matching-file)
    (sit-for 0.1)

    (add-to-list 'expected-events (list 'created matching-file))
    (add-to-list 'expected-events (list 'changed matching-file))

    (should (equal expected-events events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--relative-path-glob-patterns ()
  :tags '(no-win)
  (let* ((temp-directory (file-name-as-directory
                           (concat temporary-file-directory "common-test-dir")))
          (matching-file (concat temp-directory "file.ext"))
          (nested-dir (file-name-as-directory
                        (concat temp-directory "nested")))
          (nested-matching-file (concat nested-dir "file.ext"))
         events watch expected-events)

    (delete-directory temp-directory t)
    (mkdir temp-directory)
    (mkdir nested-dir)

    (setq watch (lsp-create-watch
                  temp-directory
                  (list (eshell-glob-regexp "**/file.ext"))
                  (lambda (event)
                    (message "received: %s" event)
                    (add-to-list 'events (cdr event)))))

    (write-region "bla" nil matching-file)
    (sit-for 0.1)

    (add-to-list 'expected-events (list 'created matching-file))
    (add-to-list 'expected-events (list 'changed matching-file))

    (should (equal expected-events events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--glob-pattern ()
  (should (string-match (eshell-glob-regexp "pom.xml") "pom.xml"))
  (should (string-match (eshell-glob-regexp "**/pom.xml") "/pom.xml"))
  (should (string-match (eshell-glob-regexp "**/*.xml") "data/pom.xml"))
  (should (not (string-match (eshell-glob-regexp "**/*.xml") "pom.xml"))))

(provide 'lsp-file-watch-test)
;;; lsp-file-watch-test.el ends here
