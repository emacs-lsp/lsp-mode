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

(ert-deftest lsp-file-watch--recursive ()
  :tags '(no-win)
  (let* ((temp-directory (make-temp-file "test-dir" t))
         (matching-file (f-join temp-directory "file.ext"))
         (nested-dir (f-join temp-directory "nested"))
         (nested-matching-file (f-join nested-dir "file.ext"))
         (create-lockfiles nil)
         events watch expected-events)

    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
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
    (sit-for 0.3)

    ;; not changed
    (should (equal expected-events events))

    (write-region "bla" nil nested-matching-file)
    (sit-for 0.3)

    (add-to-list 'expected-events (list 'created nested-matching-file))
    (add-to-list 'expected-events (list 'changed nested-matching-file))

    (should (equal expected-events events))

    (let* ((nested-dir2 (f-join temp-directory "nested2"))
           (nested-matching-file2 (f-join nested-dir2 "newNestedFile.ext")))

      (mkdir nested-dir2)
      (write-region "bla" nil nested-matching-file2)

      (sit-for 0.3)

      (add-to-list 'expected-events (list 'created nested-matching-file2))

      (should (equal expected-events events))
      (write-region "bla2" nil nested-matching-file2)

      (add-to-list 'expected-events (list 'changed nested-matching-file2))

      (sit-for 0.3)
      (should (equal expected-events events)))

    (should (equal expected-events events))

    ;; delete directory
    (delete-directory nested-dir t)

    (sit-for 0.3)
    (add-to-list 'expected-events (list 'deleted nested-matching-file))
    (add-to-list 'expected-events (list 'deleted nested-dir))

    (should (equal expected-events events))

    (lsp-kill-watch watch)

    ;; create directory and then create a file
    ;; no updates after change
    (write-region "bla1" nil matching-file)
    (sit-for 0.3)

    (should (equal expected-events events))))

(ert-deftest lsp-file-watch--non-existing ()
  :tags '(no-win)
  (lsp-kill-watch (lsp-watch-root-folder
                   "non-existing-directory"
                   #'ignore)))

(ert-deftest lsp-file-watch--relative-path-glob-patterns ()
  :tags '(no-win)
  (let* ((temp-directory (file-name-as-directory
                          (concat temporary-file-directory "common-test-dir")))
         (matching-file (concat temp-directory "file.ext"))
         (create-lockfiles nil)
         (nested-dir (file-name-as-directory
                      (concat temp-directory "nested")))
         events watch expected-events)

    (delete-directory temp-directory t)
    (mkdir temp-directory)
    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
                 (lambda (event)
                   (message "received: %s" event)
                   (add-to-list 'events (cdr event)))))

    (write-region "bla" nil matching-file)
    (sit-for 0.3)

    (add-to-list 'expected-events (list 'created matching-file))
    (add-to-list 'expected-events (list 'changed matching-file))

    (should (equal expected-events events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--glob-pattern ()
  (should (string-match (lsp-glob-to-regexp "pom.xml") "pom.xml"))
  (should (string-match (lsp-glob-to-regexp "**/pom.xml") "/pom.xml"))
  (should (string-match (lsp-glob-to-regexp "**/*.xml") "data/pom.xml"))
  (should (string-match (lsp-glob-to-regexp "**/*.xml") "pom.xml"))

  ;; Some VSCode tests
  ;; (https://github.com/Microsoft/vscode/blob/466da1c9013c624140f6d1473b23a870abc82d44/src/vs/base/test/node/glob.test.ts)
  (should (string-match (lsp-glob-to-regexp "**/.*") ".git"))
  (should (string-match (lsp-glob-to-regexp "**/.*") ".hidden.txt"))
  (should (not (string-match (lsp-glob-to-regexp "**/.*") "git")))
  (should (not (string-match (lsp-glob-to-regexp "**/.*") "hidden.txt")))

  (should (string-match (lsp-glob-to-regexp "**/.*") "path/.git"))
  (should (string-match (lsp-glob-to-regexp "**/.*") "path/.hidden.txt"))
  (should (not (string-match (lsp-glob-to-regexp "**/.*") "path/git")))
  (should (not (string-match (lsp-glob-to-regexp "**/.*") "pat.h/hidden.txt")))

  (should (string-match (lsp-glob-to-regexp "**/node_modules/**") "node_modules"))
  (should (string-match (lsp-glob-to-regexp "**/node_modules/**") "node_modules/"))
  (should (not (string-match (lsp-glob-to-regexp "**/node_modules/**") "node/_modules/")))

  (should (string-match (lsp-glob-to-regexp "?") "h"))
  (should (not (string-match (lsp-glob-to-regexp "?") "hi")))

  (should (string-match (lsp-glob-to-regexp "foo.[[]") "foo.["))

  (should (string-match (lsp-glob-to-regexp "{foo,bar}/**") "foo"))
  (should (string-match (lsp-glob-to-regexp "{foo,bar}/**") "bar"))
  (should (string-match (lsp-glob-to-regexp "{foo,bar}/**") "foo/test"))
  (should (string-match (lsp-glob-to-regexp "{foo,bar}/**") "bar/test"))

  (should (string-match (lsp-glob-to-regexp "{**/*.d.ts,**/*.js}") "/testing/foo.js"))
  (should (string-match (lsp-glob-to-regexp "{**/*.d.ts,**/*.js}") "testing/foo.d.ts"))
  (should (string-match (lsp-glob-to-regexp "{**/*.d.ts,**/*.js,foo.[0-9]}") "foo.5"))

  (should (string-match (lsp-glob-to-regexp "some/**/*") "some/foo.js"))
  (should (string-match (lsp-glob-to-regexp "some/**/*") "some/folder/foo.js"))

  (should (not (string-match (lsp-glob-to-regexp "some/**/*") "something/foo.js")))
  (should (not (string-match (lsp-glob-to-regexp "some/**/*") "something/folder/foo.js")))

  (should (not (string-match (lsp-glob-to-regexp "{**/*.d.ts,**/*.js,foo.[0-9]}") "foo.f")))
  (should (string-match (lsp-glob-to-regexp "prefix/{**/*.d.ts,**/*.js,foo.[0-9]}") "prefix/foo.8"))
  (should (not (string-match (lsp-glob-to-regexp "prefix/{**/*.d.ts,**/*.js,foo.[0-9]}") "prefix/foo.f")))

  (should (not (string-match (lsp-glob-to-regexp "foo.[!0-9]") "foo.5")))
  (should (not (string-match (lsp-glob-to-regexp "foo.[!0-9]") "foo.8")))
  (should (string-match (lsp-glob-to-regexp "foo.[!0-9]") "foo.f"))

  (should (not (string-match (lsp-glob-to-regexp "foo.[^0-9]") "foo.5")))
  (should (not (string-match (lsp-glob-to-regexp "foo.[^0-9]") "foo.8")))
  (should (string-match (lsp-glob-to-regexp "foo.[^0-9]") "foo.f"))

  ;; ???: This should properly fail since path-separators should be
  ;; ignored inside brackets, but here (and in VSCode) it fails for a
  ;; strange reason: the produced regexp is "\`foo\'" and everything
  ;; to the right of the left bracket is treated as bracket text that
  ;; never gets added because the right bracket is ignored when there
  ;; is no preceding bracket text. Hence nothing can balance the left
  ;; bracket, and all bracket text is dropped. One reasonable-looking
  ;; way of handling this to recognize that because we're unbalanced
  ;; at the end, that everything should be treated as a literal. But
  ;; after experimenting with zsh, this isn't what they use.
  (should (not (string-match (lsp-glob-to-regexp "foo[/]bar") "foo/bar"))))

(ert-deftest lsp-file-watch--ignore-list ()
  :tags '(no-win)
  (let* ((temp-directory (make-temp-file "test-dir" t))
         (nested-dir (f-join temp-directory "nested"))
         (nested-matching-file (f-join nested-dir "file.ext"))
         (create-lockfiles nil)
         (lsp-file-watch-ignored '("nested"))
         events watch)

    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
                 (lambda (event) (add-to-list 'events (cdr event)))))

    (write-region "bla" nil nested-matching-file)
    (sit-for 0.3)

    (should (null events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--adding-watches ()
  :tags '(no-win)
  (let* ((temp-directory (make-temp-file "test-dir" t))
         (nested-dir (f-join temp-directory "nested"))
         (nested-matching-file (f-join nested-dir "file.ext"))
         (create-lockfiles nil)
         events watch expected-events)

    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
                 (lambda (event)
                   (add-to-list 'events (cdr event)))))

    (write-region "bla" nil nested-matching-file)
    (sit-for 0.3)

    (add-to-list 'expected-events (list 'created nested-matching-file))
    (add-to-list 'expected-events (list 'changed nested-matching-file))

    (should (equal expected-events events))
    (lsp-kill-watch watch)))

(ert-deftest lsp--test-find-roots-for-workspace ()
  (let* ((root (make-temp-file "test-root-directory" t))
         (lsp--cur-workspace (make-lsp--workspace))
         (lsp--session (make-lsp-session
                        :folder->servers (ht (root (list lsp--cur-workspace))))))

    (should (equal (lsp-find-roots-for-workspace lsp--cur-workspace lsp--session)
                   (list root)))))


(defvar lsp--test-events nil)

(defun lsp-notify-collect (_ method params)
  (push (list (--map (-> it lsp--workspace-client lsp--client-server-id)
                     (lsp-workspaces)) method params) lsp--test-events))

(ert-deftest lsp-file-notifications-test ()
  :tags '(no-win)
  (let* ((root (make-temp-file "test-root-directory" t))
         (lsp--cur-workspace (make-lsp--workspace :client (make-lsp-client :server-id 'workspace-1)))
         (lsp--session (make-lsp-session
                        :folder->servers (ht (root (list lsp--cur-workspace)))))
         (create-lockfiles nil)
         (matching-file-1 (f-join root "file-name-matching-1"))
         (matching-file-2 (f-join root "file-name-matching-2")))

    (setq lsp--test-events nil)

    (advice-add 'lsp-notify :around 'lsp-notify-collect)

    (lsp--server-register-capability
     (lsp-make-registration
      :id "test-id"
      :method "workspace/didChangeWatchedFiles"
      :register-options? (lsp-make-did-change-watched-files-registration-options
                          :watchers
                          ;; kind = 5 == 4 | 1 admits DELETE (4) and CREATE (1) WatchKind events, but not CHANGE (2)
                          `[,(lsp-make-file-system-watcher :glob-pattern "file-name-matching-[0-9]" :kind 5)])))

    (f-write-text "some-text" 'utf-8 matching-file-1)
    (f-write-text "more-text" 'utf-8 matching-file-2)

    (f-write-text "some-text" 'utf-8 (f-join root "not-matching"))

    (delete-file matching-file-1)

    (sit-for 0.3)

    (advice-remove 'lsp-notify 'lsp-notify-collect)

    (should (equal lsp--test-events
                   `(((workspace-1) "workspace/didChangeWatchedFiles"
                      ((changes . [((type . 3) (uri . ,(lsp--path-to-uri matching-file-1)))])))
                     ((workspace-1) "workspace/didChangeWatchedFiles"
                      ((changes . [((type . 1) (uri . ,(lsp--path-to-uri matching-file-2)))])))
                     ((workspace-1) "workspace/didChangeWatchedFiles"
                      ((changes . [((type . 1) (uri . ,(lsp--path-to-uri matching-file-1)))]))))))))

(ert-deftest lsp-file-watches-cleanup-test ()
  :tags '(no-win)
  (let* ((root (make-temp-file "test-root-directory" t))
         (lsp--cur-workspace (make-lsp--workspace :client (make-lsp-client :server-id 'workspace-1)))
         (lsp--session (make-lsp-session
                        :folder->servers (ht (root (list lsp--cur-workspace)))
                        :folders (list root)))
         (create-lockfiles nil)
         (matching-file (f-join root "file-name-matching")))

    (setq lsp--test-events nil)

    (advice-add 'lsp-notify :around 'lsp-notify-collect)

    (lsp--server-register-capability
     (lsp-make-registration
      :id "test-id"
      :method "workspace/didChangeWatchedFiles"
      :register-options? (lsp-make-did-change-watched-files-registration-options
                          :watchers
                          `[,(lsp-make-file-system-watcher :glob-pattern "file-name-matching")])))

    (should (= (ht-size (lsp-session-watches)) 1))

    (lsp--server-unregister-capability
     (lsp-make-unregistration
      :id "test-id"
      :method "workspace/didChangeWatchedFiles"))

    (f-write-text "some-text" 'utf-8 matching-file)

    (sit-for 0.3)

    (advice-remove 'lsp-notify 'lsp-notify-collect)

    (should (null lsp--test-events))
    (should (ht-empty? (lsp-session-watches)))))

(provide 'lsp-file-watch-test)
;;; lsp-file-watch-test.el ends here
