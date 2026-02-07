;;; lsp-file-watch-test.el --- File watch tests   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski
;; Copyright (C) 2018-2026 lsp-mode maintainers

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
                   (add-to-list 'events (cdr event)))
                 lsp-file-watch-ignored-files
                 lsp-file-watch-ignored-directories))

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
                   #'ignore
                   lsp-file-watch-ignored-files
                   lsp-file-watch-ignored-directories)))

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
                   (add-to-list 'events (cdr event)))
                 lsp-file-watch-ignored-files
                 lsp-file-watch-ignored-directories))

    (write-region "bla" nil matching-file)
    (sit-for 0.3)

    (add-to-list 'expected-events (list 'created matching-file))
    (add-to-list 'expected-events (list 'changed matching-file))

    (should (equal expected-events events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--glob-split ()
  (should (equal (lsp-glob-unbrace-at-top-level "{/home/alice/project/**/*.hs,/home/alice/project/stack.yaml}")
                 '("/home/alice/project/**/*.hs" "/home/alice/project/stack.yaml")))
  (should (equal (lsp-glob-unbrace-at-top-level "{/home/alice/project/**/*.{ml,eliom},/home/alice/project/dune.project}")
                 '("/home/alice/project/**/*.{ml,eliom}" "/home/alice/project/dune.project"))))

(ert-deftest lsp-file-watch--glob-pattern ()
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "pom.xml") "pom.xml"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/pom.xml") "/pom.xml"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/*.xml") "data/pom.xml"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/*.xml") "pom.xml"))

  ;; Some VSCode tests
  ;; (https://github.com/Microsoft/vscode/blob/466da1c9013c624140f6d1473b23a870abc82d44/src/vs/base/test/node/glob.test.ts)
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") ".git"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") ".hidden.txt"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "git"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "hidden.txt"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "path/.git"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "path/.hidden.txt"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "path/git"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "**/.*") "pat.h/hidden.txt"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/node_modules/**") "node_modules"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "**/node_modules/**") "node_modules/"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "**/node_modules/**") "node/_modules/"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "?") "h"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "?") "hi"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[[]") "foo.["))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{foo,bar}/**") "foo"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{foo,bar}/**") "bar"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{foo,bar}/**") "foo/test"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{foo,bar}/**") "bar/test"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{**/*.d.ts,**/*.js}") "/testing/foo.js"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{**/*.d.ts,**/*.js}") "testing/foo.d.ts"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "{**/*.d.ts,**/*.js,foo.[0-9]}") "foo.5"))

  (should (string-match (lsp-glob-convert-to-wrapped-regexp "some/**/*") "some/foo.js"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "some/**/*") "some/folder/foo.js"))

  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "some/**/*") "something/foo.js"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "some/**/*") "something/folder/foo.js"))

  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "{**/*.d.ts,**/*.js,foo.[0-9]}") "foo.f"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "prefix/{**/*.d.ts,**/*.js,foo.[0-9]}") "prefix/foo.8"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "prefix/{**/*.d.ts,**/*.js,foo.[0-9]}") "prefix/foo.f"))

  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[!0-9]") "foo.5"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[!0-9]") "foo.8"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[!0-9]") "foo.f"))

  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[^0-9]") "foo.5"))
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[^0-9]") "foo.8"))
  (should (string-match (lsp-glob-convert-to-wrapped-regexp "foo.[^0-9]") "foo.f"))

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
  (should-not (string-match (lsp-glob-convert-to-wrapped-regexp "foo[/]bar") "foo/bar")))

(ert-deftest lsp-file-watch--ignore-list ()
  :tags '(no-win)
  (let* ((temp-directory (make-temp-file "test-dir" t))
         (nested-dir (f-join temp-directory "nested"))
         (nested-matching-file (f-join nested-dir "file.ext"))
         (create-lockfiles nil)
         (ignored-files lsp-file-watch-ignored-files)
         (ignored-directories '("nested"))
         events watch)

    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
                 (lambda (event) (add-to-list 'events (cdr event)))
                 ignored-files
                 ignored-directories))

    (write-region "bla" nil nested-matching-file)
    (sit-for 0.3)

    (should (null events))
    (lsp-kill-watch watch)))

(ert-deftest lsp-file-watch--invalid-regex-handling ()
  "Test that invalid regex patterns are handled gracefully (issue #3439)."
  ;; Clear warning cache for clean test
  (clrhash lsp--warned-invalid-regexps)
  ;; Test with valid patterns - should match
  (should (equal "[/\\\\]\\.git\\'"
                 (lsp--string-match-any '("[/\\\\]\\.git\\'") "/project/.git")))
  (should (equal "[/\\\\]__pycache__\\'"
                 (lsp--string-match-any '("[/\\\\]__pycache__\\'") "/project/__pycache__")))
  ;; Test with no match
  (should-not (lsp--string-match-any '("[/\\\\]\\.git\\'") "/project/src"))
  ;; Test with invalid regex - should return nil and not signal error
  ;; Use unclosed bracket which is invalid in ALL Emacs versions (28, 29, 30+)
  (should-not (lsp--string-match-any '("[/\\\\][unclosed") "/project/__pycache__"))
  ;; Test that valid patterns still work when mixed with invalid ones
  (should (equal "[/\\\\]\\.git\\'"
                 (lsp--string-match-any '("[invalid-unclosed"
                                          "[/\\\\]\\.git\\'")
                                        "/project/.git")))
  ;; Edge case: empty list
  (should-not (lsp--string-match-any '() "/project/.git"))
  ;; Edge case: empty string to match against
  (should-not (lsp--string-match-any '("[/\\\\]\\.git\\'") ""))
  ;; Edge case: all invalid patterns (unclosed brackets - invalid in all versions)
  (should-not (lsp--string-match-any '("[bad1" "[bad2")
                                     "/project/.git")))

(ert-deftest lsp-file-watch--invalid-regex-warning-cache ()
  "Test that invalid regex warnings are only shown once per pattern."
  ;; Clear warning cache
  (clrhash lsp--warned-invalid-regexps)
  ;; First call should cache the invalid pattern
  ;; Use unclosed bracket which is invalid in ALL Emacs versions
  (should-not (lsp--string-match-any '("[cached-unclosed") "/test"))
  (should (gethash "[cached-unclosed" lsp--warned-invalid-regexps))
  ;; Verify cache prevents repeated warnings (pattern is already cached)
  (let ((cache-size (hash-table-count lsp--warned-invalid-regexps)))
    (lsp--string-match-any '("[cached-unclosed") "/test2")
    ;; Cache size should not increase for same pattern
    (should (= cache-size (hash-table-count lsp--warned-invalid-regexps)))))

(ert-deftest lsp-file-watch--adding-watches ()
  :tags '(no-win)
  (let* ((temp-directory (make-temp-file "test-dir" t))
         (nested-dir (f-join temp-directory "nested"))
         (nested-matching-file (f-join nested-dir "file.ext"))
         (create-lockfiles nil)
         (ignored-files lsp-file-watch-ignored-files)
         (ignored-directories lsp-file-watch-ignored-directories)
         events watch expected-events)

    (mkdir nested-dir)

    (setq watch (lsp-watch-root-folder
                 temp-directory
                 (lambda (event)
                   (add-to-list 'events (cdr event)))
                 ignored-files
                 ignored-directories))

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

;;; Symlink and Depth Limit Tests
;;
;; These tests verify the symlink policy and depth limit features:
;; - `lsp-file-watch-follow-symlinks': nil | 'within-root | t
;; - `lsp-file-watch-max-depth': integer | nil

(defmacro lsp-file-watch--with-temp-dirs (bindings &rest body)
  "Execute BODY with temporary directories bound in BINDINGS.
Each element of BINDINGS is (VAR [TRUENAME]).
Directories are automatically cleaned up after BODY executes."
  (declare (indent 1))
  (let* ((vars (mapcar #'car bindings))
         (truename-flags (mapcar (lambda (b) (nth 1 b)) bindings))
         (temp-creates (cl-mapcar
                        (lambda (var truename)
                          `(,var ,(if truename
                                      '(file-truename (make-temp-file "lsp-test" t))
                                    '(make-temp-file "lsp-test" t))))
                        vars truename-flags)))
    `(let* (,@temp-creates)
       (unwind-protect
           (progn ,@body)
         ,@(mapcar (lambda (v) `(ignore-errors (delete-directory ,v t))) vars)))))

(defun lsp-file-watch--get-watchable-dirs (root &optional policy max-depth)
  "Get watchable directories under ROOT with optional POLICY and MAX-DEPTH.
POLICY should be nil, `within-root', or t. When not provided, defaults to
nil (never follow). This is a test helper that provides sensible defaults."
  (let* ((lsp-file-watch-follow-symlinks policy)
         (lsp-file-watch-max-depth max-depth)
         (truename-root (file-truename root)))
    (lsp--all-watchable-directories
     truename-root nil truename-root (make-hash-table :test 'equal) 0)))

;; Symlink policy: within-root

(ert-deftest lsp-file-watch--symlink-within-root-followed ()
  "Symlinks pointing inside workspace root should be followed."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root))
    (let ((sub (f-join root "sub"))
          (target (f-join root "target")))
      (make-directory sub t)
      (make-directory target t)
      (make-symbolic-link target (f-join sub "link"))
      (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
        (should (member (file-truename target) result))))))

(ert-deftest lsp-file-watch--symlink-outside-root-skipped ()
  "Symlinks pointing outside workspace root should be excluded."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root) (outside))
    (make-symbolic-link outside (f-join root "link"))
    (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
      (should-not (member (file-truename outside) result)))))

;; Symlink policy: nil (never follow)

(ert-deftest lsp-file-watch--policy-nil-never-follows ()
  "With policy nil, symlinks are listed but never followed."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((target (f-join root "target"))
          (link (f-join root "link")))
      (make-directory target t)
      (make-symbolic-link target link)
      (let ((result (lsp-file-watch--get-watchable-dirs root nil)))
        (should (member root result))      ; Root included
        (should (member link result))      ; Symlink listed as leaf
        (should (= 1 (cl-count target result :test #'equal)))  ; Target only once
        (should (= 3 (length result)))))))  ; Exactly: root, target, link

;; Symlink policy: t (always follow)

(ert-deftest lsp-file-watch--policy-t-always-follows ()
  "With policy t, all symlinks are followed including external ones."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root) (outside))
    (make-symbolic-link outside (f-join root "link"))
    (let ((result (lsp-file-watch--get-watchable-dirs root t)))
      (should (member (file-truename outside) result)))))

;; Edge case: workspace root is itself a symlink

(ert-deftest lsp-file-watch--root-is-symlink ()
  "Symlink as workspace root should work correctly."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((real-dir t))
    (let ((link-dir (concat (make-temp-file "lsp-link") "-link")))
      (make-symbolic-link real-dir link-dir)
      (make-directory (f-join real-dir "sub") t)
      (unwind-protect
          (let* ((truename-root (file-truename link-dir))
                 (result (lsp-file-watch--get-watchable-dirs link-dir 'within-root)))
            (should (member truename-root result))
            (should (member (f-join truename-root "sub") result)))
        (delete-file link-dir)))))

;; Depth limit tests

(ert-deftest lsp-file-watch--depth-limit-stops-recursion ()
  "Depth limit should stop recursion at the specified level."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((d1 (f-join root "d1"))
          (d2 (f-join root "d1/d2"))
          (d3 (f-join root "d1/d2/d3"))
          (warned nil))
      (make-directory d3 t)  ; Creates d1, d2, d3
      (cl-letf (((symbol-function 'lsp-warn) (lambda (&rest _) (setq warned t))))
        (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root 2)))
          ;; Depth 0, 1, 2 should be included
          (should (member root result))
          (should (member d1 result))
          (should (member d2 result))
          ;; Depth 3 should be excluded
          (should-not (member d3 result))
          (should warned))))))

(ert-deftest lsp-file-watch--max-depth-zero-root-only ()
  "With max-depth 0, only the root directory is watched."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (make-directory (f-join root "sub") t)
    (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root 0)))
      (should (= 1 (length result)))
      (should (member root result)))))

(ert-deftest lsp-file-watch--max-depth-nil-unlimited ()
  "With max-depth nil, recursion is unlimited."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((deep (f-join root "a/b/c/d/e")))
      (make-directory deep t)
      (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root nil)))
        (should (member deep result))
        (should (= 6 (length result)))))))  ; root + 5 levels

;; Error handling

(ert-deftest lsp-file-watch--dangling-symlink-skipped ()
  "Dangling symlinks should be skipped gracefully."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root))
    (make-symbolic-link "/nonexistent/path/lsp-test" (f-join root "broken"))
    (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
      (should result)  ; Should complete without error
      (should-not (member (f-join root "broken") result)))))

(ert-deftest lsp-file-watch--root-equality-included ()
  "Root directory is included via equality check (not just file-in-directory-p)."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
      (should (member root result)))))

;; Cycle detection edge cases

(ert-deftest lsp-file-watch--circular-symlink-handled ()
  "Circular symlinks should not cause infinite loops."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((dir-a (f-join root "a"))
          (dir-b (f-join root "b")))
      (make-directory dir-a t)
      (make-directory dir-b t)
      (make-symbolic-link dir-b (f-join dir-a "to-b"))
      (make-symbolic-link dir-a (f-join dir-b "to-a"))
      ;; Should complete without hanging due to visited hash table
      (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
        (should result)
        (should (member root result))
        (should (member dir-a result))
        (should (member dir-b result))))))

(ert-deftest lsp-file-watch--symlink-to-parent-handled ()
  "Symlink pointing to parent directory should not cause infinite recursion."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((sub (f-join root "sub")))
      (make-directory sub t)
      (make-symbolic-link root (f-join sub "parent-link"))
      ;; Should complete without infinite loop
      (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
        (should result)
        (should (member root result))
        (should (member sub result))))))

;; Runtime callback filtering

(ert-deftest lsp-file-watch--runtime-callback-within-root-filter ()
  "Runtime callback filters out files from symlinks pointing outside workspace."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t) (outside t))
    (let* ((new-dir (f-join root "subdir"))
           (lsp-file-watch-follow-symlinks 'within-root)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil)
           (watch (make-lsp-watch :root-directory root))
           (collected-events nil))
      (make-directory new-dir t)
      (make-symbolic-link outside (f-join new-dir "external"))
      (write-region "inside" nil (f-join new-dir "file.txt"))
      (write-region "outside" nil (f-join outside "external.txt"))
      ;; Mock file-notify-add-watch to prevent real file watcher setup
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _callback) (gensym "mock-watch"))))
        ;; Call the real production callback with a synthetic directory-created event
        (lsp--folder-watch-callback
         (list nil 'created new-dir)
         (lambda (event) (push event collected-events))
         watch nil nil))
      ;; Exactly one file event should be collected (only file.txt)
      (let ((file-events (seq-filter (lambda (ev)
                                       (not (file-directory-p (cl-third ev))))
                                     collected-events)))
        (should (= 1 (length file-events)))
        (should (string= (cl-third (car file-events))
                          (f-join new-dir "file.txt")))))))

(ert-deftest lsp-file-watch--runtime-callback-policy-t-includes-external ()
  "Runtime callback with policy t follows symlinks and includes external files."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t) (outside t))
    (let* ((new-dir (f-join root "subdir"))
           (lsp-file-watch-follow-symlinks t)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil)
           (watch (make-lsp-watch :root-directory root))
           (collected-events nil))
      (make-directory new-dir t)
      (make-symbolic-link outside (f-join new-dir "external"))
      (write-region "inside" nil (f-join new-dir "file.txt"))
      (write-region "outside" nil (f-join outside "external.txt"))
      ;; Mock file-notify-add-watch to prevent real file watcher setup
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _callback) (gensym "mock-watch"))))
        ;; Call the real production callback with a synthetic directory-created event
        (lsp--folder-watch-callback
         (list nil 'created new-dir)
         (lambda (event) (push event collected-events))
         watch nil nil))
      ;; Policy t follows symlinks so both internal and external files should appear
      (let ((file-events (seq-filter (lambda (ev)
                                       (not (file-directory-p (cl-third ev))))
                                     collected-events)))
        (should (= 2 (length file-events)))
        (should (seq-find (lambda (ev)
                            (string= (cl-third ev)
                                     (f-join new-dir "file.txt")))
                          file-events))
        (should (seq-find (lambda (ev)
                            (string= (cl-third ev)
                                     (f-join new-dir "external" "external.txt")))
                          file-events))))))

(ert-deftest lsp-file-watch--runtime-callback-policy-nil-no-symlink-files ()
  "Runtime callback with policy nil does not descend into symlinked directories."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t) (outside t))
    (let* ((new-dir (f-join root "subdir"))
           (lsp-file-watch-follow-symlinks nil)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil)
           (watch (make-lsp-watch :root-directory root))
           (collected-events nil))
      (make-directory new-dir t)
      (make-symbolic-link outside (f-join new-dir "external"))
      (write-region "inside" nil (f-join new-dir "file.txt"))
      (write-region "outside" nil (f-join outside "external.txt"))
      ;; Mock file-notify-add-watch to prevent real file watcher setup
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _callback) (gensym "mock-watch"))))
        ;; Call the real production callback with a synthetic directory-created event
        (lsp--folder-watch-callback
         (list nil 'created new-dir)
         (lambda (event) (push event collected-events))
         watch nil nil))
      ;; Policy nil does not follow symlinks so only internal file should appear
      (let ((file-events (seq-filter (lambda (ev)
                                       (not (file-directory-p (cl-third ev))))
                                     collected-events)))
        (should (= 1 (length file-events)))
        (should (string= (cl-third (car file-events))
                          (f-join new-dir "file.txt")))))))

(ert-deftest lsp-file-watch--runtime-callback-symlink-alias-event-path ()
  "Runtime callback resolves symlink event paths before filtering.
When the file-notify event reports a path through a symlink alias
rather than the truename, the within-root filter should still
include files that are actually inside the workspace."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let* ((real-sub (f-join root "real-sub"))
           (link-sub (f-join root "link-sub"))
           (lsp-file-watch-follow-symlinks 'within-root)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil)
           (watch (make-lsp-watch :root-directory root))
           (collected-events nil))
      (make-directory real-sub t)
      (make-symbolic-link real-sub link-sub)
      (write-region "content" nil (f-join real-sub "file.el"))
      ;; Mock file-notify-add-watch to prevent real file watcher setup
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _callback) (gensym "mock-watch"))))
        ;; Simulate event with the SYMLINK path (not the truename).
        ;; Before the fix, the callback would use the raw symlink path
        ;; for directory-files-recursively, returning paths like
        ;; root/link-sub/file.el which fail string-prefix-p against
        ;; the truename root.  After the fix, file-name is resolved
        ;; to its truename first.
        (lsp--folder-watch-callback
         (list nil 'created link-sub)
         (lambda (event) (push event collected-events))
         watch nil nil))
      ;; The file should be collected even though the event used the symlink path
      (let ((file-events (seq-filter (lambda (ev)
                                       (not (file-directory-p (cl-third ev))))
                                     collected-events)))
        (should (= 1 (length file-events)))
        ;; Path should be under the resolved truename, not the symlink alias
        (should (string-prefix-p (file-name-as-directory root)
                                 (cl-third (car file-events))))))))

(ert-deftest lsp-file-watch--runtime-callback-symlink-alias-outside-root ()
  "Runtime callback rejects files from symlink alias resolving outside root.
When the event path is a symlink inside the workspace that points to
a directory outside the workspace, truename resolution should cause
the within-root filter to correctly reject those files."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t) (outside t))
    (let* ((link-dir (f-join root "ext-link"))
           (lsp-file-watch-follow-symlinks 'within-root)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil)
           (watch (make-lsp-watch :root-directory root))
           (collected-events nil))
      (make-symbolic-link outside link-dir)
      (write-region "external" nil (f-join outside "secret.txt"))
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _callback) (gensym "mock-watch"))))
        (lsp--folder-watch-callback
         (list nil 'created link-dir)
         (lambda (event) (push event collected-events))
         watch nil nil))
      ;; No file events should be collected — the symlink resolves outside root
      (let ((file-events (seq-filter (lambda (ev)
                                       (not (file-directory-p (cl-third ev))))
                                     collected-events)))
        (should (= 0 (length file-events)))))))

(ert-deftest lsp-file-watch--watch-root-folder-stores-truename ()
  "Verify that `lsp-watch-root-folder' stores a truename in the watch struct.
Even when called with a symlink path, the resulting watch's root-directory
should be the resolved truename."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((real-dir t))
    (let* ((link-dir (concat (make-temp-file "lsp-link") "-link"))
           (lsp-file-watch-follow-symlinks 'within-root)
           (lsp-file-watch-threshold nil)
           (lsp-file-watch-max-depth nil))
      (make-symbolic-link real-dir link-dir)
      (unwind-protect
          (cl-letf (((symbol-function 'file-notify-add-watch)
                     (lambda (_dir _flags _callback) (gensym "mock-watch"))))
            (let ((watch (lsp-watch-root-folder link-dir #'ignore nil nil)))
              ;; root-directory should be the truename, not the symlink path
              (should (string= (lsp-watch-root-directory watch)
                               (file-truename link-dir)))
              (should (string= (lsp-watch-root-directory watch)
                               real-dir))))
        (delete-file link-dir)))))

(ert-deftest lsp-file-watch--dangling-symlink-policy-t-no-crash ()
  "Dangling symlinks should not crash even with the most permissive policy (t)."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (make-symbolic-link "/nonexistent/lsp-test-target" (f-join root "dangling"))
    (let ((result (lsp-file-watch--get-watchable-dirs root t)))
      (should result)
      (should (member root result)))))

(ert-deftest lsp-file-watch--depth-limit-with-symlinks ()
  "Depth limit should apply correctly when symlinks are present in the tree."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let ((target (f-join root "target"))
          (a (f-join root "target" "a"))
          (b (f-join root "target" "a" "b")))
      (make-directory b t)  ; Creates target, a, b
      (make-symbolic-link target (f-join root "link"))
      (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root 2)))
        ;; Depth 0: root, depth 1: target, depth 2: a => all included
        (should (member root result))
        (should (member target result))
        (should (member a result))
        ;; Depth 3: b => excluded by depth limit
        (should-not (member b result))
        ;; Symlink root/link resolves to root/target (already visited) => skipped
        (should-not (member (f-join root "link") result))))))

(ert-deftest lsp-file-watch--ignored-dirs-with-symlinks ()
  "Ignored directory patterns should exclude symlink targets matching the pattern."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t))
    (let* ((node-modules (f-join root "node_modules"))
           (src (f-join root "src"))
           (lsp-file-watch-follow-symlinks 'within-root)
           (lsp-file-watch-max-depth nil)
           (ignored '("[/\\\\]\\.git\\'" "[/\\\\]node_modules\\'")))
      (make-directory node-modules t)
      (make-directory src t)
      (let ((result (lsp--all-watchable-directories
                     root ignored (file-truename root)
                     (make-hash-table :test 'equal) 0)))
        ;; Root and src should be included
        (should (member root result))
        (should (member src result))
        ;; node_modules should be excluded by ignore pattern
        (should-not (member node-modules result))))))

(ert-deftest lsp-file-watch--file-truename-error-handled ()
  "When file-truename fails on a symlink, the error is caught and link skipped."
  :tags '(no-win)
  (lsp-file-watch--with-temp-dirs ((root t) (outside))
    (let* ((link (f-join root "bad-link"))
           (sub (f-join root "sub"))
           (original-file-truename (symbol-function 'file-truename))
           (logged nil))
      ;; Create a symlink so f-symlink? returns t, triggering the
      ;; condition-case path that guards file-truename in
      ;; lsp--all-watchable-directories.
      (make-symbolic-link outside link)
      (make-directory sub t)
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (path &rest args)
                   (if (string-match-p "bad-link" path)
                       (signal 'file-error (list "Permission denied" path))
                     (apply original-file-truename path args))))
                ((symbol-function 'lsp-log)
                 (lambda (&rest _) (setq logged t))))
        (let ((result (lsp-file-watch--get-watchable-dirs root 'within-root)))
          ;; Should complete without error
          (should result)
          (should (member root result))
          ;; Normal subdirectory should still be included
          (should (member sub result))
          ;; Unresolvable symlink should be excluded (can't verify boundary)
          (should-not (member link result))
          ;; The error should have been logged
          (should logged))))))

;;; lsp-file-watch-test.el ends here
