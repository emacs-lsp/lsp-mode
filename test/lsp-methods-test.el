;;; lsp-methods-test.el --- Tests for lsp-methods    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>

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

;;; Code:

(require 'json)
(require 'lsp-mode)

(defconst lsp-methods-test--changes "
[
  {
    \"newText\": \"extracted\",
    \"range\": {
      \"end\": {
        \"character\": 6,
        \"line\": 4
      },
      \"start\": {
        \"character\": 6,
        \"line\": 4
      }
    }
  },
  {
    \"newText\": \"();\",
    \"range\": {
      \"end\": {
        \"character\": 6,
        \"line\": 4
      },
      \"start\": {
        \"character\": 6,
        \"line\": 4
      }
    }
  },
  {
    \"newText\": \"\",
    \"range\": {
      \"end\": {
        \"character\": 30,
        \"line\": 4
      },
      \"start\": {
        \"character\": 6,
        \"line\": 4
      }
    }
  },
  {
    \"newText\": \"\\n\\n\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"private\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \" \",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"void\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \" \",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"extracted\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"() {\\n\\t\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"String s = \\\"SomeString\\\";\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  },
  {
    \"newText\": \"\\n}\",
    \"range\": {
      \"end\": {
        \"character\": 3,
        \"line\": 5
      },
      \"start\": {
        \"character\": 3,
        \"line\": 5
      }
    }
  }
]")

(ert-deftest lsp--test-overlapping-updates ()
  (let* ((input "package test;

public class Temp {
  public void name() {
      String s = \"SomeString\";
  }
}")
         (expected "package test;

public class Temp {
  public void name() {
      extracted();
  }

private void extracted() {
	String s = \"SomeString\";
}
}")
         (actual (with-temp-buffer
                   (insert input)
                   (lsp--apply-text-edits
                    (lsp--read-json lsp-methods-test--changes))
                   (buffer-string))))
    (should (string= actual expected))))

(ert-deftest lsp-create-test ()
  (let ((new-file-name (make-temp-file "new")))
    (delete-file new-file-name)
    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-create-file :uri (lsp--path-to-uri new-file-name)
                               :kind "create")]))
    (should (f-exists? new-file-name)))
  (let ((new-file-name (make-temp-file "should be overridden")))
    (f-write-text "should be overridden" nil new-file-name)
    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-create-file :uri (lsp--path-to-uri new-file-name)
                               :kind "create"
                               :options? (lsp-make-create-file-options :overwrite? t))]))
    (should (equal (f-read-text new-file-name) "")))
  (let ((new-file-name (make-temp-file "should not be overridden")))
    (f-write-text "text" nil new-file-name)
    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-create-file :uri (lsp--path-to-uri new-file-name)
                               :kind "create"
                               :options? (lsp-make-create-file-options :overwrite? nil))]))
    (should (equal (f-read-text new-file-name) "text")))
  ;; nested file without parent dir
  (let ((new-file-name (make-temp-file "new")))
    (delete-file new-file-name)
    (setq new-file-name (f-join new-file-name "nested"))
    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-create-file :uri (lsp--path-to-uri new-file-name)
                               :kind "create")]))
    (should (f-exists? new-file-name))))

(ert-deftest lsp-delete-test ()
  (let ((delete-file-name (make-temp-file "to-delete")))
    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-delete-file :uri (lsp--path-to-uri delete-file-name)
                               :kind "delete")]))
    (should-not (f-exists? delete-file-name))))

(ert-deftest lsp-update-test ()
  (let ((old-file-name (make-temp-file "old"))
        (new-file-name (make-temp-file "new")))

    (f-delete new-file-name)

    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-rename-file :old-uri (lsp--path-to-uri old-file-name)
                               :new-uri (lsp--path-to-uri new-file-name)
                               :kind "rename")]))
    (should-not (f-exists? old-file-name))
    (should (f-exists? new-file-name)))

  ;; override
  (let ((old-file-name (make-temp-file "old"))
        (new-file-name (make-temp-file "new")))

    (f-write-text "should be overridden" nil new-file-name)

    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-rename-file :old-uri (lsp--path-to-uri old-file-name)
                               :new-uri (lsp--path-to-uri new-file-name)
                               :kind "rename"
                               :options? (lsp-make-rename-file-options :overwrite? t))]))
    (should-not (f-exists? old-file-name))
    (should (f-exists? new-file-name)))

  ;; nested directory
  (let* ((old-file-name (make-temp-file "old-"))
         (new-file-name (make-temp-file "new-"))
         (_ (delete-file new-file-name))
         (new-file-name (f-join new-file-name "nested" "nested2")))

    (lsp--apply-workspace-edit
     (lsp-make-workspace-edit
      :document-changes?
      `[,(lsp-make-rename-file :old-uri (lsp--path-to-uri old-file-name)
                               :new-uri (lsp--path-to-uri new-file-name)
                               :kind "rename")]))
    (should-not (f-exists? old-file-name))
    (should (f-exists? new-file-name))))

;;; `lsp-rename'
(defmacro lsp-test--simulated-input (keys &rest body)
  "Execute body, while simulating the pressing of KEYS.
KEYS is passed to `execute-kbd-macro', after being run trough
`kbd'. Returns the result of the last BODY form."
  (declare (indent 1))
  `(let (result)
     ;; This somehow fixes the test, which works without ert-runner just fine.
     ;; Perhaps `execute-kbd-macro' changes back to the first non-temporary
     ;; buffer first?
     (save-current-buffer
       (execute-kbd-macro (kbd ,keys) 1 (lambda () (setq result (progn ,@body)))))
     result))

(defun lsp-test--rename-overlays? (pos)
  "Return non-nil if there are `lsp-rename' overlays at POS.
POS is a point in the current buffer."
  (--any? (equal (overlay-get it 'face) 'lsp-face-rename)
          (overlays-at pos)))

(ert-deftest lsp--read-rename ()
  "Ensure that `lsp--read-rename' works.
If AT-POINT is nil, it throws a `user-error'.

If a placeholder is given, it shall be the default value,
otherwise the bounds are to be used.

Rename overlays are removed afterwards, even if the user presses
C-g."
  (should-error (lsp--read-rename nil) :type 'user-error)
  (with-temp-buffer
    (insert "identifier")
    (should (string= "identifier"
                     (lsp-test--simulated-input "RET"
                       (lsp--read-rename '((1 . 11) . nil)))))
    (should (string= "ident"
                     (lsp-test--simulated-input "RET"
                       (lsp--read-rename '((1 . 10) . "ident")))))
    (goto-char 1)
    (condition-case nil
        (cl-letf (((symbol-function #'read-string)
                   (lambda (&rest _)
                     ;; NOTE: BEGIN and END means a range [BEGIN;END[, so at
                     ;; point 10, there shouldn't be an overlay anymore. This is
                     ;; consistent with of `bounds-thing-at-point', and it
                     ;; worked during manual testing.
                     (should (lsp-test--rename-overlays? 1))
                     (should (lsp-test--rename-overlays? 9))
                     (should-not (lsp-test--rename-overlays? 10))
                     (keyboard-quit))))
          (lsp--read-rename '((1 . 10) . "id")))
      (quit))
    ;; but not after `lsp--read-rename'
    (should-not (lsp-test--rename-overlays? 1))
    (should-not (lsp-test--rename-overlays? 9))
    (should-not (lsp-test--rename-overlays? 10))))

(ert-deftest lsp--get-symbol-to-rename ()
  "Test `lsp--get-symbol-to-rename'.
It should error if renaming cannot be done, make use of
prepareRename as much as possible, with or without bounds, and it
should work without the latter."
  ;; We don't support rename
  (cl-letf (((symbol-function #'lsp-feature?) #'ignore))
    (should-error (lsp--get-symbol-to-rename) :type 'error))
  (cl-letf (((symbol-function #'lsp--text-document-position-params) #'ignore)
            ((symbol-function #'lsp--range-to-region) #'identity))
    (with-temp-buffer
      (insert "identifier")
      (goto-char 1)
      ;; We do support rename, but no prepareRename
      (cl-letf (((symbol-function #'lsp-feature?)
                 (lambda (f) (member f '("textDocument/rename")))))
        (should (equal (cons (bounds-of-thing-at-point 'symbol) nil)
                       (lsp--get-symbol-to-rename)))
        (goto-char (point-max))
        (insert " ")
        ;; we are not on an identifier
        (should (equal nil (lsp--get-symbol-to-rename))))
      ;; Do the following tests with an identifier at point
      (goto-char 1)
      (cl-letf (((symbol-function #'lsp-feature?)
                 (lambda (f) (member f '("textDocument/rename"
                                    "textDocument/prepareRename")))))
        (cl-letf (((symbol-function #'lsp-request)
                   (lambda (&rest _) (lsp-make-prepare-rename-result
                                 :range '(1 . 12)
                                 :placeholder nil))))
          (should (equal '((1 . 12) . nil) (lsp--get-symbol-to-rename))))
        (cl-letf (((symbol-function #'lsp-request)
                   (lambda (&rest _) (lsp-make-prepare-rename-result
                                 :range '(1 . 12)
                                 :placeholder "_"))))
          (should (equal '((1 . 12) . "_") (lsp--get-symbol-to-rename))))))))

;;; lsp-methods-test.el ends here
