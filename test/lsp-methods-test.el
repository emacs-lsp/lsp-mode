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
(require 'lsp)

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
                   (lsp--apply-text-edits (let ((json-encoding-pretty-print t)
                                                (json-array-type 'list)
                                                (json-object-type 'hash-table)
                                                (json-false nil))
                                            (json-read-from-string lsp-methods-test--changes)))
                   (buffer-string))))
    (should (string= actual expected))))

(ert-deftest lsp-create-test ()
  (let ((new-file-name (make-temp-file "new")))
    (delete-file new-file-name)
    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("uri"  (lsp--path-to-uri new-file-name))
                    ("kind" "create"))))))
    (should (f-exists? new-file-name)))
  (let ((new-file-name (make-temp-file "should be overridden")))
    (f-write-text "should be overridden" nil new-file-name)
    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("uri"  (lsp--path-to-uri new-file-name))
                    ("kind" "create")
                    ("options" (ht ("override" t))))))))
    (should (equal (f-read-text new-file-name) "")))
  (let ((new-file-name (make-temp-file "should not be overridden")))
    (f-write-text "text" nil new-file-name)
    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("uri"  (lsp--path-to-uri new-file-name))
                    ("kind" "create")
                    ("options" (ht ("override" nil))))))))
    (should (equal (f-read-text new-file-name) "text"))))

(ert-deftest lsp-delete-test ()
  (let ((delete-file-name (make-temp-file "to-delete")))
    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("uri"  (lsp--path-to-uri delete-file-name))
                    ("kind" "delete"))))))
    (should-not (f-exists? delete-file-name))))

(ert-deftest lsp-update-test ()
  (let ((old-file-name (make-temp-file "old"))
        (new-file-name (make-temp-file "new")))

    (f-delete new-file-name)

    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("oldUri"  (lsp--path-to-uri old-file-name))
                    ("newUri"  (lsp--path-to-uri new-file-name))
                    ("kind" "rename"))))))
    (should-not (f-exists? old-file-name))
    (should (f-exists? new-file-name)))

  ;; override
  (let ((old-file-name (make-temp-file "old"))
        (new-file-name (make-temp-file "new")))

    (f-write-text "should be overridden" nil new-file-name)

    (lsp--apply-workspace-edit
     (ht ("documentChanges"
          (list (ht ("oldUri"  (lsp--path-to-uri old-file-name))
                    ("newUri"  (lsp--path-to-uri new-file-name))
                    ("kind" "rename")
                    ("options" (ht ("override" t))))))))
    (should-not (f-exists? old-file-name))
    (should (f-exists? new-file-name))))
