;;; lsp-io-test.el --- unit tests for lsp-io.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Vibhav Pant <vibhavp@gmail.com>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'lsp-mode)
(require 'cl-lib)

(defvar lsp--test-results nil)
(defvar lsp--parser-function nil)

(defvar lsp--test-workspace
  (make-lsp--workspace
   :client (make-lsp--client :ignore-messages '("readFile .* requested"))))

(defun lsp--create-process-message ()
  (let ((fn (lsp--create-filter-function nil)))
    (lambda (input)
      (cl-letf (((symbol-function 'lsp--parser-on-message) (lambda (_msg _workspace)))
                ((symbol-function 'json-read-from-string)
                 (lambda (msg)
                   (push msg lsp--test-results)))
                ((symbol-function 'json-parse-string)
                 (lambda (msg &rest _)
                   (push msg lsp--test-results))))
        (funcall fn nil input)
        (prog1 lsp--test-results
          (setq lsp--test-results nil))))))

(ert-deftest lsp--parser-read--multiple-messages ()
  (let* ((fn (lsp--create-process-message))
         (messages-in '("Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length:2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length:2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"))
         (messages (funcall fn (string-join messages-in))))
    (should (equal messages '("{}" "{}" "{}" "{}" "{}" "{}" "{}")))))

(ert-deftest lsp--parser-read--multibyte ()
  (let* ((fn (lsp--create-process-message))
         (message-in "Content-Length: 3\r\n\r\n\xe2\x80\x99")
         (messages (funcall fn message-in)))
    (should (equal messages '("’")))))

(ert-deftest lsp--parser-read--multibyte-nospace ()
  (let* ((fn (lsp--create-process-message))
         (message-in "Content-Length:3\r\n\r\n\xe2\x80\x99")
         (messages (funcall fn message-in)))
    (should (equal messages '("’")))))

(ert-deftest lsp--parser-read--multibyte-received ()
  (let* ((fn (lsp--create-process-message))
         (message-in "Content-Length: 1152\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"sts/highlight\",\"params\":{\"doc\":{\"version\":0,\"uri\":\"file:///home/kyoncho/Public/Desktop/hellow/hello-world/src/main/java/com/example/helloworld/HelloWorldController.java\"},\"codeLenses\":[{\"range\":{\"start\":{\"line\":9,\"character\":0},\"end\":{\"line\":9,\"character\":11}},\"command\":{\"title\":\"← SampleBean\",\"command\":\"sts.showHoverAtPosition\",\"arguments\":[{\"line\":9,\"character\":0}]},\"data\":\"← SampleBean\"},{\"range\":{\"start\":{\"line\":14,\"character\":4},\"end\":{\"line\":14,\"character\":14}},\"command\":{\"title\":\"← SampleBean\",\"command\":\"sts.showHoverAtPosition\",\"arguments\":[{\"line\":14,\"character\":4}]},\"data\":\"← SampleBean\"},{\"range\":{\"start\":{\"line\":17,\"character\":4},\"end\":{\"line\":17,\"character\":31}},\"command\":{\"title\":\"http://127.0.0.1:8080/hello-world\",\"command\":\"sts.open.url\",\"arguments\":[\"http://127.0.0.1:8080/hello-world\"]},\"data\":\"http://127.0.0.1:8080/hello-world\"},{\"range\":{\"start\":{\"line\":25,\"character\":4},\"end\":{\"line\":25,\"character\":32}},\"command\":{\"title\":\"http://127.0.0.1:8080/hello-world2\",\"command\":\"sts.open.url\",\"arguments\":[\"http://127.0.0.1:8080/hello-world2\"]},\"data\":\"http://127.0.0.1:8080/hello-world2\"}]}}")
         (messages (funcall fn message-in)))
    (should (equal messages '("{\"jsonrpc\":\"2.0\",\"method\":\"sts/highlight\",\"params\":{\"doc\":{\"version\":0,\"uri\":\"file:///home/kyoncho/Public/Desktop/hellow/hello-world/src/main/java/com/example/helloworld/HelloWorldController.java\"},\"codeLenses\":[{\"range\":{\"start\":{\"line\":9,\"character\":0},\"end\":{\"line\":9,\"character\":11}},\"command\":{\"title\":\"← SampleBean\",\"command\":\"sts.showHoverAtPosition\",\"arguments\":[{\"line\":9,\"character\":0}]},\"data\":\"← SampleBean\"},{\"range\":{\"start\":{\"line\":14,\"character\":4},\"end\":{\"line\":14,\"character\":14}},\"command\":{\"title\":\"← SampleBean\",\"command\":\"sts.showHoverAtPosition\",\"arguments\":[{\"line\":14,\"character\":4}]},\"data\":\"← SampleBean\"},{\"range\":{\"start\":{\"line\":17,\"character\":4},\"end\":{\"line\":17,\"character\":31}},\"command\":{\"title\":\"http://127.0.0.1:8080/hello-world\",\"command\":\"sts.open.url\",\"arguments\":[\"http://127.0.0.1:8080/hello-world\"]},\"data\":\"http://127.0.0.1:8080/hello-world\"},{\"range\":{\"start\":{\"line\":25,\"character\":4},\"end\":{\"line\":25,\"character\":32}},\"command\":{\"title\":\"http://127.0.0.1:8080/hello-world2\",\"command\":\"sts.open.url\",\"arguments\":[\"http://127.0.0.1:8080/hello-world2\"]},\"data\":\"http://127.0.0.1:8080/hello-world2\"}]}}")))))

(ert-deftest lsp--parser-read--multiple-chunks ()
  (let* ((fn (lsp--create-process-message)))
    (should (equal (funcall fn "Content-Length: 14\r\n\r\n{") nil))
    (should (equal (funcall fn "\"somedata\":1") nil))
    (should (equal (funcall fn "}Content-Length: 14\r\n\r\n{")
                   '("{\"somedata\":1}")))
    (should (equal (funcall fn "\"somedata\":2}")
                   '("{\"somedata\":2}")))))

(ert-deftest lsp--parser-read--multiple-multibyte-chunks ()
  (let* ((fn (lsp--create-process-message)))
    (should (equal (funcall fn "Content-Length: 18\r") nil))
    (should (equal (funcall fn "\n\r\n{\"somedata\":\"\xe2\x80") nil))
    (should (equal (funcall fn "\x99\"}Content-Length: 14\r\n\r\n{")
                   '("{\"somedata\":\"’\"}")))
    (should (equal (funcall fn "\"somedata\":2}")
                   '("{\"somedata\":2}")))))

(ert-deftest lsp--non-related-content-on-stdout ()
  (let* ((fn (lsp--create-process-message)))
    (should (equal (funcall fn "ConentOnStdoutContent-Length: 14\r\n\r\n{\"somedata\":1}")
                   '("{\"somedata\":1}")))))

(ert-deftest lsp--parser-read--ignored-messages ()
  (lsp--on-notification
   lsp--test-workspace
   (lsp--read-json "{\"jsonrpc\":\"2.0\",\"method\":\"window/logMessage\",\"params\":{\"type\":2,\"message\":\"readFile /some/path requested by TypeScript but content not available\"}}"))
  (lsp--on-notification lsp--test-workspace (lsp--read-json "{\"jsonrpc\":\"2.0\",\"method\":\"window/logMessage\",\"params\":{\"type\":2,\"message\":\"Important message\"}}"))
  (should (equal (with-current-buffer "*lsp-log*" (buffer-substring-no-properties (point-min) (point-max))) "Important message\n")))

;;; lsp-io-test.el ends here
