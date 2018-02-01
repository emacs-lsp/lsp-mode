;;; lsp-io-tests.el --- unit tests for lsp-io.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Vibhav Pant <vibhavp@gmail.com>.

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
(require 'lsp-methods)
(require 'lsp-io)

(defvar lsp--test-workspace
  (make-lsp--workspace
   :client (make-lsp--client :ignore-messages '("readFile .* requested"))))

(ert-deftest lsp--parser-read--multiple-messages ()
  (let* ((p (make-lsp--parser :workspace lsp--test-workspace))
         (messages-in '("Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"))
         (messages (lsp--parser-read p (string-join messages-in))))
    (should (equal messages '("{}" "{}" "{}" "{}" "{}")))))

(ert-deftest lsp--parser-read--multibyte ()
  (let* ((p (make-lsp--parser :workspace lsp--test-workspace))
				 (message-in "Content-Length: 3\r\n\r\n\xe2\x80\x99")
         (messages (lsp--parser-read p message-in)))
    (should (equal messages '("’")))))

(ert-deftest lsp--parser-read--multiple-chunks ()
  (let* ((p (make-lsp--parser :workspace lsp--test-workspace)))
		(should (equal (lsp--parser-read p "Content-Length: 14\r\n\r\n{") nil))
		(should (equal (lsp--parser-read p "\"somedata\":1") nil))
		(should (equal (lsp--parser-read p "}Content-Length: 14\r\n\r\n{")
									 '("{\"somedata\":1}")))
		(should (equal (lsp--parser-read p "\"somedata\":2}")
									 '("{\"somedata\":2}")))))

(ert-deftest lsp--parser-read--multiple-multibyte-chunks ()
  (let* ((p (make-lsp--parser :workspace lsp--test-workspace)))
		(should (equal (lsp--parser-read p "Content-Length: 18\r") nil))
		(should (equal (lsp--parser-read p "\n\r\n{\"somedata\":\"\xe2\x80") nil))
		(should (equal (lsp--parser-read p "\x99\"}Content-Length: 14\r\n\r\n{")
									 '("{\"somedata\":\"’\"}")))
		(should (equal (lsp--parser-read p "\"somedata\":2}")
									 '("{\"somedata\":2}")))))

(ert-deftest lsp--parser-read--ignored-messages ()
  (cl-letf* ((log '())
             (p (make-lsp--parser :workspace lsp--test-workspace))
             ((symbol-function 'message)
              (lambda (&rest args) (push (apply 'format args) log))))
    (lsp--on-notification p (lsp--read-json "{\"jsonrpc\":\"2.0\",\"method\":\"window/logMessage\",\"params\":{\"type\":2,\"message\":\"readFile /some/path requested by TypeScript but content not available\"}}"))
    (lsp--on-notification p (lsp--read-json "{\"jsonrpc\":\"2.0\",\"method\":\"window/logMessage\",\"params\":{\"type\":2,\"message\":\"Important message\"}}"))
    (should (equal log '("Important message")))))

(provide 'lsp-io-tests)
;;; lsp-io-tests.el ends here
