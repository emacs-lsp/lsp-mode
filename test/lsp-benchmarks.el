;;; lsp-benchmarks.el --- lsp benchmarks             -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  Ivan Yonchovski
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

;;

;;; Code:
(require 'lsp-mode)
(defvar lsp-benchmark--test-results nil)
(defvar lsp-benchmark-process (make-process :command '("ls")
                                            :name "xx"
                                            :buffer "xx"))
(defun lsp-benchmarks--create-process-message ()
  (let ((fn (lsp--create-filter-function nil)))
    (lambda (input)
      (flet ((lsp--parser-on-message (msg _workspace)
                                     (push msg lsp-benchmark--test-results)))
        (funcall fn lsp-benchmark-process input)
        (prog1 lsp-benchmark--test-results
          (setq lsp-benchmark--test-results nil))))))

(defvar lsp-benchmark-function (lsp-benchmarks--create-process-message))
(defvar lsp-benchmark-data (--map (s-replace "\n" "\r\n" it)
                                  (lsp--read-from-file "fixtures/requests")))
;; (-take 1 lsp-benchmark-data)
;; (funcall lsp-benchmark-function (first lsp-benchmark-data))

;; (progn
;;   ;; (profiler-start 'cpu)
;;   (let ((gc-cons-threshold 10000000000))

;;     (profiler-stop)
;;     (profiler-start 'cpu)
;;     (dotimes (_ 100)(mapc (lambda (it) (funcall lsp-benchmark-function it))
;;            lsp-benchmark-data))

;;     (profiler-report)
;;     (garbage-collect)))

;; (progn
;;   ;; (profiler-start 'cpu)
;;   (profiler-stop)
;;   (profiler-start 'cpu)
;;   (benchmark-run 10 (mapc (lambda (it) (funcall lsp-benchmark-function it))
;;                           lsp-benchmark-data))
;;   (profiler-report))
;; ;; (funcall lsp-benchmark-function )

;; ;;; lsp-benchmarks.el ends here
;; (benchmark-run 10 (mapc (lambda (it) (funcall lsp-benchmark-function it))
;;                         lsp-benchmark-data))
;; ;;
;;

(let ((gc-cons-threshold 10000000000))
  (prog1 (benchmark-run 10
           (mapc (lambda (it) (funcall lsp-benchmark-function it))
                 lsp-benchmark-data))
    (garbage-collect)))

(provide 'lsp-benchmarks)
