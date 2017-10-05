;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com> -*- lexical-binding: t -*-

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

;;(setq ert-debug-on-error t)

(require 'ert)
(require 'lsp-methods)

(defmacro with-buffer-contents (s &rest forms)
    "Create a temporary buffer with contents S and execute FORMS."
    `(save-excursion
       (with-temp-buffer
         (progn
           (buffer-enable-undo)
           (goto-char 0)
           (insert ,s)
           (goto-char 0)
           ,@forms))))

(defun capture-changes (change-event)
  (add-hook 'after-change-functions
            (lambda (start end length)
              (setq change-event (lsp--text-document-content-change-event start end length)))
            nil
            t))

(ert-deftest lsp--content-change-event-delete-line1 ()
  (let ((change-event nil))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
     (goto-char (point-min))
     (kill-line 1)
    (should (equal '(:range (:start (:line 0 :character 0)
                             :end (:line 1 :character 0))
                     :rangeLength 5
                     :text "")
                   change-event))))

(ert-deftest lsp--content-change-event-join-lines ()
  (let ((change-event))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
     (goto-char 5)
     (kill-line))
    (should (equal '(:range (:start (:line 0 :character 4)
                             :end (:line 1 :character 0))
                     :rangeLength 1
                     :text "")
                   change-event))))

(ert-deftest lsp--content-change-event-delete-last-line ()
  (let ((change-event))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
    (progn
       (goto-char 5)
       (let ((beg (point)))
         (goto-char (point-max))
         (delete-region beg (point))))) ;; Buffer contents now a single-line: "1234"
    (should (equal '(:range (:start (:line 0 :character 4)
                             :end (:line 1 :character 4))
                     :rangeLength 5
                     :text "")
                   change-event)))))

(ert-deftest lsp--content-change-event-insert-at-beginning-of-line ()
  (let ((change-event nil))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
     (progn
       (goto-char 0)
       (insert "0")))
    (should (equal '(:range (:start (:line 0 :character 0)
                             :end (:line 0 :character 0))
                            :rangeLength 0
                            :text "0")
                   change-event))))

(ert-deftest lsp--content-change-event-insert-at-end-of-line ()
  (let ((change-event))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
     (progn
       (goto-char 5)
       (insert "5 1/2")))
    (should (equal '(:range (:start (:line 0 :character 4)
                             :end (:line 0 :character 4))
                            :rangeLength 0
                            :text "5 1/2")
                   change-event))))

(ert-deftest lsp--content-change-event-substitute-range-of-different-length ()
  (let ((change-event))
    (with-buffer-contents
     "1234
5678"
     (capture-changes change-event)
     (progn
       (goto-char 1)
       (search-forward "23")
       (replace-match "abc")))
    (should (equal '(:range (:start (:line 0 :character 1)
                             :end (:line 0 :character 3))
                            :rangeLength 2
                            :text "abc")
                   change-event))))
