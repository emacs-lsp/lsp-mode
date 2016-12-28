;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>

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

(defun lsp--stdio-send-sync (message proc)
  ;; (message "lsp--stdio-send-sync: %s" message)
  (process-send-string proc
		       message)
  (with-local-quit
    (accept-process-output proc)))

(defun lsp--stdio-send-async (message proc)
  ;; (message "lsp--stdio-send-async: %s" message)
  (process-send-string proc
		       message))

(provide 'lsp-send)
