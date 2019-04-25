;;; lsp-clojure.el --- Clojure Client settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Benedek Fazekas

;; Author: Benedek Fazekas <benedek.fazekas@gmail.com>
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

;; lsp-clojure client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-clojure nil
  "Settings for clojure."
  :group 'tools
  :tag "Language Server")

(defcustom lsp-clojure-server-command  '("bash" "-c" "clojure-lsp")
  "The clojure-lisp server command."
  :group 'lsp-clojure
  :risky t
  :type 'list)

;; enables jumping into jarred dependencies
;; in order to make this work the project needs to have a .lsp/config.edn with
;; {"dependency-scheme" "jar"} in it so the server returns the URI in the right format
(defun lsp-clj--file-in-jar (uri)
  (string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" uri)
  (when-let* ((entry (match-string 3 uri))
              (path (lsp--uri-to-path (match-string 2 uri)))
              (name (format "%s:%s" path entry))
              (content (lsp-send-request (lsp-make-request "clojure/dependencyContents" (list :uri uri)))))
    (if (find-buffer-visiting name)
        (message "buffer %s exists" name)
      (with-current-buffer (generate-new-buffer name)
        (insert content)
        (set-visited-file-name name)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (set-auto-mode)
        (current-buffer)))
    name))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clojure-server-command))
                  :major-modes '(clojure-mode clojurec-mode clojurescript-mode)
                  :uri-handlers (lsp-ht ("jar" #'lsp-clj--file-in-jar))
                  :server-id 'clojure-lsp))

(provide 'lsp-clojure)
