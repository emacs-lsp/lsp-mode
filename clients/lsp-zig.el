;;; lsp-zig.el --- lsp-mode Zig integration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Riccardo Binetti

;; Author: Riccardo Binetti <rbino@gmx.com>
;; Keywords: languages,tools

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

;;  client for zls, the Zig language server

;;; Code:

(require 'lsp-mode)

(defgroup lsp-zig nil
  "LSP support for Zig via zls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/zigtools/zls"))

(defcustom lsp-zig-zls-executable "zls"
  "The zls executable to use.

Leave as just the executable name to use the default behavior of finding the
executable with variable `exec-path'."
  :group 'lsp-zig
  :type 'string)

(defcustom lsp-zig-server-store-path
  (expand-file-name "zig/" lsp-server-install-dir)
  "The path to the file in which zls will be stored."
  :type 'file
  :group 'lsp-zig)

(defcustom lsp-zig-server-version "0.11.0"
  "The zls version to install."
  :type 'file
  :group 'lsp-zig)

(defconst lsp-zig-download-url-format
  "https://github.com/zigtools/zls/releases/download/%s/zls-%s-%s.%s"
  "Format to the download url link.")

(defun lsp-zig--zls-url ()
  "Return Url points to the zls' zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x86_64" "aarch64")))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "windows" "zip"))
      (darwin
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "macos" "tar.gz"))
      (gnu/linux
       (format lsp-zig-download-url-format
               lsp-zig-server-version arch "linux" "tar.gz")))))

(defvar lsp-zig--server-download-url (lsp-zig--zls-url)
  "The actual url used to download language server.")

(defvar lsp-zig--downloaded-file (f-join lsp-zig-server-store-path "temp.tar")
  "The full file path after downloading the server zipped file.")

;;
;;; Util

(defmacro lsp-zig--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun lsp-zig--execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (lsp-zig--mute-apply
      (= 0 (shell-command (concat cmd " "
                                  (mapconcat #'shell-quote-argument
                                             (cl-remove-if #'null args)
                                             " ")))))))

;;
;;; Installation

(defun lsp-zig--stored-zls-executable ()
  "Return the stored zls executable.

This is differ from the variable `lsp-zig-zls-executable'; this is local storage
and not the global storage."
  (executable-find (f-join lsp-zig-server-store-path "bin/zls")))

(defun lsp-zig--extract-compressed-file ()
  "Install zls."
  (cond ((file-exists-p lsp-zig--downloaded-file)
         ;; Suprisingly, you can just use `tar' to unzip a zip file on Windows.
         ;; Therefore, just use the same command.
         (lsp-zig--execute "tar" "-xvzf" lsp-zig--downloaded-file "-C" lsp-zig-server-store-path)
         ;; Delete the zip file.
         (ignore-errors (delete-file lsp-zig--downloaded-file)))
        (t
         (error "Can't extract the downloaded file: %s" lsp-zig--downloaded-file))))

(lsp-dependency
 'zls
 '(:system "zls")
 `(:download :url ,lsp-zig--server-download-url
             :store-path ,lsp-zig--downloaded-file))

;;
;;; Core

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () (or (executable-find lsp-zig-zls-executable)
                                  (lsp-zig--stored-zls-executable)))
                   (lambda ()
                     (or (executable-find lsp-zig-zls-executable)
                         (file-executable-p (lsp-zig--stored-zls-executable)))))
  :activation-fn (lsp-activate-on "zig")
  :priority -1
  :server-id 'zls
  :download-server-fn
  (lambda (_client _callback error-callback _update?)
    (lsp-package-ensure 'zls #'lsp-zig--extract-compressed-file error-callback))))

(lsp-consistency-check lsp-zig)

(provide 'lsp-zig)
;;; lsp-zig.el ends here
