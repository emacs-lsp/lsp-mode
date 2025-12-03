;;; lsp-ruby-lsp.el --- lsp-mode for the Ruby ruby-lsp gem -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Šimon Lukašík

;; Author: Šimon Lukašík
;; Keywords: languages

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

;; LSP client for the Ruby ruby-lsp - an optionated language server for Ruby.
;; Not to be confused with lsp-ruby that has been deprecated for a while.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ruby-lsp nil
  "LSP support for the ruby-lsp language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/shopify/ruby-lsp"))

(defcustom lsp-ruby-lsp-use-bundler nil
  "Run ruby-lsp using bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-ruby-lsp)

(defcustom lsp-ruby-lsp-library-directories
  '("~/.rbenv/" "/usr/lib/ruby/" "~/.rvm/" "~/.gem/" "~/.asdf")
  "List of directories which will be considered to be libraries."
  :type '(repeat string)
  :group 'lsp-ruby-lsp
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-ruby-lsp-server-command '("ruby-lsp")
  "Command to start ruby-lsp language server."
  :type '(repeat string)
  :group 'lsp-ruby-lsp
  :package-version '(lsp-mode . "9.0.1"))

(defun lsp-ruby-lsp--build-command ()
  (append
   (if lsp-ruby-lsp-use-bundler '("bundle" "exec"))
   lsp-ruby-lsp-server-command))

(defun lsp-ruby-lsp--open-file (arg_hash)
  "Open a file. This function is for code-lens provided by ruby-lsp-rails."
  (let* ((arguments (gethash "arguments" arg_hash))
         (uri (aref (aref arguments 0) 0))
         (path-with-line-number (split-string (lsp--uri-to-path uri) "#L"))
         (path (car path-with-line-number))
         (line-number (cadr path-with-line-number)))
    (find-file path)
    (when line-number (forward-line (1- (string-to-number line-number))))))

(defun lsp-ruby-lsp--run-test (arg_hash)
  "Run a test file. This function is for code-lens provided by ruby-lsp-rails."
  (let* ((arguments (gethash "arguments" arg_hash))
         (command (aref arguments 2))
         (default-directory (lsp-workspace-root))
         (buffer-name "*run test results*")
         (buffer (progn
                   (when (get-buffer buffer-name) (kill-buffer buffer-name))
                   (generate-new-buffer buffer-name))))
    (async-shell-command command buffer)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-ruby-lsp--build-command)
  :activation-fn (lsp-activate-on "ruby")
  :library-folders-fn (lambda (_workspace) lsp-ruby-lsp-library-directories)
  :priority -2
  :action-handlers (ht ("rubyLsp.openFile" #'lsp-ruby-lsp--open-file)
                       ("rubyLsp.runTest" #'lsp-ruby-lsp--run-test)
                       ("rubyLsp.runTestInTerminal" #'lsp-ruby-lsp--run-test))
  :server-id 'ruby-lsp-ls))

(lsp-consistency-check lsp-ruby-lsp)

(provide 'lsp-ruby-lsp)
;;; lsp-ruby-lsp.el ends here
