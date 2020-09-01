;;; lsp-javascript.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp,

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

;; LSP Clients for the JavaScript and TypeScript Programming Languages.

;;; Code:

(require 'lsp-mode)

(lsp-dependency 'javascript-typescript-langserver
                '(:system "javascript-typescript-stdio")
                '(:npm :package "javascript-typescript-langserver"
                       :path "javascript-typescript-stdio"))

(defgroup lsp-typescript-javascript nil
  "Support for TypeScript/JavaScript, using Sourcegraph's JavaScript/TypeScript language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sourcegraph/javascript-typescript-langserver"))

(defcustom lsp-clients-typescript-javascript-server-args '()
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript-javascript
  :risky t
  :type '(repeat string))

(defun lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
  "Check if the javascript-typescript language server should be enabled based on FILENAME."
  (or (string-match-p (rx (one-or-more anything) "." (or "ts" "js") (opt "x") string-end) filename)
      (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode)
           (not (derived-mode-p 'json-mode)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          (cons (lsp-package-path 'javascript-typescript-langserver)
                                                                lsp-clients-typescript-javascript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -3
                  :completion-in-comments? t
                  :server-id 'jsts-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'javascript-typescript-langserver
                                         callback
                                         error-callback))))


(defgroup lsp-typescript nil
  "LSP support for TypeScript, using Theia/Typefox's TypeScript Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/theia-ide/typescript-language-server"))

(defcustom lsp-clients-typescript-server-args '("--stdio")
  "Extra arguments for the typescript-language-server language server."
  :group 'lsp-typescript
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-typescript-log-verbosity "info"
  "The server log verbosity."
  :group 'lsp-typescript
  :type 'string)

(defcustom lsp-clients-typescript-plugins (vector)
  "The list of plugins to load.
It should be a vector of plist with keys `:location' and `:name'
where `:name' is the name of the package and `:location' is the
directory containing the package. Example:
\(vector
   \(list :name \"@vsintellicode/typescript-intellicode-plugin\"
         :location \"<path>.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.1.9/\"))"
  :group 'lsp-typescript
  :type  '(restricted-sexp :tag "Vector"
                           :match-alternatives
                           (lambda (xs)
                             (and (vectorp xs) (seq-every-p
                                                (-lambda ((&plist :name :location))
                                                  (and name location))
                                                xs)))))

(lsp-dependency 'typescript-language-server
                '(:system "typescript-language-server")
                '(:npm :package "typescript-language-server"
                       :path "typescript-language-server"))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          `(,(lsp-package-path 'typescript-language-server)
                                                            "--tsserver-path"
                                                            ,(lsp-package-path 'typescript)
                                                            ,@lsp-clients-typescript-server-args)))
                  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                  :priority -2
                  :completion-in-comments? t
                  :initialization-options (lambda ()
                                            (list :plugins lsp-clients-typescript-plugins
                                                  :logVerbosity lsp-clients-typescript-log-verbosity
                                                  :tsServerPath (lsp-package-path 'typescript)))
                  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                  :server-id 'ts-ls
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'typescript
                                         (-partial #'lsp-package-ensure
                                                   'typescript-language-server
                                                   callback
                                                   error-callback)
                                         error-callback))))


(defgroup lsp-flow nil
  "LSP support for the Flow Javascript type checker."
  :group 'lsp-mode
  :link '(url-link "https://flow.org"))

(defcustom lsp-clients-flow-server "flow"
  "The Flow executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-flow
  :risky t
  :type 'file)

(defcustom lsp-clients-flow-server-args '("lsp")
  "Extra arguments for starting the Flow language server."
  :group 'lsp-flow
  :risky t
  :type '(repeat string))

(defun lsp-clients-flow-tag-file-present-p (file-name)
  "Check if the '// @flow' or `/* @flow */' tag is present in
the contents of FILE-NAME."
  (if-let ((buffer (find-buffer-visiting file-name)))
      (with-current-buffer buffer
        (lsp-clients-flow-tag-string-present-p))
    (with-temp-buffer
      (insert-file-contents file-name)
      (lsp-clients-flow-tag-string-present-p))))

(defun lsp-clients-flow-tag-string-present-p ()
  "Helper for `lsp-clients-flow-tag-file-present-p' that works
with the file contents."
  (save-excursion
    (goto-char (point-min))
    (let (stop found)
      (while (not stop)
        (unless (re-search-forward "[^\n[:space:]]" nil t)
          (setq stop t))
        (if (= (point) (point-min)) (setq stop t) (backward-char))
        (cond ((or (looking-at "//+[ ]*@flow")
                   (looking-at "/\\**[ ]*@flow")
                   (looking-at "[ ]*\\*[ ]*@flow"))
               (setq found t) (setq stop t))
              ((or (looking-at "//") (looking-at "*"))
               (forward-line))
              ((looking-at "/\\*")
               (save-excursion
                 (unless (re-search-forward "*/" nil t) (setq stop t)))
               (forward-line))
              (t (setq stop t))))
      found)))

(defun lsp-clients-flow-project-p (file-name)
  "Check if FILE-NAME is part of a Flow project, that is, if
there is a .flowconfig file in the folder hierarchy."
  (locate-dominating-file file-name ".flowconfig"))

(defun lsp-clients-flow-activate-p (file-name _mode)
  "Check if the Flow language server should be enabled for a
particular FILE-NAME and MODE."
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (or (lsp-clients-flow-project-p file-name)
           (lsp-clients-flow-tag-file-present-p file-name))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection (lambda ()
                                          (cons lsp-clients-flow-server
                                                lsp-clients-flow-server-args)))
                  :priority -1
                  :activation-fn 'lsp-clients-flow-activate-p
                  :server-id 'flow-ls))

(provide 'lsp-javascript)
;;; lsp-javascript.el ends here
