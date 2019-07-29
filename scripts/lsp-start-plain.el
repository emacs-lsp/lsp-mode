;;; lsp-start-plain.el --- LSP mode quick starter      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ivan Yonchovski

;; Author: Zhu Zihao <all_but_last@163.com>
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

;; This file is a helper to start a minimal lsp environment.
;; To use this, start your Emacs with "emacs -q" and load this file.

;; It will install  `lsp-mode', `lsp-ui' and `company-lsp'
;; with their depedencies to start a minimal lsp environment.

;; And it forces Emacs to load `.el' files rather than `.elc' files
;; for more readable backtrace.

;;; Code:

(require 'package)

(setq debug-on-error t)

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       (no-byte-compile t)
       (package-user-dir (expand-file-name (make-temp-name "lsp-tmp-elpa")
                                           user-emacs-directory))
       (custom-file (expand-file-name "custom.el" package-user-dir))
       (pkg-list '(lsp-mode lsp-ui company-lsp yasnippet lsp-java lsp-python-ms lsp-haskell helm-lsp lsp-treemacs dap-mode lsp-origami)))

  (package-initialize)
  (package-refresh-contents)

  (mapcar (lambda (pkg)
            (unless (package-installed-p pkg)
              (package-install pkg))
            (require pkg))
          pkg-list)

  (add-hook 'kill-emacs-hook `(lambda ()
                                (delete-directory ,package-user-dir t))))

(provide 'lsp-start-plain)
;;; lsp-start-plain.el ends here
