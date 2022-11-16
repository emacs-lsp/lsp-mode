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

;; It will install `lsp-mode', `lsp-ui' with their dependencies to start a
;; minimal lsp environment.

;; And it forces Emacs to load `.el' files rather than `.elc' files
;; for more readable backtrace.

;;; Code:

(require 'package)

(setq debug-on-error t
      no-byte-compile t
      byte-compile-warnings nil
      inhibit-startup-screen t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      package-user-dir (make-temp-file "lsp-tmp-elpa")
      custom-file (expand-file-name "custom.el" package-user-dir))

(delete-file package-user-dir)

(let* ((pkg-list '(lsp-mode lsp-ui yasnippet lsp-java lsp-python-ms lsp-haskell helm-lsp lsp-treemacs dap-mode lsp-origami lsp-dart company flycheck lsp-pyright
                            ;; modes
                            rust-mode php-mode scala-mode dart-mode clojure-mode typescript-mode csharp-mode haskell-mode)))

  (package-initialize)
  (package-refresh-contents)

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg))
          (require pkg))
        pkg-list)

  (yas-global-mode)
  (add-hook 'prog-mode-hook 'lsp)
  (add-hook 'kill-emacs-hook `(lambda ()
                                (delete-directory ,package-user-dir t))))

(provide 'lsp-start-plain)
;;; lsp-start-plain.el ends here
