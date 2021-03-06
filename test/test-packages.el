;;; test-packages.el --- CI test for emacs-lsp packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 emacs-lsp maintainers
;;
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
;;
;;; Commentary:
;;
;;  CI test for emacs-lsp packages
;;
;; See https://github.com/emacs-lsp/lsp-mode/issues/2655 for more information.
;;
;;; Code:

(require 'package)

(setq user-emacs-directory (expand-file-name (make-temp-name ".emacs.d")
                                             "~")
      package-user-dir (expand-file-name (make-temp-name "tmp-elpa")
                                         user-emacs-directory))

(defun package-version (name)
  "Get version of the package by NAME."
  (let ((pkg (cadr (assq name package-alist)))) (when pkg (package-desc-version pkg))))

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       (pkgs '(dap-mode
               helm-lsp
               ;;lsp-dart  ; TODO: Add this after the test has passed.
               lsp-docker
               lsp-focus
               lsp-haskell
               lsp-ivy
               lsp-java
               lsp-metals
               lsp-mssql
               lsp-origami
               lsp-pyright
               lsp-python-ms
               lsp-sonarlint
               lsp-sourcekit
               lsp-treemacs
               lsp-ui)))
  (package-initialize)
  (package-refresh-contents)

  (progn  ; Install `lsp-mode' from source
    (add-to-list 'load-path (expand-file-name "./"))
    (add-to-list 'load-path (expand-file-name "./clients/"))
    (package-install-file (expand-file-name "./"))
    (message "[INFO] `lsp-mode` version: %s" (package-version 'lsp-mode)))

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg)))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda ()
               ;; NOTE: If you are testing this on your own PC, then you
               ;; might want to uncomment the line below to do the clean up
               ;; after the test. (This will take more time to complete the
               ;; CI process)
               ;;
               ;; Please leave it comment in CI so we don't accidentally install
               ;; depedencies twice.
               ;;(delete-directory ,user-emacs-directory t)
               )))

;;; test-packages.el ends here
