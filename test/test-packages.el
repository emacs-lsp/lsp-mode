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

(defun package-version (name)
  "Get version of the package by NAME."
  (let ((pkg (cadr (assq name package-alist)))) (when pkg (package-desc-version pkg))))

(defun package-build-desc (name)
  "Build package description by PKG-NAME."
  (cadr (assq name package-archive-contents)))

(defun package-get-reqs (key name)
  "Return KEY requires from package NAME."
  (assoc key (package-desc-reqs (package-build-desc name))))

(defun package-emacs-version (name)
  "Return Emacs version from package NAME."
  (let ((ver (ignore-errors (cadr (package-get-reqs 'emacs name)))) (ver-no ""))
    (when ver
      (dolist (no ver)
        (setq ver-no (concat ver-no (number-to-string no) "."))))
    (or (ignore-errors (substring ver-no 0 (1- (length ver-no)))) "0.0")))

(defun package-check-emacs-version (pkg)
  "Return non-nil if PKG is good to be installed."
  (version<= (package-emacs-version pkg) emacs-version))


(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       package-enable-at-startup package-check-signature
       (pkgs '(ccls
               dap-mode
               helm-lsp
               lsp-dart
               lsp-docker
               lsp-focus
               lsp-grammarly
               lsp-haskell
               lsp-ivy
               lsp-java
               lsp-ltex
               lsp-metals
               lsp-mssql
               lsp-origami
               lsp-pyright
               lsp-python-ms
               lsp-sonarlint
               lsp-sourcekit
               lsp-tailwindcss
               lsp-treemacs
               lsp-ui)))
  (package-initialize)

  (advice-add
   'package-install-from-archive
   :before (lambda (pkg-desc)
             (setq byte-compile-error-on-warn
                   (if (ignore-errors (memq (package-desc-name pkg-desc) pkgs)) t nil))))

  (progn  ; Install `lsp-mode' from source
    (add-to-list 'load-path (expand-file-name "./"))
    (add-to-list 'load-path (expand-file-name "./clients/"))
    (package-install-file (expand-file-name "./"))
    (message "[INFO] `lsp-mode` version: %s" (package-version 'lsp-mode)))

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (if (package-check-emacs-version pkg)
                (condition-case _
                    (progn
                      (package-refresh-contents)
                      (package-install pkg))
                  (error
                   (message "[INFO] retrying the installation of %s" pkg)
                   (package-refresh-contents)
                   (package-install pkg)))
              (message "[INFO] Package `%s` is not test, minimum Emacs version %s"
                       pkg (package-emacs-version pkg)))))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda ()
               (unless (boundp 'emacs-lsp-ci)
                 (delete-directory ,user-emacs-directory t)))))

;;; test-packages.el ends here
