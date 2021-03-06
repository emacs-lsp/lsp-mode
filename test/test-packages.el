;;; test-packages.el --- CI test for emacs-lsp packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 emacs-lsp maintainers
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

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       (pkgs '(lsp-sourcekit)))
  (package-initialize)
  (package-refresh-contents)

  (progn  ; Install `lsp-mode' from source
    (add-to-list 'load-path "./")
    (add-to-list 'load-path "./clients/")
    (package-install-file "./"))

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
