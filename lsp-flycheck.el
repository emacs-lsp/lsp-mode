;;; lsp-flycheck.el --- Flycheck support for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017  fmdkdd

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

;;; Commentary:

;; Put this in your config file:
;;
;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-flycheck)
;;   (lsp-flycheck-setup))

;;; Code:

(require 'lsp-notifications)
(require 'flycheck)

(defun lsp--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  ;; Turn all errors from lsp--diagnostics into flycheck-error objects and pass
  ;; them immediately to the callback
  (let ((errors))
    (maphash (lambda (file diagnostics)
               (dolist (diag diagnostics)
                 (push (flycheck-error-new
                        :buffer (current-buffer)
                        :checker checker
                        :filename file
                        :line (1+ (lsp-diagnostic-line diag))
                        :column (1+ (lsp-diagnostic-column diag))
                        :message (lsp-diagnostic-message diag)
                        :level (pcase (lsp-diagnostic-severity diag)
                                 (1 'error)
                                 (2 'warning)
                                 (_ 'info))
                        :id (lsp-diagnostic-code diag))
                       errors)))
             lsp--diagnostics)
    (funcall callback 'finished errors)))

(defun lsp-flycheck-setup ()
  "Setup Flycheck for use with lsp-mode."
  ;; Disable automatic syntax checks, since lsp-mode will call `flycheck-buffer'
  ;; directly.
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-checker 'lsp)
  (add-to-list 'flycheck-checkers 'lsp)
  (flycheck-mode)
  (add-hook 'lsp-after-diagnostics-hook #'flycheck-buffer))

(flycheck-define-generic-checker 'lsp
  "A syntax checker using the Language Server Protocol (RLS)
provided by lsp-mode.

See https://github.com/vibhavp/emacs-lsp."
  :start #'lsp--flycheck-start
  :modes 'rust-mode                     ; Need a default mode
  :predicate (lambda () (not (null global-lsp-mode))))

(defun lsp-flycheck-add-mode (mode)
  "Add MODE as a valid major-mode for the lsp checker."
  (unless (flycheck-checker-supports-major-mode-p 'lsp mode)
    (flycheck-add-mode 'lsp mode)))

(provide 'lsp-flycheck)

;;; lsp-flycheck ends here
