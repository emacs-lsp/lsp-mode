;;; lsp-checker.el --- LSP checkers integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacs-lsp maintainers
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
;;  LSP checkers integration
;;
;;; Code:

(require 'lsp-mode)

(defcustom lsp-diagnostic-package :auto
  "`lsp-mode' diagnostics auto-configuration."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck)
    (const :tag "Pick flymake" :flymake)
    (const :tag "Use neither flymake nor lsp" :none)
    (const :tag "Prefer flymake" t)
    (const :tag "Prefer flycheck" nil))
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(make-obsolete-variable 'lsp-prefer-flymake 'lsp-diagnostic-package "lsp-mode 6.2")

(defcustom lsp-flycheck-default-level 'error
  "Error level to use when the server does not report back a diagnostic level."
  :type '(choice (const error)
                 (const warning)
                 (const info))
  :group 'lsp-mode)

(defvar lsp-diagnostics-attributes
  `((unnecessary :foreground "dim gray")
    (deprecated  :strike-through t))
  "The Attributes used on the diagnostics.
List containing (tag attributes) where tag is the LSP diagnostic tag and
attributes is a `plist' containing face attributes which will be applied
on top the flycheck face for that error level.")

;; Flycheck integration

(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-define-error-level "ext:flycheck" (level &rest properties))
(declare-function flycheck-buffer "ext:flycheck")

(declare-function lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-cpp")

(defvar flycheck-check-syntax-automatically)
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defun lsp--flycheck-level (flycheck-level tags)
  "Generate flycheck level from the original FLYCHECK-LEVEL (e.
g. `error', `warning') and list of LSP TAGS."
  (let ((name (format "lsp-flycheck-%s-%s"
                      flycheck-level
                      (mapconcat #'symbol-name tags "-"))))
    (or (intern-soft name)
        (let* ((face (--doto (intern (format "lsp-%s-face" name))
                       (copy-face (-> flycheck-level
                                      (get 'flycheck-overlay-category)
                                      (get 'face))
                                  it)
                       (mapc (lambda (tag)
                               (apply #'set-face-attribute it nil
                                      (cl-rest (assoc tag lsp-diagnostics-attributes))))
                             tags)))
               (category (--doto (intern (format "lsp-%s-category" name))
                           (setf (get it 'face) face
                                 (get it 'priority) 100)))
               (new-level (intern name))
               (bitmap (or (get flycheck-level 'flycheck-fringe-bitmaps)
                           (get flycheck-level 'flycheck-fringe-bitmap-double-arrow))))
          (flycheck-define-error-level new-level
            :severity (get flycheck-level 'flycheck-error-severity)
            :compilation-level (get flycheck-level 'flycheck-compilation-level)
            :overlay-category category
            :fringe-bitmap bitmap
            :fringe-face (get flycheck-level 'flycheck-fringe-face)
            :error-list-face face)
          new-level))))

(defun lsp--flycheck-calculate-level (severity tags)
  "Calculate flycheck level by SEVERITY and TAGS."
  (let ((level (pcase severity
                 (1 'error)
                 (2 'warning)
                 (3 'info)
                 (4 'info)
                 (_ lsp-flycheck-default-level)))
        ;; materialize only first tag.
        (tags (seq-map (lambda (tag)
                         (cond
                          ((= tag lsp/diagnostic-tag-unnecessary) 'unnecessary)
                          ((= tag lsp/diagnostic-tag-deprecated) 'deprecated)))
                       tags)))
    (if tags
        (lsp--flycheck-level level tags)
      level)))

(defun lsp--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer t)

  (->> (lsp--get-buffer-diagnostics)
       (-map (-lambda ((&Diagnostic :message :severity? :tags? :code?
                                    :range (&Range :start (&Position :line      start-line
                                                                     :character start-character)
                                                   :end   (&Position :line      end-line
                                                                     :character end-character))))
               (flycheck-error-new
                :buffer (current-buffer)
                :checker checker
                :filename buffer-file-name
                :message message
                :level (lsp--flycheck-calculate-level severity? tags?)
                :id code?
                :line (lsp-translate-line (1+ start-line))
                :column (1+ (lsp-translate-column start-character))
                :end-line (lsp-translate-line (1+ end-line))
                :end-column (1+ (lsp-translate-column end-character)))))
       (funcall callback 'finished)))

(defun lsp--flycheck-buffer ()
  "Trigger flyckeck on buffer."
  (remove-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer t)
  (flycheck-buffer))

(defun lsp--flycheck-report ()
  "Report flycheck.
This callback is invoked when new diagnostics are received
from the language server."
  (when (and (or (memq 'idle-change flycheck-check-syntax-automatically)
                 (and (memq 'save flycheck-check-syntax-automatically)
                      (not (buffer-modified-p))))
             lsp--cur-workspace)
    ;; make sure diagnostics are published even if the diagnostics
    ;; have been received after idle-change has been triggered
    (->> lsp--cur-workspace
         (lsp--workspace-buffers)
         (mapc (lambda (buffer)
                 (when (lsp-buffer-live-p buffer)
                   (lsp-with-current-buffer buffer
                     (add-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer nil t)
                     (lsp--idle-reschedule (current-buffer)))))))))

(defun lsp-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (flycheck-mode 1)
  (setq-local flycheck-checker 'lsp)
  (lsp-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp)
  (add-hook 'lsp-diagnostics-updated-hook #'lsp--flycheck-report nil t)
  (add-hook 'lsp-managed-mode-hook #'lsp--flycheck-report nil t))

;;;###autoload
(with-eval-after-load 'flycheck
  (flycheck-define-generic-checker 'lsp
    "A syntax checker using the Language Server Protocol (LSP)
provided by lsp-mode.
See https://github.com/emacs-lsp/lsp-mode."
    :start #'lsp--flycheck-start
    :modes '(lsp-placeholder-mode) ;; placeholder
    :predicate (lambda () lsp-mode)
    :error-explainer (lambda (e)
                       (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
                              (lsp-cpp-flycheck-clang-tidy-error-explainer e))
                             (t (flycheck-error-message e))))))

;;;###autoload
(defun lsp-checker--enable ()
  "Enable LSP checker support."
  (when lsp-checker-enable
    (lsp-checker-mode 1)))

(defun lsp-checker--disable ()
  "Disable LSP checker support."
  (lsp-checker-mode -1))

;;;###autoload
(define-minor-mode lsp-checker-mode
  "Toggle LSP checker integration."
  :group 'lsp-mode
  :global nil
  :lighter ""
  (cond
   (lsp-checker-mode
    (cond
     ((or
       (and (eq lsp-diagnostic-package :auto)
            (functionp 'flycheck-mode))
       (and (eq lsp-diagnostic-package :flycheck)
            (or (functionp 'flycheck-mode)
                (user-error "The lsp-diagnostic-package is set to :flycheck but flycheck is not installed?")))
       ;; legacy
       (null lsp-diagnostic-package))
      (lsp-flycheck-enable))
     ((and (not (version< emacs-version "26.1"))
           (or (eq lsp-diagnostic-package :auto)
               (eq lsp-diagnostic-package :flymake)
               (eq lsp-diagnostic-package t)))
      (require 'flymake)
      (lsp--flymake-setup))
     ((not (eq lsp-diagnostic-package :none))
      (lsp--warn "Unable to autoconfigure flycheck/flymake. The diagnostics won't be rendered.")))

    (add-hook 'lsp-unconfigure-hook #'lsp-checker--disable nil t))
   (t
    (remove-hook 'lsp-unconfigure-hook #'lsp-checker--disable t))))

;;;###autoload
(add-hook 'lsp-configure-hook (lambda ()
                                (when (and lsp-auto-configure
                                           lsp-checker-enable)
                                  (lsp-checker--enable))))

(provide 'lsp-checker)
;;; lsp-checker.el ends here
