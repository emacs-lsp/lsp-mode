;;; lsp-diagnostics.el --- LSP diagnostics integration -*- lexical-binding: t; -*-
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
;;  LSP diagnostics integration
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-diagnostics nil
  "LSP support for diagnostics"
  :prefix "lsp-disagnostics-"
  :group 'lsp-mode
  :tag "LSP Diagnostics")

;;;###autoload
(define-obsolete-variable-alias 'lsp-diagnostic-package
  'lsp-diagnostics-provider  "lsp-mode 7.0.1")

(defcustom lsp-diagnostics-provider :auto
  "The checker backend provider."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck)
    (const :tag "Pick flymake" :flymake)
    (const :tag "Use neither flymake nor lsp" :none)
    (const :tag "Prefer flymake" t)
    (const :tag "Prefer flycheck" nil))
  :group 'lsp-diagnostics
  :package-version '(lsp-mode . "6.3"))

;;;###autoload
(define-obsolete-variable-alias 'lsp-flycheck-default-level
  'lsp-diagnostics-flycheck-default-level  "lsp-mode 7.0.1")

(defcustom lsp-diagnostics-flycheck-default-level 'error
  "Error level to use when the server does not report back a diagnostic level."
  :type '(choice
          (const error)
          (const warning)
          (const info))
  :group 'lsp-diagnostics)

(defcustom lsp-diagnostics-attributes
  `((unnecessary :foreground "gray")
    (deprecated  :strike-through t))
  "The Attributes used on the diagnostics.
List containing (tag attributes) where tag is the LSP diagnostic tag and
attributes is a `plist' containing face attributes which will be applied
on top the flycheck face for that error level."
  :type '(repeat list)
  :group 'lsp-diagnostics)

(defcustom lsp-diagnostics-disabled-modes nil
  "A list of major models for which `lsp-diagnostics-mode' should be disabled."
  :type '(repeat symbol)
  :group 'lsp-diagnostics
  :package-version '(lsp-mode . "8.0.0"))

;; Flycheck integration

(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-define-error-level "ext:flycheck" (level &rest properties))
(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-valid-checker-p "ext:flycheck")
(declare-function flycheck-stop "ext:flycheck")

(defvar flycheck-mode)
(defvar flycheck-check-syntax-automatically)
(defvar flycheck-checker)
(defvar flycheck-checkers)


(defvar-local lsp-diagnostics--flycheck-enabled nil
  "True when lsp diagnostics flycheck integration has been enabled in this buffer.")

(defvar-local lsp-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp diagnostics was activated.")

(defun lsp-diagnostics--flycheck-level (flycheck-level tags)
  "Generate flycheck level from the original FLYCHECK-LEVEL (e.
g. `error', `warning') and list of LSP TAGS."
  (let ((name (format "lsp-flycheck-%s-%s"
                      flycheck-level
                      (mapconcat #'symbol-name tags "-"))))
    (or (intern-soft name)
        (let* ((face (--doto (intern (format "%s-face" name))
                       (copy-face (-> flycheck-level
                                      (get 'flycheck-overlay-category)
                                      (get 'face))
                                  it)
                       (mapc (lambda (tag)
                               (apply #'set-face-attribute it nil
                                      (cl-rest (assoc tag lsp-diagnostics-attributes))))
                             tags)))
               (category (--doto (intern (format "%s-category" name))
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

(defun lsp-diagnostics--flycheck-calculate-level (severity tags)
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
        (lsp-diagnostics--flycheck-level level tags)
      level)))

(defun lsp-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)

  (->> (lsp--get-buffer-diagnostics)
       (-map (-lambda ((&Diagnostic :message :severity? :tags? :code? :source?
                                    :range (&Range :start (&Position :line      start-line
                                                                     :character start-character)
                                                   :end   (&Position :line      end-line
                                                                     :character end-character))))
               (flycheck-error-new
                :buffer (current-buffer)
                :checker checker
                :filename buffer-file-name
                :message message
                :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                :id code?
                :group source?
                :line (lsp-translate-line (1+ start-line))
                :column (1+ (lsp-translate-column start-character))
                :end-line (lsp-translate-line (1+ end-line))
                :end-column (1+ (lsp-translate-column end-character)))))
       (funcall callback 'finished)))

(defun lsp-diagnostics--flycheck-buffer ()
  "Trigger flyckeck on buffer."
  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(defun lsp-diagnostics--flycheck-report ()
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
                 (when (and (lsp-buffer-live-p buffer)
                            (or
                             (not (bufferp buffer))
                             (and (get-buffer-window buffer)
                                  (not (-contains? (buffer-local-value 'lsp-on-idle-hook buffer)
                                                   'lsp-diagnostics--flycheck-buffer)))))
                   (lsp-with-current-buffer buffer
                     (add-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer nil t)
                     (lsp--idle-reschedule (current-buffer)))))))))

(cl-defgeneric lsp-diagnostics-flycheck-error-explainer (e _server-id)
  "Explain a `flycheck-error' E in a generic way depending on the SERVER-ID."
  (flycheck-error-message e))

(defvar lsp-diagnostics-mode) ;; properly defined by define-minor-mode below

;;;###autoload
(defun lsp-diagnostics-lsp-checker-if-needed ()
  (unless (flycheck-valid-checker-p 'lsp)
    (flycheck-define-generic-checker 'lsp
      "A syntax checker using the Language Server Protocol (LSP)
provided by lsp-mode.
See https://github.com/emacs-lsp/lsp-mode."
      :start #'lsp-diagnostics--flycheck-start
      :modes '(lsp-placeholder-mode) ;; placeholder
      :predicate (lambda () lsp-diagnostics-mode)
      :error-explainer (lambda (e)
                         (lsp-diagnostics-flycheck-error-explainer
                          e (lsp--workspace-server-id (car-safe (lsp-workspaces))))))))

(defun lsp-diagnostics-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (require 'flycheck)
  (lsp-diagnostics-lsp-checker-if-needed)
  (and (not lsp-diagnostics--flycheck-enabled)
       (not (eq flycheck-checker 'lsp))
       (setq lsp-diagnostics--flycheck-checker flycheck-checker))
  (setq-local lsp-diagnostics--flycheck-enabled t)
  (flycheck-mode 1)
  (flycheck-stop)
  (setq-local flycheck-checker 'lsp)
  (lsp-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp)
  (add-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report nil t)
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics--flycheck-report nil t))

(defun lsp-diagnostics-flycheck-disable ()
  "Disable flycheck integration for the current buffer is it was enabled."
  (when lsp-diagnostics--flycheck-enabled
    (flycheck-stop)
    (when (eq flycheck-checker 'lsp)
      (setq-local flycheck-checker lsp-diagnostics--flycheck-checker))
    (setq lsp-diagnostics--flycheck-checker nil)
    (setq-local lsp-diagnostics--flycheck-enabled nil)
    (when flycheck-mode
      (flycheck-mode 1))))

;; Flymake integration

(declare-function flymake-mode "ext:flymake")
(declare-function flymake-make-diagnostic "ext:flymake")
(declare-function flymake-diag-region "ext:flymake")

(defvar flymake-diagnostic-functions)
(defvar flymake-mode)
(defvar-local lsp-diagnostics--flymake-report-fn nil)

(defun lsp-diagnostics--flymake-setup ()
  "Setup flymake."
  (setq lsp-diagnostics--flymake-report-fn nil)
  (add-hook 'flymake-diagnostic-functions 'lsp-diagnostics--flymake-backend nil t)
  (add-hook 'lsp-diagnostics-updated-hook 'lsp-diagnostics--flymake-after-diagnostics nil t)
  (flymake-mode 1))

(defun lsp-diagnostics--flymake-after-diagnostics ()
  "Handler for `lsp-diagnostics-updated-hook'."
  (cond
   ((and lsp-diagnostics--flymake-report-fn flymake-mode)
    (lsp-diagnostics--flymake-update-diagnostics))
   ((not flymake-mode)
    (setq lsp-diagnostics--flymake-report-fn nil))))

(defun lsp-diagnostics--flymake-backend (report-fn &rest _args)
  "Flymake backend using REPORT-FN."
  (let ((first-run (null lsp-diagnostics--flymake-report-fn)))
    (setq lsp-diagnostics--flymake-report-fn report-fn)
    (when first-run
      (lsp-diagnostics--flymake-update-diagnostics))))

(defun lsp-diagnostics--flymake-update-diagnostics ()
  "Report new diagnostics to flymake."
  (funcall lsp-diagnostics--flymake-report-fn
           (-some->> (lsp-diagnostics t)
             (gethash (lsp--fix-path-casing buffer-file-name))
             (--map (-let* (((&Diagnostic :message :severity?
                                          :range (range &as &Range
                                                        :start (&Position :line start-line :character)
                                                        :end (&Position :line end-line))) it)
                            ((start . end) (lsp--range-to-region range)))
                      (when (= start end)
                        (if-let ((region (flymake-diag-region (current-buffer)
                                                              (1+ start-line)
                                                              character)))
                            (setq start (car region)
                                  end (cdr region))
                          (lsp-save-restriction-and-excursion
                            (goto-char (point-min))
                            (setq start (point-at-bol (1+ start-line))
                                  end (point-at-eol (1+ end-line))))))
                      (flymake-make-diagnostic (current-buffer)
                                               start
                                               end
                                               (cl-case severity?
                                                 (1 :error)
                                                 (2 :warning)
                                                 (t :note))
                                               message))))
           ;; This :region keyword forces flymake to delete old diagnostics in
           ;; case the buffer hasn't changed since the last call to the report
           ;; function. See https://github.com/joaotavora/eglot/issues/159
           :region (cons (point-min) (point-max))))



;;;###autoload
(defun lsp-diagnostics--enable ()
  "Enable LSP checker support."
  (when (and (member lsp-diagnostics-provider '(:auto :none :flycheck :flymake t nil))
             (not (member major-mode lsp-diagnostics-disabled-modes)))
    (lsp-diagnostics-mode 1)))

(defun lsp-diagnostics--disable ()
  "Disable LSP checker support."
  (lsp-diagnostics-mode -1))

;;;###autoload
(define-minor-mode lsp-diagnostics-mode
  "Toggle LSP diagnostics integration."
  :group 'lsp-diagnostics
  :global nil
  :lighter ""
  (cond
   (lsp-diagnostics-mode
    (cond
     ((and (or
            (and (eq lsp-diagnostics-provider :auto)
                 (functionp 'flycheck-mode))
            (and (eq lsp-diagnostics-provider :flycheck)
                 (or (functionp 'flycheck-mode)
                     (user-error "The lsp-diagnostics-provider is set to :flycheck but flycheck is not installed?")))
            ;; legacy
            (null lsp-diagnostics-provider))
           (require 'flycheck nil t))
      (lsp-diagnostics-flycheck-enable))
     ((or (eq lsp-diagnostics-provider :auto)
          (eq lsp-diagnostics-provider :flymake)
          (eq lsp-diagnostics-provider t))
      (require 'flymake)
      (lsp-diagnostics--flymake-setup))
     ((not (eq lsp-diagnostics-provider :none))
      (lsp--warn "Unable to autoconfigure flycheck/flymake. The diagnostics won't be rendered.")))

    (add-hook 'lsp-unconfigure-hook #'lsp-diagnostics--disable nil t))
   (t (lsp-diagnostics-flycheck-disable)
      (remove-hook 'lsp-unconfigure-hook #'lsp-diagnostics--disable t))))

;;;###autoload
(add-hook 'lsp-configure-hook (lambda ()
                                (when lsp-auto-configure
                                  (lsp-diagnostics--enable))))

(lsp-consistency-check lsp-diagnostics)

(provide 'lsp-diagnostics)
;;; lsp-diagnostics.el ends here
