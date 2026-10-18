;;; lsp-diagnostics.el --- LSP diagnostics integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2026 emacs-lsp maintainers
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
  :type '(repeat (list symbol plist))
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
                                    :range (&Range :start (start &as &Position
                                                                 :line      start-line
                                                                 :character start-character)
                                                   :end   (end   &as &Position
                                                                 :line      end-line
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
                :end-column (unless (lsp--position-equal start end)
                              (1+ (lsp-translate-column end-character))))))
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

(defun lsp-diagnostics-flymake-default-message-formatter (diag)
  "Default formatter for the flymake text of an LSP `Diagnostic' DIAG.
Returns the message, appending the code in brackets when present, e.g.
\"foo is unused [reportUnusedExpression]\"."
  (-let (((&Diagnostic :message :code?) diag))
    (if code?
        (format "%s [%s]" message code?)
      message)))

(defcustom lsp-diagnostics-flymake-message-formatter
  #'lsp-diagnostics-flymake-default-message-formatter
  "Function used to build the flymake text string for an LSP diagnostic.

Flymake stores a single string per diagnostic
(`flymake-diagnostic-text'), so any presentation choice (whether to
append the rule code, the source, severity prefix, etc.) must be made
before the `flymake-diagnostic' is constructed.  This hook provides the
place to make that choice.

Called with one argument, the LSP `Diagnostic' plist (with fields such
as `:message', `:code?', `:source?', `:severity?', `:tags?').  Must
return a string.

The default `lsp-diagnostics-flymake-default-message-formatter' appends
`[code]' when the diagnostic carries one and returns the bare message
otherwise.  Override to suppress the code, prepend the source, or
reformat the message in any other way."
  :type 'function
  :group 'lsp-diagnostics
  :package-version '(lsp-mode . "10.0.1"))

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
             (--map (-let* (((&Diagnostic :severity?
                                          :range (range &as &Range
                                                        :start (&Position :line start-line :character)
                                                        :end (&Position :line end-line))) it)
                            ((start . end) (lsp--range-to-region range))
                            (text (funcall lsp-diagnostics-flymake-message-formatter it)))
                      (when (= start end)
                        (if-let* ((region (flymake-diag-region (current-buffer)
                                                               (1+ start-line)
                                                               character)))
                            (setq start (car region)
                                  end (cdr region))
                          (lsp-save-restriction-and-excursion
                            (goto-char (point-min))
                            (setq start (line-beginning-position (1+ start-line))
                                  end (line-end-position (1+ end-line))))))
                      (flymake-make-diagnostic (current-buffer)
                                               start
                                               end
                                               (cl-case severity?
                                                 (1 :error)
                                                 (2 :warning)
                                                 (t :note))
                                               text))))
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

(defsubst lsp-diagnostics--position< (line-a char-a line-b char-b)
  "Non-nil if 0-based position (LINE-A, CHAR-A) precedes (LINE-B, CHAR-B)."
  (or (< line-a line-b)
      (and (= line-a line-b) (< char-a char-b))))

(defun lsp-diagnostics--remap-point (line char esl esc oel oec nel nec)
  "Map 0-based (LINE, CHAR) across an edit; return a cons (NEW-LINE . NEW-CHAR).
The edit replaced the text from (ESL, ESC) to (OEL, OEC) with text whose
end is now at (NEL, NEC).  A point before the edit is returned unchanged;
a point at or after the edit's old end is shifted accordingly.  Callers
must keep points inside the replaced region away from here (see
`lsp-diagnostics--remap-diagnostic')."
  (cond
   ;; At or before the edit's start: unaffected.
   ((not (lsp-diagnostics--position< esl esc line char))
    (cons line char))
   ;; On the edit's old end line, past the change: rebase onto the new end.
   ((= line oel)
    (cons nel (+ nec (- char oec))))
   ;; On a later line: only the line number shifts.
   (t
    (cons (+ line (- nel oel)) char))))

(lsp-defun lsp-diagnostics--remap-diagnostic
  ((diagnostic &as &Diagnostic
               :range (&Range :start (start &as &Position :line sl :character sc)
                              :end (end &as &Position :line el :character ec)))
   esl esc oel oec nel nec)
  "Return DIAGNOSTIC with its range remapped across an edit, or nil to drop it.
The edit replaced the text from 0-based (ESL, ESC) to (OEL, OEC) with
text now ending at (NEL, NEC).  A diagnostic disjoint from the replaced
region is shifted to its new location; one that overlaps the replaced
region is dropped (returns nil) so the server can republish it.  See
issue #3888."
  (if (and (lsp-diagnostics--position< sl sc oel oec)
           (lsp-diagnostics--position< esl esc el ec))
      ;; Overlaps the replaced region: its text changed, so drop it.
      nil
    (-let (((new-start-line . new-start-char)
            (lsp-diagnostics--remap-point sl sc esl esc oel oec nel nec))
           ((new-end-line . new-end-char)
            (lsp-diagnostics--remap-point el ec esl esc oel oec nel nec)))
      (lsp:set-position-line start new-start-line)
      (lsp:set-position-character start new-start-char)
      (lsp:set-position-line end new-end-line)
      (lsp:set-position-character end new-end-char)
      diagnostic)))

(defun lsp-diagnostics--update-after-change (esl esc oel oec nel nec)
  "Remap the current buffer's stored diagnostics after a local edit.
Keeps diagnostics aligned with the buffer until the server republishes,
rather than discarding them.  The edit replaced the text from 0-based
(ESL, ESC) to (OEL, OEC) with text now ending at (NEL, NEC); see
`lsp-diagnostics--remap-diagnostic' for the remapping rule.  Fixes issue
#3888 for every edit (typing and workspace edits alike)."
  (when-let* ((file-path (buffer-file-name)))
    (let ((path (lsp--fix-path-casing file-path))
          (changed nil))
      (dolist (workspace (lsp-workspaces))
        (let* ((diagnostics (lsp--workspace-diagnostics workspace))
               (file-diagnostics (gethash path diagnostics)))
          (when file-diagnostics
            (setq changed t)
            (if-let* ((remapped (-keep (lambda (diagnostic)
                                         (lsp-diagnostics--remap-diagnostic
                                          diagnostic esl esc oel oec nel nec))
                                       file-diagnostics)))
                (puthash path remapped diagnostics)
              (remhash path diagnostics)))))
      (when changed
        (run-hooks 'lsp-diagnostics-updated-hook)))))

(defun lsp-diagnostics--clear-after-edit (&rest _)
  "Clear the current buffer's diagnostics across all of its workspaces.
Used as the fallback when a `remap' edit cannot be mapped to a line
range, and as the action for the `clear' value of
`lsp-diagnostics-on-edit' (see `lsp-diagnostics--clear-on-workspace-edit')."
  (when-let* ((file-path (buffer-file-name)))
    (let ((path (lsp--fix-path-casing file-path))
          (changed nil))
      (dolist (workspace (lsp-workspaces))
        (-let [diagnostics (lsp--workspace-diagnostics workspace)]
          (when (gethash path diagnostics)
            (setq changed t)
            (remhash path diagnostics))))
      (when changed
        (run-hooks 'lsp-diagnostics-updated-hook)))))

(defun lsp-diagnostics--clear-on-workspace-edit (&rest _)
  "Clear the buffer's diagnostics after a workspace edit.
Only acts when `lsp-diagnostics-on-edit' is `clear'; with the default
`remap' the diagnostics are instead kept and remapped by
`lsp-on-change'."
  (when (eq lsp-diagnostics-on-edit 'clear)
    (lsp-diagnostics--clear-after-edit)))

(add-hook 'lsp-after-apply-edits-hook #'lsp-diagnostics--clear-on-workspace-edit)

(lsp-consistency-check lsp-diagnostics)

(provide 'lsp-diagnostics)
;;; lsp-diagnostics.el ends here
