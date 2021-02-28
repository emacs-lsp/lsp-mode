;;; lsp-dired.el --- `lsp-mode' diagnostics integrated into `dired' -*- lexical-binding: t -*-

;; Copyright (C) 2021

;; Author: Alexander Miller <alexanderm@web.de>
;; Author: Ivan Yonchovski <yyoncho@gmail.com>

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
;;; `lsp-mode' diagnostics integrated into `dired'

;;; Code:

(require 'dired)
(require 'pcase)
(require 'lsp-mode)

(defvar lsp-dired--ranger-adjust nil)
(with-eval-after-load 'ranger (setf lsp-dired--ranger-adjust t))

(defvar-local lsp-dired-displayed nil
  "Flags whether icons have been added.")

(defvar-local lsp-dired--covered-subdirs nil
  "List of subdirs icons were already added for.")

(defun lsp-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (display-graphic-p)
             (not lsp-dired-displayed)
             dired-subdir-alist)
    (setq-local lsp-dired-displayed t)
    (pcase-dolist (`(,path . ,pos) dired-subdir-alist)
      (lsp-dired--insert-for-subdir path pos))))

(defun lsp-dired--insert-for-subdir (path pos)
  "Display icons for subdir PATH at given POS."
  (let ((buf (current-buffer)))
    ;; run the function after current to make sure that we are creating the
    ;; overlays after `treemacs-icons-dired' has run.
    (run-with-idle-timer
     0.0 nil
     (lambda ()
       (unless (and (member path lsp-dired--covered-subdirs)
                    (not (buffer-live-p buf)))
         (with-current-buffer buf
           (add-to-list 'lsp-dired--covered-subdirs path)
           (let (buffer-read-only)
             (save-excursion
               (goto-char pos)
               (forward-line (if lsp-dired--ranger-adjust 1 2))
               (cl-block :file
                 (while (not (eobp))
                   (if (dired-move-to-filename nil)
                       (let* ((file (dired-get-filename nil t))
                              (bol (progn
                                     (point-at-bol)
                                     (search-forward-regexp "^[[:space:]]*" (line-end-position) t)
                                     (point)))
                              (face (lsp-dired--face-for-path file)))
                         (when face
                           (-doto (make-overlay bol (point-at-eol))
                             (overlay-put 'evaporate t)
                             (overlay-put 'face face))))
                     (cl-return-from :file nil))
                   (forward-line 1)))))))))))

(defface lsp-dired-path-face '((t :inherit font-lock-string-face))
  "Face used for breadcrumb paths on headerline."
  :group 'lsp-faces)

(defface lsp-dired-path-error-face
  '((t :underline (:style wave :color "Red1")))
  "Face used for breadcrumb paths on headerline when there is an error under that path"
  :group 'lsp-faces)

(defface lsp-dired-path-warning-face
  '((t :underline (:style wave :color "Yellow")))
  "Face used for breadcrumb paths on headerline when there is an warning under that path"
  :group 'lsp-faces)

(defface lsp-dired-path-info-face
  '((t :underline (:style wave :color "Green")))
  "Face used for breadcrumb paths on headerline when there is an info under that path"
  :group 'lsp-faces)

(defface lsp-dired-path-hint-face
  '((t :underline (:style wave :color "Green")))
  "Face used for breadcrumb paths on headerline when there is an hint under that path"
  :group 'lsp-faces)

(defun lsp-dired--face-for-path (dir)
  "Calculate the face for DIR."
  (when-let ((diags (lsp-diagnostics-stats-for (directory-file-name dir))))
    (cl-labels ((check-severity
                 (severity)
                 (not (zerop (aref diags severity)))))
      (cond
       ((check-severity lsp/diagnostic-severity-error)
        'lsp-dired-path-error-face)
       ((check-severity lsp/diagnostic-severity-warning)
        'lsp-dired-path-warning-face)
       ((check-severity lsp/diagnostic-severity-information)
        'lsp-dired-path-info-face)
       ((check-severity lsp/diagnostic-severity-hint)
        'lsp-dired-path-hint-face)))))

(defun lsp-dired--insert-subdir-advice (&rest args)
  "Advice to dired & dired+ insert-subdir commands.
Will add icons for the subdir in the `car' of ARGS."
  (let* ((path (car args))
         (pos (cdr (assoc path dired-subdir-alist))))
    (when pos
      (lsp-dired--insert-for-subdir path pos))))

(defun lsp-dired--kill-subdir-advice (&rest _args)
  "Advice to dired kill-subdir commands.
Will remove the killed subdir from `lsp-dired--covered-subdirs'."
  (setf lsp-dired--covered-subdirs (delete (dired-current-directory)
                                           lsp-dired--covered-subdirs)))

(defun lsp-dired--reset (&rest _args)
  "Reset metadata on revert."
  (setq-local lsp-dired--covered-subdirs nil)
  (setq-local lsp-dired-displayed nil))

;;;###autoload
(define-minor-mode lsp-dired-mode
  "Display `lsp-mode' icons for each file in a dired buffer."
  :require    'lsp-dired
  :init-value nil
  :global     t
  :group      'lsp-mode
  (cond
   (lsp-dired-mode
    (add-hook 'dired-after-readin-hook #'lsp-dired--display)
    (advice-add 'dired-kill-subdir :before #'lsp-dired--kill-subdir-advice)
    (advice-add 'dired-insert-subdir :after #'lsp-dired--insert-subdir-advice)
    (advice-add 'diredp-insert-subdirs :after #'lsp-dired--insert-subdir-advice)
    (advice-add 'dired-revert :before #'lsp-dired--reset)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (lsp-dired--display)))))
   (t
    (advice-remove 'dired-kill-subdir #'lsp-dired--kill-subdir-advice)
    (advice-remove 'dired-insert-subdir #'lsp-dired--insert-subdir-advice)
    (advice-remove 'diredp-insert-subdirs #'lsp-dired--insert-subdir-advice)
    (advice-remove 'dired-revert #'lsp-dired--reset)
    (remove-hook 'dired-after-readin-hook #'lsp-dired--display)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (dired-revert)))))))

(provide 'lsp-dired)

;;; lsp-dired.el ends here
