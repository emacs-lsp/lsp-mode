;;; lsp-lens.el --- LSP lens -*- lexical-binding: t; -*-
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
;;  LSP lens
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-lens nil
  "LSP support for lens"
  :prefix "lsp-lens-"
  :group 'lsp-mode
  :tag "LSP Lens")

(defcustom lsp-lens-debounce-interval 0.001
  "Debounce interval for loading lenses."
  :group 'lsp-lens
  :type 'number)

(defcustom lsp-lens-place-position 'end-of-line
  "The position to place lens relative to returned lens position."
  :group 'lsp-lens
  :type '(choice (const above-line)
                 (const end-of-line))
  :package-version '(lsp-mode . "8.0.0"))

(defface lsp-lens-mouse-face
  '((t :height 0.8 :inherit link))
  "The face used for code lens overlays."
  :group 'lsp-lens)

(defface lsp-lens-face
  '((t :inherit lsp-details-face))
  "The face used for code lens overlays."
  :group 'lsp-lens)

(defvar-local lsp-lens--modified? nil)

(defvar-local lsp-lens--overlays nil
  "Current lenses.")

(defvar-local lsp-lens--page nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp-lens--last-count nil
  "The number of lenses the last time they were rendered.")

(defvar lsp-lens-backends '(lsp-lens--backend)
  "Backends providing lenses.")

(defvar-local lsp-lens--refresh-timer nil
  "Refresh timer for the lenses.")

(defvar-local lsp-lens--data nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp-lens--backend-cache nil)

(defun lsp-lens--text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line."
  ;; `current-column' takes prettification into account
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun lsp-lens--update (ov)
  "Redraw quick-peek overlay OV."
  (let* ((offset (lsp-lens--text-width (save-excursion
                                         (beginning-of-visual-line)
                                         (point))
                                       (save-excursion
                                         (beginning-of-line-text)
                                         (point))))
         (str (if (eq 'end-of-line lsp-lens-place-position)
                  (overlay-get ov 'lsp--lens-contents)
                (concat (make-string offset ?\s)
                        (overlay-get ov 'lsp--lens-contents)))))
    (save-excursion
      (goto-char (overlay-start ov))
      (if (eq 'end-of-line lsp-lens-place-position)
          (overlay-put ov 'after-string (propertize (concat " " str) 'cursor t))
        (overlay-put ov 'before-string (concat str "\n")))
      (overlay-put ov 'lsp-original str))))

(defun lsp-lens--overlay-ensure-at (pos)
  "Find or create a lens for the line at POS."
  (-doto (save-excursion
           (goto-char pos)
           (if (eq 'end-of-line lsp-lens-place-position)
               (make-overlay (point-at-eol) -1 nil t t)
             (make-overlay (point-at-bol) (1+ (point-at-eol)) nil t t)))
    (overlay-put 'lsp-lens t)
    (overlay-put 'evaporate t)
    (overlay-put 'lsp-lens-position pos)))

(defun lsp-lens--show (str pos metadata)
  "Show STR in an inline window at POS including METADATA."
  (let ((ov (lsp-lens--overlay-ensure-at pos)))
    (save-excursion
      (goto-char pos)
      (setf (overlay-get ov 'lsp--lens-contents) str)
      (setf (overlay-get ov 'lsp--metadata) metadata)
      (lsp-lens--update ov)
      ov)))

(defun lsp-lens--idle-function (&optional buffer)
  "Create idle function for buffer BUFFER."
  (when (and (or (not buffer) (eq (current-buffer) buffer))
             (not (equal (cons (window-start) (window-end)) lsp-lens--page)))
    (lsp-lens--schedule-refresh nil)))

(defun lsp-lens--overlay-matches-pos (ov pos)
  "Check if OV is a lens covering POS."
  (and (overlay-get ov 'lsp-lens)
       (overlay-start ov)
       (<= (overlay-start ov) pos)
       (< pos (overlay-end ov))))

(defun lsp-lens--after-save ()
  "Handler for `after-save-hook' for lens mode."
  (lsp-lens--schedule-refresh t))

(defun lsp-lens--schedule-refresh (&optional buffer-modified?)
  "Call each of the backend.
BUFFER-MODIFIED? determines whether the buffer was modified or
not."
  (-some-> lsp-lens--refresh-timer cancel-timer)

  (setq lsp-lens--page (cons (window-start) (window-end)))
  (setq lsp-lens--refresh-timer
        (run-with-timer lsp-lens-debounce-interval
                        nil
                        #'lsp-lens-refresh
                        (or lsp-lens--modified? buffer-modified?)
                        (current-buffer))))

(defun lsp-lens--schedule-refresh-modified ()
  "Schedule a lens refresh due to a buffer-modification.
See `lsp-lens--schedule-refresh' for details."
  (lsp-lens--schedule-refresh t))

(defun lsp-lens--keymap (command)
  "Build the lens keymap for COMMAND."
  (-doto (make-sparse-keymap)
    (define-key [mouse-1] (lsp-lens--create-interactive-command command))))

(defun lsp-lens--create-interactive-command (command?)
  "Create an interactive COMMAND? for the lens.
COMMAND? shall be an `&Command' (e.g. `&CodeLens' :command?) and
mustn't be nil."
  (if (functionp (lsp:command-command command?))
      (lsp:command-command command?)
    (lambda ()
      (interactive)
      (lsp--execute-command command?))))

(defun lsp-lens--display (lenses)
  "Show LENSES."
  ;; rerender only if there are lenses which are not processed or if their count
  ;; has changed(e. g. delete lens should trigger redisplay).
  (let ((scroll-preserve-screen-position t))
    (setq lsp-lens--modified? nil)
    (when (or (-any? (-lambda ((&CodeLens :_processed processed))
                       (not processed))
                     lenses)
              (eq (length lenses) lsp-lens--last-count)
              (not lenses))
      (setq lsp-lens--last-count (length lenses))
      (mapc #'delete-overlay lsp-lens--overlays)
      (setq lsp-lens--overlays
            (->> lenses
                 (-filter #'lsp:code-lens-command?)
                 (--map (prog1 it (lsp-put it :_processed t)))
                 (-group-by (-compose #'lsp:position-line #'lsp:range-start #'lsp:code-lens-range))
                 (-map
                  (-lambda ((_ . lenses))
                    (let* ((sorted (-sort (-on #'< (-compose #'lsp:position-character
                                                             #'lsp:range-start
                                                             #'lsp:code-lens-range))
                                          lenses))
                           (data (-map
                                  (-lambda ((lens &as &CodeLens
                                                  :command? (command &as
                                                                     &Command :title :_face face)))
                                    (propertize
                                     title
                                     'face (or face 'lsp-lens-face)
                                     'action (lsp-lens--create-interactive-command command)
                                     'pointer 'hand
                                     'mouse-face 'lsp-lens-mouse-face
                                     'local-map (lsp-lens--keymap command)))
                                  sorted)))
                      (lsp-lens--show
                       (s-join (propertize "|" 'face 'lsp-lens-face) data)
                       (-> sorted cl-first lsp:code-lens-range lsp:range-start lsp--position-to-point)
                       data)))))))))

(defun lsp-lens-refresh (buffer-modified? &optional buffer)
  "Refresh lenses using lenses backend.
BUFFER-MODIFIED? determines whether the BUFFER is modified or not."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (backend lsp-lens-backends)
          (funcall backend buffer-modified?
                   (lambda (lenses version)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (lsp-lens--process backend lenses version))))))))))

(defun lsp-lens--process (backend lenses version)
  "Process LENSES originated from BACKEND.
VERSION is the version of the file. The lenses has to be
refreshed only when all backends have reported for the same
version."
  (setq lsp-lens--data (or lsp-lens--data (make-hash-table)))
  (puthash backend (cons version (append lenses nil)) lsp-lens--data)

  (-let [backend-data (->> lsp-lens--data ht-values (-filter #'cl-rest))]
    (when (and
           (= (length lsp-lens-backends) (ht-size lsp-lens--data))
           (seq-every-p (-lambda ((version))
                          (or (not version) (eq version lsp--cur-version)))
                        backend-data))
      ;; display the data only when the backends have reported data for the
      ;; current version of the file
      (lsp-lens--display (apply #'append (-map #'cl-rest backend-data)))))
  version)

(lsp-defun lsp--lens-backend-not-loaded? ((&CodeLens :range
                                                     (&Range :start)
                                                     :command?
                                                     :_pending pending))
  "Return t if LENS has to be loaded."
  (let ((window (get-buffer-window (current-buffer))))
    ;; (window-start/end) does not consider current window buffer if not passed manually
    (and (< (window-start window) (lsp--position-to-point start) (window-end window))
         (not command?)
         (not pending))))

(lsp-defun lsp--lens-backend-present? ((&CodeLens :range (&Range :start) :command?))
  "Return t if LENS has to be loaded."
  (or command?
      (not (< (window-start) (lsp--position-to-point start) (window-end)))))

(defun lsp-lens--backend-fetch-missing (lenses callback file-version)
  "Fetch LENSES without command in for the current window.

TICK is the buffer modified tick. If it does not match
`buffer-modified-tick' at the time of receiving the updates the
updates must be discarded..
CALLBACK - the callback for the lenses.
FILE-VERSION - the version of the file."
  (seq-each
   (lambda (it)
     (with-lsp-workspace (lsp-get it :_workspace)
       (lsp-put it :_pending t)
       (lsp-put it :_workspace nil)
       (lsp-request-async "codeLens/resolve" it
                          (-lambda ((&CodeLens :command?))
                            (lsp-put it :_pending nil)
                            (lsp-put it :command command?)
                            (when (seq-every-p #'lsp--lens-backend-present? lenses)
                              (funcall callback lenses file-version)))
                          :mode 'tick)))
   (seq-filter #'lsp--lens-backend-not-loaded? lenses)))

(defun lsp-lens--backend (modified? callback)
  "Lenses backend using `textDocument/codeLens'.
MODIFIED? - t when buffer is modified since the last invocation.
CALLBACK - callback for the lenses."
  (when (lsp--find-workspaces-for "textDocument/codeLens")
    (if modified?
        (progn
          (setq lsp-lens--backend-cache nil)
          (lsp-request-async "textDocument/codeLens"
                             `(:textDocument (:uri ,(lsp--buffer-uri)))
                             (lambda (lenses)
                               (setq lsp-lens--backend-cache
                                     (seq-mapcat
                                      (-lambda ((workspace . workspace-lenses))
                                        ;; preserve the original workspace so we can later use it to resolve the lens
                                        (seq-do (-rpartial #'lsp-put :_workspace workspace) workspace-lenses)
                                        workspace-lenses)
                                      lenses))
                               (if (-every? #'lsp:code-lens-command? lsp-lens--backend-cache)
                                   (funcall callback lsp-lens--backend-cache lsp--cur-version)
                                 (lsp-lens--backend-fetch-missing lsp-lens--backend-cache callback lsp--cur-version)))
                             :error-handler #'ignore
                             :mode 'tick
                             :no-merge t
                             :cancel-token (concat (buffer-name (current-buffer)) "-lenses")))
      (if (-all? #'lsp--lens-backend-present? lsp-lens--backend-cache)
          (funcall callback lsp-lens--backend-cache lsp--cur-version)
        (lsp-lens--backend-fetch-missing lsp-lens--backend-cache callback lsp--cur-version)))))

(defun lsp-lens--refresh-buffer ()
  "Trigger lens refresh on buffer."
  (remove-hook 'lsp-on-idle-hook #'lsp-lens--refresh-buffer t)
  (when (bound-and-true-p lsp-lens-mode)
    (lsp-lens-refresh t)))

(defun lsp--lens-on-refresh (workspace)
  "Clear lens within all buffers of WORKSPACE, refreshing all workspace buffers."
  (cl-assert (not (eq nil workspace)))
  (->> (lsp--workspace-buffers workspace)
       (mapc (lambda (buffer)
               (lsp-with-current-buffer buffer
                 (if (lsp--buffer-visible-p)
                     (when (bound-and-true-p lsp-lens-mode)
                       (lsp-lens-refresh t))
                   (progn
                     (add-hook 'lsp-on-idle-hook #'lsp-lens--refresh-buffer nil t)
                     (lsp--idle-reschedule (current-buffer)))))))))

;;;###autoload
(defun lsp-lens--enable ()
  "Enable lens mode."
  (when (and lsp-lens-enable
             (lsp-feature? "textDocument/codeLens"))
    (lsp-lens-mode 1)))

(defun lsp-lens--disable ()
  "Disable lens mode."
  (lsp-lens-mode -1))

;;;###autoload
(defun lsp-lens-show ()
  "Display lenses in the buffer."
  (interactive)
  (->> (lsp-request "textDocument/codeLens"
                    `(:textDocument (:uri
                                     ,(lsp--path-to-uri buffer-file-name))))
       (seq-map (-lambda ((lens &as &CodeAction :command?))
                  (if command?
                      lens
                    (lsp-request "codeLens/resolve" lens))))
       lsp-lens--display))

;;;###autoload
(defun lsp-lens-hide ()
  "Delete all lenses."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (seq-do #'delete-overlay lsp-lens--overlays)
    (setq lsp-lens--overlays nil)))

;;;###autoload
(define-minor-mode lsp-lens-mode
  "Toggle code-lens overlays."
  :group 'lsp-lens
  :global nil
  :init-value nil
  :lighter " Lens"
  (cond
   (lsp-lens-mode
    (add-hook 'lsp-unconfigure-hook #'lsp-lens--disable nil t)
    (add-hook 'lsp-configure-hook #'lsp-lens--enable nil t)
    (add-hook 'lsp-on-idle-hook #'lsp-lens--idle-function nil t)
    (add-hook 'lsp-on-change-hook #'lsp-lens--schedule-refresh-modified nil t)
    (add-hook 'after-save-hook #'lsp-lens--after-save nil t)
    (add-hook 'before-revert-hook #'lsp-lens-hide nil t)
    (lsp-lens-refresh t))
   (t
    (remove-hook 'lsp-on-idle-hook #'lsp-lens--idle-function t)
    (remove-hook 'lsp-on-change-hook #'lsp-lens--schedule-refresh-modified t)
    (remove-hook 'after-save-hook #'lsp-lens--after-save t)
    (remove-hook 'before-revert-hook #'lsp-lens-hide t)
    (when lsp-lens--refresh-timer
      (cancel-timer lsp-lens--refresh-timer))
    (setq lsp-lens--refresh-timer nil)
    (lsp-lens-hide)
    (setq lsp-lens--last-count nil)
    (setq lsp-lens--backend-cache nil)
    (remove-hook 'lsp-configure-hook #'lsp-lens--enable t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-lens--disable t))))


;; avy integration

(declare-function avy-process "ext:avy" (candidates &optional overlay-fn cleanup-fn))
(declare-function avy--key-to-char "ext:avy" (c))
(defvar avy-action)

;;;###autoload
(defun lsp-avy-lens ()
  "Click lsp lens using `avy' package."
  (interactive)
  (unless lsp-lens--overlays
    (user-error "No lenses in current buffer"))
  (let* ((avy-action 'identity)
         (position (if (eq lsp-lens-place-position 'end-of-line)
                       'after-string
                     'before-string))
         (action (cl-third
                  (avy-process
                   (-mapcat
                    (lambda (overlay)
                      (-map-indexed
                       (lambda (index lens-token)
                         (list overlay index
                               (get-text-property 0 'action lens-token)))
                       (overlay-get overlay 'lsp--metadata)))
                    lsp-lens--overlays)
                   (-lambda (path ((ov index) . _win))
                     (let* ((path (mapcar #'avy--key-to-char path))
                            (str (propertize (string (car (last path)))
                                             'face 'avy-lead-face))
                            (old-str (overlay-get ov position))
                            (old-str-tokens (s-split "|" old-str))
                            (old-token (seq-elt old-str-tokens index))
                            (tokens `(,@(-take index old-str-tokens)
                                      ,(-if-let ((_ prefix suffix)
                                                 (s-match "\\(^[[:space:]]+\\)\\(.*\\)" old-token))
                                           (concat prefix str suffix)
                                         (concat str old-token))
                                      ,@(-drop (1+ index) old-str-tokens)))
                            (new-str (s-join (propertize "|" 'face 'lsp-lens-face) tokens))
                            (new-str (if (or (s-ends-with? "\n" new-str)
                                             (eq lsp-lens-place-position 'end-of-line))
                                         new-str
                                       (concat new-str "\n"))))
                       (overlay-put ov position new-str)))
                   (lambda ()
                     (--map (overlay-put it position
                                         (overlay-get it 'lsp-original))
                            lsp-lens--overlays))))))
    (when action (funcall-interactively action))))

(lsp-consistency-check lsp-lens)

(provide 'lsp-lens)
;;; lsp-lens.el ends here
