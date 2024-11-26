;;; lsp-inline-completion.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 emacs-lsp maintainers

;; Author: Rodrigo Kassick
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (ht "2.3") (spinner "1.7.3"))
;; Version: 9.0.1

;; URL: https://github.com/emacs-lsp/lsp-mode
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

;; Inline Completions support
;; Specification here https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_inlineCompletion

;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'cl-lib)

(declare-function company--active-p "ext:company")
(declare-function company-cancel "ext:company" (&optional result))
(declare-function company-manual-begin "ext:company")

(defun lsp-inline-completion--trigger-kind (implicit)
  (plist-put (lsp--text-document-position-params)
             :context (ht ("triggerKind"
                           (if implicit
                               lsp/inline-completion-trigger-automatic
                             lsp/inline-completion-trigger-invoked)))))

;;;###autoload
(defun lsp-inline-completion-parse-items (response)
  "Calls textDocument_inlineCompletion and returns a list of InlineCompletionItem's"

  (-some-->
      response
    ;; Kludge to workaround multiple backends responding -- it may
    ;; come as a list (multiple servers) or as a single ht (single
    ;; server). Promote it to list and move on
    (if (ht-p it) (list it) it)

    ;; Response may or may not come inside an :items. Parse each
    ;; response to ensure compatibility.
    (cl-map 'list (lambda (elt)
                    (if (lsp-inline-completion-list? elt)
                        (lsp:inline-completion-list-items elt)
                      elt))
            it)

    ;; Join everything into a single list
    (apply 'seq-concatenate `(list ,@it))))


;;;;;; Default UI -- overlay

(defvar lsp-inline-completion-active-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'lsp-inline-completion-next)
    (define-key map (kbd "C-n") #'lsp-inline-completion-next)
    (define-key map (kbd "M-n") #'lsp-inline-completion-next)
    (define-key map (kbd "C-p") #'lsp-inline-completion-prev)
    (define-key map (kbd "M-p") #'lsp-inline-completion-prev)
    (define-key map (kbd "<return>") #'lsp-inline-completion-accept)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [down-mouse-3] #'ignore)
    (define-key map [up-mouse-1] #'ignore)
    (define-key map [up-mouse-3] #'ignore)
    (define-key map [mouse-1] #'lsp-inline-completion-accept)
    (define-key map [mouse-3] #'lsp-inline-completion-accept)
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-g") #'lsp-inline-completion-cancel)
    (define-key map (kbd "<escape>") #'lsp-inline-completion-cancel)
    (define-key map (kbd "C-c C-k") #'lsp-inline-completion-cancel)
    (define-key map [mouse-movement] #'ignore)
    (define-key map [t] #'lsp-inline-completion-cancel-with-input)
    map)
  "Keymap active when showing inline code suggestions")

;;;###autoload
(defface lsp-inline-completion-overlay-face
  '((t :inherit shadow))
  "Face for the inline code suggestions overlay."
  :group 'lsp-mode)

;; Local Buffer State

(defvar-local lsp-inline-completion--items nil "The completions provided by the server")
(defvar-local lsp-inline-completion--current nil "The current suggestion to be displayed")
(defvar-local lsp-inline-completion--overlay nil "The overlay displaying code suggestions")
(defvar-local lsp-inline-completion--start-point nil "The point where the completion started")
(defvar-local lsp-inline-completion--showing-company nil "If company was active when the tooltip is shown")

(defcustom lsp-before-inline-completion-hook nil
  "Hooks run before starting code suggestions"
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-inline-completion-hook nil
  "Hooks executed after asking for code suggestions."
  :type 'hook
  :group 'lsp-mode)

;; TODO: Add parameters to the hooks!
(defcustom lsp-inline-completion-accepted-hook nil
  "Hooks executed after accepting a code suggestion."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-shown-hook nil
  "Hooks executed after showing a suggestion."
  :type 'hook
  :group 'lsp-mode)

(defsubst lsp-inline-completion--overlay-visible ()
  "Return whether the `overlay' is avaiable."
  (and (overlayp lsp-inline-completion--overlay)
       (overlay-buffer lsp-inline-completion--overlay)))

(defun lsp-inline-completion--clear-overlay ()
  "Hide the suggestion overlay"
  (when (lsp-inline-completion--overlay-visible)
    (delete-overlay lsp-inline-completion--overlay))
  (setq lsp-inline-completion--overlay nil))


(defun lsp-inline-completion--get-overlay (beg end)
  "Build the suggestions overlay"
  (when (overlayp lsp-inline-completion--overlay)
    (lsp-inline-completion--clear-overlay))

  (setq lsp-inline-completion--overlay (make-overlay beg end nil nil t))
  (overlay-put lsp-inline-completion--overlay 'keymap lsp-inline-completion-active-map)
  (overlay-put lsp-inline-completion--overlay 'priority 9000)

  lsp-inline-completion--overlay)


;;;###autoload
(defun lsp-inline-completion-show-keys ()
  "Shows active keymap hints in the minibuffer"

  (unless (and lsp-inline-completion--items
               (numberp lsp-inline-completion--current))
    (error "No completions to show"))

  (let ((message-log-max nil))
    (message (concat "Completion "
                     (propertize (format "%d" (1+ lsp-inline-completion--current)) 'face 'bold)
                     "/"
                     (propertize (format "%d" (length lsp-inline-completion--items)) 'face 'bold)

                     (-when-let (keys (where-is-internal #'lsp-inline-completion-next lsp-inline-completion-active-map))
                       (concat ". "
                               (propertize " Next" 'face 'italic)
                               (format ": [%s]"
                                       (string-join (--map (propertize (key-description it) 'face 'help-key-binding)
                                                           keys)
                                                    "/"))))
                     (-when-let (keys (where-is-internal #'lsp-inline-completion-accept lsp-inline-completion-active-map))
                       (concat (propertize " Accept" 'face 'italic)
                               (format ": [%s]"
                                       (string-join (--map (propertize (key-description it) 'face 'help-key-binding)
                                                           keys)
                                                    "/"))))))))

(defun lsp-inline-completion-show-overlay ()
  "Makes the suggestion overlay visible"
  (unless (and lsp-inline-completion--items
               (numberp lsp-inline-completion--current))
    (error "No completions to show"))

  (lsp-inline-completion--clear-overlay)

  (setq lsp-inline-completion--showing-company
        (and (bound-and-true-p company-mode)
             (company--active-p)))

  (when lsp-inline-completion--showing-company
    (company-cancel))

  (-let* ((suggestion
           (elt lsp-inline-completion--items
                lsp-inline-completion--current))
          ((&InlineCompletionItem? :insert-text :range?) suggestion)
          ((&RangeToPoint :start :end) range?)
          (start-point (or start (point)))
          (showing-at-eol (save-excursion
                            (goto-char start-point)
                            (eolp)))
          (beg (if showing-at-eol (1- start-point) start-point))
          (end-point  (or end (1+ beg)))
          (text (cond
                 ((lsp-markup-content? insert-text) (lsp:markup-content-value insert-text))
                 (t insert-text)))
          (propertizedText (concat
                            (buffer-substring beg start-point)
                            (propertize text 'face 'lsp-inline-completion-overlay-face)))
          (ov (lsp-inline-completion--get-overlay beg end-point))
          (completion-is-substr (string-equal
                                 (buffer-substring beg lsp-inline-completion--start-point)
                                 (substring propertizedText 0 (- lsp-inline-completion--start-point beg))))
          display-str after-str target-position)

    (goto-char beg)

    (put-text-property 0 (length propertizedText) 'cursor t propertizedText)

    (if completion-is-substr
        (progn
          ;; Show the prefix as `display'
          (setq display-str (substring propertizedText 0 (- lsp-inline-completion--start-point beg)))
          (setq after-str (substring propertizedText (- lsp-inline-completion--start-point beg) nil))
          (setq target-position lsp-inline-completion--start-point))


      (setq display-str (substring propertizedText 0 1))
      (setq after-str (substring propertizedText 1))
      (setq target-position beg))

    (overlay-put ov 'display display-str)
    (overlay-put ov 'after-string after-str)

    (goto-char target-position)

    (run-hooks 'lsp-inline-completion-shown-hook)))

(defun lsp-inline-completion-accept ()
  "Accepts the current suggestion"
  (interactive)
  (unless (lsp-inline-completion--overlay-visible)
    (error "Not showing suggestions"))

  (lsp-inline-completion--clear-overlay)
  (-let* ((suggestion (elt lsp-inline-completion--items lsp-inline-completion--current))
          ((&InlineCompletionItem? :insert-text :range? :command?) suggestion)
          ((kind . text) (cond
                          ((lsp-markup-content? insert-text)
                           (cons 'snippet (lsp:markup-content-value insert-text) ))
                          (t (cons 'text insert-text))))
          ((start . end) (when range?
                           (-let (((&RangeToPoint :start :end) range?)) (cons start end))))
          (text-insert-start (or start lsp-inline-completion--start-point))
          text-insert-end
          (completion-is-substr (string-equal
                                 (buffer-substring text-insert-start lsp-inline-completion--start-point)
                                 (substring text 0 (- lsp-inline-completion--start-point text-insert-start)))))

    (when text-insert-start
      (goto-char text-insert-start))

    ;; When range is provided, must replace the text of the range by the text
    ;; to insert
    (when (and start end (/= start end))
      (delete-region start end))

    ;; Insert suggestion, keeping the cursor at the start point
    (insert text)
    (setq text-insert-end (point))

    ;; If a template, format it -- keep track of the end position!
    (when (eq kind 'snippet)
      (let ((end-marker (set-marker (make-marker) (point))))
        (lsp--expand-snippet (buffer-substring text-insert-start text-insert-end)
                             text-insert-start
                             text-insert-end)
        (setq text-insert-end (marker-position end-marker))
        (set-marker end-marker nil)))

    ;; Post command
    (when command?
      (lsp--execute-command command?))

    (if completion-is-substr
        (goto-char lsp-inline-completion--start-point)
      (goto-char text-insert-start))

    ;; hooks
    (run-hook-with-args-until-failure 'lsp-inline-completion-accepted-hook text text-insert-start text-insert-end)))

;;;###autoload
(defun lsp-inline-completion-cancel ()
  "Close the suggestion overlay"
  (interactive)
  (when (lsp-inline-completion--overlay-visible)

    (lsp-inline-completion--clear-overlay)

    (when lsp-inline-completion--start-point
      (goto-char lsp-inline-completion--start-point))

    (when lsp-inline-completion--showing-company
      (company-manual-begin))))

(defun lsp-inline-completion-cancel-with-input (event &optional arg)
  "Cancel the inline completion and executes whatever event was received"
  (interactive (list last-input-event current-prefix-arg))

  (lsp-inline-completion-cancel)

  (let ((command (lookup-key (current-active-maps) (vector event)))
        (current-prefix-arg arg))

    (when (commandp command)
      (call-interactively command))))

(defun lsp-inline-completion-next ()
  "Display the next inline completion"
  (interactive)
  (unless (lsp-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-inline-completion--current
        (mod (1+ lsp-inline-completion--current)
             (length lsp-inline-completion--items)))

  (lsp-inline-completion-show-overlay))

(defun lsp-inline-completion-prev ()
  "Display the previous inline completion"
  (interactive)
  (unless (lsp-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-inline-completion--current
        (mod (1- lsp-inline-completion--current)
             (length lsp-inline-completion--items)))

  (lsp-inline-completion-show-overlay))

;;;###autoload
(defun lsp-inline-completion-display (&optional implicit)
  "Displays the inline completions overlay"
  (interactive)

  (lsp--spinner-start)
  (unwind-protect
      (if-let* ((resp (lsp-request-while-no-input "textDocument/inlineCompletion"
                                                  (lsp-inline-completion--trigger-kind implicit)))
                (items (lsp-inline-completion-parse-items resp)))

          (progn
            (lsp-inline-completion--clear-overlay)
            (setq lsp-inline-completion--items items)
            (setq lsp-inline-completion--current 0)
            (setq lsp-inline-completion--start-point (point))
            (lsp-inline-completion-show-overlay))
        (unless implicit
          (message "No Suggestions!")))
    ;; Clean up
    (lsp--spinner-stop)))

(provide 'lsp-inline-completion)
