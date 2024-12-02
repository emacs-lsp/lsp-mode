;;; lsp-inline-completion.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 emacs-lsp maintainers

;; Author: Rodrigo Kassick
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (spinner "1.7.3"))
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

(defun lsp-inline-completion--params (implicit &optional identifier position)
  "Returns a InlineCompletionParams instance"
  (lsp-make-inline-completion-params
   :textDocument (or identifier (lsp--text-document-identifier))
   :position (or position (lsp--cur-position))
   :context (lsp-make-inline-completion-context
             :triggerKind (if implicit
                              lsp/inline-completion-trigger-automatic
                            lsp/inline-completion-trigger-invoked))))

(defun lsp-inline-completion--parse-items (response)
  "Parses the reponse from the server and returns a list of
InlineCompletionItem objects"

  (pcase response
    ;; Server responded with a completion list
    ((lsp-interface InlineCompletionList :items)
     (seq-into items 'list))

    ;; Server responded with a sequence of completion items
    ((pred (lambda (i)
             (and (sequencep i)
                  (lsp-inline-completion-item? (elt i 0)))))
     (seq-into response 'list))

    ;; A sequence means multiple server may have responded. Iterate over them and normalize
    ((pred sequencep)
     (let ((item-seq (cl-map 'list #'lsp-inline-completion--parse-items response)))
       (apply 'seq-concatenate `(list ,@item-seq))))))

;;;;;; Default UI -- overlay

(defvar lsp-inline-completion-active-map
  (let ((map (make-sparse-keymap)))
    ;; accept
    (define-key map (kbd "C-<return>") #'lsp-inline-completion-accept)
    (define-key map [mouse-1] #'lsp-inline-completion-accept-on-click)
    ;; navigate
    (define-key map (kbd "C-n") #'lsp-inline-completion-next)
    (define-key map (kbd "C-p") #'lsp-inline-completion-prev)
    ;; cancel
    (define-key map (kbd "C-g") #'lsp-inline-completion-cancel)
    (define-key map (kbd "<escape>") #'lsp-inline-completion-cancel)
    (define-key map (kbd "C-c C-k") #'lsp-inline-completion-cancel)
    ;; useful -- recenter without loosing the completion
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    ;; ignore
     (define-key map [down-mouse-1] #'ignore)
    (define-key map [up-mouse-1] #'ignore)
    (define-key map [mouse-movement] #'ignore)
    ;; Any event outside of the map, cancel and use it
    (define-key map [t] #'lsp-inline-completion-cancel-with-input)
    map)
  "Keymap active when showing inline code suggestions")

(defface lsp-inline-completion-overlay-face
  '((t :inherit shadow))
  "Face for the inline code suggestions overlay."
  :group 'lsp-mode)

;; Local Buffer State

(defvar-local lsp-inline-completion--items nil "The completions provided by the server")
(defvar-local lsp-inline-completion--current nil "The current suggestion to be displayed")
(defvar-local lsp-inline-completion--overlay nil "The overlay displaying code suggestions")
(defvar-local lsp-inline-completion--start-point nil "The point where the completion started")

(defcustom lsp-before-inline-completion-hook nil
  "Hooks run before starting code suggestions"
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-inline-completion-hook nil
  "Hooks executed after asking for code suggestions."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-accepted-hook nil
  "Hooks executed after accepting a code suggestion. The hooks receive the
text range that was updated by the completion"
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-cancelled-hook nil
  "Hooks executed after cancelling the completion UI"
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-before-show-hook nil
  "Hooks executed before showing a suggestion."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-shown-hook nil
  "Hooks executed after showing a suggestion."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-inline-completion-overlay-priority 9000
  "The priority of the overlay."
  :type '(choice (const :tag "No Priority" nil)
                 (integer :tag "Simple, Overriding Priority")
                 (cons :tag "Composite"
                       (choice (integer :tag "Primary")
                               (const :tag "Primary Unset" nil))
                       (integer :tag "Secondary")))
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
  (overlay-put lsp-inline-completion--overlay 'priority lsp-inline-completion-overlay-priority)

  lsp-inline-completion--overlay)


(defun lsp-inline-completion--show-keys ()
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

  (run-hooks 'lsp-inline-completion-before-show-hook)

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

    (lsp-inline-completion--show-keys)
    (run-hooks 'lsp-inline-completion-shown-hook)))

(defun lsp-inline-completion--insert-sugestion (text kind start end command?)
  (let* ((text-insert-start (or start lsp-inline-completion--start-point))
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
                           (-let (((&RangeToPoint :start :end) range?)) (cons start end)))))

    (with-no-warnings
      ;; Compiler does not believes this macro is defined
      (lsp-with-undo-amalgamate
        (lsp-inline-completion--insert-sugestion text kind start end command?)))))

(defun lsp-inline-completion-accept-on-click (event)
  (interactive "e")

  (lsp-inline-completion-accept)
  (-let (((col . row) (posn-actual-col-row (event-end event))))
    (move-to-window-line row)
    (beginning-of-line)
    (forward-char (- col
                     (if (bound-and-true-p display-line-numbers-mode)
                         (+ 2 (line-number-display-width))
                       0)))))

(defun lsp-inline-completion-cancel ()
  "Close the suggestion overlay"
  (interactive)
  (when (lsp-inline-completion--overlay-visible)

    (lsp-inline-completion--clear-overlay)

    (when lsp-inline-completion--start-point
      (goto-char lsp-inline-completion--start-point))

    (run-hooks 'lsp-inline-completion-cancelled-hook)))

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

  (unless implicit
    (lsp--spinner-start) )

  (unwind-protect
      (if-let* ((resp (lsp-request-while-no-input "textDocument/inlineCompletion"
                                                  (lsp-inline-completion--params implicit)))
                (items (lsp-inline-completion--parse-items resp)))

          (progn
            (lsp-inline-completion--clear-overlay)
            (setq lsp-inline-completion--items items)
            (setq lsp-inline-completion--current 0)
            (setq lsp-inline-completion--start-point (point))
            (lsp-inline-completion-show-overlay))
        (unless implicit
          (lsp--info "No Suggestions!")))
    ;; Clean up
    (unless implicit
      (lsp--spinner-stop))))


;; Inline Completion Mode
;;;###autoload
(defcustom lsp-inline-completion-enable t
  "If non-nil it will enable inline completions on idle."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-inline-completion-idle-delay 2
  "The number of seconds before trying to fetch inline completions, when
lsp-inline-completion-mode is active"
  :type 'number
  :group 'lsp-mode
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-inline-completion-inhibit-predicates nil
  "When a function of this list returns non nil, lsp-inline-completion-mode will not show the completion"
  :type '(repeat function)
  :group 'lsp-mode)

(defvar-local lsp-inline-completion--idle-timer nil
  "The idle timer used by lsp-inline-completion-mode")

;;;###autoload
(define-minor-mode lsp-inline-completion-mode
  "Mode automatically displaying inline completions."
  :lighter nil
  (cond
   ((and lsp-inline-completion-mode lsp--buffer-workspaces)
    (add-hook 'lsp-on-change-hook #'lsp-inline-completion--after-change nil t))
   (t
    (when lsp-inline-completion--idle-timer
      (cancel-timer lsp-inline-completion--idle-timer))

    (lsp-inline-completion-cancel)

    (remove-hook 'lsp-on-change-hook #'lsp-inline-completion--after-change t))))

(defun lsp-inline-completion--maybe-display (original-buffer original-point)
  ;; This is executed on an idle timer -- ensure state did not change before
  ;; displaying
  (when (and (buffer-live-p original-buffer)
             (eq (current-buffer) original-buffer)
             (eq (point) original-point)
             (--none? (funcall it) lsp-inline-completion-inhibit-predicates))
    (setq last-command this-command)
    (setq this-command 'lsp-inline-completion-display)
    (lsp-inline-completion-display 'implicit)))

(defun lsp-inline-completion--after-change (&rest _)
  ;; This function is in lsp-on-change-hooks, which is executed on a timer by
  ;; lsp-on-change. Do not assume that the buffer/window state has not been
  ;; modified in the meantime! Use the values in lsp--after-change-vals to
  ;; ensure this.

  (when lsp-inline-completion--idle-timer
    (cancel-timer lsp-inline-completion--idle-timer))

  (when (and lsp-inline-completion-mode lsp--buffer-workspaces)
    (let ((original-buffer (plist-get lsp--after-change-vals :buffer))
          (original-point (plist-get lsp--after-change-vals :point)))
      (setq lsp-inline-completion--idle-timer
            (run-with-idle-timer lsp-inline-completion-idle-delay
                                 nil
                                 #'lsp-inline-completion--maybe-display
                                 original-buffer
                                 original-point)))))

;;;###autoload
(add-hook 'lsp-configure-hook (lambda ()
                                (when (and lsp-inline-completion-enable
                                           (lsp-feature? "textDocument/inlineCompletion"))
                                  (lsp-inline-completion-mode))))

;; Company default integration

(declare-function company--active-p "ext:company")
(declare-function company-cancel "ext:company" (&optional result))
(declare-function company-manual-begin "ext:company")
(defvar company--begin-inhibit-commands)
(defcustom lsp-inline-completion-mode-inhibit-when-company-active t
  "If the inline completion mode should avoid calling completions when company is active"
  :type 'boolean
  :group 'lsp-mode)

(defvar-local lsp-inline-completion--showing-company nil "If company was active when the tooltip is shown")

(defun lsp-inline-completion--company-save-state-and-hide ()
  (setq lsp-inline-completion--showing-company
        (and (bound-and-true-p company-mode)
             (company--active-p)))

  (when lsp-inline-completion--showing-company
    (company-cancel)))

(defun lsp-inline-completion--company-restore-state ()
  (when lsp-inline-completion--showing-company
      (company-manual-begin))
  (setq lsp-inline-completion--showing-company nil))

(defun lsp-inline-completion--company-active-p ()
  (and (bound-and-true-p company-mode) (company--active-p)))

;;;###autoload
(define-minor-mode lsp-inline-completion-company-integration-mode
  "Minor mode to be used when company mode is active with lsp-inline-completion-mode"
  :lighter nil
  (cond
   ((and lsp-inline-completion-company-integration-mode lsp--buffer-workspaces (bound-and-true-p company-mode))
    (add-hook 'lsp-inline-completion-before-show-hook #'lsp-inline-completion--company-save-state-and-hide nil t)
    (add-hook 'lsp-inline-completion-cancelled-hook #'lsp-inline-completion--company-restore-state nil t)
    (unless (memq #'lsp-inline-completion-display company--begin-inhibit-commands)
      (setq-local company--begin-inhibit-commands
                  (cons #'lsp-inline-completion-display company--begin-inhibit-commands)))
    (when (and lsp-inline-completion-mode-inhibit-when-company-active
               (not (memq  #'lsp-inline-completion--company-active-p lsp-inline-completion-inhibit-predicates)))
      (setq-local lsp-inline-completion-inhibit-predicates
                  (cons #'lsp-inline-completion--company-active-p lsp-inline-completion-inhibit-predicates))))

   (t
    (remove-hook 'lsp-inline-completion-before-show-hook #'lsp-inline-completion--company-save-state-and-hide t)
    (remove-hook 'lsp-inline-completion-cancelled-hook #'lsp-inline-completion--company-save-state-and-hide t)
    (when (boundp 'company--begin-inhibit-commands)
      (setq-local company--begin-inhibit-commands (delq #'lsp-inline-completion-display company--begin-inhibit-commands)))
    (setq-local lsp-inline-completion-inhibit-predicates
          (delq #'lsp-inline-completion--company-active-p lsp-inline-completion-inhibit-predicates)))))

(provide 'lsp-inline-completion)
