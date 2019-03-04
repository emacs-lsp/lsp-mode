;;; lsp-mode-steps.el --- LSP mode   steps                           -*- lexical-binding: t; -*-

;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(setq lexical-binding t)

(require 'xref)
(defvar lsp-mode-steps-xref-index nil)

(defun lsp-steps-xref-goto-xref-item (xref-item func)
  "Set buffer and point according to xref-item XREF-ITEM.

Use FUNC to display buffer."
  (with-slots (summary location) xref-item
    (let* ((marker (xref-location-marker location))
           (buf (marker-buffer marker))
           (offset (marker-position marker)))
      (with-current-buffer buf
        (goto-char offset)
        (funcall func buf)))))

(setq xref-show-xrefs-function (lambda (xrefs _)
                                 (lsp-steps-xref-goto-xref-item (seq-elt xrefs lsp-mode-steps-xref-index) 'switch-to-buffer)))

(defun lsp-mode-steps--async-call (pred  callback)
  "Call CALLBACK when PRED becomes true."
  (let (timer
        (retry-count 0))
    (setq timer (run-with-timer
                 1
                 1
                 (lambda ()
                   (if (funcall pred)
                       (progn
                         (cancel-timer timer)
                         (funcall callback))
                     (setq retry-count (1+ retry-count))
                     (message "The function failed, attempt %s" retry-count)))))))

(defmacro lsp-mode-steps-async  (form callback)
  "Call CALLBACK when FORM becomes true."
  `(lsp-mode-steps--async-call (lambda () ,form) ,callback))

(Given "^I have workspace folder \"\\(.+\\)\"$"
       (lambda (folder)
         (lsp-workspace-folders-add (f-join lsp-mode-support-path "projects" folder))))

(And "^I open file \"\\([^\"]+\\)\"$"
     (lambda (file)
       (find-file (f-join lsp-mode-support-path "projects" file))))

(And "^I log current line"
     (lambda ()
       (message "Placed on line  %s: %s (point = %s)" (line-number-at-pos) (buffer-substring (point-at-bol)
                                                                                             (point-at-eol))
                (point))))


(Then "^the \"\\([^\"]+\\)\" status will become \"\\([^\"]+\\)\"$"
      (lambda (server-id status callback)
        (lsp-mode-steps-async
         (eql (lsp--workspace-status (lsp-find-workspace (intern server-id) nil))
              (intern status))
         callback)))

(When "^I select item \"\\([^\"]+\\)\" from the next xref call$"
      (lambda (arg)
        (setq lsp-mode-steps-xref-index (1- (string-to-number arg)))))

(When "^I goto imenu \"\\([^\"]+\\)\" -> \"\\([^\"]+\\)\"$"
      (lambda (type element)
        (->> imenu-create-index-function
             funcall
             (assoc "Function")
             cl-rest
             (assoc "fn1")
             cl-rest
             goto-char)))
