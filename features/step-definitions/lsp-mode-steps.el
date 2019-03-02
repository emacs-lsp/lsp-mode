;;; lsp-mode-steps.el --- LSP mode   steps                           -*- lexical-binding: t; -*-

;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(setq lexical-binding t)

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
       (find-file (f-join lsp-mode-support-path "projects" file))
       ))


(Then "^the \"\\([^\"]+\\)\" status will become \"\\([^\"]+\\)\"$"
      (lambda (server-id status callback)
        ;; (message "The function failed, attempt %s %s" server-id status)
        (lsp-mode-steps-async
         (eql (lsp--workspace-status (lsp-find-workspace (intern server-id) nil))
              (intern status))
         callback)))
