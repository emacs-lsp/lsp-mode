;;; ng-lsp.el --- async lsp-mode           -*- lexical-binding: t; -*-

(defun lsp-emacsng-stdio-connection (&optional command test-command)
  (list :connect (lambda (filter sentinel name environment-fn)
                     (let ((final-command (lsp-resolve-final-function command)))
                       (unless ng-lsp-mode-pipe
                         (setq ng-lsp-mode-pipe
                               (lsp-make-connection (seq-first final-command)
                                                    (seq-rest final-command)
                                                    #'ng-lsp-handler))
                         (let ((proc ng-lsp-mode-pipe))
                           (lsp-json-config proc
                                            :object-type 'hash-table
                                            :false-object :json-false
                                            :ser-false-object nil
                                            :ser-null-object nil)
                           (cons proc proc)))))
        :test? (or
                test-command
                (lambda () (-> command lsp-resolve-final-function lsp-server-present?)))))

(defun ng-lsp-stdio-connection-advice (orig-fun &rest args)
  (apply 'lsp-emacsng-stdio-connection args))

(when (fboundp 'lsp-make-connection)
 (advice-add 'lsp-stdio-connection :around #'ng-lsp-stdio-connection-advice))

(provide 'ng-lsp)
