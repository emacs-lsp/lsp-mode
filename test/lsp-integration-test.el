;;; lsp-integration-test.el --- lsp integration tests   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'deferred)
(require 'dash)
(require 'ert)
(require 'f)

(require 'lsp-mode)
(require 'lsp-modeline)
(require 'lsp-completion)
(require 'lsp-diagnostics)
(require 'lsp-pyls)

(defconst lsp-test-location (file-name-directory (or load-file-name buffer-file-name)))

(defun lsp-test--wait-for (form &optional d)
  (--doto (or d (deferred:new #'identity))
    (run-with-timer
     0.001 nil
     (lambda ()
       (if-let ((result (eval form)))
           (deferred:callback-post it result)
         (lsp-test--wait-for form it))))))

(defmacro lsp-test-wait (form)
  `(lsp-test--wait-for '(progn ,form)))

(defun lsp-def-request-async (method params &rest args)
  (--doto (deferred:new #'identity)
    (apply #'lsp-request-async method params (-partial #'deferred:callback-post it)
           :error-handler (-partial #'deferred:errorback-post it)
           args)))

(ert-deftest lsp-text-document-hover-request ()
  (lsp-workspace-folders-add (f-join lsp-test-location "fixtures"))
  (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
  (lsp)
  (-> (lsp-test-wait
       (eq 'initialized (lsp--workspace-status
                         (cl-first (lsp-workspaces)))))
      (deferred:nextc (lambda (_)
                        (goto-char (point-min))
                        (search-forward "fn1")
                        (lsp-def-request-async "textDocument/hover"
                                               (lsp--text-document-position-params)) ))
      (deferred:nextc (lambda (contents)
                        (should (lsp-hover? contents))))
      (deferred:sync!))
  (kill-buffer)
  (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures")))

(defmacro deferred::nextc (d &rest body)
  `(deferred:nextc ,d (lambda (result)
                        ,@body)))

(defmacro lsp-with-pyls (&rest body)
  `(progn
     (lsp-workspace-folders-add (f-join lsp-test-location "fixtures"))
     (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
     (lsp)
     ,@body

     (find-file (f-join lsp-test-location "fixtures/pyls/test.py"))
     (save-buffer)
     (kill-buffer)
     (lsp-workspace-folders-remove (f-join lsp-test-location "fixtures"))))

(ert-deftest lsp-text-document-hover-request-tick ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'tick)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (insert "x")
           (delete-char -1)))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(ert-deftest lsp-test-current-buffer-mode ()
  (lsp-with-pyls
   (-> (lsp-test-wait
        (eq 'initialized (lsp--workspace-status
                          (cl-first (lsp-workspaces)))))
       (deferred::nextc
         (goto-char (point-min))
         (search-forward "fn1")
         (prog1 (deferred:earlier
                  (lsp-def-request-async "textDocument/hover"
                                         (lsp--text-document-position-params)
                                         :mode 'current)
                  (deferred::nextc (deferred:wait 1000) :timeout))
           (switch-to-buffer "*scratch*")
           (let ((post-command-hook (->> (f-join lsp-test-location "fixtures/pyls/test.py")
                                         find-buffer-visiting
                                         (buffer-local-value 'post-command-hook))))
             (run-hooks 'post-command-hook))))
       (deferred::nextc (should (equal result :timeout)))
       (deferred:sync!))))

(ert-deftest lsp-org-position-translation-test ()
  :tags '(org)
  (with-current-buffer (find-file-noselect "test/fixtures/org-mode/demo.org")
    (org-mode)
    (goto-char (point-min))

    (search-forward "def external_function(s):")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)
    (deferred:sync!
      (-> (lsp-test-wait
           (eq 'initialized (lsp--workspace-status
                             (cl-first (lsp-workspaces)))))
          (deferred::nextc
            (should (equal (lsp--buffer-content)
                           (f-read-text "org_demo_file_2.py"))))
          (deferred::nextc
            (search-backward "def")

            (should (equal (lsp--point-to-position (point))
                           '(:line 0 :character 0)))

            (should (equal (lsp--position-to-point (lsp-make-position
                                                    :line 0
                                                    :character 0))
                           (point))))
          (deferred:try
            :finally (lambda (&rest _)
                       (lsp-virtual-buffer-disconnect lsp--virtual-buffer)))))))

(defun lsp-notify-wrapper (params)
  (let ((lsp--virtual-buffer-mappings (ht)))
    (pcase (plist-get params :method)
      (`"textDocument/didChange"
       (setq my/params params)
       (-let [(&plist :params
                      (&plist :textDocument (&plist :uri :version)
                              :contentChanges [(&plist :range (&plist :start :end )
                                                       :text)]))
              params]
         (with-current-buffer (get-buffer-create (format "*%s*" (f-filename (lsp--uri-to-path uri))))
           (let ((start-point (if start
                                  (lsp--position-to-point (lsp-make-position :line (plist-get start :line)
                                                                             :character (plist-get start :character)))
                                (point-min)))
                 (end-point (if end
                                (lsp--position-to-point (lsp-make-position :line (plist-get end :line)
                                                                           :character (plist-get end :character)))
                              (point-max))))
             (display-buffer-in-side-window (current-buffer) ())
             (delete-region start-point end-point)
             (goto-char start-point)
             (insert text)))))
      (`"textDocument/didOpen"
       (-let [(&plist :params (&plist :textDocument
                                      (&plist :uri
                                              :version
                                              :text)))
              params]
         (with-current-buffer (get-buffer-create (format "*%s*" (f-filename (lsp--uri-to-path uri))))
           (display-buffer-in-side-window (current-buffer) ())

           (delete-region (point-min) (point-max))
           (insert (or text ""))))))))

(ert-deftest lsp-virtual-position-to-point ()
  :tags '(org)
  (with-current-buffer (find-file-noselect "fixtures/org-mode/demo.org")
    (goto-char (point-min))
    (search-forward "def external_function(s):")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)
    (deferred:sync!
      (-> (lsp-test-wait
           (eq 'initialized (lsp--workspace-status
                             (cl-first (lsp-workspaces)))))
          (deferred::nextc
            (should (equal (lsp--buffer-content)
                           (f-read-text "org_demo_file_2.py"))))
          (deferred::nextc
            (search-backward "def")

            (should (equal (lsp--point-to-position (point))
                           '(:line 0 :character 0)))

            (should (equal (lsp--position-to-point (ht ("line" 0)
                                                       ("character" 0)))
                           (point)))

            (forward-line 1))
          (deferred:try
            :finally (lambda (&rest _)
                       (lsp-virtual-buffer-disconnect lsp--virtual-buffer)))))))

(ert-deftest lsp-transformation-of-did-change-events ()
  :tags '(org)
  (with-current-buffer (find-file-noselect "fixtures/org-mode/demo.org")
    (advice-add 'lsp--send-notification :before 'lsp-notify-wrapper)
    (goto-char (point-min))
    (search-forward "def external_function(s):")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)

    (setq lsp-diagnostics-provider nil)

    (-> (lsp-test-wait
         (eq 'initialized (lsp--workspace-status
                           (cl-first (lsp-workspaces)))))

        (deferred::nextc




          )
        (deferred:try
          :finally (lambda (&rest _)
                     (lsp-virtual-buffer-disconnect lsp--virtual-buffer)))
        (deferred:sync!))
    (advice-remove 'lsp--send-notification 'lsp-notify-wrapper)))

(ert-deftest lsp-org-transformation-of-did-change-events-1 ()
  :tags '(org)
  (with-current-buffer (find-file-noselect "fixtures/org-mode/demo.org")
    (advice-add 'lsp--send-notification :before 'lsp-notify-wrapper)
    (goto-char (point-min))
    (search-forward "def external_function(s):")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)

    (setq lsp-diagnostics-provider nil)

    (-> (lsp-test-wait
         (eq 'initialized (lsp--workspace-status
                           (cl-first (lsp-workspaces)))))
        (deferred::nextc
          (save-excursion
            (unwind-protect
                (progn
                  (goto-char (point-at-eol))
                  (forward-line 2)
                  (forward-line)
                  (insert "\n")
                  (should (equal (lsp--buffer-content)
                                 (with-current-buffer "*org_demo_file_2.py*"
                                   (buffer-string)))))
              (delete-region (point) (- (point) 1))
              (should (equal (lsp--buffer-content)
                             (with-current-buffer "*org_demo_file_2.py*"
                               (buffer-string))))))

          ;;         (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (insert "
          ;;                       boo
          ;;           ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) (length "
          ;;                       boo
          ;;           ")))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (unwind-protect
          ;;               (progn
          ;;                 (goto-char (point-at-eol))
          ;;                 (let ((start-point (point)))
          ;;                   (forward-line 3)
          ;;                   (goto-char (point-at-bol))
          ;;                   (forward-char 4)
          ;;                   (kill-region start-point (point))
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;                 (should (equal (lsp--buffer-content)
          ;;                                (with-current-buffer "*org_demo_file_2.py*"
          ;;                                  (buffer-string)))))
          ;;             (yank)
          ;;             (should (equal (lsp--buffer-content)
          ;;                            (with-current-buffer "*org_demo_file_2.py*"
          ;;                              (buffer-string)))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (forward-line 2)
          ;;                   (insert "r
          ;; ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) (length "r
          ;; ")))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (let ((start-point (point)))
          ;;                     (forward-line 3)
          ;;                     (goto-char (point-at-bol))
          ;;                     (forward-char 4)
          ;;                     (kill-region start-point (point))
          ;;                     (should (equal (lsp--buffer-content)
          ;;                                    (with-current-buffer "*org_demo_file_2.py*"
          ;;                                      (buffer-string)))))
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (yank)
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (forward-line 2)
          ;;                   (insert "  ")
          ;;                   (insert " ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) 3))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (+ (point-at-bol) 4))
          ;;                   (delete-region (point) (1+ (point)))
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (insert "e")
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (insert "
          ;;            ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) (length "
          ;;            ")))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (insert "
          ;;                      boo
          ;;                    ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) (length "
          ;;                      boo
          ;;                    ")))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (forward-line 2)
          ;;                   (forward-line)
          ;;                   (insert " ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (1+ (point)))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (forward-line 2)
          ;;                   (forward-line)
          ;;                   (goto-char (point-at-bol))
          ;;                   (insert "   x")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) 4))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (forward-line 2)
          ;;                   (forward-line)
          ;;                   (insert "  ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) 2))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (forward-line 2)
          ;;                   (forward-line)
          ;;                   (insert "   ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) 3))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (forward-line 2)
          ;;                   (forward-line)
          ;;                   (insert "    ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) 4))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (insert "
          ;;           ")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (- (point) (length "
          ;;           ")))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-eol))
          ;;                   (insert "\n")
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (delete-region (point) (1- (point)))
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))

          ;;           (unwind-protect
          ;;               (progn
          ;;                 (goto-char (+ (point-at-bol) 3))
          ;;                 (delete-region (point) (1+ (point)))
          ;;                 (should (equal (lsp--buffer-content)
          ;;                                (with-current-buffer "*org_demo_file_2.py*"
          ;;                                  (buffer-string)))))
          ;;             (insert "d")
          ;;             (should (equal (lsp--buffer-content)
          ;;                            (with-current-buffer "*org_demo_file_2.py*"
          ;;                              (buffer-string)))))


          ;;           ;; delete indentation + from the original buffer
          ;;           (unwind-protect
          ;;               (progn
          ;;                 (goto-char (+ (point-at-bol) 2))
          ;;                 (delete-region (point) (+ (point) 2))
          ;;                 (should (equal (lsp--buffer-content)
          ;;                                (with-current-buffer "*org_demo_file_2.py*"
          ;;                                  (buffer-string)))))
          ;;             (insert " d")
          ;;             (should (equal (lsp--buffer-content)
          ;;                            (with-current-buffer "*org_demo_file_2.py*"
          ;;                              (buffer-string)))))


          ;;           ;; delete indentation

          ;;           (unwind-protect
          ;;               (progn
          ;;                 (goto-char (point-at-bol))
          ;;                 (delete-region (point) (1+ (point)))

          ;;                 (should (equal (lsp--buffer-content)
          ;;                                (with-current-buffer "*org_demo_file_2.py*"
          ;;                                  (buffer-string)))))
          ;;             (insert " ")
          ;;             (should (equal (lsp--buffer-content)
          ;;                            (with-current-buffer "*org_demo_file_2.py*"
          ;;                              (buffer-string)))))



          ;;           ;; delete 2 chars from indentation
          ;;           (unwind-protect
          ;;               (progn
          ;;                 (goto-char (point-at-bol))
          ;;                 (delete-region (point) (+ (point) 2))

          ;;                 (should (equal (lsp--buffer-content)
          ;;                                (with-current-buffer "*org_demo_file_2.py*"
          ;;                                  (buffer-string)))))
          ;;             (insert "  ")
          ;;             (should (equal (lsp--buffer-content)
          ;;                            (with-current-buffer "*org_demo_file_2.py*"
          ;;                              (buffer-string)))))

          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-bol))
          ;;                   (delete-region (point) (+ (point) 2))

          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (insert "  ")))
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (point-at-bol))
          ;;                   (insert "  ")

          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (goto-char (point-at-bol))
          ;;               (delete-region (point) (+ (point) 2)))
          ;;             )
          ;;           (save-excursion
          ;;             (unwind-protect
          ;;                 (progn
          ;;                   (goto-char (+ (point-at-bol) 4))
          ;;                   (delete-region (point) (1+ (point)))
          ;;                   (should (equal (lsp--buffer-content)
          ;;                                  (with-current-buffer "*org_demo_file_2.py*"
          ;;                                    (buffer-string)))))
          ;;               (insert "e")
          ;;               (should (equal (lsp--buffer-content)
          ;;                              (with-current-buffer "*org_demo_file_2.py*"
          ;;                                (buffer-string))))))
          )
        (deferred:try
          :finally (lambda (&rest _)
                     (lsp-virtual-buffer-disconnect lsp--virtual-buffer)
                     (advice-remove 'lsp--send-notification 'lsp-notify-wrapper)))
        (deferred:sync!))))

(ert-deftest lsp-org-transformation-of-did-change-events-2 ()
  :tags '(org)
  (with-current-buffer (find-file-noselect "fixtures/org-mode/demo.org")
    (advice-add 'lsp--send-notification :before 'lsp-notify-wrapper)
    (goto-char (point-min))
    (search-forward "def external_function(s):")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)

    (setq lsp-diagnostics-provider nil)

    (-> (lsp-test-wait
         (eq 'initialized (lsp--workspace-status
                           (cl-first (lsp-workspaces)))))

        (deferred::nextc
          (unwind-protect
              (progn
                (delete-char 1)
                (should (equal (lsp--buffer-content)
                               (with-current-buffer "*org_demo_file_2.py*"
                                 (buffer-string)))))
            (insert "\n")
            (should (equal (lsp--buffer-content)
                           (with-current-buffer "*org_demo_file_2.py*"
                             (buffer-string))))))

        (deferred:try
          :finally (lambda (&rest _)
                     (lsp-virtual-buffer-disconnect lsp--virtual-buffer)))
        (deferred:sync!))
    (advice-remove 'lsp--send-notification 'lsp-notify-wrapper)))

(ert-deftest lsp-org-test-current-org-mode-content ()
  :tags '(no-win)
  (with-current-buffer (find-file-noselect "fixtures/org-mode/demo.org")
    (goto-char (point-min))
    (search-forward "import org_demo_file_2")
    (lsp-workspace-folders-add (f-expand "fixtures/org-mode"))
    (lsp-org)
    (-> (lsp-test-wait
         (eq 'initialized (lsp--workspace-status
                           (cl-first (lsp-workspaces)))))
        (deferred::nextc
          (should (equal (lsp--buffer-content)
                         (f-read-text "org_demo_file.py"))))
        (deferred::nextc
          (search-forward "foobar = 10")

          (goto-char (point-at-bol))
          (save-excursion
            (lsp-rename "barfoo"))

          (should (equal (lsp--buffer-content)
                         (s-replace "foobar"
                                    "barfoo"
                                    (f-read-text "org_demo_file.py"))))
          (goto-char (point-min))
          (while (search-forward "barfoo" nil t)
            (replace-match "foobar")))
        (deferred:sync!))))

(provide 'lsp-integration-test)
;;; lsp-integration-test.el ends here
