;;; lsp-sqls.el --- SQL Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Shunya Ishii

;; Author: Shunya Ishii
;; Keywords: sql lsp

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

;; LSP client for SQL

;;; Code:

(require 'lsp-mode)

(defgroup lsp-sqls nil
  "LSP support for SQL, using sqls"
  :group 'lsp-mode
  :link '(url-link "https://github.com/lighttiger2505/sqls")
  :package-version `(lsp-mode . "7.0"))

(defcustom lsp-sqls-server "sqls"
  "Path to the `sqls` binary."
  :group 'lsp-sqls
  :risky t
  :type 'file
  :package-version `(lsp-mode . "7.0"))

(defcustom lsp-sqls-workspace-config-path "workspace"
  "If non-nil then setup workspace configuration with json file path."
  :group 'lsp-sqls
  :risky t
  :type '(choice (const "workspace")
                 (const "root"))
  :package-version `(lsp-mode . "7.0"))

(defun lsp-sqls--make-launch-cmd ()
  (-let [base `(,lsp-sqls-server)]
    ;; we can add some options to command. (e.g. "-config")
    base))


(defcustom lsp-sqls-timeout 0.5
  "Timeout to use for `sqls' requests."
  :type 'number
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-sqls-connections nil
  "The connections to the SQL server(s)."
  :type '(repeat (alist :key-type (choice
                                   (const :tag "Driver" driver)
                                   (const :tag "Connection String" dataSourceName))
                        :value-type string)))

(defun lsp-sqls-setup-workspace-configuration ()
  "Setup workspace configuration using json file depending on `lsp-sqls-workspace-config-path'."

  (if lsp-sqls-connections
      (lsp--set-configuration `(:sqls (:connections ,(apply #'vector lsp-sqls-connections))))
    (when-let ((config-json-path (cond
                                  ((equal lsp-sqls-workspace-config-path "workspace")
                                   ".sqls/config.json")
                                  ((equal lsp-sqls-workspace-config-path "root")
                                   (-> (lsp-workspace-root)
                                       (f-join ".sqls/config.json"))))))
      (when (file-exists-p config-json-path)
        (lsp--set-configuration (lsp--read-json-file config-json-path))))))

(defun lsp-sqls--show-results (result)
  (with-current-buffer (get-buffer-create "*sqls results*")
    (with-help-window (buffer-name)
      (erase-buffer)
      (insert result))))

(defun lsp-sql-execute-query (&optional command start end)
  "Execute COMMAND on buffer text against current database.
Buffer text is between START and END.  If START and END are nil,
use the current region if set, otherwise the entire buffer."
  (interactive)
  (lsp-sqls--show-results
   (lsp-request
    "workspace/executeCommand"
    (list :command "executeQuery"
          :arguments (or
                      (when command
                        (lsp:command-arguments? command))
                      (vector (lsp--buffer-uri)))
          :timeout lsp-sqls-timeout
          :range (list
                  :start (lsp--point-to-position
                          (cond
                           (start start)
                           ((use-region-p) (region-beginning))
                            (t (point-min))))
                  :end (lsp--point-to-position
                        (cond
                         (end end)
                         ((use-region-p) (region-end))
                         (t (point-max)))))))))

(defun lsp-sql-execute-paragraph (&optional command)
  "Execute COMMAND on paragraph against current database."
  (interactive)
  (let ((start (save-excursion (backward-paragraph) (point)))
        (end (save-excursion (forward-paragraph) (point))))
    (lsp-sql-execute-query command start end)))

(defun lsp-sql-show-databases (&optional _command)
  "Show databases."
  (interactive)
  (lsp-sqls--show-results
   (lsp-request
    "workspace/executeCommand"
    (list :command "showDatabases" :timeout lsp-sqls-timeout))))

(defun lsp-sql-show-schemas (&optional _command)
  "Show schemas."
  (interactive)
  (lsp-sqls--show-results
   (lsp-request
    "workspace/executeCommand"
    (list :command "showSchemas" :timeout lsp-sqls-timeout))))

(defun lsp-sql-show-connections (&optional _command)
  "Show connections."
  (interactive)
  (lsp-sqls--show-results
   (lsp-request
    "workspace/executeCommand"
    (list :command "showConnections" :timeout lsp-sqls-timeout))))

(defun lsp-sql-switch-database (&optional _command)
  "Switch database."
  (interactive)
  (lsp-workspace-command-execute
   "switchDatabase"
   (vector (completing-read
            "Select database: "
            (s-lines (lsp-workspace-command-execute "showDatabases"))
            nil
            t))))

(defun lsp-sql-switch-connection (&optional _command)
  "Switch connection."
  (interactive)
  (lsp-workspace-command-execute
   "switchConnections"
   (vector (cl-first
            (s-match "\\([[:digit:]]*\\)"
                     (completing-read
                      "Select connection: "
                      (s-lines (lsp-workspace-command-execute  "showConnections"))
                      nil
                      t))))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-sqls--make-launch-cmd)
                  :major-modes '(sql-mode)
                  :priority -1
                  :action-handlers (ht ("executeQuery" #'lsp-sql-execute-query)
                                       ("showDatabases" #'lsp-sql-show-databases)
                                       ("showSchemas" #'lsp-sql-show-schemas)
                                       ("showConnections" #'lsp-sql-show-connections)
                                       ("switchDatabase" #'lsp-sql-switch-database)
                                       ("switchConnections" #'lsp-sql-switch-connection))
                  :server-id 'sqls
                  :initialized-fn (lambda (workspace)
                                    (-> workspace
                                        (lsp--workspace-server-capabilities)
                                        (lsp:set-server-capabilities-execute-command-provider? t))
                                    (with-lsp-workspace workspace
                                      (lsp-sqls-setup-workspace-configuration)))))

(lsp-consistency-check lsp-sqls)

(provide 'lsp-sqls)
;;; lsp-sqls.el ends here
