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

(defun lsp-sqls-setup-workspace-configuration ()
  "Setup workspace configuration using json file depending on `lsp-sqls-workspace-config-path'."
  (when-let ((config-json-path (cond
                                ((equal lsp-sqls-workspace-config-path "workspace")
                                 ".sqls/config.json")
                                ((equal lsp-sqls-workspace-config-path "root")
                                 (-> (lsp-workspace-root)
                                     (f-join ".sqls/config.json"))))))
    (when (file-exists-p config-json-path)
      (lsp--set-configuration (lsp--read-json-file config-json-path)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-sqls--make-launch-cmd)
                  :major-modes '(sql-mode)
                  :priority -1
                  :server-id 'sqls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp-sqls-setup-workspace-configuration)))))

(provide 'lsp-sqls)
;;; lsp-sqls.el ends here
