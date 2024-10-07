;;; .local/straight/repos/lsp-mode/clients/lsp-nextflow.el -*- lexical-binding: t; -*-


;; Copyright (C) 2024 Edmund Miller

;; Author: Edmund Miller
;; Keywords: lsp, nextflow, groovy

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

;; LSP Clients for the Nextflow Programming Language.

;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-nextflow nil
  "LSP support for nextflow, using nextflow-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nextflowLanguageServer/nextflow-language-server"))

(defcustom lsp-nextflow-server-file (f-join lsp-server-install-dir "nextflow-language-server-all.jar")
  "JAR file path for nextflow-language-server-all.jar."
  :group 'lsp-nextflow
  :risky t
  :type 'file)

(defun lsp-nextflow--lsp-command ()
  "Generate LSP startup command."
  `("java" "-jar" ,(expand-file-name lsp-nextflow-server-file)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-nextflow--lsp-command)
                  :major-modes '(nextflow-mode)
                  :priority -1
                  :server-id 'nextflow-ls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "nextflow"))))))

(lsp-consistency-check lsp-nextflow)

(provide 'lsp-nextflow)
;;; lsp-nextflow.el ends here
