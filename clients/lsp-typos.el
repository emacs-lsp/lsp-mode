;;; lsp-typos.el --- LSP client for typos-lsp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thanh Vuong
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; Keywords: lsp, typos
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
;; LSP client for typos-lsp
;;
;; This client is disabled by default to avoid confusion as it will run
;; everywhere lsp mode is on if enabled. Users could use
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; to have it run per project.
;;
;; Consult https://github.com/tekumara/typos-lsp and
;; https://github.com/crate-ci/typos for more information.
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-typos nil
  "LSP support for typos source code spell checker."
  :group 'lsp-mode
  :link '(url-link "https://github.com/tekumara/typos-lsp"))

(defcustom lsp-typos-path "typos-lsp"
  "Path to typos-lsp executable."
  :group 'lsp-typos
  :type 'string)

(defcustom lsp-typos-enable nil
  "Enable or disable LSP Typos."
  :group 'lsp-typos
  :type 'boolean
  :safe #'booleanp)

(defcustom lsp-typos-config nil
  "Path to toml config file."
  :group 'lsp-typos
  :type '(choice (const :tag "None" nil)
                 (string :tag "Toml File Path") )
  :safe (lambda (it) (or (stringp it) (null it))))

(defcustom lsp-typos-diagnostic-severity "Warning"
  "Severity level for typos diagnostics.
Can be one of \"Error\",\"Warning\", \"Information\", or \"Hint\"."
  :group 'lsp-typos
  :type '(choice
          (const :tag "Error" "Error")
          (const :tag "Warning" "Warning")
          (const :tag "Information" "Information")
          (const :tag "Hint" "Hint"))
  :safe #'stringp)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-typos-path)
  :activation-fn (lambda (_file-name _mode) lsp-typos-enable)
  :initialization-options (lambda ()
                            (append (when lsp-typos-config
                                      `(:config ,lsp-typos-config))
                                    `(:diagnosticSeverity ,lsp-typos-diagnostic-severity)))
  :priority -1
  :add-on? t
  :multi-root t
  :server-id 'typos-lsp))

(provide 'lsp-typos)
;;; lsp-typos.el ends here
