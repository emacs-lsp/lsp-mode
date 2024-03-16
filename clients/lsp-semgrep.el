;;; lsp-semgrep.el --- semgrep support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Austin Theriault
;;
;; Author: Austin Theriault <austin@cutedogs.org>
;; Keywords: language tools sast
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Semgrep support for lsp-mode
;;
;;; Code:



(require 'lsp-mode)

(defgroup lsp-semgrep nil
  "LSP support for Semgrep."
  :group 'lsp-mode
  :link `(url-link "https://github.com/returntocorp/semgrep"))

(defgroup lsp-semgrep-scan nil
  "Semgrep LS scan options."
  :group 'lsp-semgrep
  )

(defgroup lsp-semgrep-metrics nil
  "Semgrep LS metrics options."
  :group 'lsp-semgrep)

;; General options

(defcustom lsp-semgrep-trace-server "off"
  "Trace Semgrep LS server"
  :group 'lsp-semgrep
  :type '(choice (const "off")
          (const "messages")
          (const "verbose")))

(defcustom lsp-semgrep-server-command '("semgrep" "lsp")
  "Semgrep LS server command."
  :group 'lsp-semgrep
  :type '(repeat string))

(defcustom lsp-semgrep-languages '("apex"
                                  "bash"
                                  "sh"
                                  "c"
                                  "cairo"
                                  "clojure"
                                  "cpp"
                                  "c++"
                                  "csharp"
                                  "c#"
                                  "dart"
                                  "dockerfile"
                                  "docker"
                                  "ex"
                                  "elixir"
                                  "go"
                                  "golang"
                                  "hack"
                                  "html"
                                  "java"
                                  "js"
                                  "javascript"
                                  "json"
                                  "jsonnet"
                                  "julia"
                                  "kt"
                                  "kotlin"
                                  "lisp"
                                  "lua"
                                  "ocaml"
                                  "php"
                                  "python2"
                                  "python3"
                                  "py"
                                  "python"
                                  "r"
                                  "regex"
                                  "none"
                                  "ruby"
                                  "rust"
                                  "scala"
                                  "scheme"
                                  "solidity"
                                  "sol"
                                  "swift"
                                  "tf"
                                  "hcl"
                                  "terraform"
                                  "ts"
                                  "typescript"
                                  "vue"
                                  "xml"
                                  "yaml")
  "List of languages to enable Semgrep LS for."
  :group 'lsp-semgrep
  :type '(repeat string))
;; Scan options

(defcustom lsp-semgrep-scan-configuration []
  "Semgrep rule files, or registry rules to scan with, e.g. ['r/all','rules.yaml']."
  :group 'lsp-semgrep-scan
  :type '(repeat string))

(defcustom lsp-semgrep-scan-exclude []
  "List of files or directories to exclude from scan."
  :group 'lsp-semgrep-scan
  :type '(repeat string))

(defcustom lsp-semgrep-scan-include []
  "List of files or directories to include in scan."
  :group 'lsp-semgrep-scan
  :type '(repeat string))

(defcustom lsp-semgrep-scan-jobs 1
  "Number of parallel jobs to run."
  :group 'lsp-semgrep-scan
  :type 'integer)

(defcustom lsp-semgrep-scan-max-memory 0
  "Maximum memory to use for scan, in MB."
  :group 'lsp-semgrep-scan
  :type 'integer)

(defcustom lsp-semgrep-scan-max-target-bytes 1000000
  "Maximum size of target file to scan, in bytes."
  :group 'lsp-semgrep-scan
  :type 'integer)

(defcustom lsp-semgrep-scan-timeout 30
  "Maximum time to wait for scan to complete, in seconds."
  :group 'lsp-semgrep-scan
  :type 'integer)

(defcustom lsp-semgrep-scan-timeout-threshold 30
  "Maximum time to wait for scan to complete, in seconds."
  :group 'lsp-semgrep-scan
  :type 'integer)

(defcustom lsp-semgrep-scan-only-git-dirty t
  "Only scan files that are dirty in git."
  :group 'lsp-semgrep-scan
  :type 'boolean)

;; Metrics options

(defcustom lsp-semgrep-metrics-enabled t
  "Enable metrics collection."
  :group 'lsp-semgrep-metrics
  :type 'boolean)

(defcustom lsp-semgrep-metrics-extension-type "emacs"
  "Extension host type."
  :group 'lsp-semgrep-metrics
  :type 'string)

;; Custom commands

(defun semgrep-scan-workspace (full)
  "Scan workspace with Semgrep.
If FULL is non-nil, scan all files in workspace, regardless of git status."
  (interactive (list (lsp--completing-read "Scan: " (list "Changed files in workspace" "All files in workspace") 'identity)))
  (lsp-notify "semgrep/scanWorkspace" (list :full (if (string= full "All files in workspace") t :json-false))))

(defun semgrep-refresh-rules ()
  "Refresh Semgrep rules."
  (interactive)
  (lsp-notify "semgrep/refreshRules" lsp--empty-ht))


(defun semgrep-login ()
  "Login to Semgrep."
  (interactive)
  (lsp-request-async "semgrep/login" lsp--empty-ht
                     (lambda (result)
                       (list
                        (browse-url (lsp-get result :url))
                        (lsp-message "Please login to Semgrep and return to Emacs.")
                        (lsp-notify "semgrep/loginFinish" result)))))

(defun semgrep-logout ()
  "Logout from Semgrep."
  (interactive)
  (lsp-notify "semgrep/logout" lsp--empty-ht))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-semgrep-server-command))
  :activation-fn (lambda (_file-name _mode)
                   (-contains? lsp-semgrep-languages (lsp-buffer-language)))
  :server-id 'semgrep-ls
  :priority -1
  :add-on? t
  :initialization-options
  (lambda ()
    (list
     :scan (list
            :configuration lsp-semgrep-scan-configuration
            :exclude lsp-semgrep-scan-exclude
            :include lsp-semgrep-scan-include
            :jobs lsp-semgrep-scan-jobs
            :maxMemory lsp-semgrep-scan-max-memory
            :maxTargetBytes lsp-semgrep-scan-max-target-bytes
            :timeout lsp-semgrep-scan-timeout
            :timeoutThreshold lsp-semgrep-scan-timeout-threshold
            :onlyGitDirty lsp-semgrep-scan-only-git-dirty)
     :metrics (list
               :enabled lsp-semgrep-metrics-enabled
               :extensionType lsp-semgrep-metrics-extension-type)
     :trace (list
             :server lsp-semgrep-trace-server)))))

(lsp-consistency-check lsp-semgrep)

(provide 'lsp-semgrep)
;;; lsp-semgrep.el ends here
