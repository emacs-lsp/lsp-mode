;;; lsp-trunk.el --- trunk support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Trunk Technologies, Inc.
;;
;; Author: Tyler Jang <tyler@trunk.io>
;; Keywords: trunk, lsp, meta-linter
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
;;  Trunk support for lsp-mode
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-trunk nil
  "LSP support for Trunk."
  :group 'lsp-mode
  :link `(url-link "https://docs.trunk.io"))

(defcustom lsp-trunk-executable "trunk"
  "Path to the trunk executable"
  :group 'lsp-trunk
  :type 'string)

(defcustom lsp-trunk-args '("lsp-proxy")
  "Additional arguments to pass to the trunk startup"
  :group 'lsp-trunk
  :type '(repeat string))

(defun lsp-trunk-check-for-init (filename &optional _)
  "Check if the file exists in a workspace that has a .trunk/trunk.yaml"
  (when-let* ((dir (file-name-directory filename))
             (trunk-file ".trunk/trunk.yaml"))
    (locate-dominating-file dir trunk-file)))

(defun lsp-trunk-check-disable (command)
  "Disable a linter in your repo."
  (shell-command
   (concat lsp-trunk-executable " check disable "
           (mapconcat 'identity (gethash "arguments" command) " "))))

(defun lsp-trunk-check-enable (command)
  "Enable a linter in your repo."
  (shell-command
   (concat lsp-trunk-executable " check enable "
           (mapconcat 'identity (gethash "arguments" command) " "))))

(defun lsp-trunk-open-config (&optional _command)
  "Open the trunk config file."
  (find-file ".trunk/trunk.yaml"))

(lsp-register-client
 (make-lsp-client
  :activation-fn #'lsp-trunk-check-for-init
  :new-connection (lsp-stdio-connection 
                   (lambda () (append (list lsp-trunk-executable) lsp-trunk-args)))
  :server-id 'trunk-lsp
  :initialization-options (lambda ()
                            (list
                             :version "0.1.0"
                             :clientType "emacs"
                             :clientVersion (symbol-value 'emacs-version)))
  :notification-handlers (ht ("$/progress" #'ignore))
  :action-handlers (ht ("trunk.checkDisable" #'lsp-trunk-check-disable)
                       ("trunk.checkEnable" #'lsp-trunk-check-enable)
                       ("trunk.openConfigFile" #'lsp-trunk-open-config))
  :priority -2
  :add-on? t))

(lsp-consistency-check lsp-trunk)

(provide 'lsp-trunk)
;;; lsp-trunk.el ends here
