;;; lsp-galaxy.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, galaxy

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

;; LSP Clients for the GalaxyProject

;;; Code:
(require 'lsp-mode)

(defgroup lsp-galaxy nil
  "LSP support for Galaxy"
  :group 'lsp-mode
  :link '(url-link "https://github.com/galaxyproject/galaxy-language-server/")
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-galaxy-galaxy-cmd '["python" "-m" "galaxyls"]
  "Path to binary used in Galaxy Language Server.  Defaults to `python -m galaxyls' if nil."
  :type 'lsp-string-vector
  :group 'lsp-galaxy
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-galaxy-language-server-host "127.0.0.1"
  "Choose host address."
  :type 'string
  :group 'lsp-galaxy
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-galaxy-language-server-port 2087
  "Choose listen port."
  :type 'integer
  :group 'lsp-galaxy
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-galaxy-language-server-client-version "2.1.0"
  "Choose client version."
  :type 'string
  :group 'lsp-galaxy
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-galaxy-file-filter '[".xml"]
  "A vector of directories filtering galaxy file."
  :type 'lsp-string-vector
  :group 'lsp-galaxy
  :package-version '(lsp-mode . "7.1.0"))


(lsp-register-custom-settings
 '(("galaxy.galaxyCmd" lsp-galaxy-galaxy-cmd)
   ("galaxy.fileFilter" lsp-galaxy-file-filter)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (list lsp-galaxy-galaxy-cmd "--tcp"
                           (format "--host %s --port %d"
                                   lsp-galaxy-language-server-host
                                   lsp-galaxy-language-server-port))))
  :major-modes '(planemo-mode)
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "galaxy"))))
  :priority -1
  :server-id 'galaxyls))

(provide 'lsp-galaxy)
;;; lsp-galaxy.el ends here
