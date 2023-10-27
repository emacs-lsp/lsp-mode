;;; lsp-openscad.el --- openscad client         -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Len Trigg

;; Author: Len Trigg
;; Keywords: openscad lsp

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

;; lsp-openscad client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-openscad nil
  "LSP support for openscad."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Leathong/openscad-LSP"))

(defcustom lsp-openscad-server
  "openscad-lsp"
  "Path to the openscad language server."
  :group 'lsp-openscad
  :risky t
  :type 'file)

(defcustom lsp-openscad-server-connection-type
  'tcp
  "Type of connection to use with the OpenSCAD Language Server: tcp or stdio."
  :group 'lsp-openscad
  :risky t
  :type 'symbol)

(defcustom lsp-openscad-search-paths ""
  "Customized search path."
  :type 'string
  :group 'lsp-openscad)

(defcustom lsp-openscad-format-exe "clang-format"
  "Path to the clang-format executable."
  :type 'string
  :group 'lsp-openscad)

(defcustom lsp-openscad-format-style "file"
  "Style argument to use with clang-format."
  :type 'string
  :group 'lsp-openscad)

(lsp-register-custom-settings
 '(("openscad.search_paths" lsp-openscad-search-paths)
   ("openscad.fmt_exe" lsp-openscad-format-exe)
   ("openscad.fmt_style" lsp-openscad-format-style)))

(defun lsp-openscad-server-stdio-start-fun ()
  "Create arguments to start openscad language server in stdio mode."
  `(,lsp-openscad-server "--stdio" ))

(defun lsp-openscad-server-tcp-start-fun (port)
  "Create arguments to start openscad language server in TCP mode on PORT."
  `(,lsp-openscad-server "--port" ,(number-to-string port)))

(defun lsp-openscad-server-connection ()
  "Create command line arguments to start openscad language server."
  (if (eq lsp-openscad-server-connection-type 'tcp)
      (lsp-tcp-connection 'lsp-openscad-server-tcp-start-fun)
    (lsp-stdio-connection 'lsp-openscad-server-stdio-start-fun)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-openscad-server-connection)
                  :major-modes '(scad-mode)
                  :priority -1
                  :initialized-fn (lambda (workspace)
                                    ;; OpenSCAD-LSP returns an empty list of
                                    ;; completion options at initialization
                                    ;; so completionProvider capability is {}
                                    ;; When using plists, this value is parsed as
                                    ;; null/nil so we need to force it to "t"
                                    ;; to enable completion
                                    (let ((caps (lsp--workspace-server-capabilities workspace)))
                                      (unless (lsp-get caps :completionProvider)
                                        (lsp:set-server-capabilities-completion-provider? caps t)))
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "openscad"))))
                  :server-id 'openscad))

(provide 'lsp-openscad)
;;; lsp-openscad.el ends here
