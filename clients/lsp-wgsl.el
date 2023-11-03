;;; lsp-wgsl.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2023 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, wgsl, shaders, graphics programming,

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

;; LSP Clients for WGSL (WebGPU Shading Language).

;;; Code:

(defgroup lsp-wgsl nil
  "LSP support for wgsl, using wgsl-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/wgsl-analyzer/wgsl-analyzer")
  :package-version '(lsp-mode . "8.0.1"))


(defcustom lsp-wgsl-server-command "wgsl_analyzer"
  "Command to run the wgsl-analyzer executable."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "8.0.1"))

;; Various toggling settings for the lsp server
(defcustom lsp-wgsl-inlayhints-enabled t
  "Whether to enable inlay hints or not."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "8.0.1"))

;; TODO: maybe type choice instead?
(defcustom lsp-wgsl-inlayhints-type-verbosity "compact"
  "The type verbosity to use for inlay hints."
  :type '(choice (string "full")
                 (string "compact")
                 (string "inner"))
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "8.0.1"))


;; Various interactive functions to use the custom LSP extensions from the server
(defun lsp-wgsl-full-source ()
  "Gets the full source of the file with all imports and preprocessor definitions resolved."
  (interactive)
  (lsp-request-async
   "wgsl-analyzer/fullSource"
   (list :textDocument (list :uri (lsp--buffer-uri)))
   (lambda (source)
     (let ((buffer (get-buffer-create "*WGSL-full-source*")))
       (with-current-buffer buffer
         (setq-local buffer-read-only nil)
         (erase-buffer)
         (insert source)
         (read-only-mode)
         ;; activate only syntax highlighting
         (font-lock-add-keywords nil wgsl-font-lock-keywords)
         (font-lock-mode))
       (switch-to-buffer buffer)))))


;; Error("missing field `customImports`"
;; TODO: best way to handle the custom imports logic?
(defcustom lsp-wgsl-custom-imports (lsp-ht)
  ""
  :type 'list
  :group 'lsp-wgsl)

;; TODO: make it work for empty lists!
(defcustom lsp-wgsl-shaderdefs (list "TEST")
  ""
  :type 'list
  :group 'lsp-wgsl)


;; wgsl-analyzer is a bit weird with how it gets config.
;; Currently it relies on a custom extension to query the clients.
;; (could not get standard custom-settings blocks to work)
(defun lsp-wgsl--send-configuration (&rest _)
  (list :customImports lsp-wgsl-custom-imports
        :diagnostics (list :typeErrors (lsp-json-bool t)
                           :nagaParsingErrors (lsp-json-bool t)
                           :nagaValidationErrors (lsp-json-bool nil)
                           :nagaVersion "main")
        :inlayHints (list :enabled (lsp-json-bool lsp-wgsl-inlayhints-enabled)
                          :typeHints (lsp-json-bool nil)
                          :parameterHints (lsp-json-bool nil)
                          :structLayoutHints (lsp-json-bool nil)
                          :typeVerbosity lsp-wgsl-inlayhints-type-verbosity)
        :shaderDefs []
        :trace (list :extension t
                     :server t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     lsp-wgsl-server-command))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      ;; wgsl-analyzer handles configuration in a VERY non-standard way
                                      ;; https://github.com/wgsl-analyzer/wgsl-analyzer/issues/77
                                      (lsp--set-configuration '())))
                  :request-handlers (lsp-ht ("wgsl-analyzer/requestConfiguration" #'lsp-wgsl--send-configuration))
                  :activation-fn (lsp-activate-on "wgsl")
                  :priority -1
                  :server-id 'wgsl-analyzer))


(lsp-consistency-check lsp-wgsl)

(provide 'lsp-wgsl)
;;; lsp-wgsl.el ends here
