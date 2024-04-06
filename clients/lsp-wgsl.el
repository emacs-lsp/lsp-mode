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

(require 'lsp-mode)

(defgroup lsp-wgsl nil
  "LSP support for wgsl, using wgsl-analyzer."
  :group 'lsp-mode
  :link '(url-link "https://github.com/wgsl-analyzer/wgsl-analyzer")
  :package-version '(lsp-mode . "9.0.0"))


(defcustom lsp-wgsl-server-command "wgsl_analyzer"
  "Command to run the wgsl-analyzer executable."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

;; Various toggling settings for the lsp server
(defcustom lsp-wgsl-diagnostics-type-errors t
  "Whether to show type errors in diagnostics or not."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-diagnostics-naga-parsing-errors t
  "Whether to show naga parsing errors in diagnostics or not."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-diagnostics-naga-validation-errors t
  "Whether to show naga validation errors in diagnostics or not."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-diagnostics-naga-version "main"
  "Naga version to use."
  :type 'string
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-inlayhints-enabled t
  "Whether to enable inlay hints or not."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-inlayhints-typehints t
  "Whether to enable type hints or not when using inlay hints."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-inlayhints-parameterhints t
  "Whether to enable parameter hints or not when using inlay hints."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-inlayhints-structlayout t
  "Whether to enable struct layout hints or not when using inlay hints."
  :type 'boolean
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

;; TODO: maybe type choice instead?
(defcustom lsp-wgsl-inlayhints-type-verbosity "compact"
  "The type verbosity to use for inlay hints."
  :type '(choice (string "full")
                 (string "compact")
                 (string "inner"))
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-custom-imports (lsp-ht)
  "List of custom imports in the style of Bevy"
  :type 'ht
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-wgsl-shaderdefs []
  "Defines that should be valid for preprocessor operations like ifdef,
e.g, ['USE_TYPES', 'DEBUG']"
  :type 'lsp-string-vector
  :group 'lsp-wgsl
  :package-version '(lsp-mode . "9.0.0"))

;; wgsl-analyzer is a bit weird with how it gets config.
;; Currently it relies on a custom extension to query the clients.
;; (could not get standard custom-settings blocks to work)
(defun lsp-wgsl--send-configuration (&rest _)
  ;; TODO: why doesnt this behave like the normal lists?!?!? I cant just send a list?!?!?! why the fuck?!?!
  (list :customImports lsp-wgsl-custom-imports
        :diagnostics (list :typeErrors (lsp-json-bool lsp-wgsl-diagnostics-type-errors)
                           :nagaParsingErrors (lsp-json-bool lsp-wgsl-diagnostics-naga-parsing-errors)
                           :nagaValidationErrors (lsp-json-bool lsp-wgsl-diagnostics-naga-validation-errors)
                           :nagaVersion lsp-wgsl-diagnostics-naga-version)
        :inlayHints (list :enabled (lsp-json-bool lsp-wgsl-inlayhints-enabled)
                          :typeHints (lsp-json-bool lsp-wgsl-inlayhints-typehints)
                          :parameterHints (lsp-json-bool lsp-wgsl-inlayhints-parameterhints)
                          :structLayoutHints (lsp-json-bool lsp-wgsl-inlayhints-structlayout)
                          :typeVerbosity lsp-wgsl-inlayhints-type-verbosity)
        :shaderDefs lsp-wgsl-shaderdefs
        ;; not configurable at the moment, as they don't seem to have much effect.
        ;; Fails if not given.
        :trace (list :extension t
                     :server t)))

(defvar wgsl-font-lock-keywords)

;; Various interactive functions to use the custom LSP extensions from the server
(defun lsp-wgsl-full-source ()
  "Gets the full source of the file with all imports and preprocessor
definitions resolved."
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

(defun lsp-wgsl-syntax-tree ()
  "Gets the syntax tree of the current buffer."
  (interactive)
  (lsp-request-async
   "wgsl-analyzer/syntaxTree"
   (list :textDocument (list :uri (lsp--buffer-uri))
         :range (if (use-region-p)
                    (lsp--region-to-range (region-beginning) (region-end))
                  (lsp--region-to-range (point-min) (point-max))))
   (lambda (syntax-tree)
     (let ((buffer (get-buffer-create (format "*WGSL-syntax-tree %s*" (lsp--buffer-uri)))))
       (with-current-buffer buffer
         (setq-local buffer-read-only nil)
         (erase-buffer)
         (insert syntax-tree)
         (read-only-mode))
       (switch-to-buffer buffer)))))


(lsp-dependency 'wgsl-analyzer
                '(:system lsp-wgsl-server-command)
                '(:cargo :package "wgsl_analyzer"
                         :path "wgsl_analyzer"
                         :git "https://github.com/wgsl-analyzer/wgsl-analyzer"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (or (lsp-package-path 'wgsl-analyzer)
                                         lsp-wgsl-server-command)))
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      ;; wgsl-analyzer handles configuration in a VERY non-standard way
                                      ;; https://github.com/wgsl-analyzer/wgsl-analyzer/issues/77
                                      (lsp--set-configuration '())))
                  :request-handlers (lsp-ht ("wgsl-analyzer/requestConfiguration" #'lsp-wgsl--send-configuration))
                  :major-modes '(wgsl-mode)
                  :activation-fn (lsp-activate-on "wgsl")
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'wgsl-analyzer
                                                            callback error-callback))
                  :priority -1
                  :server-id 'wgsl-analyzer))


(lsp-consistency-check lsp-wgsl)

(provide 'lsp-wgsl)
;;; lsp-wgsl.el ends here
