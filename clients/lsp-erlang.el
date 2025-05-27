;;; lsp-erlang.el --- Erlang Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Roberto Aloi, Alan Zimmerman

;; Author: Roberto Aloi, Alan Zimmerman
;; Keywords: erlang lsp

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

;; lsp-erlang client

;;; Code:

(require 'lsp-mode)
(require 'lsp-semantic-tokens)

(defgroup lsp-erlang nil
  "LSP support for the Erlang programming language.
It can use erlang-ls or erlang-language-platform (ELP)."
  :group 'lsp-mode)

(defgroup lsp-erlang-ls nil
  "LSP support for the Erlang programming language using erlang-ls."
  :group 'lsp-mode
  :link '(url-link "https://github.com/erlang-ls/erlang_ls"))

(defgroup lsp-erlang-elp nil
  "LSP support for the Erlang programming language using erlang-language-platform (ELP)."
  :group 'lsp-mode
  :link '(url-link "https://github.com/WhatsApp/erlang-language-platform"))

(defgroup lsp-erlang-elp-semantic-tokens nil
  "LSP semantic tokens support for ELP."
  :group 'lsp-erlang-elp
  :link '(url-link "https://github.com/WhatsApp/erlang-language-platform")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-erlang-server 'erlang-ls
  "Choose LSP server."
  :type '(choice (const :tag "erlang-ls" erlang-ls)
                 (const :tag "erlang-language-platform" erlang-language-platform))
  :group 'lsp-erlang
  :package-version '(lsp-mode . "6.2"))

;; ---------------------------------------------------------------------
;; ELP config settings.  Auto-generated using scripts/lsp-generate-settings.el
;; And the process in https://github.com/emacs-lsp/lsp-haskell/blob/master/CONTIBUTING.md

(lsp-defcustom lsp-elp-buck-query-use-bxl-enable nil
  "Use BXL to query for buck project model."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.buck.query.useBxl.enable")

(lsp-defcustom lsp-elp-diagnostics-disabled nil
  "List of ELP diagnostics to disable."
  :type 'lsp-string-vector
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.diagnostics.disabled")

(lsp-defcustom lsp-elp-diagnostics-enable-experimental nil
  "Whether to show experimental ELP diagnostics that might
have more false positives than usual."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.diagnostics.enableExperimental")

(lsp-defcustom lsp-elp-diagnostics-enable-otp nil
  "Whether to report diagnostics for OTP files."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.diagnostics.enableOtp")

(lsp-defcustom lsp-elp-diagnostics-on-save-enable nil
  "Update native diagnostics only when the file is saved."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.diagnostics.onSave.enable")

(lsp-defcustom lsp-elp-eqwalizer-all nil
  "Whether to report Eqwalizer diagnostics for the whole project and not only for opened files."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.eqwalizer.all")

(lsp-defcustom lsp-elp-eqwalizer-chunk-size 100
  "Chunk size to use for project-wide eqwalization."
  :type 'number
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.eqwalizer.chunkSize")

(lsp-defcustom lsp-elp-eqwalizer-max-tasks 32
  "Maximum number of tasks to run in parallel for project-wide eqwalization."
  :type 'number
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.eqwalizer.maxTasks")

(lsp-defcustom lsp-elp-highlight-dynamic-enable nil
  "If enabled, highlight variables with type `dynamic()` when Eqwalizer results are available."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.highlightDynamic.enable")

(lsp-defcustom lsp-elp-hover-actions-doc-links-enable nil
  "Whether to show Hover Actions of type `docs'. Only applies when
`#elp.hoverActions.enable#` is set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.hoverActions.docLinks.enable")

(lsp-defcustom lsp-elp-hover-actions-enable nil
  "Whether to show Hover Actions."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.hoverActions.enable")

(lsp-defcustom lsp-elp-inlay-hints-parameter-hints-enable t
  "Whether to show function parameter name inlay hints at the call
site."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.inlayHints.parameterHints.enable")

(lsp-defcustom lsp-elp-lens-debug-enable nil
  "Whether to show the `Debug` lenses. Only applies when
`#elp.lens.enable#` is set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.debug.enable")

(lsp-defcustom lsp-elp-lens-enable nil
  "Whether to show Code Lenses in Erlang files."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.enable")

(lsp-defcustom lsp-elp-lens-links-enable nil
  "Whether to show the `Link` lenses. Only applies when
`#elp.lens.enable#` is set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.links.enable")

(lsp-defcustom lsp-elp-lens-run-coverage-enable nil
  "Display code coverage information when running tests via the
Code Lenses. Only applies when `#elp.lens.enabled` and
`#elp.lens.run.enable#` are set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.run.coverage.enable")

(lsp-defcustom lsp-elp-lens-run-enable nil
  "Whether to show the `Run` lenses. Only applies when
`#elp.lens.enable#` is set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.run.enable")

(lsp-defcustom lsp-elp-lens-run-interactive-enable nil
  "Whether to show the `Run Interactive` lenses. Only applies when
`#elp.lens.enable#` is set."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.lens.run.interactive.enable")

(lsp-defcustom lsp-elp-log "error"
  "Configure LSP-based logging using env_logger syntax."
  :type 'string
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.log")

(lsp-defcustom lsp-elp-signature-help-enable t
  "Whether to show Signature Help."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.signatureHelp.enable")

(lsp-defcustom lsp-elp-types-on-hover-enable nil
  "Display types when hovering over expressions."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.typesOnHover.enable")

;; ---------------------------------------------------------------------

;; erlang-ls

(defcustom lsp-erlang-ls-server-path
  "erlang_ls"
  "Path to the Erlang Language Server binary."
  :group 'lsp-erlang-ls
  :risky t
  :type 'file)

(defcustom lsp-erlang-ls-server-connection-type
  'stdio
  "Type of connection to use with the Erlang Language Server: tcp or stdio."
  :group 'lsp-erlang-ls
  :risky t
  :type 'symbol)

(defun lsp-erlang-ls-server-start-fun (port)
  "Command to start erlang_ls in TCP mode on the given PORT."
  `(,lsp-erlang-ls-server-path
    "--transport" "tcp"
    "--port" ,(number-to-string port)))

(defun lsp-erlang-ls-server-connection ()
  "Command to start erlang_ls in stdio mode."
  (if (eq lsp-erlang-ls-server-connection-type 'tcp)
      (lsp-tcp-connection 'lsp-erlang-ls-server-start-fun)
    (lsp-stdio-connection `(,lsp-erlang-ls-server-path "--transport" "stdio"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-erlang-ls-server-connection)
                  :major-modes '(erlang-mode)
                  :priority -1
                  :server-id 'erlang-ls))


;; erlang-language-platform

(lsp-defcustom lsp-erlang-elp-types-on-hover t
  "Show eqWAlizer types on hover."
  :type 'boolean
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "9.0.0")
  :lsp-path "elp.typesOnHover.enable")

(defcustom lsp-erlang-elp-server-command '("elp" "server")
  "Command to start erlang-language-platform."
  :type '(repeat string)
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-erlang-elp-otp-download-version "27.1"
  "OTP version used as part of the file name when downlading the ELP binary.
It must match those used in https://github.com/WhatsApp/erlang-language-platform/releases/latest"
  :type '(choice (string :tag "25.3")
                 (string :tag "26.2")
                 (string :tag "27.1"))
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-erlang-elp-download-url
  (format "https://github.com/WhatsApp/erlang-language-platform/releases/latest/download/elp-%s-otp-%s.tar.gz"
          (pcase system-type
            ('gnu/linux
             (if (string-match "^aarch64-.*" system-configuration)
                 "linux-aarch64-unknown-linux-gnu"
               "linux-x86_64-unknown-linux-gnu"))
             ('darwin
              (if (string-match "^aarch64-.*" system-configuration)
                  "macos-aarch64-apple-darwin"
                "macos-x86_64-apple-darwin")))
            lsp-erlang-elp-otp-download-version)
          "Automatic download url for erlang-language-platform."
          :type 'string
          :group 'lsp-erlang-elp
          :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-erlang-elp-store-path (f-join lsp-server-install-dir
                                                "erlang"
                                                (if (eq system-type 'windows-nt)
                                                    "elp.exe"
                                                  "elp"))
  "The path to the file in which `elp' will be stored."
  :type 'file
  :group 'lsp-erlang-elp
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency
 'erlang-language-platform
 `(:download :url lsp-erlang-elp-download-url
             :decompress :targz
             :store-path lsp-erlang-elp-store-path
             :set-executable? t)
 '(:system "elp"))

;; Semantic tokens

;; Modifier faces

(defface lsp-erlang-elp-bound-modifier-face
  '((t :underline t))
  "The face modification to use for bound variables in patterns."
  :group 'lsp-erlang-elp-semantic-tokens)

(defface lsp-erlang-elp-exported-function-modifier-face
  '((t :underline t))
  "The face modification to use for exported functions."
  :group 'lsp-erlang-elp-semantic-tokens)

(defface lsp-erlang-elp-deprecated-function-modifier-face
  '((t :strike-through t))
  "The face modification to use for deprecated functions."
  :group 'lsp-erlang-elp-semantic-tokens)


;; ---------------------------------------------------------------------
;; Semantic token modifier face customization

(defcustom lsp-erlang-elp-bound-modifier 'lsp-erlang-elp-bound-modifier-face
  "Face for semantic token modifier for `bound' attribute."
  :type 'face
  :group 'lsp-erlang-elp-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-erlang-elp-exported-function-modifier 'lsp-erlang-elp-exported-function-modifier-face
  "Face for semantic token modifier for `exported_function' attribute."
  :type 'face
  :group 'lsp-erlang-elp-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-erlang-elp-deprecated-function-modifier 'lsp-erlang-elp-deprecated-function-modifier-face
  "Face for semantic token modifier for `deprecated_function' attribute."
  :type 'face
  :group 'lsp-erlang-elp-semantic-tokens
  :package-version '(lsp-mode . "9.0.0"))

;; ---------------------------------------------------------------------

(defun lsp-erlang-elp--semantic-modifiers ()
  "Mapping between rust-analyzer keywords and fonts to apply.
The keywords are sent in the initialize response, in the semantic
tokens legend."
  `(
    ("bound" . ,lsp-erlang-elp-bound-modifier)
    ("exported_function" . ,lsp-erlang-elp-exported-function-modifier)
    ("deprecated_function" . ,lsp-erlang-elp-deprecated-function-modifier)))

;; ---------------------------------------------------------------------
;; Client

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-erlang-elp-server-command))
                            (lsp-package-path 'erlang-language-platform)
                            "elp")
                       ,@(cl-rest lsp-erlang-elp-server-command))))
  :activation-fn (lsp-activate-on "erlang")
  :priority (if (eq lsp-erlang-server 'erlang-language-platform) 1 -2)
  :semantic-tokens-faces-overrides `(:discard-default-modifiers t
                                                                :modifiers
                                                                ,(lsp-erlang-elp--semantic-modifiers))
  :server-id 'elp
  :custom-capabilities `((experimental . ((snippetTextEdit . ,(and lsp-enable-snippet (fboundp 'yas-minor-mode))))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'erlang-language-platform callback error-callback))))

(defun lsp-erlang-switch-server (&optional lsp-server)
  "Switch priorities of lsp servers, unless LSP-SERVER is already active."
  (interactive)
  (let ((current-server (if (> (lsp--client-priority (gethash 'erlang-ls lsp-clients)) 0)
                            'erlang-ls
                          'erlang-language-platform)))
    (unless (eq lsp-server current-server)
      (dolist (server '(erlang-ls erlang-language-platform))
        (when (natnump (setf (lsp--client-priority (gethash server lsp-clients))
                             (* (lsp--client-priority (gethash server lsp-clients)) -1)))
          (message (format "Switched to server %s." server)))))))

(lsp-consistency-check lsp-erlang)

(provide 'lsp-erlang)
;;; lsp-erlang.el ends here
