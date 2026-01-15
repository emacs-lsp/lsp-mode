;;; lsp-sml.el --- Standard ML client settings -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Ariel Davis
;; Copyright (C) 2023-2026 lsp-mode maintainers

;; Author: Ariel Davis <ariel.z.davis@icloud.com>
;; Keywords: languages, lsp, sml, standard-ml, millet

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

;; lsp-sml client via Millet

;;; Code:

(require 'lsp-mode)
(require 'lsp-completion)

(defgroup lsp-sml nil
  "LSP support for Standard ML, using the Millet language server."
  :link '(url-link "https://github.com/azdavis/millet")
  :group 'lsp-mode)

(defcustom lsp-sml-millet-format-engine "none"
  "**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL.
IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

How to format open SML files on save."
  :type '(choice (const :tag "No formatting." "none")
                 (const :tag "Naive formatting." "naive")
                 (const :tag "Formatting provided by https://github.com/shwestrick/smlfmt." "smlfmt"))
  :group 'lsp-sml)

(defcustom lsp-sml-millet-server-diagnostics-filter "syntax"
  "What diagnostics to send per file."
  :type '(choice (const :tag "No filter, i.e. all available diagnostics are sent." "none")
                 (const :tag "If there are syntax errors (lex, parse, etc),
send only those, and do not send e.g. statistics diagnostics." "syntax"))
  :group 'lsp-sml)

(defcustom lsp-sml-millet-server-diagnostics-moreInfoHint-enable t
  "Show a hint on diagnostic messages about clicking the error code number for
more information."
  :type 'boolean
  :group 'lsp-sml)

(defcustom lsp-sml-millet-server-diagnostics-onChange-enable nil
  "Send diagnostics when file contents change before saving."
  :type 'boolean
  :group 'lsp-sml)

(defcustom lsp-sml-millet-server-hover-token-enable t
  "Show information about tokens on hover."
  :type 'boolean
  :group 'lsp-sml)

(defcustom lsp-sml-millet-server-path "millet-ls"
  "Path to the `millet-ls` executable."
  :type 'string
  :group 'lsp-sml)

(lsp-register-custom-settings
 '(("millet.format.engine" lsp-sml-millet-format-engine)
   ("millet.server.diagnostics.filter" lsp-sml-millet-server-diagnostics-filter)
   ("millet.server.diagnostics.moreInfoHint.enable" lsp-sml-millet-server-diagnostics-moreInfoHint-enable)
   ("millet.server.diagnostics.onChange.enable" lsp-sml-millet-server-diagnostics-onChange-enable)
   ("millet.server.hover.token.enable" lsp-sml-millet-server-hover-token-enable)
   ("millet.server.path" lsp-sml-millet-server-path)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-sml-millet-server-path))
                  :activation-fn (lsp-activate-on "sml" "millet.toml")
                  :language-id "sml"
                  :priority -1
                  :server-id 'millet))

(lsp-consistency-check lsp-sml)

(provide 'lsp-sml)
;;; lsp-sml.el ends here
