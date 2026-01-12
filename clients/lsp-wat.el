;;; lsp-wat.el --- lsp-mode WAT integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pig Fang
;; Copyright (C) 2026 lsp-mode maintainers

;; Author: Pig Fang
;; Keywords: languages,tools

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

;; client for WebAssembly Text Format via wasm-language-tools

;;; Code:

(require 'lsp-mode)

(defgroup lsp-wat nil
  "LSP support for WebAssembly Text Format."
  :link '(url-link "https://github.com/g-plane/wasm-language-tools")
  :group 'lsp-mode)

(defcustom lsp-wat-server-command '("wat_server")
  "The wat_server command."
  :group 'lsp-wat
  :type 'string)

(defcustom lsp-wat-format-split-closing-parens nil
  "Control whether closing parentheses should be splitted into different lines."
  :group 'lsp-wat
  :type 'boolean)

(defcustom lsp-wat-format-wrap-before-locals "always"
  "Control whether to insert line break before function locals."
  :group 'lsp-wat
  :type '(choice (const "never")
                 (const "overflow")
                 (const "multiOnly")
                 (const "always")))

(defcustom lsp-wat-format-wrap-before-fields "multiOnly"
  "Control whether to insert line break before struct fields."
  :group 'lsp-wat
  :type '(choice (const "never")
                 (const "overflow")
                 (const "multiOnly")
                 (const "always")))

(defcustom lsp-wat-format-wrap-before-const-expr "always"
  "Control whether to insert line break before constant expression."
  :group 'lsp-wat
  :type '(choice (const "never")
                 (const "overflow")
                 (const "multiOnly")
                 (const "always")))

(defcustom lsp-wat-format-multi-line-locals "smart"
  "Control how to insert whitespace between multiple locals in a function."
  :group 'lsp-wat
  :type '(choice (const "never")
                 (const "overflow")
                 (const "smart")
                 (const "wrap")
                 (const "always")))

(defcustom lsp-wat-format-multi-line-fields "smart"
  "Control how to insert whitespace between multiple fields in a struct."
  :group 'lsp-wat
  :type '(choice (const "never")
                 (const "overflow")
                 (const "smart")
                 (const "wrap")
                 (const "always")))

(defcustom lsp-wat-format-format-comments nil
  "Control whether whitespace should be inserted at the beginning and end of comments."
  :group 'lsp-wat
  :type 'boolean)

(defcustom lsp-wat-format-ignore-comment-directive "fmt-ignore"
  "Text directive for ignoring formatting specific module or module field."
  :group 'lsp-wat
  :type 'string)

(defcustom lsp-wat-lint-unused "warn"
  "Lint for detecting unused items."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-unread "warn"
  "Lint for detecting unread locals."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-shadow "warn"
  "Lint for detecting shadowing."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-implicit-module "allow"
  "Lint for top-level module fields without declaring a module."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-multi-modules "deny"
  "Lint for detecting multiple modules in a single file."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-unreachable "hint"
  "Lint for detecting unreachable code."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-deprecated "warn"
  "Lint for deprecated usage."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-needless-mut "warn"
  "Lint for detecting mutable globals that are never mutated."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-needless-try-table "warn"
  "Lint for detecting `try_table` block without catch clauses."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-lint-useless-catch "warn"
  "Lint for detecting useless catch clauses."
  :group 'lsp-wat
  :type '(choice (const "allow")
                 (const "hint")
                 (const "warn")
                 (const "deny")))

(defcustom lsp-wat-inlay-hint-types t
  "Inlay hints for indicating types."
  :group 'lsp-wat
  :type 'boolean)

(defcustom lsp-wat-inlay-hint-ending t
  "Inlay hints that show at the end of blocks and functions."
  :group 'lsp-wat
  :type 'boolean)

(defcustom lsp-wat-inlay-hint-index t
  "Inlay hints for showing idx."
  :group 'lsp-wat
  :type 'boolean)

(lsp-register-custom-settings
 '(("wasmLanguageTools.format.splitClosingParens" lsp-wat-format-split-closing-parens)
   ("wasmLanguageTools.format.wrapBeforeLocals" lsp-wat-format-wrap-before-locals)
   ("wasmLanguageTools.format.wrapBeforeFields" lsp-wat-format-wrap-before-fields)
   ("wasmLanguageTools.format.wrapBeforeConstExpr" lsp-wat-format-wrap-before-const-expr)
   ("wasmLanguageTools.format.multiLineLocals" lsp-wat-format-multi-line-locals)
   ("wasmLanguageTools.format.multiLineFields" lsp-wat-format-multi-line-fields)
   ("wasmLanguageTools.format.formatComments" lsp-wat-format-format-comments)
   ("wasmLanguageTools.format.ignoreCommentDirective" lsp-wat-format-ignore-comment-directive)
   ("wasmLanguageTools.lint.unused" lsp-wat-lint-unused)
   ("wasmLanguageTools.lint.unread" lsp-wat-lint-unread)
   ("wasmLanguageTools.lint.shadow" lsp-wat-lint-shadow)
   ("wasmLanguageTools.lint.implicitModule" lsp-wat-lint-implicit-module)
   ("wasmLanguageTools.lint.multiModules" lsp-wat-lint-multi-modules)
   ("wasmLanguageTools.lint.unreachable" lsp-wat-lint-unreachable)
   ("wasmLanguageTools.lint.deprecated" lsp-wat-lint-deprecated)
   ("wasmLanguageTools.lint.needlessMut" lsp-wat-lint-needless-mut)
   ("wasmLanguageTools.lint.needlessTryTable" lsp-wat-lint-needless-try-table)
   ("wasmLanguageTools.lint.uselessCatch" lsp-wat-lint-useless-catch)
   ("wasmLanguageTools.inlayHint.types" lsp-wat-inlay-hint-types)
   ("wasmLanguageTools.inlayHint.ending" lsp-wat-inlay-hint-ending)
   ("wasmLanguageTools.inlayHint.index" lsp-wat-inlay-hint-index)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-wat-server-command))
  :activation-fn (lsp-activate-on "wat")
  :server-id 'wat_server))

(lsp-consistency-check lsp-wat)

(provide 'lsp-wat)
;;; lsp-wat.el ends here
