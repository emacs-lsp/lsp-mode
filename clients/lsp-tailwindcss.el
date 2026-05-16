;;; lsp-tailwindcss.el --- A lsp-mode client for Tailwind CSS  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Merrick Luo
;; Copyright (C) 2024-2026 emacs-lsp maintainers

;; Author: Merrick Luo <merrick@luois.me>
;; Keywords: language tools

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

;; LSP client for the Tailwind CSS language server.

;;; Code:

(require 'lsp-mode)
(require 'f)

(defgroup lsp-tailwindcss nil
  "Adds lsp-mode support for Tailwind CSS."
  :group 'lsp-mode
  :link '(url-link "https://github.com/tailwindlabs/tailwindcss-intellisense"))

(defcustom lsp-tailwindcss-add-on-mode nil
  "Specify lsp-tailwindcss as add-on so it can work with other language servers."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-server-version "0.14.7"
  "Specify the version of tailwindcss intellisense."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-server-path ""
  "Path to the tailwindcss lsp server binary (node.js executable).

Leave empty to use the managed installation."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode typescript-tsx-mode tsx-ts-mode)
  "Major modes that lsp-tailwindcss should activate."
  :type '(repeat symbol)
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-rustywind-command (executable-find "rustywind")
  "[Experimental] Command used for sorting the tailwindcss classes."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-rustywind-extra-args nil
  "[Experimental] Extra arguments to use when invoking rustywind."
  :type '(repeat string)
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-skip-config-check nil
  "Force skip config file check.
Only use it when your config file is in an unconventional location
and you are sure tailwindcss language server can find it."
  :type 'boolean
  :group 'lsp-tailwindcss)

;;;###autoload
(defun lsp-tailwindcss-rustywind ()
  "[Experimental] Sort tailwindcss class name using rustywind."
  (interactive)
  (if (and lsp-tailwindcss-rustywind-command
           (f-executable-p lsp-tailwindcss-rustywind-command))
      (let ((tmpfile (make-nearby-temp-file "rustywind" nil nil))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))

        (unwind-protect
            (save-restriction
              (widen)
              (write-region nil nil tmpfile)

              (let ((rustywind-args (append (list "--write" (file-local-name tmpfile)) lsp-tailwindcss-rustywind-extra-args)))
                (when (zerop (apply #'process-file lsp-tailwindcss-rustywind-command nil nil nil rustywind-args))
                  (insert-file-contents tmpfile nil nil nil t))))

          (delete-file tmpfile)))
    (error "Can't find rustywind executable at %s" lsp-tailwindcss-rustywind-command)))

;;;###autoload
(defun lsp-tailwindcss-rustywind-before-save ()
  "[Experimental] Run rustywind when saving buffer.
By adding this to `before-save-hook', it only runs when
lsp-tailwindcss can be activated, see `lsp-tailwindcss--activate-p'."
  (when (lsp-tailwindcss--activate-p)
    (lsp-tailwindcss-rustywind)))

;;;###autoload
(defun lsp-tailwindcss-installed-server-version ()
  "Get the installed version of tailwindcss language server."
  (interactive)
  (let ((package-json-file (f-join lsp-server-install-dir "tailwindcss/extension/package.json")))
    (if (f-exists? package-json-file)
        (let ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
          (message "%s" (gethash "version" (json-read-file package-json-file))))
      (error "Can't find package.json file at %s" package-json-file))))

;;; Language server global settings:

(defcustom lsp-tailwindcss-emmet-completions nil
  "Enable completions when using Emmet-style syntax.
For example div.bg-red-500.uppercase."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-show-pixel-equivalents t
  "Show px equivalents for rem CSS values in completions and hovers."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-root-font-size 16
  "Root font size in pixels.
Used to convert rem CSS values to their px equivalents.
See `lsp-tailwindcss-show-pixel-equivalents'."
  :type 'number
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-validate t
  "Enable linting.
Rules can be configured individually using the lsp-tailwindcss-lint-* settings:
  ignore: disable lint rule entirely
  warning: rule violations will be considered \"warnings\"
  error: rule violations will be considered \"errors\""
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-hovers t
  "Enable hovers."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-suggestions t
  "Enable autocomplete suggestions."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-code-actions t
  "Enable code actions."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-invalid-screen "error"
  "Unknown screen name used with the @screen directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-invalid-variant "error"
  "Unknown variant name used with the @variants directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-invalid-tailwind-directive "error"
  "Unknown value used with the @tailwind directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-invalid-apply "error"
  "Unsupported use of the @apply directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-invalid-config-path "error"
  "Unknown or invalid path used with the theme helper."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-css-conflict "warning"
  "Class names on the same HTML element which apply the same CSS property or properties."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-lint-recommended-variant-order "warning"
  "Class variants not in the recommended order (applies in JIT mode only)."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-experimental-class-regex ""
  "Custom regex to match tailwindcss classes."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-class-attributes ["class" "className" "ngClass" "class:list"]
  "The HTML attributes to provide class completions, hover previews, linting etc."
  :type 'lsp-string-vector
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-class-functions nil
  "Functions in which to provide completions, hover previews, linting etc.
Currently, this works for both function calls and tagged template literals
in JavaScript / TypeScript."
  :type 'lsp-string-vector
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-experimental-config-file nil
  "Manually specify the Tailwind config file or files.
It can be a single string or a hash map.  See
https://github.com/tailwindlabs/tailwindcss-intellisense#tailwindcssexperimentalconfigfile

Example:
  (setq lsp-tailwindcss-experimental-config-file \"config/tailwindcss.conf.js\")

  (setq lsp-tailwindcss-experimental-config-file
        (ht
         (\"themes/simple/tailwind.config.js\" \"themes/simple/**\")
         (\"themes/neon/tailwind.config.js\" \"themes/neon/**\")))"
  :type 'sexp
  :group 'lsp-tailwindcss)

(lsp-register-custom-settings
 '(("tailwindCSS.emmetCompletions" lsp-tailwindcss-emmet-completions t)
   ("tailwindCSS.showPixelEquivalents" lsp-tailwindcss-show-pixel-equivalents t)
   ("tailwindCSS.rootFontSize" lsp-tailwindcss-root-font-size)
   ("tailwindCSS.validate" lsp-tailwindcss-validate t)
   ("tailwindCSS.hovers" lsp-tailwindcss-hovers t)
   ("tailwindCSS.suggestions" lsp-tailwindcss-suggestions t)
   ("tailwindCSS.codeActions" lsp-tailwindcss-code-actions t)
   ("tailwindCSS.lint.invalidScreen" lsp-tailwindcss-lint-invalid-screen)
   ("tailwindCSS.lint.invalidVariant" lsp-tailwindcss-lint-invalid-variant)
   ("tailwindCSS.lint.invalidTailwindDirective" lsp-tailwindcss-lint-invalid-tailwind-directive)
   ("tailwindCSS.lint.invalidApply" lsp-tailwindcss-lint-invalid-apply)
   ("tailwindCSS.lint.invalidConfigPath" lsp-tailwindcss-lint-invalid-config-path)
   ("tailwindCSS.lint.cssConflict" lsp-tailwindcss-lint-css-conflict)
   ("tailwindCSS.lint.recommendedVariantOrder" lsp-tailwindcss-lint-recommended-variant-order)
   ("tailwindCSS.experimental.classRegex" lsp-tailwindcss-experimental-class-regex)
   ("tailwindCSS.experimental.configFile" lsp-tailwindcss-experimental-config-file)
   ("tailwindCSS.classAttributes" lsp-tailwindcss-class-attributes)
   ("tailwindCSS.classFunctions" lsp-tailwindcss-class-functions)))

;;; Server setup:

(defun lsp-tailwindcss--download-url ()
  "Build language server download url from version."
  (let ((version lsp-tailwindcss-server-version))
    (lsp-vscode-extension-url "bradlc" "vscode-tailwindcss" version)))

(defun lsp-tailwindcss-server-command ()
  "Return the path to the server command when installed from the vscode extension."
  (f-join lsp-server-install-dir "tailwindcss/extension/dist/tailwindServer.js"))

(lsp-dependency 'tailwindcss-language-server
                `(:download
                  :url lsp-tailwindcss--download-url
                  :decompress :zip
                  :store-path ,(f-join lsp-server-install-dir "tailwindcss" "server")
                  :binary-path lsp-tailwindcss-server-command))

(defun lsp-tailwindcss--has-config-file ()
  "Check if there is a tailwindcss config file.
To keep it simple and performant, only check for conventional locations.
See `lsp-tailwindcss-skip-config-check'."
  (or lsp-tailwindcss-skip-config-check
      lsp-tailwindcss-experimental-config-file
      (f-glob "tailwind.config.*" (lsp-workspace-root))
      (f-glob "**/tailwind.config.*" (lsp-workspace-root))))

(defun lsp-tailwindcss--package-version (&optional file)
  "Return tailwindcss version from package.json closest to FILE or current buffer.
Checks both \"dependencies\" and \"devDependencies\"."
  (when-let* ((start-file (or file (buffer-file-name)))
              (package-root (locate-dominating-file start-file "package.json"))
              (package-file (expand-file-name "package.json" package-root)))
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (data (json-read-file package-file)))
      (or (gethash "tailwindcss" (gethash "dependencies" data (make-hash-table :test 'equal)))
          (gethash "tailwindcss" (gethash "devDependencies" data (make-hash-table :test 'equal)))))))

(defun lsp-tailwindcss--version-v4-p (&optional version)
  "Check if tailwindcss package VERSION is v4 or above.
Will try to lookup the version if not provided."
  (when-let* ((version (or version (lsp-tailwindcss--package-version))))
    (string-match-p "^[~^]?\\([4-9]\\|[0-9]\\{2\\}\\)\\.?" version)))

(defun lsp-tailwindcss--activate-p (&rest _args)
  "Check if tailwindcss language server can/should start."
  (and (lsp-workspace-root)
       (apply #'provided-mode-derived-p major-mode lsp-tailwindcss-major-modes)
       (or (lsp-tailwindcss--has-config-file) (lsp-tailwindcss--version-v4-p))))

(defun lsp-tailwindcss--company-dash-hack (workspace)
  "Append \"-\" to the lsp completion trigger characters for WORKSPACE.
Workaround for company-mode completion not working when typing \"-\" in classname."
  (with-lsp-workspace workspace
    (let* ((caps (lsp--workspace-server-capabilities workspace))
           (comp (lsp:server-capabilities-completion-provider? caps))
           (trigger-chars (append (lsp:completion-options-trigger-characters? comp) nil)))
      (lsp:set-completion-options-trigger-characters?
       comp
       (vconcat
        (cl-pushnew "-" trigger-chars :test #'string=))))))

(defun lsp-tailwindcss--server-path ()
  "Return path to the tailwindcss language server."
  (if (string-empty-p lsp-tailwindcss-server-path)
      (lsp-package-path 'tailwindcss-language-server)
    lsp-tailwindcss-server-path))

(defun lsp-tailwindcss--initialization-options ()
  "Return initialization options for the tailwindcss language server."
  (ht ("configuration" (lsp-configuration-section "tailwindcss"))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `("node" ,(lsp-tailwindcss--server-path) "--stdio")))
  :activation-fn #'lsp-tailwindcss--activate-p
  :server-id 'tailwindcss
  :priority -1
  :add-on? lsp-tailwindcss-add-on-mode
  :initialization-options #'lsp-tailwindcss--initialization-options
  :initialized-fn #'lsp-tailwindcss--company-dash-hack
  :notification-handlers (ht ("@/tailwindCSS/projectInitialized" #'ignore)
                             ("@/tailwindCSS/projectsDestroyed" #'ignore))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'tailwindcss-language-server callback error-callback))))

(lsp-consistency-check lsp-tailwindcss)

(provide 'lsp-tailwindcss)
;;; lsp-tailwindcss.el ends here
