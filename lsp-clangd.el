;;; lsp-clangd.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, clangd

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

;; LSP Client for the Clangd  Language Server.

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)
(require 'rx)
(require 'seq)

(require 'dash)
(require 'dash-functional)
(require 's)

(defgroup lsp-clangd nil
  "LSP support for C-family languages (C, C++, Objective-C, Objective-C++), using clangd."
  :group 'lsp-mode
  :link '(url-link "https://clang.llvm.org/extra/clangd"))

(defcustom lsp-clients-clangd-executable nil
  "The clangd executable to use.
When `'non-nil' use the name of the clangd executable file
available in your path to use. Otherwise the system will try to
find a suitable one. Set this variable before loading lsp."
  :group 'lsp-clangd
  :risky t
  :type 'file)

(defvar lsp-clients--clangd-default-executable nil
  "Clang default executable full path when found.
This must be set only once after loading the clang client.")

(defcustom lsp-clients-clangd-args '()
  "Extra arguments for the clangd executable."
  :group 'lsp-clangd
  :risky t
  :type '(repeat string))

(defun lsp-clients--clangd-command ()
  "Generate the language server startup command."
  (unless lsp-clients--clangd-default-executable
    (setq lsp-clients--clangd-default-executable
          (catch 'path
            (mapc (lambda (suffix)
                    (let ((path (executable-find (concat "clangd" suffix))))
                      (when path (throw 'path path))))
                  '("" "-10" "-9" "-8" "-7" "-6")))))

  `(,(or lsp-clients-clangd-executable lsp-clients--clangd-default-executable)
    ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--clangd-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd))

(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql clangd)))
  "Extract a representative line from clangd's CONTENTS, to show in the echo area.
This function tries to extract the type signature from CONTENTS,
or the first line if it cannot do so. A single line is always
returned to avoid that the echo area grows uncomfortably."
  (with-temp-buffer
    (-let [value (lsp:markup-content-value contents)]
      (insert value)
      (goto-char (point-min))
      (if (re-search-forward (rx (seq "```cpp\n"
                                      (opt (group "//"
                                                  (zero-or-more nonl)
                                                  "\n"))
                                      (group
                                       (one-or-more
                                        (not (any "`")))
                                       "\n")
                                      "```")) nil t nil)
          (progn (narrow-to-region (match-beginning 2) (match-end 2))
                 (lsp--render-element (lsp-join-region (point-min) (point-max))))
        (car (s-lines (lsp--render-element contents)))))))

(provide 'lsp-clangd)
;;; lsp-clangd.el ends here
