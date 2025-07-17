;;; lsp-clangd.el --- LSP clients for the C Languages Family -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daniel Martin & emacs-lsp maintainers
;; URL: https://github.com/emacs-lsp/lsp-mode
;; Keywords: languages, c, cpp, clang

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP clients for the C Languages Family.

;; ** Clang-tidy Flycheck integration (Clangd) **
;;
;; If you invoke `flycheck-display-error-explanation' on a
;; `clang-tidy' error (if Clangd is configured to show `clang-tidy'
;; diagnostics), Emacs will open a detailed explanation about the
;; message by querying the LLVM website. As an embedded web browser is
;; used to show the documentation, this feature requires that Emacs is
;; compiled with libxml2 support.

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'dom)
(eval-when-compile (require 'subr-x))

(require 'dash)
(require 's)

(defvar flycheck-explain-error-buffer)
(declare-function flycheck-error-id "ext:flycheck" (err) t)
(declare-function flycheck-error-group "ext:flycheck" (err) t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)

(defcustom lsp-clangd-version "15.0.6"
  "Clangd version to download.
It has to be set before `lsp-clangd.el' is loaded and it has to
be available here: https://github.com/clangd/clangd/releases/"
  :type 'string
  :group 'lsp-clangd
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clangd-download-url
  (format (pcase system-type
            ('darwin "https://github.com/clangd/clangd/releases/download/%s/clangd-mac-%s.zip")
            ('windows-nt "https://github.com/clangd/clangd/releases/download/%s/clangd-windows-%s.zip")
            (_ "https://github.com/clangd/clangd/releases/download/%s/clangd-linux-%s.zip"))
          lsp-clangd-version
          lsp-clangd-version)
  "Automatic download url for clangd"
  :type 'string
  :group 'lsp-clangd
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clangd-binary-path
  (f-join lsp-server-install-dir (format "clangd/clangd_%s/bin"
                                         lsp-clangd-version)
          (pcase system-type
            ('windows-nt "clangd.exe")
            (_ "clangd")))
  "The path to `clangd' binary."
  :type 'file
  :group 'lsp-clangd
  :package-version '(lsp-mode . "8.0.0"))

(lsp-dependency
 'clangd
 `(:download :url lsp-clangd-download-url
             :decompress :zip
             :store-path ,(f-join lsp-server-install-dir "clangd" "clangd.zip")
             :binary-path lsp-clangd-binary-path
             :set-executable? t))

(defun lsp-cpp-flycheck-clang-tidy--skip-http-headers ()
  "Position point just after HTTP headers."
  (re-search-forward "^$"))

(defun lsp-cpp-flycheck-clang-tidy--narrow-to-http-body ()
  "Narrow the current buffer to contain the body of an HTTP response."
  (lsp-cpp-flycheck-clang-tidy--skip-http-headers)
  (narrow-to-region (point) (point-max)))

(defun lsp-cpp-flycheck-clang-tidy--decode-region-as-utf8 (start end)
  "Decode a region from START to END in UTF-8."
  (condition-case nil
      (decode-coding-region start end 'utf-8)
    (coding-system-error nil)))

(defun lsp-cpp-flycheck-clang-tidy--remove-crlf ()
  "Remove carriage return and line feeds from the current buffer."
  (save-excursion
    (while (re-search-forward "\r$" nil t)
      (replace-match "" t t))))

(defun lsp-cpp-flycheck-clang-tidy--extract-relevant-doc-section ()
  "Extract the parts of the LLVM clang-tidy documentation that are relevant.

This function assumes that the current buffer contains the result
of browsing `clang.llvm.org', as returned by `url-retrieve'.
More concretely, this function returns the main <div> element
with class `section', and also removes `headerlinks'."
  (goto-char (point-min))
  (lsp-cpp-flycheck-clang-tidy--narrow-to-http-body)
  (lsp-cpp-flycheck-clang-tidy--decode-region-as-utf8 (point-min) (point-max))
  (lsp-cpp-flycheck-clang-tidy--remove-crlf)
  (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
         (section (dom-by-tag dom 'section)))
    (dolist (headerlink (dom-by-class section "headerlink"))
      (dom-remove-node section headerlink))
    section))

(defun lsp-cpp-flycheck-clang-tidy--explain-error (explanation &rest args)
  "Explain an error in the Flycheck error explanation buffer using EXPLANATION.

EXPLANATION is a function with optional ARGS that, when
evaluated, inserts the content in the appropriate Flycheck
buffer."
  (with-current-buffer flycheck-explain-error-buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (apply explanation args)
      (goto-char (point-min)))))

(defun lsp-cpp-flycheck-clang-tidy--show-loading-status ()
  "Show a loading string while clang-tidy documentation is fetched from llvm.org.
Recent versions of `flycheck' call `display-message-or-buffer' to
display error explanations. `display-message-or-buffer' displays
the documentation string either in the echo area or in a separate
window, depending on the string's height. This function forces to
always display it in a separate window by appending the required
number of newlines."
  (let* ((num-lines-threshold
          (round (if resize-mini-windows
                     (cond ((floatp max-mini-window-height)
                            (* (frame-height)
                               max-mini-window-height))
                           ((integerp max-mini-window-height)
                            max-mini-window-height)
                           (t
                            1))
                   1)))
         (extra-new-lines (make-string (1+ num-lines-threshold) ?\n)))
    (concat "Loading documentation..." extra-new-lines)))

(defun lsp-cpp-flycheck-clang-tidy--show-documentation (error-id)
  "Show clang-tidy documentation about ERROR-ID.

Information comes from the clang.llvm.org website."
  ;; Example error-id: modernize-loop-convert
  ;; Example url: https://clang.llvm.org/extra/clang-tidy/checks/modernize/loop-convert.html
  (setq error-id (s-join "/" (s-split-up-to "-" error-id 1 t)))
  (url-retrieve (format
                 "https://clang.llvm.org/extra/clang-tidy/checks/%s.html" error-id)
                (lambda (status)
                  (if-let* ((error-status (plist-get status :error)))
                      (lsp-cpp-flycheck-clang-tidy--explain-error
                       #'insert
                       (format
                        "Error accessing clang-tidy documentation: %s"
                        (error-message-string error-status)))
                    (let ((doc-contents
                           (lsp-cpp-flycheck-clang-tidy--extract-relevant-doc-section)))
                      (lsp-cpp-flycheck-clang-tidy--explain-error
                       #'shr-insert-document doc-contents)))))
  (lsp-cpp-flycheck-clang-tidy--show-loading-status))

;;;###autoload
(defun lsp-cpp-flycheck-clang-tidy-error-explainer (error)
  "Explain a clang-tidy ERROR by scraping documentation from llvm.org."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (if-let* ((clang-tidy-error-id (flycheck-error-id error)))
      (condition-case err
          (lsp-cpp-flycheck-clang-tidy--show-documentation clang-tidy-error-id)
        (error
         (format
          "Error accessing clang-tidy documentation: %s"
          (error-message-string err))))
    (error "The clang-tidy error message does not contain an [error-id]")))


;;; lsp-clangd
(defgroup lsp-clangd nil
  "LSP support for C-family languages (C, C++, Objective-C, Objective-C++, CUDA), using clangd."
  :group 'lsp-mode
  :link '(url-link "https://clang.llvm.org/extra/clangd"))

(defcustom lsp-clients-clangd-executable nil
  "The clangd executable to use.
When `'non-nil' use the name of the clangd executable file
available in your path to use. Otherwise the system will try to
find a suitable one. Set this variable before loading lsp."
  :group 'lsp-clangd
  :risky t
  :type '(choice (file :tag "Path")
                 (const :tag "Auto" nil)))

(defvar lsp-clients--clangd-default-executable nil
  "Clang default executable full path when found.
This must be set only once after loading the clang client.")

(defcustom lsp-clients-clangd-args '("--header-insertion-decorators=0")
  "Extra arguments for the clangd executable."
  :group 'lsp-clangd
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-clangd-library-directories '("/usr")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-clangd
  :package-version '(lsp-mode . "9.0.0"))

(defun lsp-clients--clangd-command ()
  "Generate the language server startup command."
  (unless lsp-clients--clangd-default-executable
    (setq lsp-clients--clangd-default-executable
          (or (lsp-package-path 'clangd)
              (-first #'executable-find
                      (-map (lambda (version)
                              (concat "clangd" version))
                            ;; Prefer `clangd` without a version number appended.
                            (cl-list* "" (-map
                                          (lambda (vernum) (format "-%d" vernum))
                                          (number-sequence 17 6 -1)))))
              (lsp-clients-executable-find "xcodebuild" "-find-executable" "clangd")
              (lsp-clients-executable-find "xcrun" "--find" "clangd"))))

  `(,(or lsp-clients-clangd-executable lsp-clients--clangd-default-executable "clangd")
    ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--clangd-command)
                  :activation-fn (lsp-activate-on "c" "cpp" "objective-c" "cuda")
                  :priority -1
                  :server-id 'clangd
                  :library-folders-fn (lambda (_workspace) lsp-clients-clangd-library-directories)
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'clangd callback error-callback))))

(defun lsp-clangd-join-region (beg end)
  "Apply join-line from BEG to END.
This function is useful when an indented function prototype needs
to be shown in a single line."
  (save-excursion
    (let ((end (copy-marker end)))
      (goto-char beg)
      (while (< (point) end)
        (join-line 1)))
    (s-trim (buffer-string))))

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
                 (lsp--render-element (lsp-make-marked-string
                                       :language "cpp"
                                       :value (lsp-clangd-join-region (point-min) (point-max)))))
        (car (s-lines (lsp--render-element contents)))))))

(cl-defmethod lsp-diagnostics-flycheck-error-explainer (e (_server-id (eql clangd)))
  "Explain a `flycheck-error' E that was generated by the Clangd language server."
  (cond ((string-equal "clang-tidy" (flycheck-error-group e))
         (lsp-cpp-flycheck-clang-tidy-error-explainer e))
        (t (flycheck-error-message e))))

(defun lsp-clangd-find-other-file (&optional new-window)
  "Switch between the corresponding C/C++ source and header file.
If NEW-WINDOW (interactively the prefix argument) is non-nil,
open in a new window.

Only works with clangd."
  (interactive "P")
  (let ((other (lsp-send-request (lsp-make-request
                                  "textDocument/switchSourceHeader"
                                  (lsp--text-document-identifier)))))
    (unless (s-present? other)
      (user-error "Could not find other file"))
    (funcall (if new-window #'find-file-other-window #'find-file)
             (lsp--uri-to-path other))))

(lsp-consistency-check lsp-clangd)

(provide 'lsp-clangd)
;;; lsp-clangd.el ends here
