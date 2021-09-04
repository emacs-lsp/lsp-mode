;;; lsp-modeline.el --- LSP modeline features -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacs-lsp maintainers
;;
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
;;
;;; Commentary:
;;
;;  LSP modeline
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-modeline nil
  "LSP support for modeline"
  :prefix "lsp-modeline-"
  :group 'lsp-mode
  :tag "LSP Modeline")

(defcustom lsp-modeline-code-actions-kind-regex "$\\|quickfix.*\\|refactor.*"
  "Regex for the code actions kinds to show in the modeline."
  :type 'string
  :group 'lsp-modeline)

(defcustom lsp-modeline-code-actions-segments '(count icon)
  "Define what should display on the modeline when code actions are available."
  :type '(repeat (choice
                  (const :tag "Show the lightbulb icon" icon)
                  (const :tag "Show the name of the preferred code action" name)
                  (const :tag "Show the count of how many code actions available" count)))
  :group 'lsp-modeline
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-modeline-code-action-fallback-icon "💡"
  "Define what should display on the modeline when code actions are available."
  :type 'string
  :group 'lsp-modeline
  :package-version '(lsp-mode . "8.0.0"))

(defface lsp-modeline-code-actions-face
  '((t :inherit homoglyph))
  "Face used to code action text on modeline."
  :group 'lsp-modeline)

(defface lsp-modeline-code-actions-preferred-face
  '((t :foreground "yellow"))
  "Face used to code action text on modeline."
  :group 'lsp-modeline)

;;;###autoload
(define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope
  'lsp-modeline-diagnostics-scope  "lsp-mode 7.0.1")

(defcustom lsp-modeline-diagnostics-scope :workspace
  "The modeline diagnostics scope."
  :group 'lsp-modeline
  :type '(choice (const :tag "File" :file)
                 (const :tag "Project" :workspace)
                 (const :tag "All Projects" :global))
  :package-version '(lsp-mode . "6.3"))

(declare-function all-the-icons-octicon "ext:all-the-icons" t t)
(declare-function lsp-treemacs-errors-list "ext:lsp-treemacs" t)


;; code actions

(defvar-local lsp-modeline--code-actions-string nil
  "Holds the current code action string on modeline.")

(defun lsp-modeline--code-action-face (preferred-code-action)
  "Return the face checking if there is any PREFERRED-CODE-ACTION."
  (if preferred-code-action
      'lsp-modeline-code-actions-preferred-face
    'lsp-modeline-code-actions-face))

(defun lsp-modeline--code-actions-icon (face)
  "Build the icon for modeline code actions using FACE."
  (if (require 'all-the-icons nil t)
      (all-the-icons-octicon "light-bulb"
                             :face face
                             :v-adjust -0.0575)
    (propertize lsp-modeline-code-action-fallback-icon 'face face)))

(defun lsp-modeline--code-action-name (actions preferred-code-action-title)
  "Return the code action name from ACTIONS and PREFERRED-CODE-ACTION-TITLE."
  (or preferred-code-action-title
      (->> actions
           lsp-seq-first
           lsp-modeline--code-action->string)))

(defun lsp-modeline--code-action->string (action)
  "Convert code ACTION to friendly string."
  (->> action
       lsp:code-action-title
       (replace-regexp-in-string "[\n\t ]+" " ")))

(defun lsp-modeline--build-code-actions-segments (actions)
  "Build the code ACTIONS string from the defined segments."
  (let* ((preferred-code-action (-some->> actions
                                  (-first #'lsp:code-action-is-preferred?)
                                  lsp-modeline--code-action->string))
         (face (lsp-modeline--code-action-face preferred-code-action)))
    (mapconcat
     (lambda (segment)
       (pcase segment
         ('icon (lsp-modeline--code-actions-icon face))
         ('name (propertize (lsp-modeline--code-action-name actions preferred-code-action)
                            'face face))
         ('count (propertize (number-to-string (seq-length actions))
                             'face face))))
     lsp-modeline-code-actions-segments " ")))

(defun lsp-modeline--build-code-actions-string (actions)
  "Build the string to be presented on modeline for code ACTIONS."
  (-let* ((single-action? (= (length actions) 1))
          (keybinding (concat "("
                              (-some->> #'lsp-execute-code-action
                                where-is-internal
                                (-find (lambda (o)
                                         (not (member (aref o 0) '(menu-bar normal-state)))))
                                key-description)
                              ")"))
          (built-string (lsp-modeline--build-code-actions-segments actions))
          (preferred-code-action (-some->> actions
                                   (-first #'lsp:code-action-is-preferred?)
                                   lsp-modeline--code-action->string)))
    (add-text-properties 0 (length built-string)
                         (list 'help-echo
                               (concat (format "Apply code actions %s\nmouse-1: " keybinding)
                                       (if single-action?
                                           (lsp-modeline--code-action-name actions preferred-code-action)
                                         "select from multiple code actions"))
                               'mouse-face 'mode-line-highlight
                               'local-map (make-mode-line-mouse-map
                                           'mouse-1 (lambda ()
                                                      (interactive)
                                                      (if single-action?
                                                          (lsp-execute-code-action (lsp-seq-first actions))
                                                        (lsp-execute-code-action (lsp--select-action actions))))))
                         built-string)
    (unless (string= "" built-string)
      (concat built-string " "))))

(defun lsp--modeline-update-code-actions (actions)
  "Update modeline with new code ACTIONS."
  (when lsp-modeline-code-actions-kind-regex
    (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
                                (or (not kind?)
                                    (s-match lsp-modeline-code-actions-kind-regex kind?)))
                              actions)))
  (setq lsp-modeline--code-actions-string
        (if (seq-empty-p actions) ""
          (lsp-modeline--build-code-actions-string actions)))
  (force-mode-line-update))

(defun lsp-modeline--check-code-actions (&rest _)
  "Request code actions to update modeline for given BUFFER."
  (when (lsp-feature? "textDocument/codeAction")
    (lsp-request-async
     "textDocument/codeAction"
     (lsp--text-document-code-action-params)
     #'lsp--modeline-update-code-actions
     :mode 'unchanged
     :cancel-token :lsp-modeline-code-actions)))

(defun lsp-modeline--enable-code-actions ()
  "Enable code actions on modeline mode."
  (when (and lsp-modeline-code-actions-enable
             (lsp-feature? "textDocument/codeAction"))
    (lsp-modeline-code-actions-mode 1)))

(defun lsp-modeline--disable-code-actions ()
  "Disable code actions on modeline mode."
  (lsp-modeline-code-actions-mode -1))

;;;###autoload
(define-minor-mode lsp-modeline-code-actions-mode
  "Toggle code actions on modeline."
  :group 'lsp-modeline
  :global nil
  :lighter ""
  (cond
   (lsp-modeline-code-actions-mode
    (add-to-list 'global-mode-string '(t (:eval lsp-modeline--code-actions-string)))

    (add-hook 'lsp-on-idle-hook 'lsp-modeline--check-code-actions nil t)
    (add-hook 'lsp-configure-hook #'lsp-modeline--enable-code-actions nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-code-actions nil t))
   (t
    (remove-hook 'lsp-on-idle-hook 'lsp-modeline--check-code-actions t)
    (remove-hook 'lsp-configure-hook #'lsp-modeline--enable-code-actions t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-code-actions t)
    (setq global-mode-string (remove '(t (:eval lsp-modeline--code-actions-string)) global-mode-string)))))


;; diagnostics

(defvar-local lsp-modeline--diagnostics-string nil
  "Value of current buffer diagnostics statistics.")

(defvar lsp-modeline--diagnostics-wks->strings nil
  "Plist of workspaces to their modeline strings.
The `:global' workspace is global one.")

(defun lsp-modeline-diagnostics-statistics ()
  "Calculate diagnostics statistics based on `lsp-modeline-diagnostics-scope'."
  (let ((diagnostics (cond
                      ((equal :file lsp-modeline-diagnostics-scope)
                       (list (lsp--get-buffer-diagnostics)))
                      (t (->> (eq :workspace lsp-modeline-diagnostics-scope)
                              (lsp-diagnostics)
                              (ht-values)))))
        (stats (make-vector lsp/diagnostic-severity-max 0))
        strs
        (i 0))
    (mapc (lambda (buf-diags)
            (mapc (lambda (diag)
                    (-let [(&Diagnostic? :severity?) diag]
                      (when severity?
                        (cl-incf (aref stats severity?)))))
                  buf-diags))
          diagnostics)
    (while (< i lsp/diagnostic-severity-max)
      (when (> (aref stats i) 0)
        (setq strs
              (nconc strs
                     `(,(propertize
                         (format "%s" (aref stats i))
                         'face
                         (cond
                          ((= i lsp/diagnostic-severity-error) 'error)
                          ((= i lsp/diagnostic-severity-warning) 'warning)
                          ((= i lsp/diagnostic-severity-information) 'success)
                          ((= i lsp/diagnostic-severity-hint) 'success)))))))
      (cl-incf i))
    (-> (s-join "/" strs)
        (propertize 'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Show diagnostics"
                    'local-map (when (require 'lsp-treemacs nil t)
                                 (make-mode-line-mouse-map
                                  'mouse-1 #'lsp-treemacs-errors-list))))))

(defun lsp-modeline--diagnostics-reset-modeline-cache ()
  "Reset the modeline diagnostics cache."
  (plist-put lsp-modeline--diagnostics-wks->strings (car (lsp-workspaces)) nil)
  (plist-put lsp-modeline--diagnostics-wks->strings :global nil)
  (setq lsp-modeline--diagnostics-string nil))

(defun lsp-modeline--diagnostics-update-modeline ()
  "Update diagnostics modeline string."
  (cl-labels ((calc-modeline ()
                             (let ((str (lsp-modeline-diagnostics-statistics)))
                               (if (string-empty-p str) ""
                                 (concat str " ")))))
    (setq lsp-modeline--diagnostics-string
          (cl-case lsp-modeline-diagnostics-scope
            (:file (or lsp-modeline--diagnostics-string
                       (calc-modeline)))
            (:workspace
             (let ((wk (car (lsp-workspaces))))
               (or (plist-get lsp-modeline--diagnostics-wks->strings wk)
                   (let ((ml (calc-modeline)))
                     (setq lsp-modeline--diagnostics-wks->strings
                           (plist-put lsp-modeline--diagnostics-wks->strings wk ml))
                     ml))))
            (:global
             (or (plist-get lsp-modeline--diagnostics-wks->strings :global)
                 (let ((ml (calc-modeline)))
                   (setq lsp-modeline--diagnostics-wks->strings
                         (plist-put lsp-modeline--diagnostics-wks->strings :global ml))
                   ml)))))))

(defun lsp-modeline--enable-diagnostics ()
  "Enable diagnostics on modeline mode."
  (when (and lsp-modeline-diagnostics-enable
             (lsp-feature? "textDocument/publishDiagnostics"))
    (lsp-modeline-diagnostics-mode 1)))

(defun lsp-modeline--disable-diagnostics ()
  "Disable diagnostics on modeline mode."
  (lsp-modeline-diagnostics-mode -1))

;;;###autoload
(define-obsolete-function-alias 'lsp-diagnostics-modeline-mode
  'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1")

;;;###autoload
(define-minor-mode lsp-modeline-diagnostics-mode
  "Toggle diagnostics modeline."
  :group 'lsp-modeline
  :global nil
  :lighter ""
  (cond
   (lsp-modeline-diagnostics-mode
    (add-hook 'lsp-configure-hook #'lsp-modeline--enable-diagnostics nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-diagnostics nil t)
    (add-to-list 'global-mode-string '(t (:eval (lsp-modeline--diagnostics-update-modeline))))
    (add-hook 'lsp-diagnostics-updated-hook 'lsp-modeline--diagnostics-reset-modeline-cache))
   (t
    (remove-hook 'lsp-configure-hook #'lsp-modeline--enable-diagnostics t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-diagnostics t)
    (remove-hook 'lsp-diagnostics-updated-hook 'lsp-modeline--diagnostics-reset-modeline-cache)
    (setq global-mode-string (remove '(t (:eval (lsp-modeline--diagnostics-update-modeline))) global-mode-string)))))


;; workspace status

(defun lsp-modeline--workspace-status-string ()
  "Build the workspace status string."
  '(t (:eval (-keep #'lsp--workspace-status-string (lsp-workspaces)))))

(defun lsp-modeline--enable-workspace-status ()
  "Enable workspace status on modeline."
  (let ((status (lsp-modeline--workspace-status-string)))
    (setq-local global-mode-string (if (-contains? global-mode-string status)
                                       global-mode-string
                                     (cons status global-mode-string)))))

(defun lsp-modeline--disable-workspace-status ()
  "Disable workspace status on modeline."
  (let ((status (lsp-modeline--workspace-status-string)))
    (setq-local global-mode-string (remove status global-mode-string))))

;;;###autoload
(define-minor-mode lsp-modeline-workspace-status-mode
  "Toggle workspace status on modeline."
  :group 'lsp-modeline
  :global nil
  :lighter ""
  (cond
   (lsp-modeline-workspace-status-mode
    (add-hook 'lsp-configure-hook #'lsp-modeline--enable-workspace-status nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-workspace-status nil t))
   (t
    (remove-hook 'lsp-configure-hook #'lsp-modeline--enable-workspace-status t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-workspace-status t))))

(lsp-consistency-check lsp-modeline)

(provide 'lsp-modeline)
;;; lsp-modeline.el ends here
