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

(defcustom lsp-modeline-code-actions-kind-regex "$\\|quickfix.*\\|refactor.*"
  "Regex for the code actions kinds to show in the modeline."
  :type 'string
  :group 'lsp-mode)

(defface lsp-modeline-code-actions-face
  '((t :inherit homoglyph))
  "Face used to code action text on modeline."
  :group 'lsp-faces)

(defvar-local lsp--modeline-code-actions-string nil
  "Holds the current code action string on modeline.")

(declare-function all-the-icons-octicon "ext:all-the-icons" t t)

(defun lsp--modeline-code-actions-icon ()
  "Build the icon for modeline code actions."
  (if (require 'all-the-icons nil t)
      (all-the-icons-octicon "light-bulb"
                             :face lsp-modeline-code-actions-face
                             :v-adjust -0.0575)
    (propertize "💡" 'face lsp-modeline-code-actions-face)))

(defun lsp--modeline-code-action->string (action)
  "Convert code ACTION to friendly string."
  (->> action
       lsp:code-action-title
       (replace-regexp-in-string "[\n\t ]+" " ")))

(defun lsp--modeline-build-code-actions-string (actions)
  "Build the string to be presented on modeline for code ACTIONS."
  (-let* ((icon (lsp--modeline-code-actions-icon))
          (first-action-string (propertize (or (-some->> actions
                                                 (-first #'lsp:code-action-is-preferred?)
                                                 lsp--modeline-code-action->string)
                                               (->> actions
                                                    lsp-seq-first
                                                    lsp--modeline-code-action->string))
                                           'face lsp-modeline-code-actions-face))
          (single-action? (= (length actions) 1))
          (keybinding (-some->> #'lsp-execute-code-action
                        where-is-internal
                        (-find (lambda (o)
                                 (not (member (aref o 0) '(menu-bar normal-state)))))
                        key-description
                        (format "(%s)")))
          (string (if single-action?
                      (format " %s %s " icon first-action-string)
                    (format " %s %s %s " icon first-action-string
                            (propertize (format "(%d more)" (1- (seq-length actions)))
                                        'display `((height 0.9))
                                        'face lsp-modeline-code-actions-face)))))
    (propertize string
                'help-echo (concat (format "Apply code actions %s\nmouse-1: " keybinding)
                                   (if single-action?
                                       first-action-string
                                     "select from multiple code actions"))
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda ()
                                       (interactive)
                                       (if single-action?
                                           (lsp-execute-code-action (lsp-seq-first actions))
                                         (lsp-execute-code-action (lsp--select-action actions))))))))

(defun lsp-modeline--update-code-actions (actions)
  "Update modeline with new code ACTIONS."
  (when lsp-modeline-code-actions-kind-regex
    (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
                                (or (not kind?)
                                    (s-match lsp-modeline-code-actions-kind-regex kind?)))
                              actions)))
  (setq lsp--modeline-code-actions-string
        (if (seq-empty-p actions) ""
          (lsp--modeline-build-code-actions-string actions)))
  (force-mode-line-update))

(defun lsp--modeline-check-code-actions (&rest _)
  "Request code actions to update modeline for given BUFFER."
  (when (lsp-feature? "textDocument/codeAction")
    (lsp-request-async
     "textDocument/codeAction"
     (lsp--text-document-code-action-params)
     #'lsp-modeline--update-code-actions
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
  :group 'lsp-mode
  :global nil
  :lighter ""
  (cond
   (lsp-modeline-code-actions-mode
    (add-to-list 'global-mode-string '(t (:eval lsp--modeline-code-actions-string)))

    (add-hook 'lsp-on-idle-hook 'lsp--modeline-check-code-actions nil t)
    (add-hook 'lsp-configure-hook #'lsp-modeline--enable-code-actions nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-code-actions nil t))
   (t
    (remove-hook 'lsp-on-idle-hook 'lsp--modeline-check-code-actions t)
    (remove-hook 'lsp-configure-hook #'lsp-modeline--enable-code-actions t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-modeline--disable-code-actions t)
    (setq global-mode-string (remove '(t (:eval lsp--modeline-code-actions-string)) global-mode-string)))))

(provide 'lsp-modeline)
;;; lsp-modeline.el ends here
