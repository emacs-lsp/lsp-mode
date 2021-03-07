;;; lsp-semantic-tokens.el --- Semantic tokens -*- lexical-binding: t; -*-
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
;;  Semantic tokens
;;  https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
;;
;;; Code:

(require 'lsp-mode)

(define-obsolete-variable-alias 'lsp-semantic-highlighting-warn-on-missing-face 'lsp-semantic-tokens-warn-on-missing-face "lsp-mode 7.1")

(defcustom lsp-semantic-tokens-warn-on-missing-face nil
  "Warning on missing face for token type/modifier.
When non-nil, this option will emit a warning any time a token
or modifier type returned by a language server has no face associated with it."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-semantic-tokens-apply-modifiers nil
  "Whether semantic tokens should take token modifiers into account."
  :group 'lsp-mode
  :type 'boolean)

(defface lsp-face-semhl-constant
  '((t :inherit font-lock-constant-face))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-faces)

(defface lsp-face-semhl-variable
  '((t :inherit font-lock-variable-name-face))
  "Face used for semantic highlighting scopes matching variable.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-function
  '((t :inherit font-lock-function-name-face))
  "Face used for semantic highlighting scopes matching entity.name.function.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-method
  '((t :inherit lsp-face-semhl-function))
  "Face used for semantic highlighting scopes matching entity.name.function.method.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-namespace
  '((t :inherit font-lock-type-face :weight bold))
  "Face used for semantic highlighting scopes matching entity.name.namespace.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'lsp-faces)

(defface lsp-face-semhl-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'lsp-faces)

(defface lsp-face-semhl-string
  '((t (:inherit font-lock-string-face)))
  "Face used for keywords."
  :group 'lsp-faces)

(defface lsp-face-semhl-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for numbers."
  :group 'lsp-faces)

(defface lsp-face-semhl-regexp
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face used for regexps."
  :group 'lsp-faces)

(defface lsp-face-semhl-operator
  '((t (:inherit font-lock-function-name-face)))
  "Face used for operators."
  :group 'lsp-faces)

(defface lsp-face-semhl-namespace
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespaces."
  :group 'lsp-faces)

(defface lsp-face-semhl-type
  '((t (:inherit font-lock-type-face)))
  "Face used for types."
  :group 'lsp-faces)

(defface lsp-face-semhl-struct
  '((t (:inherit font-lock-type-face)))
  "Face used for structs."
  :group 'lsp-faces)

(defface lsp-face-semhl-class
  '((t (:inherit font-lock-type-face)))
  "Face used for classes."
  :group 'lsp-faces)

(defface lsp-face-semhl-interface
  '((t (:inherit font-lock-type-face)))
  "Face used for interfaces."
  :group 'lsp-faces)

(defface lsp-face-semhl-enum
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for enums."
  :group 'lsp-faces)

(defface lsp-face-semhl-type-parameter
  '((t (:inherit font-lock-type-face)))
  "Face used for type parameters."
  :group 'lsp-faces)

;; function face already defined, move here when support
;; for theia highlighting gets removed
(defface lsp-face-semhl-member
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for members."
  :group 'lsp-faces)

(defface lsp-face-semhl-property
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for properties."
  :group 'lsp-faces)

(defface lsp-face-semhl-macro
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for macros."
  :group 'lsp-faces)

(defface lsp-face-semhl-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variables."
  :group 'lsp-faces)

(defface lsp-face-semhl-parameter
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameters."
  :group 'lsp-faces)

(defface lsp-face-semhl-label
  '((t (:inherit font-lock-comment-face)))
  "Face used for labels."
  :group 'lsp-faces)

(defface lsp-face-semhl-deprecated
  '((t :strike-through t))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-faces)

(defvar lsp-semantic-token-faces
  '(("comment" . lsp-face-semhl-comment)
    ("keyword" . lsp-face-semhl-keyword)
    ("string" . lsp-face-semhl-string)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("operator" . lsp-face-semhl-operator)
    ("namespace" . lsp-face-semhl-namespace)
    ("type" . lsp-face-semhl-type)
    ("struct" . lsp-face-semhl-struct)
    ("class" . lsp-face-semhl-class)
    ("interface" . lsp-face-semhl-interface)
    ("enum" . lsp-face-semhl-enum)
    ("typeParameter" . lsp-face-semhl-type-parameter)
    ("function" . lsp-face-semhl-function)
    ("member" . lsp-face-semhl-member)
    ("property" . lsp-face-semhl-property)
    ("macro" . lsp-face-semhl-macro)
    ("variable" . lsp-face-semhl-variable)
    ("parameter" . lsp-face-semhl-parameter)
    ("label" . lsp-face-semhl-label)
    ("enumConstant" . lsp-face-semhl-constant)
    ("dependent" . lsp-face-semhl-type)
    ("concept" . lsp-face-semhl-interface))
  "Faces to use for semantic tokens.")

(defvar lsp-semantic-token-modifier-faces
  ;; TODO: add default definitions
  '(("declaration" . lsp-face-semhl-interface)
    ("deprecated" . lsp-face-semhl-deprecated)
    ("readonly" . lsp-face-semhl-constant))
  "Semantic tokens modifier faces.
Faces to use for semantic token modifiers if
`lsp-semantic-tokens-apply-modifiers' is non-nil.")

(defvar lsp-semantic-tokens-capabilities
  `((semanticTokens
     . ((dynamicRegistration . t)
        (requests . ((range . t) (full . t)))
        (tokenModifiers . ,(if lsp-semantic-tokens-apply-modifiers
                               (apply 'vector (mapcar #'car lsp-semantic-token-modifier-faces)) []))
        (tokenTypes . ,(apply 'vector (mapcar #'car lsp-semantic-token-faces)))
        (formats . ["relative"])))))

(defvar lsp--semantic-tokens-idle-timer nil)
(defvar-local lsp--semantic-tokens-cache nil)
(defvar-local lsp--semantic-tokens-teardown nil)
(defvar-local lsp--semantic-tokens-use-ranged-requests nil)

(defun lsp--semantic-tokens-request (region fontify-immediately)
  "Request server for semantic tokens.
If REGION is non-nil, it will request tokens only for given region
otherwise it will request for whole document.
If FONTIFY-IMMEDIATELY is non-nil, it will fontify when receive the response
ignoring the timer."
  (let ((request-full-token-set
         (lambda (fontify-immediately)
           (when lsp--semantic-tokens-idle-timer
             (cancel-timer lsp--semantic-tokens-idle-timer))
           (setq lsp--semantic-tokens-idle-timer
                 (run-with-idle-timer
                  lsp-idle-delay
                  nil
                  (lambda () (lsp--semantic-tokens-request nil fontify-immediately)))))))
    (when lsp--semantic-tokens-idle-timer
      (cancel-timer lsp--semantic-tokens-idle-timer))
    (lsp-request-async
     (cond
      (region "textDocument/semanticTokens/range")
      ((lsp-feature? "textDocument/semanticTokensFull")
       "textDocument/semanticTokens/full")
      (t "textDocument/semanticTokens"))
     `( :textDocument ,(lsp--text-document-identifier)
        ,@(if region (list :range (lsp--region-to-range (car region) (cdr region))) '()))
     (lambda (response)
       (setq lsp--semantic-tokens-cache response)
       (lsp-put lsp--semantic-tokens-cache :_documentVersion lsp--cur-version)
       (lsp-put lsp--semantic-tokens-cache :_region region)
       (when fontify-immediately (font-lock-flush))
       ;; request full token set to improve fontification speed when scrolling
       (when region (funcall request-full-token-set nil)))
     :error-handler (lambda (&rest _) (funcall request-full-token-set t))
     :mode 'tick
     :cancel-token (format "semantic-tokens-%s" (lsp--buffer-uri)))))

(defun lsp-semantic-tokens--fontify (old-fontify-region beg end &optional loudly)
  "Apply fonts to retrieved semantic tokens.
OLD-FONTIFY-REGION is the region where it was applied before.
BEG and END are the regtions.
If LOUDLY is non-nil, it will build whole tokens."
  ;; TODO: support multiple language servers per buffer?
  (let ((faces (seq-some #'lsp--workspace-semantic-tokens-faces lsp--buffer-workspaces))
        (modifier-faces
         (when lsp-semantic-tokens-apply-modifiers
           (seq-some #'lsp--workspace-semantic-tokens-modifier-faces lsp--buffer-workspaces))))
    (if (or (eq nil lsp--semantic-tokens-cache)
            (eq nil faces)
            ;; delay fontification until we have fresh tokens
            (not (= lsp--cur-version (lsp-get lsp--semantic-tokens-cache :_documentVersion))))
        '(jit-lock-bounds 0 . 0)
      (funcall old-fontify-region beg end loudly)
      (-let* ((inhibit-field-text-motion t)
              ((&SematicTokensPartialResult :data) lsp--semantic-tokens-cache)
              (i0 0)
              (i-max (1- (length data)))
              (current-line 1)
              (line-delta)
              (column 0)
              (face)
              (line-start-pos)
              (line-min)
              (line-max-inclusive)
              (text-property-beg)
              (text-property-end))
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (goto-char beg)
            (goto-char (line-beginning-position))
            (setq line-min (line-number-at-pos))
            (with-silent-modifications
              (goto-char end)
              (goto-char (line-end-position))
              (setq line-max-inclusive (line-number-at-pos))
              (forward-line (- line-min line-max-inclusive))
              (let ((skip-lines (- line-min current-line)))
                (while (and (<= i0 i-max) (< (aref data i0) skip-lines))
                  (setq skip-lines (- skip-lines (aref data i0)))
                  (setq i0 (+ i0 5)))
                (setq current-line (- line-min skip-lines)))
              (forward-line (- current-line line-min))
              (setq line-start-pos (point))
              (cl-loop
               for i from i0 to i-max by 5 do
               (setq line-delta (aref data i))
               (unless (= line-delta 0)
                 (forward-line line-delta)
                 (setq line-start-pos (point))
                 (setq column 0)
                 (setq current-line (+ current-line line-delta)))
               (setq column (+ column (aref data (1+ i))))
               (setq face (aref faces (aref data (+ i 3))))
               (setq text-property-beg (+ line-start-pos column))
               (setq text-property-end (+ text-property-beg (aref data (+ i 2))))
               (when face (put-text-property text-property-beg text-property-end 'face face))
               (cl-loop for j from 0 to (1- (length modifier-faces)) do
                        (when (and (aref modifier-faces j)
                                   (> (logand (aref data (+ i 4)) (lsh 1 j)) 0))
                          (add-face-text-property text-property-beg text-property-end
                                                  (aref modifier-faces j))))
               when (> current-line line-max-inclusive) return nil)))))
      (let ((token-region (lsp-get lsp--semantic-tokens-cache :_region)))
        (if token-region
            `(jit-lock-bounds ,(max beg (car token-region)) . ,(min end (cdr token-region)))
          `(jit-lock-bounds ,beg . ,end))))))

(defun lsp-semantic-tokens--request-update ()
  "Request semantic-tokens update."
  (lsp--semantic-tokens-request
   (when lsp--semantic-tokens-use-ranged-requests
     (cons (window-start) (window-end)))
   t))

;;;###autoload
(defun lsp--semantic-tokens-initialize-buffer (is-range-provider)
  "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests."
  (let* ((old-extend-region-functions font-lock-extend-region-functions)
         ;; make sure font-lock always fontifies entire lines (TODO: do we also have
         ;; to change some jit-lock-...-region functions/variables?)
         (new-extend-region-functions
          (if (memq 'font-lock-extend-region-wholelines old-extend-region-functions)
              old-extend-region-functions
            (cons 'font-lock-extend-region-wholelines old-extend-region-functions))))
    (setq lsp--semantic-tokens-use-ranged-requests is-range-provider)
    (setq font-lock-extend-region-functions new-extend-region-functions)
    (add-function :around (local 'font-lock-fontify-region-function) #'lsp-semantic-tokens--fontify)
    (add-hook 'lsp-on-change-hook #'lsp-semantic-tokens--request-update nil t)
    (lsp-semantic-tokens--request-update)
    (setq lsp--semantic-tokens-teardown
          (lambda ()
            (setq font-lock-extend-region-functions old-extend-region-functions)
            (when lsp--semantic-tokens-idle-timer
              (cancel-timer lsp--semantic-tokens-idle-timer)
              (setq lsp--semantic-tokens-idle-timer nil))
            (setq lsp--semantic-tokens-use-ranged-requests nil)
            (setq lsp--semantic-tokens-cache nil)
            (remove-function (local 'font-lock-fontify-region-function)
                             #'lsp-semantic-tokens--fontify)
            (remove-hook 'lsp-on-change-hook #'lsp-semantic-tokens--request-update t)))))

(defun lsp--semantic-tokens-build-face-map (identifiers faces category varname)
  "Build map of FACES for IDENTIFIERS using CATEGORY and VARNAME."
  (apply 'vector
         (mapcar (lambda (id)
                   (let ((maybe-face (cdr (assoc id faces))))
                     (when (and lsp-semantic-tokens-warn-on-missing-face (not maybe-face))
                       (lsp-warn "No face has been associated to the %s '%s': consider adding a corresponding definition to %s"
                                 category id varname)) maybe-face)) identifiers)))

;;;###autoload
(defun lsp--semantic-tokens-initialize-workspace (workspace)
  "Initialize semantic tokens for WORKSPACE."
  (cl-assert workspace)
  (when-let ((token-capabilities
              (or
               (-some->
                   (lsp--registered-capability "textDocument/semanticTokens")
                 (lsp--registered-capability-options))
               (lsp:server-capabilities-semantic-tokens-provider?
                (lsp--workspace-server-capabilities workspace)))))
    (-let* (((&SemanticTokensOptions :legend) token-capabilities))
      (setf (lsp--workspace-semantic-tokens-faces workspace)
            (lsp--semantic-tokens-build-face-map (lsp:semantic-tokens-legend-token-types legend)
                                                 lsp-semantic-token-faces
                                                 "semantic token"
                                                 "lsp-semantic-token-faces"))
      (setf (lsp--workspace-semantic-tokens-modifier-faces workspace)
            (lsp--semantic-tokens-build-face-map (lsp:semantic-tokens-legend-token-modifiers legend)
                                                 lsp-semantic-token-modifier-faces
                                                 "semantic token modifier"
                                                 "lsp-semantic-token-modifier-faces")))))

;;;###autoload
(defun lsp-semantic-tokens--warn-about-deprecated-setting ()
  "Warn about deprecated semantic highlighting variable."
  (when (boundp 'lsp-semantic-highlighting)
    (pcase lsp-semantic-highlighting
      (:semantic-tokens
       (lsp-warn "It seems you wish to use semanticTokens-based
 highlighting. To do so, please remove any references to the
 deprecated variable `lsp-semantic-highlighting' from your
 configuration and set `lsp-semantic-tokens-enable' to `t'
 instead.")
       (setq lsp-semantic-tokens-enable t))
      ((or :immediate :deferred)
       (lsp-warn "It seems you wish to use Theia-based semantic
 highlighting. This protocol has been superseded by the
 semanticTokens protocol specified by LSP v3.16 and is no longer
 supported by lsp-mode. If your language server provides
 semanticToken support, please set
 `lsp-semantic-tokens-enable' to `t' to use it.")))))

;;;###autoload
(defun lsp-semantic-tokens--enable ()
  "Enable semantic tokens mode."
  (when (and lsp-semantic-tokens-enable
             (lsp-feature? "textDocument/semanticTokens"))
    (lsp-semantic-tokens--warn-about-deprecated-setting)
    (lsp-semantic-tokens-mode 1)
    (mapc #'lsp--semantic-tokens-initialize-workspace
          (lsp--find-workspaces-for "textDocument/semanticTokens"))
    (lsp--semantic-tokens-initialize-buffer
     (lsp-feature? "textDocument/semanticTokensRangeProvider"))))

(defun lsp-semantic-tokens--disable ()
  "Disable semantic tokens mode."
  (lsp-semantic-tokens-mode -1))

;;;###autoload
(define-minor-mode lsp-semantic-tokens-mode
  "Toggle semantic-tokens support."
  :group 'lsp-mode
  :global nil
  (cond
   (lsp-semantic-tokens-mode
    (add-hook 'lsp-configure-hook #'lsp-semantic-tokens--enable nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-semantic-tokens--disable nil t))
   (t
    (remove-hook 'lsp-configure-hook #'lsp-semantic-tokens--enable t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-semantic-tokens--disable t)
    (setq lsp--semantic-tokens-idle-timer nil
          lsp--semantic-tokens-cache nil
          lsp--semantic-tokens-teardown nil
          lsp--semantic-tokens-use-ranged-requests nil))))

(provide 'lsp-semantic-tokens)
;;; lsp-semantic-tokens.el ends here
