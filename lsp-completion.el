;;; lsp-completion.el --- LSP completion -*- lexical-binding: t; -*-
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
;;  LSP completion
;;
;;; Code:

(require 'lsp-mode)

(defcustom lsp-prefer-capf nil
  "Prefer capf."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-completion-enable-additional-text-edit t
  "Whether or not to apply additional text edit when performing completion.

If set to non-nil, `lsp-mode' will apply additional text edits
from the server.  Otherwise, the additional text edits are
ignored."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-completion-show-kind t
  "Whether or not to show kind of completion candidates."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-completion-show-detail t
  "Whether or not to show detail of completion candidates."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-completion-no-cache nil
  "Whether or not caching the returned completions from server."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-completion-filter-on-incomplete t
  "Whether or not filter incomplete results."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "7.0.1"))

(defconst lsp--completion-item-kind
  [nil
   "Text"
   "Method"
   "Function"
   "Constructor"
   "Field"
   "Variable"
   "Class"
   "Interface"
   "Module"
   "Property"
   "Unit"
   "Value"
   "Enum"
   "Keyword"
   "Snippet"
   "Color"
   "File"
   "Reference"
   "Folder"
   "EnumMember"
   "Constant"
   "Struct"
   "Event"
   "Operator"
   "TypeParameter"])

(defvar yas-indent-line)
(defvar company-backends)

(declare-function company-mode "ext:company")
(declare-function company-doc-buffer "ext:company")
(declare-function yas-expand-snippet "ext:yasnippet")

(defun lsp--make-completion-item (item &rest plist)
  "Make completion item from lsp ITEM and PLIST."
  (-let (((&CompletionItem :label
                           :insert-text?
                           :sort-text?
                           :_emacsStartPoint start-point)
          item)
         ((&plist :markers :prefix) plist))
    (propertize (or label insert-text?)
                'lsp-completion-item item
                'lsp-sort-text sort-text?
                'lsp-completion-start-point start-point
                'lsp-completion-markers markers
                'lsp-completion-prefix prefix)))

(defun lsp--annotate (item)
  "Annotate ITEM detail."
  (-let (((&CompletionItem :detail? :kind?) (plist-get (text-properties-at 0 item)
                                                       'lsp-completion-item)))
    (concat (when (and lsp-completion-show-detail detail?)
              (concat " " (s-replace "\r" "" detail?)))
            (when lsp-completion-show-kind
              (when-let (kind-name (and kind? (aref lsp--completion-item-kind kind?)))
                (format " (%s)" kind-name))))))

(defun lsp--looking-back-trigger-characterp (trigger-characters)
  "Return trigger character if text before point match any of the TRIGGER-CHARACTERS."
  (unless (= (point) (point-at-bol))
    (seq-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defvar lsp--capf-cache nil
  "Cached candidates for completion at point function.
In the form of list (prefix prefix-pos items :lsp-items ...).
When the completion is incomplete, cache contains value of `incomplete'.")

(defun lsp--capf-clear-cache (&rest _)
  "Clear completion caches."
  (-some-> (and (listp lsp--capf-cache) lsp--capf-cache)
    (cddr)
    (cdr)
    (plist-get :markers)
    (cl-second)
    (set-marker nil))
  (setq lsp--capf-cache nil))

(lsp-defun lsp--capf-guess-prefix ((item &as &CompletionItem :text-edit?))
  "Guess ITEM's prefix start point according to following heuristics:
- If `textEdit' exists, use insertion range start as prefix start point.
- Else, find the point before current point is longest prefix match of
`insertText' or `label'. And:
  - The character before prefix is not word constitute
Return `nil' when fails to guess prefix."
  (cond
   (text-edit?
    (lsp--position-to-point (lsp:range-start (lsp:text-edit-range text-edit?))))
   (t
    (-let* (((&CompletionItem :label :insert-text?) item)
            (text (or insert-text? label))
            (point (point))
            (start (max 1 (- point (length text))))
            (char-before (char-before start))
            start-point)
      (while (and (< start point) (not start-point))
        (unless (or (and char-before (equal (char-syntax char-before) ?w))
                    (not (string-prefix-p (buffer-substring-no-properties start point)
                                          text)))
          (setq start-point start))
        (cl-incf start)
        (setq char-before (char-before start)))
      start-point))))

(defun lsp--capf-client-items (items)
  "Convert ITEMS into client items form."
  (--> items
       (-map (-lambda ((item &as &CompletionItem
                             :label
                             :filter-text?
                             :_emacsStartPoint start-point
                             :score?))
               (propertize (or filter-text? label)
                           'lsp-completion-item item
                           'lsp-completion-start-point start-point
                           'lsp-completion-score score?))
             it)))

(defvar lsp--capf-no-reordering nil
  "Dont do client-side reordering completion items when set.")

(cl-defun lsp--capf-filter-candidates (items
                                       &rest plist
                                       &key lsp-items
                                       &allow-other-keys)
  "List all possible completions in cached ITEMS with their prefixes.
We can pass LSP-ITEMS, which will be used when there's no cache.
Also, additional data to attached to each candidate can be passed via PLIST."
  (lsp--while-no-input
   (->>
    (if items
        (-->
         (let (queries fuz-queries)
           (-keep (lambda (cand)
                    (let* ((start-point (get-text-property 0 'lsp-completion-start-point cand))
                           (query (or (plist-get queries start-point)
                                      (let ((s (buffer-substring-no-properties
                                                start-point (point))))
                                        (setq queries (plist-put queries start-point s))
                                        s)))
                           (fuz-query (or (plist-get fuz-queries start-point)
                                          (let ((s (lsp--regex-fuzzy query)))
                                            (setq fuz-queries
                                                  (plist-put fuz-queries start-point s))
                                            s))))
                      (when (string-match fuz-query cand)
                        (put-text-property 0 1 'match-data (match-data) cand)
                        (put-text-property 0 1 'sort-score
                                           (* (or (lsp--fuzzy-score query cand) 1e-05)
                                              (or (get-text-property 0 'lsp-completion-score cand)
                                                  0.001))
                                           cand)
                        cand)))
                  items))
         (if lsp--capf-no-reordering
             it
           (sort it (lambda (o1 o2)
                      (> (get-text-property 0 'sort-score o1)
                         (get-text-property 0 'sort-score o2)))))
         ;; TODO: pass additional function to sort the candidates
         (-map (-partial #'get-text-property 0 'lsp-completion-item) it))
      lsp-items)
    (-map (lambda (item) (apply #'lsp--make-completion-item item plist))))))

(defun lsp--capf-company-match (candidate)
  "Return highlight of typed prefix inside CANDIDATE."
  (let* ((prefix (downcase
                  (buffer-substring-no-properties
                   (plist-get (text-properties-at 0 candidate) 'lsp-completion-start-point)
                   (point))))
         (prefix-len (length prefix))
         (prefix-pos 0)
         (label (downcase candidate))
         (label-len (length label))
         (label-pos 0)
         matches start)
    (while (and (not matches)
                (< prefix-pos prefix-len))
      (while (and (< prefix-pos prefix-len)
                  (< label-pos label-len))
        (if (equal (aref prefix prefix-pos) (aref label label-pos))
            (progn
              (unless start (setq start label-pos))
              (cl-incf prefix-pos))
          (when start
            (setq matches (nconc matches `((,start . ,label-pos))))
            (setq start nil)))
        (cl-incf label-pos))
      (when start (setq matches (nconc matches `((,start . ,label-pos)))))
      ;; Search again when the whole prefix is not matched
      (when (< prefix-pos prefix-len)
        (setq matches nil))
      ;; Start search from next offset of prefix to find a match with label
      (unless matches
        (cl-incf prefix-pos)
        (setq label-pos 0)))
    matches))

(defun lsp--capf-get-documentation (item)
  "Get doc comment for completion ITEM."
  (unless (get-text-property 0 'lsp-completion-resolved item)
    (let ((resolved-item
           (-some->> item
             (get-text-property 0 'lsp-completion-item)
             (lsp--resolve-completion)))
          (len (length item)))
      (put-text-property 0 len 'lsp-completion-item resolved-item item)
      (put-text-property 0 len 'lsp-completion-resolved t item)))
  (-some->> item
    (get-text-property 0 'lsp-completion-item)
    (lsp:completion-item-documentation?)
    (lsp--render-element)))

(defun lsp--capf-get-context (trigger-characters)
  "Get completion context with provided TRIGGER-CHARACTERS."
  (let* (trigger-char
         (trigger-kind (cond
                        ((setq trigger-char (lsp--looking-back-trigger-characterp
                                             trigger-characters))
                         lsp/completion-trigger-kind-trigger-character)
                        ((equal lsp--capf-cache 'incomplete)
                         lsp/completion-trigger-kind-trigger-for-incomplete-completions)
                        (t lsp/completion-trigger-kind-invoked))))
    (apply #'lsp-make-completion-context
           (nconc
            `(:trigger-kind ,trigger-kind)
            (when trigger-char
              `(:trigger-character? ,trigger-char))))))

(defun lsp--sort-completions (completions)
  "Sort COMPLETIONS."
  (sort
   completions
   (-lambda ((&CompletionItem :sort-text? sort-text-left :label label-left)
             (&CompletionItem :sort-text? sort-text-right :label label-right))
     (if (equal sort-text-left sort-text-right)
         (string-lessp label-left label-right)
       (string-lessp sort-text-left sort-text-right)))))

;;;###autoload
(defun lsp-completion-at-point ()
  "Get lsp completions."
  (when (or (--some (lsp--client-completion-in-comments? (lsp--workspace-client it))
                    (lsp-workspaces))
            (not (nth 4 (syntax-ppss))))
    (let* ((trigger-chars (->> (lsp--server-capabilities)
                               (lsp:server-capabilities-completion-provider?)
                               (lsp:completion-options-trigger-characters?)))
           (bounds-start (or (-some--> (car (bounds-of-thing-at-point 'symbol))
                               (save-excursion
                                 (ignore-errors
                                   (goto-char (+ it 1))
                                   (while (lsp--looking-back-trigger-characterp trigger-chars)
                                     (cl-incf it)
                                     (forward-char))
                                   it)))
                             (point)))
           result done?
           (all-completions
            (lambda ()
              (cond
               (done? result)
               ((and (not lsp-completion-no-cache)
                     lsp--capf-cache
                     (listp lsp--capf-cache)
                     (equal (cl-second lsp--capf-cache) bounds-start)
                     (s-prefix? (car lsp--capf-cache)
                                (buffer-substring-no-properties bounds-start (point))))
                (apply #'lsp--capf-filter-candidates (cddr lsp--capf-cache)))
               (t
                (-let* ((resp (lsp-request-while-no-input
                               "textDocument/completion"
                               (plist-put (lsp--text-document-position-params)
                                          :context (lsp--capf-get-context trigger-chars))))
                        (completed (or (and resp (not (lsp-completion-list? resp)))
                                       (not (lsp:completion-list-is-incomplete resp))))
                        (items (lsp--while-no-input
                                (--> (cond
                                      ((lsp-completion-list? resp) (lsp:completion-list-items resp))
                                      (t resp))
                                     (if (or completed
                                             (seq-some #'lsp:completion-item-sort-text? it))
                                         (lsp--sort-completions it)
                                       it)
                                     (-map (lambda (item)
                                             (lsp-put item
                                                      :_emacsStartPoint
                                                      (or (lsp--capf-guess-prefix item)
                                                          bounds-start)))
                                           it))))
                        (markers (list bounds-start (copy-marker (point) t)))
                        (prefix (buffer-substring-no-properties bounds-start (point)))
                        (lsp--capf-no-reordering t))
                  (lsp--capf-clear-cache)
                  (setf done? completed
                        lsp--capf-cache (cond
                                         ((and done? (not (seq-empty-p items)))
                                          (list (buffer-substring-no-properties bounds-start (point))
                                                bounds-start
                                                (lsp--capf-client-items items)
                                                :lsp-items nil
                                                :markers markers
                                                :prefix prefix))
                                         ((not done?) 'incomplete))
                        result (lsp--capf-filter-candidates
                                (cond (done?
                                       (cl-third lsp--capf-cache))
                                      (lsp-completion-filter-on-incomplete
                                       (lsp--capf-client-items items)))
                                :lsp-items items
                                :markers markers
                                :prefix prefix))))))))
      (list
       bounds-start
       (point)
       (lambda (probe _pred action)
         (cond
          ;; metadata
          ((equal action 'metadata)
           `(metadata (category . lsp-capf)
                      (display-sort-function . identity)))
          ;; boundaries
          ((equal (car-safe action) 'boundaries) nil)
          ;; try-completion
          ((null action) (cl-first (member probe (funcall all-completions))))
          ;; test-completion
          ((equal action 'lambda) (member probe (funcall all-completions)))
          ;; retrieve candidates
          (t (funcall all-completions))))
       :annotation-function #'lsp--annotate
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char bounds-start)
         (and (lsp--looking-back-trigger-characterp trigger-chars) t))
       :company-match #'lsp--capf-company-match
       :company-doc-buffer (-compose #'company-doc-buffer
                                     #'lsp--capf-get-documentation)
       :exit-function
       (-rpartial #'lsp--capf-exit-fn trigger-chars)))))

(defun lsp--capf-exit-fn (candidate _status &optional trigger-chars)
  "Exit function of `completion-at-point'.
CANDIDATE is the selected completion item.
Others: TRIGGER-CHARS"
  (unwind-protect
      (-let* (((&plist 'lsp-completion-item item
                       'lsp-completion-start-point start-point
                       'lsp-completion-markers markers
                       'lsp-completion-prefix prefix)
               (text-properties-at 0 candidate))
              ((&CompletionItem :label :insert-text? :text-edit? :insert-text-format? :additional-text-edits?)
               item))
        (cond
         (text-edit?
          (apply #'delete-region markers)
          (insert prefix)
          (lsp--apply-text-edit text-edit?))
         ((or insert-text? label)
          (apply #'delete-region markers)
          (insert prefix)
          (delete-region start-point (point))
          (insert (or insert-text? label))))

        (when (eq insert-text-format? 2)
          (let ((yas-indent-line (lsp--indent-snippets?)))
            (yas-expand-snippet
             (lsp--to-yasnippet-snippet (buffer-substring start-point (point)))
             start-point
             (point))))

        (when lsp-completion-enable-additional-text-edit
          (if (or (get-text-property 0 'lsp-completion-resolved candidate)
                  additional-text-edits?)
              (lsp--apply-text-edits additional-text-edits?)
            (-let [(callback cleanup-fn) (lsp--create-apply-text-edits-handlers)]
              (lsp--resolve-completion-async
               item
               (lambda (resolved-item)
                 (funcall callback
                          (lsp:completion-item-additional-text-edits? resolved-item)))
               cleanup-fn))))

        (when (and lsp-signature-auto-activate
                   (lsp-feature? "textDocument/signatureHelp"))
          (lsp-signature-activate))

        (setq-local lsp-inhibit-lsp-hooks nil)

        (when (lsp--looking-back-trigger-characterp trigger-chars)
          (setq this-command 'self-insert-command)))
    (lsp--capf-clear-cache)))

(defun lsp-completion--setup-company ()
  "Setup company-mode."
  (cond
   ((and (functionp 'company-lsp)
         (not lsp-prefer-capf))
    (progn
      (company-mode 1)
      (add-to-list 'company-backends 'company-lsp)
      (setq-local company-backends (remove 'company-capf company-backends))))

   ((and (fboundp 'company-mode) lsp-enable-completion-at-point)
    (company-mode 1)
    (add-to-list 'company-backends 'company-capf)))

  (add-hook 'company-completion-started-hook
            (lambda (&rest _)
              (setq-local lsp-inhibit-lsp-hooks t))
            nil
            t)
  (add-hook 'company-after-completion-hook
            (lambda (&rest _)
              (lsp--capf-clear-cache)
              (setq-local lsp-inhibit-lsp-hooks nil))
            nil
            t))

(defun lsp-completion--clean-company ()
  "Clean company-mode."
  (remove-hook 'company-completion-started-hook
               (lambda (&rest _)
                 (setq-local lsp-inhibit-lsp-hooks t))
               t)
  (remove-hook 'company-after-completion-hook
               (lambda (&rest _)
                 (lsp--capf-clear-cache)
                 (setq-local lsp-inhibit-lsp-hooks nil))
               t))

(defun lsp-completion--enable ()
  "Enable LSP completion support."
  (when (and lsp-enable-completion-at-point
             (lsp-feature? "textDocument/completion"))
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)
    (setq-local completion-category-defaults
                (add-to-list 'completion-category-defaults '(lsp-capf (styles basic))))
    (lsp-completion-mode 1)))

(defun lsp-completion--disable ()
  "Disable LSP completion support."
  (lsp-completion-mode -1)
  (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
  (setq-local completion-category-defaults
              (cl-remove 'lsp-capf completion-category-defaults :key #'car)))

;;;###autoload
(define-minor-mode lsp-completion-mode
  "Toggle LSP completion support."
  :group 'lsp-mode
  :global nil
  :lighter ""
  (cond
   (lsp-completion-mode
    (when (bound-and-true-p company-mode)
      (add-hook 'lsp-configure-hook #'lsp-completion--enable nil t)
      (add-hook 'lsp-unconfigure-hook #'lsp-completion--disable nil t)
      (lsp-completion--setup-company)))
   (t
    (when (bound-and-true-p company-mode)
      (remove-hook 'lsp-configure-hook #'lsp-completion--enable t)
      (remove-hook 'lsp-unconfigure-hook #'lsp-completion--disable t)
      (lsp-completion--clean-company)))))

(provide 'lsp-completion)
;;; lsp-completion.el ends here
