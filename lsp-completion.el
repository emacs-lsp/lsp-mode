;;; lsp-completion.el --- LSP completion -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2025 emacs-lsp maintainers
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

(defgroup lsp-completion nil
  "LSP support for completion."
  :prefix "lsp-completion-"
  :group 'lsp-mode
  :tag "LSP Completion")

;;;###autoload
(define-obsolete-variable-alias 'lsp-prefer-capf
  'lsp-completion-provider  "lsp-mode 7.0.1")

(defcustom lsp-completion-provider :capf
  "The completion backend provider."
  :type '(choice
          (const :tag "Use company-capf" :capf)
          (const :tag "None" :none))
  :group 'lsp-completion
  :package-version '(lsp-mode . "7.0.1"))

;;;###autoload
(define-obsolete-variable-alias 'lsp-enable-completion-at-point
  'lsp-completion-enable "lsp-mode 7.0.1")

;;;###autoload
(defcustom lsp-completion-enable t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-completion)

(defcustom lsp-completion-enable-additional-text-edit t
  "Whether or not to apply additional text edit when performing completion.

If set to non-nil, `lsp-mode' will apply additional text edits
from the server.  Otherwise, the additional text edits are
ignored."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-completion-show-kind t
  "Whether or not to show kind of completion candidates."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-completion-show-detail t
  "Whether or not to show detail of completion candidates."
  :type 'boolean
  :group 'lsp-completion)

(defcustom lsp-completion-no-cache nil
  "Whether or not caching the returned completions from server."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-completion-filter-on-incomplete t
  "Whether or not filter incomplete results."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-completion-sort-initial-results t
  "Whether or not filter initial results from server."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-completion-use-last-result t
  "Temporarily use last server result when interrupted by keyboard.
This will help minimize popup flickering issue in `company-mode'."
  :type 'boolean
  :group 'lsp-completion
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-completion-default-behaviour :replace
  "Default behaviour of `InsertReplaceEdit'."
  :type '(choice
          (const :tag "Default completion inserts" :insert)
          (const :tag "Default completion replaces" :replace))
  :group 'lsp-completion
  :package-version '(lsp-mode . "8.0.0"))

(defconst lsp-completion--item-kind
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
(defvar company-abort-on-unique-match)

(defvar lsp-completion--no-reordering nil
  "Dont do client-side reordering completion items when set.")

(declare-function company-mode "ext:company")
(declare-function yas-expand-snippet "ext:yasnippet")

(defun lsp-doc-buffer (&optional string)
  "Return doc for STRING."
  (with-current-buffer (get-buffer-create "*lsp-documentation*")
    (erase-buffer)
    (fundamental-mode)
    (when string
      (save-excursion
        (insert string)
        (visual-line-mode)))
    (current-buffer)))

(defun lsp-falsy? (val)
  "Non-nil if VAL is falsy."
  ;; https://developer.mozilla.org/en-US/docs/Glossary/Falsy
  (or (not val) (equal val "") (equal val 0)))

(cl-defun lsp-completion--make-item (item &key markers prefix)
  "Make completion item from lsp ITEM and with MARKERS and PREFIX."
  (-let (((&CompletionItem :label
                           :sort-text?
                           :_emacsStartPoint start-point)
          item))
    (propertize label
                'lsp-completion-unresolved-item item
                'lsp-completion-item item
                'lsp-sort-text sort-text?
                'lsp-completion-start-point start-point
                'lsp-completion-markers markers
                'lsp-completion-prefix prefix)))

(defun lsp-completion--fix-resolve-data (item)
  "Patch `CompletionItem' ITEM for rust-analyzer otherwise resolve will fail.
See #2675"
  (let ((data (lsp:completion-item-data? item)))
    (when (lsp-member? data :import_for_trait_assoc_item)
      (unless (lsp-get data :import_for_trait_assoc_item)
        (lsp-put data :import_for_trait_assoc_item :json-false)))
    (when (lsp-member? data :for_ref)
      (unless (lsp-get data :for_ref)
         (lsp-put data :for_ref :json-false)))))

(defun lsp-completion--resolve (item)
  "Resolve completion ITEM.
ITEM can be string or a CompletionItem"
  (cl-assert item nil "Completion item must not be nil")
  (-let (((completion-item . resolved)
          (pcase item
            ((pred stringp) (cons (get-text-property 0 'lsp-completion-item item)
                                  (get-text-property 0 'lsp-completion-resolved item)))
            (_ (cons item nil)))))
    (if resolved item
      (lsp-completion--fix-resolve-data completion-item)
      (setq completion-item
            (or (ignore-errors
                  (when (lsp-feature? "completionItem/resolve")
                    (lsp-request "completionItem/resolve"
                                 (lsp-delete (lsp-copy completion-item) :_emacsStartPoint))))
                completion-item))
      (pcase item
        ((pred stringp)
         (let ((len (length item)))
           (put-text-property 0 len 'lsp-completion-item completion-item item)
           (put-text-property 0 len 'lsp-completion-resolved t item)
           item))
        (_ completion-item)))))

(defun lsp-completion--resolve-async (item callback &optional cleanup-fn)
  "Resolve completion ITEM asynchronously with CALLBACK.
The CLEANUP-FN will be called to cleanup."
  (cl-assert item nil "Completion item must not be nil")
  (-let (((completion-item . resolved)
          (pcase item
            ((pred stringp) (cons (get-text-property 0 'lsp-completion-item item)
                                  (get-text-property 0 'lsp-completion-resolved item)))
            (_ (cons item nil)))))
    (ignore-errors
      (if (and (lsp-feature? "completionItem/resolve") (not resolved))
          (progn
            (lsp-completion--fix-resolve-data completion-item)
            (lsp-request-async "completionItem/resolve"
                               (lsp-delete (lsp-copy completion-item) :_emacsStartPoint)
                               (lambda (completion-item)
                                 (when (stringp item)
                                   (let ((len (length item)))
                                     (put-text-property 0 len 'lsp-completion-item completion-item item)
                                     (put-text-property 0 len 'lsp-completion-resolved t item)
                                     item))
                                 (funcall callback completion-item)
                                 (when cleanup-fn (funcall cleanup-fn)))
                               :error-handler (lambda (err)
                                                (when cleanup-fn (funcall cleanup-fn))
                                                (error (lsp:json-error-message err)))
                               :cancel-handler cleanup-fn
                               :mode 'alive))
        (funcall callback completion-item)
        (when cleanup-fn (funcall cleanup-fn))))))

(defun lsp-completion--get-label-detail (item &optional omit-description)
  "Construct label detail from completion item ITEM."
  (-let (((&CompletionItem :detail? :label-details?) item))
    (cond ((and label-details?
                (or (lsp:label-details-detail? label-details?)
                    (lsp:label-details-description? label-details?)))
           (-let (((&LabelDetails :detail? :description?) label-details?))
             (concat
              (unless (and detail? (string-prefix-p " " detail?))
                " ")
              (when detail?
                (s-replace "\r" "" detail?))
              (unless (or omit-description
                          (and description? (string-prefix-p " " description?)))
                " ")
              (unless omit-description
                description?))))
          (detail?
           (concat (unless (and detail? (string-prefix-p " " detail?))
                     " ")
                   (s-replace "\r" "" detail?))))))

(defun lsp-completion--annotate (cand)
  "Annotation function for completion candidate CAND.

Returns unresolved completion item detail."
  (when-let* ((lsp-completion-item (get-text-property 0 'lsp-completion-unresolved-item cand)))
    (concat
     (when lsp-completion-show-detail
       (lsp-completion--get-label-detail lsp-completion-item))
     (when lsp-completion-show-kind
       (when-let* ((kind? (lsp:completion-item-kind? lsp-completion-item))
                   (kind-name (and kind? (aref lsp-completion--item-kind kind?))))
         (format " (%s)" kind-name))))))

(defun lsp-completion--looking-back-trigger-characterp (trigger-characters)
  "Return character if text before point match any of the TRIGGER-CHARACTERS."
  (unless (= (point) (line-beginning-position))
    (seq-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defvar lsp-completion--cache nil
  "Cached candidates for completion at point function.
In the form of plist (prefix-pos items :lsp-items :prefix ...).
When the completion is incomplete, `items' contains value of :incomplete.")

(defvar lsp-completion--last-result nil
  "Last completion result.")

(defun lsp-completion--clear-cache (&optional keep-last-result)
  "Clear completion caches.
KEEP-LAST-RESULT if specified."
  (-some-> lsp-completion--cache
    (cddr)
    (plist-get :markers)
    (cl-second)
    (set-marker nil))
  (setq lsp-completion--cache nil)
  (unless keep-last-result (setq lsp-completion--last-result nil)))

(lsp-defun lsp-completion--guess-prefix ((item &as &CompletionItem :text-edit?))
  "Guess ITEM's prefix start point according to following heuristics:
- If `textEdit' exists, use insertion range start as prefix start point.
- Else, find the point before current point is longest prefix match of
`insertText' or `label'. And:
  - The character before prefix is not word constitute
Return `nil' when fails to guess prefix."
  (cond
   ((lsp-insert-replace-edit? text-edit?)
    (lsp--position-to-point (lsp:range-start (lsp:insert-replace-edit-insert text-edit?))))
   (text-edit?
    (lsp--position-to-point (lsp:range-start (lsp:text-edit-range text-edit?))))
   (t
    (-let* (((&CompletionItem :label :insert-text?) item)
            (text (or (unless (lsp-falsy? insert-text?) insert-text?) label))
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

(defun lsp-completion--to-internal (items)
  "Convert ITEMS into internal form."
  (--> items
    (-map (-lambda ((item &as &CompletionItem
                          :label
                          :filter-text?
                          :_emacsStartPoint start-point
                          :score?))
            `( :label ,(or (unless (lsp-falsy? filter-text?) filter-text?) label)
               :item ,item
               :start-point ,start-point
               :score ,score?))
          it)))

(cl-defun lsp-completion--filter-candidates (items &key
                                                   lsp-items
                                                   markers
                                                   prefix
                                                   &allow-other-keys)
  "List all possible completions in cached ITEMS with their prefixes.
We can pass LSP-ITEMS, which will be used when there's no cache.
The MARKERS and PREFIX value will be attached to each candidate."
  (lsp--while-no-input
    (->>
     (if items
         (--> (let (queries fuz-queries)
                (-keep (-lambda ((cand &as &plist :label :start-point :score))
                         (let* ((query (or (plist-get queries start-point)
                                           (let ((s (buffer-substring-no-properties
                                                     start-point (point))))
                                             (setq queries (plist-put queries start-point s))
                                             s)))
                                (fuz-query (or (plist-get fuz-queries start-point)
                                               (let ((s (lsp-completion--regex-fuz query)))
                                                 (setq fuz-queries
                                                       (plist-put fuz-queries start-point s))
                                                 s)))
                                (label-len (length label))
                                (case-fold-search completion-ignore-case))
                           (when (string-match fuz-query label)
                             (put-text-property 0 label-len 'match-data (match-data) label)
                             (plist-put cand
                                        :sort-score
                                        (* (or (lsp-completion--fuz-score query label) 1e-05)
                                           (or score 0.001)))
                             cand)))
                       items))
              (if lsp-completion--no-reordering
                  it
                (sort it (lambda (o1 o2)
                           (> (plist-get o1 :sort-score)
                              (plist-get o2 :sort-score)))))
              ;; TODO: pass additional function to sort the candidates
              (-map (-rpartial #'plist-get :item) it))
       lsp-items)
     (-map (lambda (item) (lsp-completion--make-item item
                                                     :markers markers
                                                     :prefix prefix))))))

(defconst lsp-completion--kind->symbol
  '((1 . text)
    (2 . method)
    (3 . function)
    (4 . constructor)
    (5 . field)
    (6 . variable)
    (7 . class)
    (8 . interface)
    (9 . module)
    (10 . property)
    (11 . unit)
    (12 . value)
    (13 . enum)
    (14 . keyword)
    (15 . snippet)
    (16 . color)
    (17 . file)
    (18 . reference)
    (19 . folder)
    (20 . enum-member)
    (21 . constant)
    (22 . struct)
    (23 . event)
    (24 . operator)
    (25 . type-parameter)))

(defun lsp-completion--candidate-kind (item)
  "Return ITEM's kind."
  (alist-get (lsp:completion-item-kind? (get-text-property 0 'lsp-completion-item item))
             lsp-completion--kind->symbol))

(defun lsp-completion--candidate-deprecated (item)
  "Return if ITEM is deprecated."
  (let ((completion-item (get-text-property 0 'lsp-completion-item item)))
    (or (lsp:completion-item-deprecated? completion-item)
        (seq-position (lsp:completion-item-tags? completion-item)
                      lsp/completion-item-tag-deprecated))))

(defun lsp-completion--company-match (candidate)
  "Return highlight of typed prefix inside CANDIDATE."
  (if-let* ((md (cddr (plist-get (text-properties-at 0 candidate) 'match-data))))
      (let (matches start end)
        (while (progn (setq start (pop md) end (pop md))
                      (and start end))
          (setq matches (nconc matches `((,start . ,end)))))
        matches)
    (let* ((prefix (downcase
                    (buffer-substring-no-properties
                     ;; Put a safe guard to prevent staled cache from setting a wrong start point #4192
                     (max (line-beginning-position)
                          (plist-get (text-properties-at 0 candidate) 'lsp-completion-start-point))
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
      matches)))

(defun lsp-completion--company-docsig (cand)
  "Signature for completion candidate CAND.

Returns resolved completion item details."
  (and (lsp-completion--resolve cand)
       (lsp-completion--get-label-detail
        (get-text-property 0 'lsp-completion-item cand)
        t)))

(defun lsp-completion--get-documentation (item)
  "Get doc comment for completion ITEM."
  (or (get-text-property 0 'lsp-completion-item-doc item)
      (-let* (((&CompletionItem :detail?
                                :documentation?)
               (get-text-property 0 'lsp-completion-item (lsp-completion--resolve item)))
              (doc
               (if (and detail? documentation?)
                   ;; detail was resolved, that means the candidate list has no
                   ;; detail, so we may need to prepend it to the documentation
                   (cond ((lsp-markup-content? documentation?)
                          (-let (((&MarkupContent :kind :value) documentation?))
                            (cond ((and (equal kind "plaintext")
                                        (not (string-match-p (regexp-quote detail?) value)))

                                   (lsp--render-string
                                    (concat detail?
                                            (if (bound-and-true-p page-break-lines-mode)
                                                "\n\n"
                                              "\n\n")
                                            value)
                                    kind))

                                  ((and (equal kind "markdown")
                                        (not (string-match-p (regexp-quote detail?) value)))

                                   (lsp--render-string
                                    (concat
                                     "```\n"
                                     detail?
                                     "\n```"
                                     "\n---\n"
                                     value)
                                    kind)))))

                         ((and (stringp documentation?)
                               (not (string-match-p (regexp-quote detail?) documentation?)))

                          (lsp--render-string
                           (concat detail?
                                   (if (bound-and-true-p page-break-lines-mode)
                                       "\n\n"
                                     "\n\n")
                                   documentation?)
                           "plaintext")))

                 (lsp--render-element documentation?))))

        (put-text-property 0 (length item) 'lsp-completion-item-doc doc item)
        doc)))

(defun lsp-completion--get-context (trigger-characters same-session?)
  "Get completion context with provided TRIGGER-CHARACTERS and SAME-SESSION?."
  (let* ((triggered-by-char non-essential)
         (trigger-char (when triggered-by-char
                         (lsp-completion--looking-back-trigger-characterp
                          trigger-characters)))
         (trigger-kind (cond
                        (trigger-char
                         lsp/completion-trigger-kind-trigger-character)
                        ((and same-session?
                              (equal (cl-second lsp-completion--cache) :incomplete))
                         lsp/completion-trigger-kind-trigger-for-incomplete-completions)
                        (t lsp/completion-trigger-kind-invoked))))
    (apply #'lsp-make-completion-context
           (nconc
            `(:trigger-kind ,trigger-kind)
            (when trigger-char
              `(:trigger-character? ,trigger-char))))))

(defun lsp-completion--sort-completions (completions)
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
    (let* ((trigger-chars (-> (lsp--capability-for-method "textDocument/completion")
                              (lsp:completion-options-trigger-characters?)))
           (bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                             (point)))
           result done?
           (candidates
            (lambda ()
              (lsp--catch 'input
                  (let ((lsp--throw-on-input lsp-completion-use-last-result)
                        (same-session? (and lsp-completion--cache
                                            ;; Special case for empty prefix and empty result
                                            (or (cl-second lsp-completion--cache)
                                                (not (string-empty-p
                                                      (plist-get (cddr lsp-completion--cache) :prefix))))
                                            (equal (cl-first lsp-completion--cache) bounds-start)
                                            (s-prefix?
                                             (plist-get (cddr lsp-completion--cache) :prefix)
                                             (buffer-substring-no-properties bounds-start (point))))))
                    (cond
                     ((or done? result) result)
                     ((and (not lsp-completion-no-cache)
                           same-session?
                           (listp (cl-second lsp-completion--cache)))
                      (setf result (apply #'lsp-completion--filter-candidates
                                          (cdr lsp-completion--cache))))
                     (t
                      (-let* ((resp (lsp-request-while-no-input
                                     "textDocument/completion"
                                     (plist-put (lsp--text-document-position-params)
                                                :context (lsp-completion--get-context trigger-chars same-session?))))
                              (completed (and resp
                                              (not (and (lsp-completion-list? resp)
                                                        (lsp:completion-list-is-incomplete resp)))))
                              (items (lsp--while-no-input
                                       (--> (cond
                                             ((lsp-completion-list? resp)
                                              (lsp:completion-list-items resp))
                                             (t resp))
                                            (if (or completed
                                                    (seq-some #'lsp:completion-item-sort-text? it))
                                                (lsp-completion--sort-completions it)
                                              it)
                                            (-map (lambda (item)
                                                    (lsp-put item
                                                             :_emacsStartPoint
                                                             (or (lsp-completion--guess-prefix item)
                                                                 bounds-start)))
                                                  it))))
                              (markers (list bounds-start (copy-marker (point) t)))
                              (prefix (buffer-substring-no-properties bounds-start (point)))
                              (lsp-completion--no-reordering (not lsp-completion-sort-initial-results)))
                        (lsp-completion--clear-cache same-session?)
                        (setf done? completed
                              lsp-completion--cache (list bounds-start
                                                          (cond
                                                           ((and done? (not (seq-empty-p items)))
                                                            (lsp-completion--to-internal items))
                                                           ((not done?) :incomplete))
                                                          :lsp-items nil
                                                          :markers markers
                                                          :prefix prefix)
                              result (lsp-completion--filter-candidates
                                      (cond (done?
                                             (cl-second lsp-completion--cache))
                                            (lsp-completion-filter-on-incomplete
                                             (lsp-completion--to-internal items)))
                                      :lsp-items items
                                      :markers markers
                                      :prefix prefix))))))
                (:interrupted lsp-completion--last-result)
                (`,res (setq lsp-completion--last-result res))))))
      (list
       bounds-start
       (point)
       (lambda (probe pred action)
         (cond
          ((eq action 'metadata)
           '(metadata (category . lsp-capf)
                      (display-sort-function . identity)
                      (cycle-sort-function . identity)))
          ((eq (car-safe action) 'boundaries) nil)
          (t
           (complete-with-action action (funcall candidates) probe pred))))
       :annotation-function #'lsp-completion--annotate
       :company-kind #'lsp-completion--candidate-kind
       :company-deprecated #'lsp-completion--candidate-deprecated
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (let (
               ;; 2 is a heuristic number to make sure we look futher back than
               ;; the bounds-start, which can be different from the actual start
               ;; of the symbol
               (bounds-left (max (line-beginning-position) (- bounds-start 2)))
               triggered-by-char?)
           (while (and (> (point) bounds-left)
                       (not (equal (char-after) ?\s))
                       (not triggered-by-char?))
             (setq triggered-by-char? (lsp-completion--looking-back-trigger-characterp trigger-chars))
             (goto-char (1- (point))))
           (and triggered-by-char? t)))
       :company-match #'lsp-completion--company-match
       :company-docsig #'lsp-completion--company-docsig
       :company-doc-buffer (-compose #'lsp-doc-buffer
                                     #'lsp-completion--get-documentation)
       :exit-function
       (-rpartial #'lsp-completion--exit-fn candidates)))))

(defun lsp-completion--find-workspace (server-id)
  (--first (eq (lsp--client-server-id (lsp--workspace-client it)) server-id)
           (lsp-workspaces)))

(defun lsp-completion--exit-fn (candidate _status &optional candidates)
  "Exit function of `completion-at-point'.
CANDIDATE is the selected completion item.
Others: CANDIDATES"
  (unwind-protect
      (-let* ((candidate (if (plist-member (text-properties-at 0 candidate)
                                           'lsp-completion-item)
                             candidate
                           (cl-find candidate (funcall candidates) :test #'equal)))
              (candidate
               ;; see #3498 typescript-language-server does not provide the
               ;; proper insertText without resolving.
               (if (lsp-completion--find-workspace 'ts-ls)
                   (lsp-completion--resolve candidate)
                 candidate))
              ((&plist 'lsp-completion-item item
                       'lsp-completion-start-point start-point
                       'lsp-completion-markers markers
                       'lsp-completion-resolved resolved
                       'lsp-completion-prefix prefix)
               (text-properties-at 0 candidate))
              ((&CompletionItem? :label :insert-text? :text-edit? :insert-text-format?
                                 :additional-text-edits? :insert-text-mode? :command?)
               item))
        (cond
         (text-edit?
          (apply #'delete-region markers)
          (insert prefix)
          (pcase text-edit?
            ((lsp-interface TextEdit) (lsp--apply-text-edit text-edit?))
            ((lsp-interface InsertReplaceEdit :insert :replace :new-text)
             (lsp--apply-text-edit
              (lsp-make-text-edit
               :new-text new-text
               :range (if (or (and current-prefix-arg (eq lsp-completion-default-behaviour :replace))
                              (and (not current-prefix-arg) (eq lsp-completion-default-behaviour :insert)))
                          insert
                        replace))))))
         ((or (unless (lsp-falsy? insert-text?) insert-text?) label)
          (apply #'delete-region markers)
          (insert prefix)
          (delete-region start-point (point))
          (insert (or (unless (lsp-falsy? insert-text?) insert-text?) label))))

        (lsp--indent-lines start-point (point) insert-text-mode?)
        (when (equal insert-text-format? lsp/insert-text-format-snippet)
          (lsp--expand-snippet (buffer-substring start-point (point))
                               start-point
                               (point)))

        (when lsp-completion-enable-additional-text-edit
          (if (or resolved
                  (not (seq-empty-p additional-text-edits?)))
              (lsp--apply-text-edits additional-text-edits? 'completion)
            (-let [(callback cleanup-fn) (lsp--create-apply-text-edits-handlers)]
              (lsp-completion--resolve-async
               item
               (-compose callback #'lsp:completion-item-additional-text-edits?)
               cleanup-fn))))

        (if (or resolved command?)
            (when command? (lsp--execute-command command?))
          (lsp-completion--resolve-async
           item
           (-lambda ((&CompletionItem? :command?))
             (when command? (lsp--execute-command command?)))))

        (when (and (or
                    (equal lsp-signature-auto-activate t)
                    (memq :after-completion lsp-signature-auto-activate)
                    (and (memq :on-trigger-char lsp-signature-auto-activate)
                         (-when-let ((&SignatureHelpOptions? :trigger-characters?)
                                     (lsp--capability :signatureHelpProvider))
                           (lsp-completion--looking-back-trigger-characterp
                            trigger-characters?))))
                   (lsp-feature? "textDocument/signatureHelp"))
          (lsp-signature-activate))

        (setq-local lsp-inhibit-lsp-hooks nil))
    (lsp-completion--clear-cache)))

(defun lsp-completion--regex-fuz (str)
  "Build a regex sequence from STR.  Insert .* between each char."
  (apply #'concat
         (cl-mapcar
          #'concat
          (cons "" (cdr (seq-map (lambda (c) (format "[^%c]*" c)) str)))
          (seq-map (lambda (c)
                     (format "\\(%s\\)" (regexp-quote (char-to-string c))))
                   str))))

(defun lsp-completion--fuz-score (query str)
  "Calculate fuzzy score for STR with query QUERY.
The return is nil or in range of (0, inf)."
  (-when-let* ((md (cddr (or (get-text-property 0 'match-data str)
                             (let ((re (lsp-completion--regex-fuz query))
                                   (case-fold-search completion-ignore-case))
                               (when (string-match re str)
                                 (match-data))))))
               (start (pop md))
               (len (length str))
               ;; To understand how this works, consider these bad ascii(tm)
               ;; diagrams showing how the pattern "foo" flex-matches
               ;; "fabrobazo", "fbarbazoo" and "barfoobaz":

               ;;      f abr o baz o
               ;;      + --- + --- +

               ;;      f barbaz oo
               ;;      + ------ ++

               ;;      bar foo baz
               ;;      --- +++ ---

               ;; "+" indicates parts where the pattern matched.  A "hole" in
               ;; the middle of the string is indicated by "-".  Note that there
               ;; are no "holes" near the edges of the string.  The completion
               ;; score is a number bound by ]0..1]: the higher the better and
               ;; only a perfect match (pattern equals string) will have score
               ;; 1.  The formula takes the form of a quotient.  For the
               ;; numerator, we use the number of +, i.e. the length of the
               ;; pattern.  For the denominator, it first computes
               ;;
               ;;     hole_i_contrib = 1 + (Li-1)^1.05 for first hole
               ;;     hole_i_contrib = 1 + (Li-1)^0.25 for hole i of length Li
               ;;
               ;; The final value for the denominator is then given by:
               ;;
               ;;    (SUM_across_i(hole_i_contrib) + 1)
               ;;
               (score-numerator 0)
               (score-denominator 0)
               (last-b -1)
               (q-ind 0)
               (update-score
                (lambda (a b)
                  "Update score variables given match range (A B)."
                  (setq score-numerator (+ score-numerator (- b a)))
                  (unless (= a len)
                    ;; case mismatch will be pushed to near next rank
                    (unless (equal (aref query q-ind) (aref str a))
                      (cl-incf a 0.9))
                    (setq score-denominator
                          (+ score-denominator
                             (if (= a last-b) 0
                               (+ 1 (* (if (< 0 (- a last-b 1)) 1 -1)
                                       (expt (abs (- a last-b 1))
                                             ;; Give a higher score for match near start
                                             (if (eq last-b -1) 0.75 0.25))))))))
                  (setq last-b b))))
    (while md
      (funcall update-score start (cl-first md))
      ;; Due to the way completion regex is constructed, `(eq end (+ start 1))`
      (cl-incf q-ind)
      (pop md)
      (setq start (pop md)))
    (unless (zerop len)
      (/ score-numerator (1+ score-denominator) 1.0))))


;;;###autoload
(defun lsp-completion--enable ()
  "Enable LSP completion support."
  (when (and lsp-completion-enable
             (lsp-feature? "textDocument/completion"))
    (lsp-completion-mode 1)))

(defun lsp-completion--disable ()
  "Disable LSP completion support."
  (lsp-completion-mode -1))

(defun lsp-completion-passthrough-try-completion (string table pred point)
  "Passthrough try function.

If TABLE is a function, it is called with STRING, PRED and nil to get
the candidates, otherwise it is treated as the candidates.

If the candidates is non-empty, return the passed STRING and POINT."
  (when (pcase table
          ((pred functionp)
           (funcall table string pred nil))
          ((pred hash-table-p)
           (not (hash-table-empty-p table)))
          (_ table))
    (cons string point)))

(defun lsp-completion-passthrough-all-completions (_string table pred _point)
  "Passthrough all completions from TABLE with PRED."
  (defvar completion-lazy-hilit-fn)
  (when (bound-and-true-p completion-lazy-hilit)
    (setq completion-lazy-hilit-fn
          (lambda (candidate)
            (->> candidate
                 lsp-completion--company-match
                 (mapc (-lambda ((start . end))
                         (put-text-property start end 'face 'completions-common-part candidate))))
            candidate)))
  (all-completions "" table pred))

;;;###autoload
(define-minor-mode lsp-completion-mode
  "Toggle LSP completion support."
  :group 'lsp-completion
  :global nil
  :lighter ""
  (let ((completion-started-fn (lambda (&rest _)
                                 (setq-local lsp-inhibit-lsp-hooks t)))
        (after-completion-fn (lambda (result)
                               (when (stringp result)
                                 (lsp-completion--clear-cache))
                               (setq-local lsp-inhibit-lsp-hooks nil))))
    (cond
     (lsp-completion-mode
      (make-local-variable 'completion-at-point-functions)
      ;; Ensure that `lsp-completion-at-point' the first CAPF to be tried,
      ;; unless user has put it elsewhere in the list by their own
      (add-to-list 'completion-at-point-functions #'lsp-completion-at-point)
      (make-local-variable 'completion-category-defaults)
      (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (lsp-passthrough))))
      (make-local-variable 'completion-styles-alist)
      (setf (alist-get 'lsp-passthrough completion-styles-alist)
            '(lsp-completion-passthrough-try-completion
              lsp-completion-passthrough-all-completions
              "Passthrough completion."))

      (cond
       ((equal lsp-completion-provider :none))
       ((and (not (equal lsp-completion-provider :none))
             (fboundp 'company-mode))
        (setq-local company-abort-on-unique-match nil)
        (company-mode 1)
        (setq-local company-backends (cl-adjoin 'company-capf company-backends :test #'equal)))
       (t
        (lsp--warn "Unable to autoconfigure company-mode.")))

      (when (bound-and-true-p company-mode)
        (add-hook 'company-completion-started-hook
                  completion-started-fn
                  nil
                  t)
        (add-hook 'company-after-completion-hook
                  after-completion-fn
                  nil
                  t))
      (add-hook 'lsp-unconfigure-hook #'lsp-completion--disable nil t))
     (t
      (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
      (setq-local completion-category-defaults
                  (cl-remove 'lsp-capf completion-category-defaults :key #'cl-first))
      (setq-local completion-styles-alist
                  (cl-remove 'lsp-passthrough completion-styles-alist :key #'cl-first))
      (remove-hook 'lsp-unconfigure-hook #'lsp-completion--disable t)
      (when (featurep 'company)
        (remove-hook 'company-completion-started-hook
                     completion-started-fn
                     t)
        (remove-hook 'company-after-completion-hook
                     after-completion-fn
                     t))))))

;;;###autoload
(add-hook 'lsp-configure-hook (lambda ()
                                (when (and lsp-auto-configure
                                           lsp-completion-enable)
                                  (lsp-completion--enable))))

(lsp-consistency-check lsp-completion)

(provide 'lsp-completion)
;;; lsp-completion.el ends here
