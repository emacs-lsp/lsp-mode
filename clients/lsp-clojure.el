;;; lsp-clojure.el --- Clojure Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Benedek Fazekas
;; Copyright (C) 2019-2026 emacs-lsp maintainers

;; Author: Benedek Fazekas <benedek.fazekas@gmail.com>
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

;; lsp-clojure client

;;; Code:

(require 'lsp-mode)
(require 'lsp-protocol)
(require 'cl-lib)
(require 'lsp-semantic-tokens)

(defgroup lsp-clojure nil
  "LSP support for Clojure."
  :link '(url-link "https://github.com/snoe/clojure-lsp")
  :group 'lsp-mode
  :tag "Lsp Clojure")

(define-obsolete-variable-alias 'lsp-clojure-server-command
  'lsp-clojure-custom-server-command  "lsp-mode 8.0.0")

(defcustom lsp-clojure-custom-server-command nil
  "The clojure-lisp server command."
  :group 'lsp-clojure
  :risky t
  :type '(repeat string))

(defcustom lsp-clojure-server-download-url
  (format "https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-%s.zip"
          (let ((arch (car (split-string system-configuration "-"))))
            (pcase system-type
              ('gnu/linux (concat "linux-"
                                  (cond
                                   ((string= "x86_64" arch) "amd64")
                                   (t arch))))
              ('darwin (concat "macos-"
                               (cond
                                ((string= "x86_64" arch) "amd64")
                                (t arch))))
              ('windows-nt "windows-amd64"))))
  "Automatic download url for lsp-clojure."
  :type 'string
  :group 'lsp-clojure
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clojure-server-store-path
  (f-join lsp-server-install-dir
          "clojure"
          (if (eq system-type 'windows-nt)
              "clojure-lsp.exe"
            "clojure-lsp"))
  "The path to the file in which `clojure-lsp' will be stored."
  :type 'file
  :group 'lsp-clojure
  :package-version '(lsp-mode . "8.0.0"))

(defcustom lsp-clojure-trace-enable nil
  "Enable trace logs between client and clojure-lsp server."
  :group 'lsp-clojure
  :type 'boolean)

(defcustom lsp-clojure-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP clojure workspace directory."
  :group 'lsp-clojure
  :risky t
  :type 'directory)

(defcustom lsp-clojure-workspace-cache-dir (expand-file-name ".cache/" lsp-clojure-workspace-dir)
  "LSP clojure workspace cache directory."
  :group 'lsp-clojure
  :risky t
  :type 'directory)

(defcustom lsp-clojure-library-dirs (list lsp-clojure-workspace-cache-dir
                                          (expand-file-name "~/.gitlibs/libs"))
  "LSP clojure dirs that should be considered library folders."
  :group 'lsp-clojure
  :type '(repeat directory))

(defcustom lsp-clojure-test-tree-position-params nil
  "The optional test tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-clojure)

(defcustom lsp-clojure-project-tree-position-params nil
  "The optional project tree position params.
Defaults to side following treemacs default."
  :type 'alist
  :group 'lsp-clojure)

;; Internal

(eval-and-compile
  (lsp-interface (Clojure:CursorInfoParams (:textDocument :position) nil)))

(lsp-dependency
 'clojure-lsp
 `(:download :url lsp-clojure-server-download-url
   :decompress :zip
   :store-path lsp-clojure-server-store-path
   :set-executable? t)
 '(:system "clojure-lsp"))

;; Refactorings

(defun lsp-clojure--execute-command (command &optional args)
  "Send an executeCommand request for COMMAND with ARGS."
  (lsp--cur-workspace-check)
  (lsp-send-execute-command command (apply #'vector args)))

(defun lsp-clojure--refactoring-call (refactor-name &rest additional-args)
  "Send an executeCommand request for REFACTOR-NAME with ADDITIONAL-ARGS.
If there are more arguments expected after the line and column numbers."
  (lsp--cur-workspace-check)
  (lsp-clojure--execute-command refactor-name (cl-list* (lsp--buffer-uri)
                                                        (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                                        (current-column)
                                                        additional-args)))

(defun lsp-clojure-add-import-to-namespace (import-name)
  "Add to IMPORT-NAME to :import form."
  (interactive "MImport name: ")
  (lsp-clojure--refactoring-call "add-import-to-namespace" import-name))

(defun lsp-clojure-add-missing-libspec ()
  "Apply add-missing-libspec refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "add-missing-libspec"))

(defun lsp-clojure-clean-ns ()
  "Apply clean-ns refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "clean-ns"))

(defun lsp-clojure-cycle-coll ()
  "Apply cycle-coll refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-coll"))

(defun lsp-clojure-cycle-privacy ()
  "Apply cycle-privacy refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-privacy"))

(defun lsp-clojure-expand-let ()
  "Apply expand-let refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "expand-let"))

(defun lsp-clojure-extract-function (function-name)
  "Move form at point into a new function named FUNCTION-NAME."
  (interactive "MFunction name: ") ;; Name of the function
  (lsp-clojure--refactoring-call "extract-function" function-name))

(defun lsp-clojure-inline-symbol ()
  "Apply inline-symbol refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "inline-symbol"))

(defun lsp-clojure-introduce-let (binding-name)
  "Move form at point into a new let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "introduce-let" binding-name))

(defun lsp-clojure-move-to-let (binding-name)
  "Move form at point into nearest existing let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "move-to-let" binding-name))

(defun lsp-clojure-thread-first ()
  "Apply thread-first refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first"))

(defun lsp-clojure-thread-first-all ()
  "Apply thread-first-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first-all"))

(defun lsp-clojure-thread-last ()
  "Apply thread-last refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last"))

(defun lsp-clojure-thread-last-all ()
  "Apply thread-last-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last-all"))

(defun lsp-clojure-unwind-all ()
  "Apply unwind-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-all"))

(defun lsp-clojure-unwind-thread ()
  "Apply unwind-thread refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-thread"))

(defun lsp-clojure-create-function ()
  "Apply create-function refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "create-function"))

(defun lsp-clojure-create-test ()
  "Apply create-test refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "create-test"))

(defun lsp-clojure-sort-map ()
  "Apply sort-map refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "sort-map"))

(defun lsp-clojure-move-coll-entry-up ()
  "Apply move coll entry up refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "move-coll-entry-up"))

(defun lsp-clojure-move-coll-entry-down ()
  "Apply move coll entry down refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "move-coll-entry-down"))

(defun lsp-clojure-forward-slurp ()
  "Apply forward slurp refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "forward-slurp"))

(defun lsp-clojure-forward-barf ()
  "Apply forward barf refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "forward-barf"))

(defun lsp-clojure-backward-slurp ()
  "Apply backward slurp refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "backward-slurp"))

(defun lsp-clojure-backward-barf ()
  "Apply backward slurp refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "backward-barf"))

(defun lsp-clojure-move-form (dest-filename)
  "Apply move-form refactoring at point to DEST-FILENAME."
  (interactive
   (list (or (read-file-name "Move form to: ")
             (user-error "No filename selected. Aborting"))))
  (lsp-clojure--refactoring-call "move-form" (expand-file-name dest-filename)))

(defun lsp-clojure-server-info ()
  "Request server info."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-notify "clojure/serverInfo/log" nil))

(defvar lsp-clojure-server-buffer-name "*lsp-clojure-server-log*")

(defun lsp-clojure--server-log-revert-function (original-file-log-buffer &rest _)
  "Spit contents to ORIGINAL-FILE-LOG-BUFFER."
  (with-current-buffer (get-buffer-create lsp-clojure-server-buffer-name)
    (erase-buffer)
    (insert (with-current-buffer original-file-log-buffer (buffer-string)))
    (goto-char (point-max))
    (read-only-mode)))

(defun lsp-clojure-server-log ()
  "Open a buffer with the server logs."
  (interactive)
  (lsp--cur-workspace-check)
  (let* ((log-path (-> (lsp--json-serialize (lsp-request "clojure/serverInfo/raw" nil))
                       (lsp--read-json)
                       (lsp-get :log-path))))
    (with-current-buffer (find-file log-path)
      (read-only-mode)
      (goto-char (point-max)))))

(defun lsp-clojure-server-info-raw ()
  "Request server info raw data."
  (interactive)
  (lsp--cur-workspace-check)
  (message "%s" (lsp--json-serialize (lsp-request "clojure/serverInfo/raw" nil))))

(defun lsp-clojure-cursor-info ()
  "Request cursor info at point."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-notify "clojure/cursorInfo/log"
              (lsp-make-clojure-cursor-info-params
               :textDocument (lsp-make-text-document-identifier :uri (lsp--buffer-uri))
               :position (lsp-make-position :line (- (line-number-at-pos) 1)
                                            :character (current-column)))))

(defun lsp-clojure-resolve-macro-as ()
  "Ask to user how the unresolved macro should be resolved."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-clojure--execute-command "resolve-macro-as"
                                (list (lsp--buffer-uri)
                                      (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                      (current-column))))

(defun lsp-clojure--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(defun lsp-clojure--get-metadata-location (file-location)
  "Given a FILE-LOCATION return the file containing the metadata for the file."
  (format "%s.%s.metadata"
          (file-name-directory file-location)
          (file-name-base file-location)))

(defun lsp-clojure--file-in-jar (uri)
  "Check URI for a valid jar and include it in workspace."
  (string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" uri)
  (let* ((ns-path (match-string 3 uri))
         (ns (s-replace "/" "." ns-path))
         (file-location (concat lsp-clojure-workspace-cache-dir ns)))
    (unless (file-readable-p file-location)
      (lsp-clojure--ensure-dir (file-name-directory file-location))
      (with-lsp-workspace (lsp-find-workspace 'clojure-lsp nil)
        (let ((content (lsp-send-request (lsp-make-request "clojure/dependencyContents" (list :uri uri)))))
          (with-temp-file file-location
            (insert content))
          (with-temp-file (lsp-clojure--get-metadata-location file-location)
            (insert uri)))))
    file-location))

(defun lsp-clojure--server-executable-path ()
  "Return the clojure-lsp server command."
  (or (executable-find "clojure-lsp")
      (lsp-package-path 'clojure-lsp)))

(lsp-defun lsp-clojure--show-references ((&Command :arguments? args))
  "Show references for command with ARGS.
ARGS is a vector which the first element is the uri, the second the line
and the third the column."
  (lsp-show-xrefs
   (lsp--locations-to-xref-items
    (lsp-request "textDocument/references"
                 (lsp--make-reference-params
                  (lsp--text-document-position-params
                   (list :uri (seq-elt args 0))
                   (list :line (1- (seq-elt args 1))
                         :character (1- (seq-elt args 2)))))))
   nil
   t))

;; Test tree

(defvar-local lsp-clojure--test-tree-data nil)
(defconst lsp-clojure--test-tree-buffer-name "*Clojure Test Tree*")

(defvar treemacs-position)
(defvar treemacs-width)
(declare-function lsp-treemacs-render "ext:lsp-treemacs" (tree title expand-depth &optional buffer-name right-click-actions clear-cache?))
(declare-function lsp-treemacs--open-file-in-mru "ext:lsp-treemacs" (file))

(defun lsp-clojure--test-tree-ret-action (uri range)
  "Build the ret action for an item in the test tree view.
URI is the source of the item.
RANGE is the range of positions to where this item should point."
  (interactive)
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point (lsp:range-start range)))
  (run-hooks 'xref-after-jump-hook))

(lsp-defun lsp-clojure--test-tree-data->tree (uri (&clojure-lsp:TestTreeNode :name :range :kind :children?))
  "Builds a test tree.
URI is the source of the test tree.
NODE is the node with all test children data."
  (-let* ((icon (cl-case kind
                  (1 'namespace)
                  (2 'method)
                  (3 'field)))
          (base-tree (list :key name
                           :label name
                           :icon icon
                           :ret-action (lambda (&rest _) (lsp-clojure--test-tree-ret-action uri range))
                           :uri uri)))
    (if (seq-empty-p children?)
        base-tree
      (plist-put base-tree :children (seq-map (-partial #'lsp-clojure--test-tree-data->tree uri) children?)))))

(lsp-defun lsp-clojure--render-test-tree ((&clojure-lsp:TestTreeParams :uri :tree))
  "Render a test tree view for current test tree buffer data."
  (save-excursion
    (lsp-treemacs-render
     (list (lsp-clojure--test-tree-data->tree uri tree))
     "Clojure Test Tree"
     t
     lsp-clojure--test-tree-buffer-name)))

(defun lsp-clojure--show-test-tree (ignore-focus?)
  "Show a test tree for current buffer.
Focus on it if IGNORE-FOCUS? is nil."
  (if lsp-clojure--test-tree-data
      (-let* ((tree-buffer (lsp-clojure--render-test-tree lsp-clojure--test-tree-data))
              (position-params (or lsp-clojure-test-tree-position-params
                                   `((side . ,treemacs-position)
                                     (slot . 2)
                                     (window-width . ,treemacs-width))))
              (window (display-buffer-in-side-window tree-buffer position-params)))
        (unless ignore-focus?
          (select-window window)
          (set-window-dedicated-p window t)))
    (unless ignore-focus?
      (lsp-log "No Clojure test tree data found."))))

(lsp-defun lsp-clojure--handle-test-tree (_workspace (notification &as &clojure-lsp:TestTreeParams :uri))
  "Test tree notification handler for workspace WORKSPACE.
NOTIFICATION is the test tree notification data received from server.
It updates the test tree view data."
  (when (require 'lsp-treemacs nil t)
    (when-let* ((buffer (find-buffer-visiting (lsp--uri-to-path uri))))
      (with-current-buffer buffer
        (setq lsp-clojure--test-tree-data notification)
        (when (get-buffer-window lsp-clojure--test-tree-buffer-name)
          (lsp-clojure--show-test-tree t))))))

;;;###autoload
(defun lsp-clojure-show-test-tree (ignore-focus?)
  "Show a test tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (if (require 'lsp-treemacs nil t)
      (lsp-clojure--show-test-tree ignore-focus?)
    (error "The package lsp-treemacs is not installed")))

;; Project Tree

(defconst lsp-clojure--project-tree-buffer-name "*Clojure Project Tree*")

(defun lsp-clojure--project-tree-type->icon (type)
  "Convert the project tree type TYPE to icon."
  (cl-case type
    (1 'project)
    (2 'folder)
    (3 'library)
    (4 'jar)
    (5 'namespace)
    (6 'class)
    (7 'method)
    (8 'variable)
    (9 'interface)))

(defun lsp-clojure--project-tree-ret-action (uri range)
  "Build the ret action for an item in the project tree view.
URI is the source of the item."
  (interactive)
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point (lsp:range-start range)))
  (run-hooks 'xref-after-jump-hook))

(lsp-defun lsp-clojure--project-tree-children-data->tree (buffer current-node &optional _ callback)
  "Builds a project tree considering CURRENT-NODE."
  (with-current-buffer buffer
    (lsp-request-async
     "clojure/workspace/projectTree/nodes"
     current-node
     (-lambda ((&clojure-lsp:ProjectTreeNode :nodes?))
       (funcall
        callback
        (-map
         (-lambda ((node &as &clojure-lsp:ProjectTreeNode :id? :name :type :uri? :range? :detail? :final?))
           (-let ((label (if detail?
                             (format "%s %s" name (propertize detail? 'face 'lsp-details-face))
                           name)))
             `(:label ,label
               :key ,(or id? name)
               :icon ,(lsp-clojure--project-tree-type->icon type)
               ,@(unless final?
                   (list :children-async  (-partial #'lsp-clojure--project-tree-children-data->tree buffer node)))
               ,@(when uri?
                   (list :uri uri?
                         :ret-action (lambda (&rest _)
                                       (interactive)
                                       (lsp-clojure--project-tree-ret-action uri? range?)))))))
         nodes?)))
     :mode 'detached)))

(defun lsp-clojure--project-tree-data->tree ()
  "Builds a project tree considering CURRENT-NODE."
  (-let* (((&clojure-lsp:ProjectTreeNode :id? :name :nodes? :uri?) (lsp-request "clojure/workspace/projectTree/nodes" nil))
          (buffer (current-buffer)))
    (list :key (or id? name)
          :label name
          :icon "clj"
          :children (seq-map (-lambda ((node &as &clojure-lsp:ProjectTreeNode :id? :name :type :uri?))
                               (list :key (or id? name)
                                     :label name
                                     :icon (lsp-clojure--project-tree-type->icon type)
                                     :children-async (-partial #'lsp-clojure--project-tree-children-data->tree buffer node)
                                     :uri uri?))
                             nodes?)
          :uri uri?)))

(defun lsp-clojure--render-project-tree ()
  "Render a project tree view."
  (save-excursion
    (lsp-treemacs-render
     (list (lsp-clojure--project-tree-data->tree))
     "Clojure Project Tree"
     nil
     lsp-clojure--project-tree-buffer-name
     nil
     t)))

(defun lsp-clojure--show-project-tree (ignore-focus?)
  "Show a project tree for current project.
Focus on it if IGNORE-FOCUS? is nil."
  (-let* ((tree-buffer (lsp-clojure--render-project-tree))
          (position-params (or lsp-clojure-project-tree-position-params
                               `((side . ,treemacs-position)
                                 (slot . 2)
                                 (window-width . ,treemacs-width))))
          (window (display-buffer-in-side-window tree-buffer position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

;;;###autoload
(defun lsp-clojure-show-project-tree (ignore-focus?)
  "Show a project tree with source-paths and dependencies.
Focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (if (require 'lsp-treemacs nil t)
      (lsp-clojure--show-project-tree ignore-focus?)
    (error "The package lsp-treemacs is not installed")))

(defun lsp-clojure--build-command ()
  "Build clojure-lsp start command."
  (let* ((base-command (or lsp-clojure-custom-server-command
                           (-some-> (lsp-clojure--server-executable-path) list))))
    (if lsp-clojure-trace-enable
        (-map-last #'stringp
                   (lambda (command)
                     (concat command " --trace"))
                   base-command)
      base-command)))

(lsp-register-client
 (make-lsp-client
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'clojure-lsp callback error-callback))
  :semantic-tokens-faces-overrides '(:types (("macro" . font-lock-keyword-face)
                                             ("keyword" . clojure-keyword-face)
                                             ("event" . default)))
  :new-connection (lsp-stdio-connection
                   #'lsp-clojure--build-command
                   #'lsp-clojure--build-command)
  :major-modes '(clojure-mode clojurec-mode clojurescript-mode
                 clojure-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode)
  :library-folders-fn (lambda (_workspace) lsp-clojure-library-dirs)
  :uri-handlers (lsp-ht ("jar" #'lsp-clojure--file-in-jar))
  :action-handlers (lsp-ht ("code-lens-references" #'lsp-clojure--show-references))
  :notification-handlers (lsp-ht ("clojure/textDocument/testTree" #'lsp-clojure--handle-test-tree))
  :initialization-options '(:dependency-scheme "jar"
                            :show-docs-arity-on-same-line? t)
  :custom-capabilities `((experimental . ((testTree . ,(and (require 'lsp-treemacs nil t) t)))))
  :server-id 'clojure-lsp))

(lsp-consistency-check lsp-clojure)

;; For debugging

(declare-function cider-connect-clj "ext:cider" (params))

(defun lsp-clojure-nrepl-connect ()
  "Connect to the running nrepl debug server of clojure-lsp."
  (interactive)
  (let ((info (lsp-clojure-server-info-raw)))
    (save-match-data
      (when (functionp 'cider-connect-clj)
        (when-let* ((port (and (string-match "\"port\":\\([0-9]+\\)" info)
                              (match-string 1 info))))
          (cider-connect-clj `(:host "localhost"
                               :port ,port)))))))

;; Cider integration

(defun lsp-clojure-semantic-tokens-refresh (&rest _)
  "Force refresh semantic tokens."
  (when-let* ((workspace (and lsp-semantic-tokens-enable
                             (lsp-find-workspace 'clojure-lsp (buffer-file-name)))))
    (--each (lsp--workspace-buffers workspace)
      (when (lsp-buffer-live-p it)
        (lsp-with-current-buffer it
          (lsp-semantic-tokens--enable))))))

(with-eval-after-load 'cider
  (when lsp-semantic-tokens-enable
    ;; refresh tokens as cider flush font-faces after disconnected
    (add-hook 'cider-mode-hook #'lsp-clojure-semantic-tokens-refresh)))

(provide 'lsp-clojure)
;;; lsp-clojure.el ends here
