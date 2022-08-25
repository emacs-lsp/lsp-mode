;;; lsp-ansible.el --- lsp-mode ansible integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 emacs-lsp maintainers

;; Author: lsp-mode maintainers
;; Keywords: lsp, yaml, ansible

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

;; LSP Client for the Ansible Language

;;; Code:

(require 'lsp-mode)

;;; Ansible
(defgroup lsp-ansible nil
  "Settings for the Ansible Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ansible/ansible-language-server")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-language-server-command
  '("ansible-language-server" "--stdio")
  "The command that starts the ansible language server."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-ansible-path "ansible"
  "Path to the ansible executable.
$PATH is searched for the executable."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-use-fully-qualified-collection-names t
  "Toggles use of fully qualified collection names when inserting a module name.
Disabling it will only use FQCNs when necessary, that is when the collection is
not configured for the task."
  :type 'boolean
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-ansible-lint-arguments ""
  "Optional command line arguments to be appended to ansible-lint invocation.
See ansible-lint documentation."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-ansible-lint-enabled t
  "Enables/disables use of ansible-lint."
  :type 'boolean
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-ansible-lint-path "ansible-lint"
  "Path to the ansible-lint executable.
$PATH is searched for the executable."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-completion-provide-redirect-modules t
  "Toggle redirected module provider when completing modules."
  :type 'boolean
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-completion-provide-module-option-aliases t
  "Toggle alias provider when completing module options."
  :type 'boolean
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-container-engine "auto"
  "The container engine to be used while running with execution environment.
Valid values are auto, podman and docker.  For auto it will look for podman then
docker."
  :type '(choice (const "auto")
                 (const "podman")
                 (const "docker"))
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-enabled nil
  "Enable or disable the use of an execution environment."
  :type 'boolean
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-image "quay.io/ansible/creator-ee:latest"
  "Specify the name of the execution environment image."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-pull-policy "missing"
  "Specify the image pull policy.
Valid values are always, missing, never and tag.  Setting always will always
pull the image when extension is activated or reloaded.  Setting missing will
pull if not locally available.  Setting never will never pull the image and
setting tag will always pull if the image tag is ‘latest’, otherwise pull
if not locally available."
  :type '(choice (const "always")
                 (const "missing")
                 (const "never")
                 (const "tag"))
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-pull-arguments ""
  "Specify any additional parameters for the pull command.
Example: ‘--tls-verify=false’"
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-container-options ""
  "Extra parameters passed to the container engine command.
Example: ‘-–net=host’"
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-execution-environment-volume-mounts []
  "Additonnal volumes to mount in container.
The value is a vector of plists.  Expected properties are:
- src: the name of the local volume or path to be mounted within execution
  environment
- dest: the path where the file or directory are mounted in the container
- options: the property is optional, and is a comma-separated list of options.
  Example: ro,Z"
  :type '(lsp-repeatable-vector plist)
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-python-interpreter-path ""
  "Path to the python/python3 executable.
This setting may be used to make the extension work with ansible and
ansible-lint installations in a Python virtual environment."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-ansible-python-activation-script ""
  "Path to a custom activate script.
It will be used instead of `lsp-ansible-python-interpreter-path' to run in a
Python virtual environment."
  :type 'string
  :group 'lsp-ansible
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency 'ansible-language-server
                '(:system "ansible-language-server")
                '(:npm :package "@ansible/ansible-language-server"
                       :path "ansible-language-server"))

(lsp-register-custom-settings
 '(("ansible.ansible.path" lsp-ansible-ansible-path)
   ("ansible.ansible.useFullyQualifiedCollectionNames" lsp-ansible-use-fully-qualified-collection-names t)
   ("ansible.ansibleLint.arguments" lsp-ansible-ansible-lint-arguments)
   ("ansible.ansibleLint.enabled" lsp-ansible-ansible-lint-enabled t)
   ("ansible.ansibleLint.path" lsp-ansible-ansible-lint-path)
   ("ansible.completion.provideRedirectModules" lsp-ansible-completion-provide-redirect-modules t)
   ("ansible.completion.provideModuleOptionAliases" lsp-ansible-completion-provide-module-option-aliases t)
   ("ansible.executionEnvironment.containerEngine" lsp-ansible-execution-environment-container-engine)
   ("ansible.executionEnvironment.enabled" lsp-ansible-execution-environment-enabled t)
   ("ansible.executionEnvironment.image" lsp-ansible-execution-environment-image)
   ("ansible.executionEnvironment.pull.policy" lsp-ansible-execution-environment-pull-policy)
   ("ansible.executionEnvironment.pull.arguments" lsp-ansible-execution-environment-pull-arguments)
   ("ansible.executionEnvironment.containerOptions" lsp-ansible-execution-environment-container-options)
   ("ansible.executionEnvironment.volumeMounts" lsp-ansible-execution-environment-volume-mounts)
   ("ansible.python.interpreterPath" lsp-ansible-python-interpreter-path)
   ("ansible.python.activationScript" lsp-ansible-python-activation-script)))

(defun lsp-ansible-check-ansible-minor-mode (&rest _)
  "Check whether ansible minor mode is active.
This prevents the Ansible server from being turned on in all yaml files."
  (and (derived-mode-p 'yaml-mode)
       ;; emacs-ansible provides ansible, not ansible-mode
       (with-no-warnings (bound-and-true-p ansible))))

(declare-function lsp-completion--clear-cache "lsp-completion" (&optional keep-last-result))

(defun lsp-ansible-resync-inventory ()
  "Resync the inventory cache used by Ansible Language Server for hosts completion."
  (interactive)
  (lsp-notify "resync/ansible-inventory" nil)
  (require 'lsp-completion)
  (lsp-completion--clear-cache))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-ansible-language-server-command))
                            (lsp-package-path 'ansible-language-server))
                       ,@(cl-rest lsp-ansible-language-server-command))))
  :priority 1
  :activation-fn #'lsp-ansible-check-ansible-minor-mode
  :server-id 'ansible-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'ansible-language-server callback error-callback))))

(lsp-consistency-check lsp-ansible)

(provide 'lsp-ansible)
;;; lsp-ansible.el ends here
