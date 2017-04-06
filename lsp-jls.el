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

(defconst jls-version "0.1.0-201702132114")
(defconst jls-jar-version "1.4.0.v20161219-1356")

;;;###autoload
(defcustom lsp-jls-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
  :group 'lsp-mode
  :risky t
  :type 'directory )

(defun jls--locate-server-jar ()
  "returns the jar file location of the ls"
  (expand-file-name
   (format "server/plugins/org.eclipse.equinox.launcher_%s.jar" jls-jar-version)
   jls-server-install-dir))

(defun jls--locate-server-binary ()
  "returns the zip file location of the ls"
  (expand-file-name
   (format "jdt-language-server-%s.tar.gz" jls-version)
   jls-server-install-dir))

(defun jls--locate-server-config ()
  "returns the server config based on OS"
  (let ( (config (cond
                  ((string-equal system-type "windows-nt") ; Microsoft Windows
                   "config_wini")
                  ((string-equal system-type "darwin") ; Mac OS X
                   "config_mac")
                  ((string-equal system-type "gnu/linux") ; linux
                   "config_linux"))))
    (message (format "using config for %s" config))
    (expand-file-name config jls-server-install-dir)))

(defun jls--download-server ()
  "Download server and installs it"
  (interactive)
  (let ((dest jls-server-install-dir)
        (dest-file (jls--locate-server-binary))
        (url (format "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-%s.tar.gz" jls-version)))
    (unless (file-exists-p dest)
      (progn
        (message (format "Creating destination directory %s" dest))
        (make-directory dest t)))
    (message (format "Downloading server module from %s. Please wait." url))
    (url-handler-mode t)
    (if (file-exists-p url)
        (progn
          (url-copy-file url dest-file)
          (message (format "Downloaded server module from %s to %s." url dest-file))
          (shell-command-to-string (format "tar -xvzf %s -C %s" (jls--locate-server-binary) jls-server-install-dir)))
      (error "Not found %s" url))))

(defun lsp--java-ls-command ()
  (let ((server-jar (jls--locate-server-jar))
        (server-config (jls--locate-server-config))
        (root-dir (lsp--java-get-root)))
    `( "java"
       "-Declipse.application=org.eclipse.jdt.ls.core.id1"
       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
       "-Dosgi.bundles.defaultStartLevel=4"
       "-Declipse.product=org.eclipse.jdt.ls.core.product"
       "-Dlog.protocol=true"
       "-Dlog.level=ALL"
       "-noverify"
       "-Xmx1G"
       "-jar"
       ,server-jar
       "-configuration"
       ,server-config
       "-data"
       ,root-dir)))

(defun lsp--java-get-root ()
  "TODO: use projectile directory"
  (let ((dir default-directory))
    (message "getting java root")
    (if (string= dir "/")
        (user-error (concat "Couldn't find java root, using:" dir))
      dir)))

(provide 'lsp-jls)


