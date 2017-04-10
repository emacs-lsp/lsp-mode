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

;;;###autoload
(defcustom lsp-jls-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
  :group 'lsp-mode
  :risky t
  :type 'directory )

(defun lsp--jls-locate-server-jar ()
  "Return the jar file location of the language server.

The entry point of the language server is in `lsp-jls-server-install-dir'/plugins/org.eclipse.equinox.launcher_`version'.jar."
  (ignore-errors
    (let* ((plugindir (expand-file-name "plugins" lsp-jls-server-install-dir))
            (server-jar-filenames (directory-files plugindir t "org.eclipse.equinox.launcher_.*")))
      (if (not (= (length server-jar-filenames) 1))
        (message (format "Found more than one java language server entry points: %s" server-jar-filenames))
        (car server-jar-filenames)))))

(defun lsp--jls-locate-server-config ()
  "returns the server config based on OS"
  (let ( (config (cond
                  ((string-equal system-type "windows-nt") ; Microsoft Windows
                   "config_win")
                  ((string-equal system-type "darwin") ; Mac OS X
                   "config_mac")
                  ((string-equal system-type "gnu/linux") ; linux
                   "config_linux"))))
    (message (format "using config for %s" config))
    (expand-file-name config lsp-jls-server-install-dir)))

(defun lsp--java-ls-command ()
  (let ((server-jar (lsp--jls-locate-server-jar))
        (server-config (lsp--jls-locate-server-config))
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
    (if (string= dir "/")
        (user-error (concat "Couldn't find java root, using:" dir))
      dir)))

(provide 'lsp-jls)


