;;; lsp-csharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jostein Kjønigsen, Saulius Menkevicius

;; Author: Saulius Menkevicius <saulius.menkevicius@fastmail.com>
;; Keywords:

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

;; lsp-csharp client

;;; Code:

(require 'lsp-mode)
(require 'gnutls)
(require 'f)

(defgroup lsp-csharp nil
  "LSP support for C#, using the Omnisharp Language Server.
Version 1.34.3 minimum is required."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OmniSharp/omnisharp-roslyn"))

(defcustom lsp-csharp-server-version
  "v1.34.8"
  "OmniSharp Roslyn server version to use
Will use this value to download server binaries from the
https://github.com/OmniSharp/omnisharp-roslyn/releases page."
  :group 'lsp-csharp
  :type '(string :tag "Version string starting with 'v', e.g. 'v1.34.8'"))

(defcustom lsp-csharp-server-install-dir
  (locate-user-emacs-file ".cache/omnisharp-roslyn/")
  "Installation directory for OmniSharp Roslyn server."
  :group 'lsp-csharp
  :type 'directory)

(defcustom lsp-csharp-server-path
  nil
  "The path to the OmniSharp Roslyn language-server binary.
Set this if you have the binary installed or have it built yourself."
  :group 'lsp-csharp
  :type '(string :tag "Single string value or nil"))

(defun lsp-csharp--installed-server-dir ()
  "The location of the auto-installed OmniSharp server."
  (f-join (expand-file-name lsp-csharp-server-install-dir)
          lsp-csharp-server-version))

(defun lsp-csharp--installed-server-bin ()
  "The location of OmniSharp executable/script for the installed server
to use to start the server."
  (f-join (lsp-csharp--installed-server-dir)
          (cond ((eq system-type 'windows-nt) "OmniSharp.exe")
                (t "run"))))

(defun lsp-csharp--server-package-filename ()
  "Returns name of tgz/zip file to be used for downloading the server
for auto installation.

On Windows we're trying to avoid a crash starting 64bit .NET PE binaries in
Emacs by using x86 version of omnisharp-roslyn on older (<= 26.4) versions
of Emacs. See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2017-06/msg00893.html"
  (cond ((eq system-type 'windows-nt)
         (if (and (string-match "^x86_64-.*" system-configuration)
                  (version<= "26.4" emacs-version))
             "omnisharp-win-x64.zip"
           "omnisharp-win-x86.zip"))
        ((eq system-type 'darwin)
         "omnisharp-osx.tar.gz")
        ((and (eq system-type 'gnu/linux)
	            (or (eq (string-match "^x86_64" system-configuration) 0)
	                (eq (string-match "^i[3-6]86" system-configuration) 0)))
         "omnisharp-linux-x64.tar.gz")
        (t "omnisharp-mono.tar.gz")))

(defun lsp-csharp--server-package-url ()
  "Returns URL to tgz/zip file to be used for downloading the server
for auto installation."
  (concat "https://github.com/OmniSharp/omnisharp-roslyn/releases/download"
          "/" lsp-csharp-server-version
          "/" (lsp-csharp--server-package-filename)))

(defun lsp-csharp--install-server (url filename reinstall)
  "Downloads and extracts a tgz/zip into the same directory."

  ;; remove the file if reinstall is set
  (if (and reinstall (f-exists-p filename))
      (f-delete filename))

  (lsp-csharp--download url filename)

  (let ((target-dir (f-dirname filename)))
    (message (format "lsp-csharp: extracting \"%s\" to \"%s\""
                     (f-filename filename)
                     target-dir))

    (lsp-csharp--extract filename target-dir)))

(defun lsp-csharp--get-or-install-server ()
  "Resolves path to server binary installed, otherwise, if not found
will ask the user if we can download and install it.

Returns location of script or a binary to use to start the server."
  (let ((installed-bin (lsp-csharp--installed-server-bin)))
    (if (file-exists-p installed-bin)
        installed-bin
      (if (yes-or-no-p "OmniSharp Roslyn Server is not installed. Do you want to download and install it now?")
          (let ((cache-dir (expand-file-name (locate-user-emacs-file ".cache/")))
                (o-r-dir (expand-file-name (locate-user-emacs-file ".cache/omnisharp-roslyn/")))
                (server-dir (lsp-csharp--installed-server-dir))
                (package-filename (lsp-csharp--server-package-filename))
                (package-url (lsp-csharp--server-package-url)))

            (f-mkdir cache-dir o-r-dir server-dir)

            (lsp-csharp--install-server package-url
                                        (f-join server-dir package-filename)
                                        nil)

            (unless (file-exists-p installed-bin)
              (error "Failed to auto-install the server; file \"%s\" was not found" installed-bin))

            installed-bin)
        (error "Server binary is required for LSP C# to work.")))))

(defun lsp-csharp--download (url filename)
  "Downloads file from URL as FILENAME. Will not do anything should
the file exist already."
  (unless (f-exists-p filename)
    (message (format "lsp-csharp: downloading from \"%s\"..." url))
    (let ((gnutls-algorithm-priority
	         (if (and (not gnutls-algorithm-priority)
		                (boundp 'libgnutls-version)
		                (>= libgnutls-version 30603)
		                (version<= emacs-version "26.2"))
	             "NORMAL:-VERS-TLS1.3"
	           gnutls-algorithm-priority)))
      (url-copy-file url filename nil))))

(defun lsp-csharp--extract (filename target-dir)
  "Extracts FILENAME which is a downloaded omnisharp-roslyn server
tarball or a zip file (based on a current platform) to TARGET-DIR."
  (cond
   ((eq system-type 'windows-nt)
    ;; on windows, we attempt to use powershell v5+, available on Windows 10+
    (let ((powershell-version (substring
                               (shell-command-to-string "powershell -command \"(Get-Host).Version.Major\"")
                               0 -1)))
      (if (>= (string-to-number powershell-version) 5)
          (call-process "powershell"
                        nil
                        nil
                        nil
                        "-command"
                        (concat "add-type -assembly system.io.compression.filesystem;"
                                "[io.compression.zipfile]::ExtractToDirectory(\"" filename "\", \"" target-dir "\")"))

        (message (concat "lsp-csharp: for automatic server installation procedure"
                         " to work on Windows you need to have powershell v5+ installed")))))

   ((or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    (call-process "tar" nil nil t "xf" filename "-C" target-dir))

   (t (error "lsp-csharp cannot extract \"%s\" on platform %s (yet)" filename system-type))))

(defun lsp-csharp--language-server-command ()
  "Resolves path and arguments to use to start the server.
Will attempt to install the server if it is not installed already for the
current platform."
  (if lsp-csharp-server-path
      (list lsp-csharp-server-path "-lsp")
    (list (lsp-csharp--get-or-install-server) "-lsp")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-csharp--language-server-command)
                  :major-modes '(csharp-mode)
                  :server-id 'csharp))

(provide 'lsp-csharp)
;;; lsp-csharp.el ends here
