;;; lsp-lua.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 E. Alexander Barbosa

;; Author: E. Alexander Barbosa <elxbarbosa@outlook.com>
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

;; LSP Clients for the Lua Programming Language

;;; Code:

(require 'lsp-mode)
(require 'f)
(require 'files)

(defgroup lsp-emmy-lua nil
  "Lua LSP client, provided by the EmmyLua Language Server."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/EmmyLua/EmmyLua-LanguageServer"))

(defcustom lsp-clients-emmy-lua-java-path "java"
  "Java Runtime binary location."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-jar-path (f-join lsp-server-install-dir "EmmyLua-LS-all.jar")
  "Emmy Lua language server jar file."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-emmy-lua-args '("-jar")
  "Arguments to the Lua Language server."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type  '(repeat string))

(defcustom lsp-clients-emmy-lua-command `(,lsp-clients-emmy-lua-java-path
                                          ,@lsp-clients-emmy-lua-args
                                          ,lsp-clients-emmy-lua-jar-path)
  "Final command to call the Lua Language server."
  :group 'lsp-emmy-lua
  :version "7.1"
  :risky t
  :type '(repeat string))

(defun lsp-clients-emmy-lua-test ()
  "Test the Emmy Lua binaries and files."
  (and (executable-find lsp-clients-emmy-lua-java-path)
       (f-exists? lsp-clients-emmy-lua-jar-path)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-emmy-lua-command
                                        #'lsp-clients-emmy-lua-test)
  :major-modes '(lua-mode)
  :server-id 'emmy-lua
  :priority -1
  :notification-handlers (lsp-ht ("emmy/progressReport" #'ignore))))


;;; lua-language-server
(defgroup lsp-lua-language-server nil
  "Lua LSP client, provided by the Lua Language Server."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/sumneko/lua-language-server"))

(defcustom lsp-clients-lua-language-server-install-dir (f-join lsp-server-install-dir "lua-language-server/")
  "Installation directory for Lua Language Server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/Linux/lua-language-server")
  "Location of Lua Language Server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
  "Location of Lua Language Server main.lua."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type 'file)

(defcustom lsp-clients-lua-language-server-args '("-E")
  "Arguments to run the Lua Language server."
  :group 'lsp-lua-language-server
  :version "7.1"
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-lua-language-server-command `(,lsp-clients-lua-language-server-bin
                                                     ,@lsp-clients-lua-language-server-args
                                                     ,lsp-clients-lua-language-server-main-location)
  "Command to start Lua Language server."
  :group 'lsp-lua-language-server
  :type '(repeat string))


(defun lsp-clients-lua-language-server-test ()
  "Test Lua language server binaries and files."
  (and (f-exists? lsp-clients-lua-language-server-main-location)
       (f-exists? lsp-clients-lua-language-server-bin)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-lua-language-server-command
                                        #'lsp-clients-lua-language-server-test)
  :major-modes '(lua-mode)
  :priority -2
  :server-id 'lua-language-server))


;;; lua-lsp
(defgroup lsp-lua-lsp nil
  "Lua LSP client, provided by the Lua-Lsp."
  :group 'lsp-mode
  :version "7.1"
  :link '(url-link "https://github.com/Alloyed/lua-lsp"))

(defcustom lsp-clients-luarocks-bin-dir (f-join (getenv "HOME") ".luarocks/bin/")
  "LuaRocks bin directory."
  :group 'lsp-lua-lsp
  :version "7.1"
  :risky t
  :type 'directory)

(defcustom lsp-clients-lua-lsp-server-install-dir (f-join lsp-clients-luarocks-bin-dir "lua-lsp")
  "Installation directory for Lua-Lsp Language Server."
  :group 'lsp-lua-lsp
  :version "7.1"
  :risky t
  :type 'file)

(defun lsp-clients-lua-lsp-test ()
  "Test Lua-lsp language server files."
  (and (f-exists? lsp-clients-lua-lsp-server-install-dir)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-clients-lua-lsp-server-install-dir
                                        #'lsp-clients-lua-lsp-test)
  :major-modes '(lua-mode)
  :priority -3
  :server-id 'lsp-lua-lsp))


(provide 'lsp-lua)
;;; lsp-lua.el ends here
