;;; lsp-prisma.el --- LSP client for Prisma Schema Language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ChuYanLon
;; Copyright (C) 2026 emacs-lsp maintainers

;; Author: ChuYanLon <a1008611abcd@126.com>
;; Keywords: languages, prisma

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

;; LSP client for Prisma Schema Language, using prisma-language-server
;; from @prisma/language-server (https://github.com/prisma/language-tools).

;;; Code:

(require 'lsp-mode)

(defgroup lsp-prisma nil
  "LSP support for Prisma Schema Language."
  :group 'lsp-mode
  :link '(url-link "https://github.com/prisma/language-tools"))

(lsp-dependency 'prisma-language-server
                '(:system "prisma-language-server")
                '(:npm :package "@prisma/language-server"
                       :path "prisma-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(lsp-package-path 'prisma-language-server)
                                       "--stdio")))
                  :activation-fn (lambda (filename _mode)
                                   (string= (file-name-extension filename) "prisma"))
                  :language-id "prisma"
                  :server-id 'prisma-ls
                  :multi-root t
                  :download-server-fn
                  (lambda (_client callback error-callback _update?)
                    (lsp-package-ensure 'prisma-language-server
                                        callback error-callback))))

(add-to-list 'lsp-language-id-configuration '(prisma-mode . "prisma"))

(lsp-consistency-check lsp-prisma)

(provide 'lsp-prisma)
;;; lsp-prisma.el ends here
