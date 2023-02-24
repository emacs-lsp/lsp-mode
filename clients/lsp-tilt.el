;;; lsp-tilt.el --- tilt LSP                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords: tools

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

;; Using tilt mode from https://github.com/Konubinix/tilt-mode

;;; Code:

(require 'lsp-mode)

(lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("tilt" "lsp" "start"))
        :activation-fn (lsp-activate-on "tiltfile")
        :server-id 'tiltfile))

(provide 'lsp-tilt)
;;; lsp-tilt.el ends here
