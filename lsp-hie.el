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

(require 'haskell)

(defun lsp--haskell-get-root ()
  "TODO: use projectile directory"
  (let ((dir (lsp--session-cabal-dir)))
    (if (string= dir "/")
        (user-error (concat "Couldn't find cabal file, using:" dir))
      dir)))

(defun lsp--session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "" ;; no cabal file, use directory only
                      )))
    (progn
      (message "cabal-dir: %s" cabal-dir)
      cabal-dir)))

(lsp-define-client 'haskell-mode "haskell" 'stdio #'lsp--haskell-get-root
  ;; :command '("hie" "--lsp" "-d" "-l" (make-temp-file "hie" nil ".log"))
  :command '("hie" "--lsp" "-d" "-l" "/tmp/hie.log")
  :name "Haskell Language Server")

(provide 'lsp-hie)
