;;; lsp-graphql-test.el --- unit tests for lsp-graphql.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 ncaq <ncaq@ncaq.net>

;; Author: ncaq <ncaq@ncaq.net>
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

;;

;;; Code:

(require 'ert)
(require 'lsp-graphql)

(ert-deftest lsp-graphql-activate-p-based-on-file-extension ()
  (should (lsp-graphql-activate-p "abc.ts"))
  (should (lsp-graphql-activate-p "abc.js"))
  (should (lsp-graphql-activate-p "abc.jsx"))
  (should (lsp-graphql-activate-p "abc.tsx"))
  (should (lsp-graphql-activate-p "abc.vue"))
  (should (lsp-graphql-activate-p "abc.graphql"))
  (should (lsp-graphql-activate-p "abc.graphqls"))
  (should (lsp-graphql-activate-p "abc.gql"))
  (should-not (lsp-graphql-activate-p "abc.tsxx"))
  (should-not (lsp-graphql-activate-p "abc.jss"))
  (should-not (lsp-graphql-activate-p "abc.tsp"))
  (should-not (lsp-graphql-activate-p "js.hs"))
  (should-not (lsp-graphql-activate-p "/path/to/graphql/test.go")))

;;; lsp-graphql-test.el ends here
