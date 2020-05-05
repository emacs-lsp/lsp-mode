SHELL := /usr/bin/env bash

generate:
	@echo "Generating..."
	@cask emacs -Q --batch \
		-L ../ \
		-l lsp-doc.el \
		-f lsp-doc-generate

.PHONY: generate
