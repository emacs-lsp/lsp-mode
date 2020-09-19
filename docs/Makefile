SHELL := /usr/bin/env bash

deps:
	cask

generate: deps
	@echo "Generating..."

	@cask emacs -Q --batch \
		-L ../ \
		-L ../clients \
		-l lsp-doc.el \
		-f lsp-doc-generate

.PHONY: deps generate
