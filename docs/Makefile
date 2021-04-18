SHELL := /usr/bin/env bash

deps:
	cask install

generate: deps
	@echo "Generating docs..."

	@cask emacs -Q --batch \
		-L ../ \
		-L ../clients \
		-l lsp-doc.el \
		-f lsp-doc-generate

.PHONY: deps generate
