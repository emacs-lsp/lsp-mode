SHELL := /usr/bin/env bash

deps:
	eask install

generate: deps
	@echo "Generating docs..."

	@eask emacs -Q --batch \
		-L ../ \
		-L ../clients \
		-l lsp-doc.el \
		-f lsp-doc-generate

.PHONY: deps generate
