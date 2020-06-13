SHELL := /usr/bin/env bash

CASK ?= cask

all:
	$(CASK) build

test: all
	$(CASK) exec ert-runner -t '!no-win' -t '!org'

docs:
	make -C docs/ generate

local-webpage: docs
	cp -rf README.md examples docs
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s CHANGELOG.org -t gfm -o docs/page/CHANGELOG.md
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material

.PHONY: all test local-webpage docs
