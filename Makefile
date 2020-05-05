SHELL := /usr/bin/env bash

all:
	cask build

test: all
	cask exec ert-runner -t '!no-win'

docs:
	make -C docs/ generate

local-webpage: docs
	cp -rf examples docs/examples
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s README.org -t gfm -o docs/README.md
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s CHANGELOG.org -t gfm -o docs/CHANGELOG.md
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material

.PHONY: all test local-webpage docs
