SHELL := /usr/bin/env bash

all:
	cask build

test: all
	cask exec ert-runner -t '!no-win'

local-webpage:
	cp -rf examples docs/examples
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s README.org -t gfm -o docs/README.md
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s doc/CHANGELOG.org -t gfm -o docs/CHANGELOG.md
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material

docs:
	make -C docs/ generate


.PHONY: all test local-webpage docs
