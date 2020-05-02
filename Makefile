SHELL := /usr/bin/env bash

all:
	cask build

test: all
	cask exec ert-runner -t '!no-win'

readme2docs:
	pandoc -s README.org -t gfm -o docs/README.md

docs:
	make -C doc/ all

.PHONY: all test readme2docs
