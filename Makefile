SHELL := /bin/bash

all:
	cask build

test: all
	cask exec ert-runner -t '!no-win'

docs:
	make -C doc/ all

.PHONY: all test
