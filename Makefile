SHELL := /bin/bash

all:
	cask build

test: all
	cask exec ert-runner -t '!no-win'

.PHONY: all test
