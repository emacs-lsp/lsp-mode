SHELL := /bin/bash

all:
	cask build

test: all
	cask exec ert-runner

.PHONY: all test
