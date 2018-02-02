SHELL := /bin/bash

all:
	cask build

test: all test/test_server.go
	$(MAKE) --directory=test test_server
	cask exec ert-runner

.PHONY: all test
