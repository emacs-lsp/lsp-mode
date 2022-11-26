SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := test/test-helper.el $(shell ls test/lsp-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

# TODO: add 'checkdoc' and 'lint' here when they pass
unix-ci: clean build compile prepare_cpp_project unix-test
# TODO: add 'windows-test' back
windows-ci: clean build compile prepare_cpp_project

build:
	$(EASK) package
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

prepare_cpp_project:
	@echo "Setting up Sample C++ project with CMake and clangd"
	cd test/fixtures/SampleCppProject/ && mkdir -p build && cd build/ && cmake ..

test-downstream-pkgs:
	@echo "Test downstream packages..."
	./test/downstream/run.sh

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Package linting..."
	$(EASK) lint package

unix-test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) exec ert-runner -L . -L clients	$(LOAD-TEST-FILES) -t '!no-win' -t '!org'

windows-test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) exec ert-runner

docs:
	make -C docs/ generate

local-webpage: docs
	cp -rf README.md examples docs
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s CHANGELOG.org -t gfm -o docs/page/CHANGELOG.md
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/emacs-lsp/docs-image/docs-image

clean:
	$(EASK) clean-all
	rm -rf test/fixtures/SampleCppProject/build test/fixtures/SampleCppProject/.cache


.PHONY: build ci compile checkdoc lint test docs local-webpage clean
