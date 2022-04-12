SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

INIT="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (package-refresh-contents))"

LINT="(progn \
		(unless (package-installed-p 'package-lint) \
		  (package-install 'package-lint)) \
		(require 'package-lint) \
		(package-lint-batch-and-exit))"

LSP-FILES := lsp-protocol.el lsp-mode.el lsp.el lsp-completion.el \
		lsp-diagnostics.el lsp-lens.el lsp-modeline.el \
		$(wildcard clients/*.el)

WIN-BOOTSTRAP=test/windows-bootstrap.el
TEST-PKGS=test/test-packages.el

TEST-FILES := test/test-helper.el $(shell ls test/lsp-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
	$(EASK) package
	$(EASK) install

# TODO: add 'checkdoc' and 'lint' here when they pass
ci: clean build compile prepare_cpp_project test test-downstream-pkgs

compile:
	@echo "Compiling..."
	$(EASK) compile

prepare_cpp_project:
	@echo "Setting up Sample C++ project with CMake and clangd"
	cd test/fixtures/SampleCppProject/ && mkdir -p build && cd build/ && cmake ..

test-downstream-pkgs:
	@echo "Test downstream packages..."
	mv ./dist/ ./test/downstream/dist/
	./test/downstream/run.sh

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint

test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	#$(EASK) exec ert-runner -L . -L clients	$(LOAD-TEST-FILES) -t '!no-win' -t '!org'

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
