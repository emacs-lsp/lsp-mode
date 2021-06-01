SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

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

all:
	$(CASK) build

unix-build:
	$(CASK) install

# TODO: add 'checkdoc' and 'lint' here when they pass
unix-ci: clean unix-build unix-compile prepare_cpp_project unix-test test-downstream-pkgs

windows-ci: CASK=
windows-ci: clean windows-compile windows-test test-downstream-pkgs

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(LSP-FILES)

prepare_cpp_project:
	@echo "Setting up Sample C++ project with CMake and clangd"
	cd test/fixtures/SampleCppProject/ && mkdir -p build && cd build/ && cmake ..

windows-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		--eval '(setq emacs-lsp-ci t)' \
		-l $(WIN-BOOTSTRAP) \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(LSP-FILES)

test-downstream-pkgs:
	@echo "Test downstream packages..."
	@$(CASK) $(EMACS) -Q --batch \
		--eval '(setq emacs-lsp-ci t)' \
		-l $(WIN-BOOTSTRAP) \
		-l $(TEST-PKGS)

checkdoc:
	$(eval LOG := $(shell mktemp -d)/checklog.log)
	@touch $(LOG)

	@echo "checking doc..."

	@for f in $(LSP-FILES); do \
		$(CASK) $(EMACS) -Q --batch \
			--eval "(checkdoc-file \"$$f\")" \
			2>&1 | tee -a $(LOG); \
	done

	@if [ -s $(LOG) ]; then \
		echo ''; \
		exit 1; \
	else \
		echo 'checkdoc ok!'; \
	fi

lint:
	@echo "package linting..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . -L clients \
		--eval $(INIT) \
		--eval $(LINT) \
		$(LSP-FILES)

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . -L clients	$(LOAD-TEST-FILES) -t '!no-win' -t '!org'

windows-test:
	@echo "Testing..."
	@$(EMACS) -Q --batch \
		--eval '(setq emacs-lsp-ci t)' \
		-l $(WIN-BOOTSTRAP) \
		-L . -L clients \
		$(LOAD-TEST-FILES) \
		--eval "(ert-run-tests-batch-and-exit \
		'(and (not (tag no-win)) (not (tag org))))"

docs:
	make -C docs/ generate

local-webpage: docs
	cp -rf README.md examples docs
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s CHANGELOG.org -t gfm -o docs/page/CHANGELOG.md
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/emacs-lsp/docs-image/docs-image

clean:
	rm -rf .cask *.elc clients/*.elc
	rm -rf test/fixtures/SampleCppProject/build test/fixtures/SampleCppProject/.cache


.PHONY: all unix-build ci unix-compile windows-compile checkdoc lint unix-test windows-test docs local-webpage clean
