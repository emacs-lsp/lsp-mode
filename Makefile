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

all:
	$(CASK) unix-build

unix-build:
	$(CASK) install

# TODO: add 'checkdoc' and 'lint' here when they pass
unix-ci: clean unix-build unix-compile unix-test

windows-ci: CASK=
windows-ci: clean windows-compile windows-test

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el clients/*.el

windows-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
	    -l test/windows-bootstrap.el \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el clients/*.el

checkdoc:
	$(eval LOG := $(shell mktemp -d)/checklog.log)
	@touch $(LOG)

	@echo "checking doc..."

	@for f in *.el ; do \
		$(CASK) $(EMACS) -Q --batch \
			-L . \
			--eval "(checkdoc-file \"$$f\")" \
			*.el 2>&1 | tee -a $(LOG); \
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
		*.el

unix-test:
	$(CASK) exec ert-runner -L . -L clients  -t '!no-win' -t '!org'

windows-test:
	@$(EMACS) -Q --batch \
		-L . -L clients \
		--eval "(require 'ert)"
		-f ert-runner/run

docs:
	make -C docs/ generate

local-webpage: docs
	cp -rf README.md examples docs
	docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/core:2.9 -s CHANGELOG.org -t gfm -o docs/page/CHANGELOG.md
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material

clean:
	rm -rf .cask *.elc clients/*.elc

.PHONY: all unix-build	 ci unix-compile windows-compile checkdoc lint unix-test windows-test docs local-webpage clean
