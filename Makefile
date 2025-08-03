EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

.PHONY: all compile test clean install-deps test-with-deps compile-with-deps

all: test

# Compile with stubs
compile:
	$(BATCH) -l test-helper.el -f batch-byte-compile denote-projectile-notes.el

# Test with minimal stubs (no external dependencies)
test:
	$(BATCH) -l ert -l test-helper.el -l denote-projectile-notes.el -l denote-projectile-notes-test.el \
		-f ert-run-tests-batch-and-exit

# Install dependencies and run tests (requires internet connection)
install-deps:
	$(BATCH) --eval "(progn \
		(require 'package) \
		(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
		(package-initialize) \
		(package-refresh-contents) \
		(package-install 'denote) \
		(package-install 'projectile))"

# Compile with real dependencies
compile-with-deps: install-deps
	$(BATCH) --eval "(progn \
		(require 'package) \
		(package-initialize))" \
		-f batch-byte-compile denote-projectile-notes.el

# Test with real dependencies
test-with-deps: install-deps
	$(BATCH) --eval "(progn \
		(require 'package) \
		(package-initialize))" \
		-l ert -l denote-projectile-notes.el -l denote-projectile-notes-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

lint:
	$(BATCH) --eval "(progn \
		(require 'package) \
		(package-initialize) \
		(unless (package-installed-p 'package-lint) \
		  (package-refresh-contents) \
		  (package-install 'package-lint)) \
		(require 'package-lint) \
		(package-lint-batch-and-exit))" denote-projectile-notes.el

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"denote-projectile-notes.el\")"
