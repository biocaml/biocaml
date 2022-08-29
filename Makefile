SHELL=/bin/bash
ESY=npx esy@0.6.12
DUNE=$(ESY) dune

.PHONY: all
all: build-watch

################################################################################
# Dependency management

.PHONY: install-deps
install-deps:
	$(ESY) 

################################################################################
# Developer build commands

.PHONY: build-watch
build-watch:
	$(DUNE) build @runtest -w

.PHONY: build
build:
	$(DUNE) build @all

.PHONY: utop
utop:
	$(DUNE) utop

.PHONY: doc
doc:
	$(DUNE) build @doc

################################################################################
# Run tests

.PHONY: test
test:
	$(DUNE) build @runtest
	$(DUNE) build @run_test_suite

################################################################################
# Clean commands

.PHONY: clean
clean:
	$(DUNE) clean || rm -rf _build
	rm -rf _build.prev

.PHONY: distclean
distclean: clean
	rm -rf _esy
	rm -rf node_modules