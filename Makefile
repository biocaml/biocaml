.PHONY: default
default: start-ocaml

################################################################################
# Developer commands
.PHONY: start-ocaml
start-ocaml:
	dune build @all @runtest -w

.PHONY: build
build:
	dune build @all

.PHONY: test
test:
	dune build @runtest @run_test_suite

.PHONY: utop
utop:
	dune utop

.PHONY: doc
doc:
	dune build @doc


################################################################################
# Clean commands
.PHONY: clean
clean:
	dune clean || rm -rf _build
	rm -rf _build.prev

.PHONY: distclean
distclean: clean
