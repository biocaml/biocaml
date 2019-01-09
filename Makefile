.PHONY: all
all:
	dune build @install
	dune build @runtest

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune build @runtest
	dune build @run_test_suite

doc:
	make doc

