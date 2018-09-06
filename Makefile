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

doc:
	make doc

