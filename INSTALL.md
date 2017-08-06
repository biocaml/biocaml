Biocaml Installation Instructions
=================================

## OPAM
Most users can install with opam by running:

    opam install biocaml

`biocaml` has several optional dependencies, which will render some
additional modules available. If `core` (resp. `async`, `lwt`) is
installed, `Biocaml_unix` (resp. `Biocaml_lwt` or `Biocaml_async`)
will be available.

## Build From Source
Developers and users needing bleeding edge features that are not yet
released can install from source. First get the code from
[GitHub](https://github.com/biocaml/biocaml) or pin `biocaml` in opam:

    opam pin add biocaml --dev-repo
	opam source biocaml

Then run

    jbuilder build

## Merlin

A `.merlin` is generated automatically during compilation. See
[`the-lambda-church/merlin`](https://github.com/the-lambda-church/merlin)
for usage instructions on Merlin itself.


## Tests
To compile tests, install `oUnit` and run `make` again. Tests can then be run by doing

    jbuilder runtest

## Benchmarks

To compile benchmarks, install `core_bench` and run `make`
again. Get the list of available benchmarks by running

	./biocaml_run_benchmarks.byte

and run one of them with

	./biocaml_run_benchmarks.byte BENCH OPTS

