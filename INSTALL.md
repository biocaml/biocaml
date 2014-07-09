Biocaml Installation Instructions
=================================

## OPAM
Most users can install with opam by running:

    opam install lwt async biocaml

Both lwt and async are optional. If not installed, Biocaml_lwt or
Biocaml_async, respectively, will not be available. The above command
assures you get all possible features of Biocaml.


## Build From Source
Developers and users needing bleeding edge features that are not yet
released can install from source. First get the code from
[GitHub](https://github.com/biocaml/biocaml).

Then run

    omake

Optionally, you can run

    omake configure.om

which will generate a configuration file that you can further
modify. Change values as desired to selectively compile or not compile
certain features.

All variables in configure.om can also be specified on the command
line. For example:

    omake COMPILE_TESTS=false

will cause tests to not be compiled even if configure.om says they
should be. To see the configuration values in effect, run

    omake print_config

For example, try

    omake print_config COMPILE_TESTS=false

To clean up, do

    omake clean

To really clean all auto-generated files, do

    omake distclean


## Install From Source
To install and uninstall, you can run

    omake install
    omake uninstall

You likely don't want to do this if you use OPAM. This will cause the
package to get installed, but OPAM will not know about it. To install
from source while using OPAM, you'll want to use OPAM's `pin` feature.


## Documentation
To compile documentation, do

    omake doc


## OTAGS
There is support for
[otags](http://askra.de/software/otags/). Generate the `TAGS` file by
running

    omake TAGS

## Merlin

One can generate a `.merlin` file with the following command:

    omake merlinize

See [`the-lambda-church/merlin`](https://github.com/the-lambda-church/merlin)
for usage instructions on Merlin itself.


## Tests
Compile tests by doing

    omake test

and run them by doing

    _build/tests/biocaml_tests


## Benchmarks
Compile benchmarks by doing

    omake benchmarks

and run them by doing

    _build/benchmarks/biocaml_benchmarks
