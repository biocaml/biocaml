#!/bin/bash
set -ev

# install opam
sudo sh -c "echo 'deb http://download.opensuse.org/repositories/home:/ocaml/xUbuntu_12.10/ /' >> /etc/apt/sources.list.d/opam.list"
sudo apt-get -y update
sudo apt-get -y --force-yes install opam

# configure and view settings
export OPAMJOBS=2
export OPAMYES=1
opam --version
opam --git-version

# install OCaml packages
opam init --comp=$OCAML_VERSION --no-setup
eval `opam config env`

cd ~
git clone https://github.com/agarwal/future.git
opam pin add -n future ~/future
git clone https://github.com/smondet/flow.git
opam pin add -n flow ~/flow

opam install \
  ocamlfind \
  omake \
  camlzip \
  xmlm \
  core \
  cfstream \
  lwt \
  async \
  re \
  flow \
  future

# run tests
cd $TRAVIS_BUILD_DIR
omake
_build/tests/biocaml_tests
_build/benchmarks/biocaml_benchmarks -help
omake doc

# publish documentation
opam install travis-senv
mkdir -p ~/.ssh
chmod 700 ~/.ssh
travis-senv decrypt -p id_dsa > ~/.ssh/id_dsa
chmod 600 ~/.ssh/id_dsa
echo "Host biocaml.org" >> ~/.ssh/config
echo "  StrictHostKeyChecking no" >> ~/.ssh/config
chmod 600 ~/.ssh/config
rsync -a _build/doc/api biocaml@biocaml.org:biocaml.org/doc/dev/
