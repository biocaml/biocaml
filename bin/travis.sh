# install Ubuntu packages
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

sudo add-apt-repository -y ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra aspcud opam

# configure and view settings
export OPAMYES=1
ocaml -version
opam --version
opam --git-version

# install OCaml packages
opam init 
eval `opam config env`

opam install \
  ocamlfind \
  omake \
  camlzip \
  xmlm \
  core \
  cfstream \
  lwt \
  async \
  re

cd ~
git clone https://github.com/agarwal/future.git
opam repo add future-dev ~/future/etc/opam
opam pin future ~/future
opam install future

git clone https://github.com/smondet/flow.git
opam pin flow ~/flow
opam install flow

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
