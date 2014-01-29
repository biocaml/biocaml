OPAM_DEPENDS="ocamlfind core.$CORE_VERSION camlzip xmlm pcre-ocaml cfstream omake lwt flow.0.3 async future.master"

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
export OPAMYES=1
ocaml -version
opam --version
opam --git-version

opam init 
eval `opam config env`
opam remote add biorepo git://github.com/biocaml/dev-opam-repo.git
opam update

opam install ${OPAM_DEPENDS}

omake
_build/tests/biocaml_tests
_build/benchmarks/biocaml_benchmarks -help
omake doc

# publish documentation
opam install travis-senv
mkdir ~/.ssh
chmod 700 ~/.ssh
travis-senv decrypt -p ID_RSA > ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa
rsync -a _build/doc/api biocaml@biocaml.org:biocaml.org/doc/dev/
