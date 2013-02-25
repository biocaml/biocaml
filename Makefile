.PHONY: build doc install-doc test install uninstall reinstall clean distclean

all: build

SETUP = ocaml setup.ml

VERSION=$(shell grep Version _oasis | cut -d' ' -f6)

src/lib/biocaml_about.ml:
	echo '(** Version string of the library: ["$(VERSION)"] *)' > $@
	echo 'let version = "$(VERSION)"' >> $@

build: setup.data src/lib/biocaml_about.ml
	$(SETUP) -build $(BUILDFLAGS)


_build/doclib/biocaml.css: src/doc/biocaml.css
	mkdir -p _build/doclib
	cp src/doc/biocaml.css $@

_build/doclib/index.html: setup.data build _build/doclib/biocaml.css _build/biohtml.cmo
	cp src/doc/figures/* _build/doclib/
	ocamlfind ocamldoc \
          -g _build/biohtml.cmo \
          -css-style biocaml.css \
	  -syntax camlp4o -package xmlm,zip,pcre,core,sexplib.syntax \
	  -charset UTF-8 -d _build/doclib/ -t "The Biocaml Library" \
	  -keep-code -colorize-code _build/src/lib/*.mli _build/src/lib/*.ml \
	  -sort -I _build/src/lib/. \
	  -intro src/doc/intro.txt

# This a "fast-compiling" sample of the documentation for testing purposes.
DOC_SAMPLES=_build/src/lib/biocaml_about.ml \
            _build/src/lib/biocaml_streamable.ml \
            _build/src/lib/biocaml_stream.mli _build/src/lib/biocaml_stream.ml \
            _build/src/lib/biocaml_transform.mli _build/src/lib/biocaml_transform.ml

_build/biohtml.cmo: src/odoc/biohtml.ml
	ocamlfind ocamlc -c src/odoc/biohtml.ml -o $@ -I +ocamldoc -I +compiler-libs


#ocamlc -c src/odoc/biohtml.ml -o $@ -I +ocamldoc  -I +ocamldoc/custom



doctest: setup.data _build/doclib/biocaml.css _build/biohtml.cmo
	mkdir -p _build/doclib
	cp src/doc/figures/* _build/doclib/
	ocamlfind ocamldoc \
          -g _build/biohtml.cmo \
          -css-style biocaml.css \
	  -syntax camlp4o -package xmlm,zip,pcre,core,sexplib.syntax \
	  -charset UTF-8 -d _build/doclib/ -t "The Biocaml Library" \
	  -keep-code -colorize-code $(DOC_SAMPLES) \
	  -sort -I _build/src/lib/. \
	  -intro src/doc/intro.txt

doc: _build/doclib/index.html

install-doc:
	@if [ "$(DOCDIR)" != "" ]; \
	then echo "Installing in $(DOCDIR)/biocaml"; \
	else echo "You should set DOCDIR" ; exit 1 ; \
	fi
	rm -fr $(DOCDIR)/biocaml
	mkdir -p $(DOCDIR)
	cp -r _build/doclib $(DOCDIR)/biocaml

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	ocamlfind remove biocaml
	$(SETUP) -install $(REINSTALLFLAGS)

install: reinstall

setup.ml: _oasis
	oasis setup -setup-update dynamic

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: setup.data

clean:
	$(RM) -fr _build

distclean: clean
	$(RM) setup.data setup.log
	$(RM) configure
	$(RM) src/lib/META
	$(RM) src/lib/libbiocaml_stubs.clib
	$(RM) src/lib/biocaml_about.ml
	$(RM) src/lib/biocaml.mllib
	$(RM) TAGS
	$(RM) setup.ml

TAGS_INCLUDE=-I $(shell ocamlfind query sexplib.syntax) -I $(shell ocamlfind query type_conv)
TAGS_LINK=-pa pa_type_conv.cma -pa pa_sexp_conv.cma

TAGS:
	otags $(TAGS_INCLUDE) $(TAGS_LINK) -o TAGS `find src -regex ".*\.ml"`

CURR_DIR=$(shell basename $(CURDIR))
PKG=biocaml
VERSION=$(shell grep Version _oasis | cut -d' ' -f6)
PKG_VERSION=$(PKG)-$(VERSION)
DIST_FILES=Changes INSTALL LICENSE Makefile README.md _oasis _tags myocamlbuild.ml setup.ml src
.PHONY: dist
dist: distclean
	oasis setup
	perl -pi -e 's#$(HOME)##g' myocamlbuild.ml setup.ml
	cd .. ; mv $(CURR_DIR) $(PKG_VERSION); tar czf $(PKG_VERSION).tgz $(patsubst %,$(PKG_VERSION)/%,$(DIST_FILES)); mv $(PKG_VERSION) $(CURR_DIR)
	cd .. ; md5sum $(PKG_VERSION).tgz > $(PKG_VERSION).tgz.md5
	rm -rf doc
