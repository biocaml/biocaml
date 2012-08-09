# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

veryclean: clean distclean
	rm -f TAGS configure myocamlbuild.ml setup.ml
	rm -f src/ext/xmlm-1.0.2/src/META src/lib/META src/lib/biocaml.mllib src/lib/doclib.odocl src/ext/xmlm-1.0.2/src/biocamlxmlm.mllib src/lib/libbiocaml_stubs.clib

TAGS:
	otags -o TAGS `find src -regex ".*\.ml"`

CURR_DIR=$(shell basename $(CURDIR))
PKG=biocaml
VERSION=$(shell grep Version _oasis | cut -d' ' -f6)
TARBALL_NAME=$(PKG)-$(VERSION)
INSTALL_FILES=Changes INSTALL LICENSE Makefile README.md _oasis _tags configure myocamlbuild.ml setup.ml doc src
.PHONY: dist
dist:
	rm -f ../biocaml.tgz ../biocaml.tgz.md5
	oasis setup
	make doc
	mkdir doc
	mv _build/src/lib/doclib.docdir doc/html
	cd .. ; tar czf $(TARBALL_NAME).tgz $(patsubst %,$(CURR_DIR)/%,$(INSTALL_FILES))
	cd .. ; md5sum $(TARBALL_NAME).tgz > $(TARBALL_NAME).tgz.md5
	rm -rf doc

foo:
	echo $(CURR_DIR)
