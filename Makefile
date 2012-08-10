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
PKG_VERSION=$(PKG)-$(VERSION)
INSTALL_FILES=Changes INSTALL LICENSE Makefile README.md TAGS _oasis _tags configure myocamlbuild.ml setup.ml doc src
.PHONY: dist
dist:
	oasis setup
	perl -pi -e 's#$(HOME)##g' myocamlbuild.ml setup.ml
	make doc
	mkdir doc
	mv _build/src/lib/doclib.docdir doc/html
	make TAGS
	cd .. ; mv $(CURR_DIR) $(PKG_VERSION); tar czf $(PKG_VERSION).tgz $(patsubst %,$(PKG_VERSION)/%,$(INSTALL_FILES)); mv $(PKG_VERSION) $(CURR_DIR)
	cd .. ; md5sum $(PKG_VERSION).tgz > $(PKG_VERSION).tgz.md5
	rm -rf doc
