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
	rm -f src/ext/xmlm-1.0.2/src/META src/lib/META src/lib/biocaml.mllib src/lib/doclib.odocl src/ext/xmlm-1.0.2/src/biocamlxmlm.mllib src/lib/libbiocaml.cli src/lib/libbiocaml_stubs.clib

TAGS:
	otags -o TAGS `find src -regex ".*\.ml"`

CURR_DIR := `basename $(CURDIR)`
.PHONY: dist
dist: clean distclean
	oasis setup
	cd .. ; tar czf $(CURR_DIR).tgz $(CURR_DIR)
	cd .. ; md5sum $(CURR_DIR).tgz > $(CURR_DIR).tgz.md5
