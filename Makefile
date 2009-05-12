lib=bioCaml

############################################################
#
# Compiling library, documentation, and applications
#
all: $(lib) apps

$(lib):
	cd src; ocamlbuild $(lib).cma $(lib).cmxa

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: clean-doc doc/html/base doc/html/$(lib)

apps:
	cd src; ocamlbuild $(patsubst src/%.ml,%.native,$(wildcard src/app/*.ml))


############################################################
#
# Installing and uninstalling
#
install: all uninstall
	cd src/_build; ocamlfind install biocaml ../META $(patsubst %,$(lib).%,cmi a cma cmxa)

uninstall:
	ocamlfind remove biocaml


############################################################
#
# Clean up
#

# delete compiled code
clean:
	cd src; ocamlbuild -clean

# delete compiled documentation
clean-doc:
	rm -rf doc/html/*

# delete all automatically generated files
clean-all: clean-doc clean

# clean everything and uninstall
fresh: clean-all uninstall

.PHONY: all $(lib) doc apps install uninstall clean clean-doc clean-all fresh
